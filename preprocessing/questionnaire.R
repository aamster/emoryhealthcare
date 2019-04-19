library(modules)

import_package('data.table', attach = T)
import_package('dplyr', attach = T)
import_package('stringr', attach = T)
# import_package('reshape2', attach = T)
import_package('magrittr', attach = T)

preprocessing_util = import('preprocessing/util')

HOSPITAL_UTILIZATION_COLS = list(
  list(old = 'HUQ071', new = 'questionnaire_overnight_hospital_patient_in_last_year'),
  list(old = 'HUD070', new = 'questionnaire_overnight_hospital_patient_in_last_year')
)

SMOKING_COLS = list(
  list(old = 'SMQ040', new = 'smoke_cigarettes')
)

###### 
# Recoding
######
recode_hospital_patient_last_year = function(code) {
  x = case_when(
    code == 1 ~ 'Yes',
    code == 2 ~ 'No',
    T ~ 'No'
  )
  return (factor(x, levels = c('Yes', 'No')))
}

recode_smoking = function(code) {
  x = case_when(
    code == 1 ~ 'Yes',
    T ~ 'No'
  )
  return (factor(x, levels = c('Yes', 'No')))
}

preprocess_med_category = function(med, med_info, hospital_util) {
  med_ = med[, .(SEQN, RXDDRGID)] %>% 
    merge(med_info[, .(RXDDRGID, RXDDCN1B, RXDDCN2B, RXDDCN3B, RXDDCN4B)], on = 'RXDDRGID')
  
  med_ = melt(
    med_, 
    id.vars = c('SEQN'), 
    measure.vars = c('RXDDCN1B', 'RXDDCN2B', 'RXDDCN3B', 'RXDDCN4B'), 
    value.name = 'drug_category'
  )
  
  med_ = med_[drug_category != ""]
  med_[, drug_category := paste0('drug_category_', drug_category)]
  med_ = med_[, .(drug_count = .N), by = .(SEQN, drug_category)]
  med_[, total_drug_count := sum(drug_count), by = .(SEQN)]
  med_[, drug_count := ifelse(drug_count > 0, 1, 0)]
  
  seqn_no_meds = setdiff(hospital_util$SEQN, med_$SEQN)
  no_meds = data.table(SEQN = seqn_no_meds, drug_category=NA, drug_count=0, total_drug_count=0)
  med_ = rbindlist(list(med_, no_meds))
  
  med_wide = dcast(med_, SEQN ~ drug_category, fill=0, value.var = 'drug_count')
  med_wide[, "NA" := NULL]
  
  return(med_wide)
}

preprocess_generic_drug_names = function(med) {
  if ('RXDDRUG' %in% names(med)) {
    drug_col = 'RXDDRUG'
    med_ = med[!drug_col %in% c('55555', '77777', '99999')]
    med_[, RXDDRUG := paste0('drug_name_', RXDDRUG)]
  } else {
    drug_col = 'RXD240B'
    med_ = med[!drug_col %in% c('55555', '77777', '99999')]
    med_[, RXD240B := paste0('drug_name_', RXD240B)]
  }
 
  
  
  
  formula = as.formula(paste0('SEQN ~ ', drug_col))
  med_wide = dcast(med_, formula, fill=0)
  med_wide[, "V1" := NULL]
  return(med_wide)
}

preprocess_meds = function(med, med_info, hospital_util, use_drug_category=T,  drug_category = 2) {
  if (use_drug_category) {
    med_ = preprocess_med_category(med, med_info, hospital_util)

  } else {
    med_ = preprocess_generic_drug_names(med)
  }

  med_[, total_drug_count := rowSums(.SD), .SDcols = names(med_)[names(med_) != "SEQN"]]
  
  return(med_)
}

read_and_preprocess_data = function(years, use_drug_category = T) {
  data = list()
  for (year in years) {
    print(year)
    base_dir = str_interp('~/emoryhealthcare/data/${year}/Questionnaire')
    files = list.files(base_dir)
    
    file_name = files[str_detect(files, 'HUQ_*')][1]
    path = str_interp('${base_dir}/${file_name}')
    hospital_util = preprocessing_util$read_and_preprocess_data(path, HOSPITAL_UTILIZATION_COLS)
    hospital_util[, `:=`(
      questionnaire_overnight_hospital_patient_in_last_year = recode_hospital_patient_last_year(questionnaire_overnight_hospital_patient_in_last_year)
    )]
    
    file_name = files[str_detect(files, 'RXQ_RX_*')][1]
    path = str_interp('${base_dir}/${file_name}')
    med = fread(path)
    
    file_name = files[str_detect(files, 'RXQ_DRUG.csv')][1]
    path = str_interp('${base_dir}/${file_name}')
    med_info = fread(path)
    med = preprocess_meds(med, med_info, hospital_util, use_drug_category = use_drug_category)
    
    file_name = files[str_detect(files, 'SMQ_*')][1]
    path = str_interp('${base_dir}/${file_name}')
    smoking = preprocessing_util$read_and_preprocess_data(path, SMOKING_COLS)
    smoking[, smoke_cigarettes := recode_smoking(smoke_cigarettes)]

    data_ = hospital_util %>%
      merge(med, all = T, by = 'SEQN') %>%
      merge(smoking, all = T, by = 'SEQN')
    
    data = append(data, list(data_))
  }
  
  data = rbindlist(data, use.names = T, fill=T)
  
  # Some dts will have cols missing in other dts
  # rbindlist will fill these rows with NA
  # set these vals to 0 instead
  for(j in seq_along(names(data)[!names(data) %in% c("SEQN", "questionnaire_overnight_hospital_patient_in_last_year")])+2){
    set(data, i = which(is.na(data[[j]])), j = as.integer(j), value = 0L)
  }
  
  return (data)
}

# med = fread('~/emoryhealthcare/data/2015-2016/Questionnaire/RXQ_RX_I.csv')
# med_info = fread('~/emoryhealthcare/data/2015-2016/Questionnaire/RXQ_DRUG.csv')
# hospital_util = fread('~/emoryhealthcare/data/2015-2016/Questionnaire/HUQ_I.csv')
# preprocess_meds(med, med_info, hospital_util, use_drug_category = F)
# read_and_preprocess_data(  years = c(
# '2015-2016'
# ))

get_data_for_clustering = function() {
  years = c(
    '2015-2016', '2013-2014', '2011-2012', '2009-2010',
    '2007-2008', '2005-2006', '2003-2004', '2001-2002',  '1999-2000'
  )
  
  data = read_and_preprocess_data(years = years, use_drug_category = F)
  
  demo_preprocessing = import('preprocessing/demographics')
  demographics = demo_preprocessing$read_and_preprocess_data(years = years)
  
  data = demographics %>%
    merge(data, on = 'SEQN')
  
  # Remove children
  data = data[demographics_age >= 20]
  
  fwrite(data, 'data/data_questionnaire_for_clustering.csv')
}

# get_data_for_clustering()
