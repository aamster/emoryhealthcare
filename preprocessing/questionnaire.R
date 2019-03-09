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

###### 
# Recoding
######
recode_hospital_patient_last_year = function(code) {
  x = case_when(
    code == 1 ~ 'Yes',
    code == 2 ~ 'No'
  )
  return (factor(x, levels = c('Yes', 'No')))
}

preprocess_meds = function(med, med_info, hospital_util) {
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
  
  seqn_no_meds = setdiff(hospital_util$SEQN, med_$SEQN)
  no_meds = data.table(seqn = seqn_no_meds, drug_category=NA, drug_count=0, total_drug_count=0)
  med_ = rbindlist(list(med_, no_meds))
  
  # top_100 = (
  #   melt(med, id.vars = c('SEQN'), measure.vars = c('RXDRSD1', 'RXDRSD2', 'RXDRSD3'), value.name = 'reason')
  #   [reason != ""]
  #   [, .(count = .N), by = .(reason)]
  #   [order(-count)]
  #   [1:100, .(SEQN, reason)]
  # )
  # setnames(med, 'RXDRSD1', 'reason')
  
  # med_ = med %>%
  #   merge(top_100, by = 'reason')
  med_wide = dcast(med_, SEQN ~ drug_category, fill=0, value.var = 'drug_count')
  med_wide[, "NA" := NULL]
  med_wide[, total_drug_count := rowSums(.SD), .SDcols = names(med_wide)[names(med_wide) != "SEQN"]]
  
  return(med_wide)
}

read_and_preprocess_data = function(years) {
  data = list()
  for (year in years) {
    print(year)
    base_dir = str_interp('~/emoryhealthcare-project/data/${year}/Questionnaire')
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
    med = preprocess_meds(med, med_info, hospital_util)

    data_ = hospital_util %>%
      merge(med, all = T, by = 'SEQN')
    
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

# med = fread('~/emoryhealthcare-project/data/2015-2016/Questionnaire/RXQ_RX_I.csv')
# med_info = fread('~/emoryhealthcare-project/data/2015-2016/Questionnaire/RXQ_DRUG.csv')
# hospital_util = fread('~/emoryhealthcare-project/data/2015-2016/Questionnaire/HUQ_I.csv')
# preprocess_meds(med, med_info, hospital_util)
# read_and_preprocess_data(years = c(
#   '2015-2016', '2013-2014', '2011-2012', '2009-2010',
#   '2007-2008', '2005-2006', '2003-2004', '2001-2002',  '1999-2000'
# ))