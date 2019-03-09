import_package('data.table', attach = T)

demo_preprocessing = import('preprocessing/demographics')
questionnaire_preprocessing = import('preprocessing/questionnaire')
examination_preprocessing = import('preprocessing/examination')
dietary_preprocessing = import('preprocessing/dietary')

read_and_preprocess_data = function(years) {
  demographics = demo_preprocessing$read_and_preprocess_data(years = years)
  questionnaire = questionnaire_preprocessing$read_and_preprocess_data(years = years)
  examination = examination_preprocessing$read_and_preprocess_data(years = years)
  #dietary = dietary_preprocessing$read_and_preprocess_data(years = years)
  
  # data = demographics %>%
    # merge(questionnaire, on = 'SEQN') %>%
    # merge(examination, on = 'SEQN') %>%
    # merge(dietary, on = 'SEQN')
  
  data = demographics %>%
    merge(questionnaire, on = 'SEQN') %>%
    merge(examination, on = 'SEQN')
  
  # Drop any drug categories which are reported by less than 1% of ppl who take medications
  # Note logic placed here since we have already dropped age < 20
  drug_category_frac = data[total_drug_count>0, lapply(.SD, sum), .SDcols = names(data)[str_detect(names(data), '^drug_category_')]] / nrow(data[total_drug_count>0])
  least_frequent_drug_categories = names(drug_category_frac)[c(drug_category_frac < .01)]
  data[, c(least_frequent_drug_categories) := NULL]
  
  # Remove join key
  data[, SEQN := NULL]
  
  print(paste('data size before dropping null obs', nrow(data)))
  # Drop any row with any missing value
  data = data[complete.cases(data)]
  print(paste('data size after dropping null obs', nrow(data)))
  
  return(data)
}
