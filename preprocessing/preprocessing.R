import_package('data.table', attach = T)

demo_preprocessing = import('preprocessing/demographics')
questionnaire_preprocessing = import('preprocessing/questionnaire')
examination_preprocessing = import('preprocessing/examination')
dietary_preprocessing = import('preprocessing/dietary')
lab_preprocessing = import('preprocessing/laboratory')

read_and_preprocess_data = function(years) {
  demographics = demo_preprocessing$read_and_preprocess_data(years = years)
  questionnaire = questionnaire_preprocessing$read_and_preprocess_data(years = years)
  examination = examination_preprocessing$read_and_preprocess_data(years = years)
  #dietary = dietary_preprocessing$read_and_preprocess_data(years = years)
  lab = lab_preprocessing$read_and_preprocess_data(years = years)
  
  data = demographics %>%
    merge(questionnaire, on = 'SEQN') %>%
    merge(examination, on = 'SEQN') %>%
    merge(lab, on = 'SEQN')

  return(data)
}
