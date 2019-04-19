import_package('data.table', attach = T)
import_package('dplyr', attach = T)
import_package('stringr', attach = T)

preprocessing_util = import('preprocessing/util')

BLOOD_PRESSURE_COLS = list(
  list(old = 'BPXSY1', new = 'examination_systolic_blood_pressure'),
  list(old = 'BPXDI1', new = 'examination_diastolic_blood_pressure')
)

BODY_MEASURES_COLS = list(
  list(old = 'BMXBMI', new = 'examination_body_measures_bmi')
)

read_and_preprocess_data = function(years) {
  data = list()
  
  for (year in years) {
    base_dir = str_interp('~/emoryhealthcare/data/${year}/Examination')
    files = list.files(base_dir)
    
    file_name = files[str_detect(files, 'BPX_*')][1]
    path = str_interp('${base_dir}/${file_name}')
    blood_pressure = preprocessing_util$read_and_preprocess_data(path, BLOOD_PRESSURE_COLS)
    
    file_name = files[str_detect(files, 'BMX_*')][1]
    path = str_interp('${base_dir}/${file_name}')
    body_measures = preprocessing_util$read_and_preprocess_data(path, BODY_MEASURES_COLS)
    
    data_ = blood_pressure %>% 
      merge(body_measures, all = T, by = 'SEQN')
    
    data = append(data, list(data_))
  }
  
  data = rbindlist(data, use.names = T)
  return (data)
}

# read_and_preprocess_data(  years = c(
#   '2015-2016', '2013-2014', '2011-2012', '2009-2010',
#   '2007-2008', '2005-2006', '2003-2004', '2001-2002',  '1999-2000'
# ))