library(modules)

import_package('data.table', attach = T)
import_package('dplyr', attach = T)
import_package('stringr', attach = T)
import_package('magrittr', attach = T)

preprocessing_util = import('preprocessing/util')

STANDARD_BIOCHEMISTRY_COLS = list(
  list(old = 'LBXSCR', new = 'lab_creatinine'),
  list(old = 'LBXSTR', new = 'lab_triglycerides'),
  list(old = 'LBXSCH', new = 'lab_cholesterol'),
  list(old = 'LBXSIR', new = 'lab_iron')
)

GLYCO_COLS = list(
  list(old = 'LBXGH', new = 'lab_a1c')
)

read_and_preprocess_data = function(years) {
  data = list()
  for (year in years) {
    base_dir = str_interp('~/emoryhealthcare/data/${year}/Laboratory')
    files = list.files(base_dir)
    
    file_name = files[str_detect(files, 'BIOPRO_\\w{1}.csv|L40_\\w{1}.csv|LAB18.csv')][1]
    path = str_interp('${base_dir}/${file_name}')
    bio = preprocessing_util$read_and_preprocess_data(path, STANDARD_BIOCHEMISTRY_COLS)
    
    file_name = files[str_detect(files, 'GHB_\\w{1}.csv|L10_\\w{1}.csv|LAB10.csv')][1]
    path = str_interp('${base_dir}/${file_name}')
    glyco = preprocessing_util$read_and_preprocess_data(path, GLYCO_COLS)
    
    data_ = bio %>% 
      merge(glyco, all = T, by = 'SEQN')

    data = append(data, list(data_))
  }
  
  data = rbindlist(data, use.names = T, fill=T)
  
  return (data)
}

read_and_preprocess_data(  years = c(
  '2015-2016', '2013-2014', '2011-2012', '2009-2010',
  '2007-2008', '2005-2006', '2003-2004', '2001-2002',  '1999-2000'
))