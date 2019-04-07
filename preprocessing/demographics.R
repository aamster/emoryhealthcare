import_package('data.table', attach = T)
library(dplyr)
library(stringr)

preprocessing_util = import('preprocessing/util')

DEMOGRAPHICS_COLS = list(
  list(old = 'RIAGENDR', new = 'demographics_gender'),
  list(old = 'RIDAGEYR', new = 'demographics_age'),
  list(old = 'RIDRETH3', new = 'demographics_race'),
  list(old = 'RIDRETH1', new = 'demographics_race'),
  # list(old = 'DMDEDUC2', new = 'demographics_education'),
  # list(old = 'DMDMARTL', new = 'demographics_maritalStatus'),
  list(old = 'INDHHIN2', new = 'demographics_householdIncome')
)

######
# Recoding
######
recode_race = function(code) {
  x = case_when(
    code == 1 ~ 'Mexican American',
    code == 2 ~ 'Other Hispanic',
    code == 3 ~ 'Non-Hispanic White',
    code == 4 ~ 'Non-Hispanic Black',
    code == 5 | code == 7 ~ 'Other Race - Including Multi-Racial',
    code == 6 ~ 'Non-Hispanic Asian'
  )
  return (factor(x))
}

recode_gender = function(code) {
  x = case_when(code == 1 ~ 'Male',
                code == 2 ~ 'Female')
  return (factor(x))
}

recode_eduction = function(code) {
  x = case_when(
    code == 1 ~ 'Less than 9th grade',
    code == 2 ~ '	9-11th grade (Includes 12th grade with no diploma)',
    code == 3 ~ '	High school graduate/GED or equivalent',
    code == 4 ~ 'Some college or AA degree',
    code == 5 ~ 'College graduate or above'
  )
  return (factor(x))
}

recode_maritalStatus = function(code) {
  x = case_when(
    code == 1 ~ 'Married',
    code == 2 ~ 'Widowed',
    code == 3 ~ 'Divorced',
    code == 4 ~ 'Separated',
    code == 5 ~ 'Never married',
    code == 6 ~ 'Living with partner'
  )
  return (factor(x))
}

convert_householdIncome_to_numeric = function(code) {
  to_numeric = function(code) {
    if (is.na(code)) {
      x = NA
    } else if (code == 1) {
      x = runif(1, 0, 4999)
    } else if (code == 2) {
      x = runif(1, 5000, 9999)
    } else if (code == 3) {
      x = runif(1, 10000, 14999)
    } else if (code == 4) {
      x = runif(1, 15000, 20000)
    } else if (code == 5) {
      x = runif(1, 20000, 24999)
    } else if (code == 6) {
      x = runif(1, 25000, 34999)
    } else if (code == 7) {
      x = runif(1, 35000, 44999)
    } else if (code == 8) {
      x = runif(1, 45000, 54999)
    } else if (code == 9) {
      x = runif(1, 55000, 64999)
    } else if (code == 10) {
      x = runif(1, 65000, 74999)
    } else if (code == 14) {
      x = runif(1, 75000, 99999)
    } else if (code == 15) {
      x = 100000
    } else {
      x = NA
    }
    return (x)
  }
  
  return (sapply(code, to_numeric))
}

read_and_preprocess_data = function(years) {
  data = list()
  
  for (year in years) {
    base_dir = str_interp('~/emoryhealthcare/data/${year}/Demographics')
    files = list.files(base_dir)
    file = files[str_detect(files, 'DEMO_*')][1]
    path = str_interp('${base_dir}/${file}')
    data_ = preprocessing_util$read_and_preprocess_data(path, DEMOGRAPHICS_COLS)
    data_[, `:=`(
      demographics_race = recode_race(demographics_race),
      demographics_gender = recode_gender(demographics_gender),
      # demographics_education = recode_eduction(demographics_education),
      # demographics_maritalStatus = recode_maritalStatus(demographics_maritalStatus),
      demographics_householdIncome = convert_householdIncome_to_numeric(demographics_householdIncome)
    )]
    
    # Remove children
    data_ = data_[demographics_age >= 20]
    
    data = append(data, list(data_))
  }
  
  data = rbindlist(data, use.names = T)
  return (data)
}

read_and_preprocess_data(c('2015-2016'))
