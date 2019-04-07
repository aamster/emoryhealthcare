import_package('data.table', attach = T)
import_package('dplyr', attach = T)
import_package('stringr', attach = T)

preprocessing_util = import('preprocessing/util')

COLS = list(
  list(old = 'DR1TKCAL', new = 'dietary_kcal'),
  list(old = 'DR1TPROT', new = 'dietary_protein'),
  list(old = 'DR1TCARB', new = 'dietary_carbs'),
  list(old = 'DR1TSUGR', new = 'dietary_sugars'),
  list(old = 'DR1TFIBE', new = 'dietary_fiber'),
  list(old = 'DR1TSFAT', new = 'dietary_satfat'),
  list(old = 'DR1TMFAT', new = 'dietary_monounsatfat'),
  list(old = 'DR1TPFAT', new = 'dietary_polyunsatfat'),
  list(old = 'DR1TCHOL', new = 'dietary_cholesterol'),
  list(old = 'DR1TATOC', new = 'dietary_vitamine'),
  list(old = 'DR1TRET', new = 'dietary_retinol'),
  list(old = 'DR1TVARA', new = 'dietary_vitamina'),
  list(old = 'DR1TLYCO', new = 'dietary_lycopene'),
  list(old = 'DR1TVB1', new = 'dietary_vitaminb1'),
  list(old = 'DR1TVB2', new = 'dietary_vitaminb2'),
  list(old = 'DR1TNIAC', new = 'dietary_niacin'),
  list(old = 'DR1TVB6', new = 'dietary_vitaminb6'),
  list(old = 'DR1TFOLA', new = 'dietary_folate'),
  list(old = 'DR1TCHL', new = 'dietary_choline'),
  list(old = 'DR1TVB12', new = 'dietary_b12'),
  list(old = 'DR1TVC', new = 'dietary_vitaminc'),
  list(old = 'DR1TVD', new = 'dietary_vitamind'),
  list(old = 'DR1TVK', new = 'dietary_vitamink'),
  list(old = 'DR1TCALC', new = 'dietary_calcium'),
  list(old = 'DR1TPHOS', new = 'dietary_phosphorous'),
  list(old = 'DR1TMAGN', new = 'dietary_magnesium'),
  list(old = 'DR1TIRON', new = 'dietary_iron'),
  list(old = 'DR1TZINC', new = 'dietary_zinc'),
  list(old = 'DR1TSODI', new = 'dietary_sodium'),
  list(old = 'DR1TPOTA', new = 'dietary_potassium'),
  list(old = 'DR1TCAFF', new = 'dietary_caffeine'),
  list(old = 'DR1TALCO', new = 'dietary_alcohol'),
  list(old = 'DR1_320Z', new = 'dietary_water')
)

read_and_preprocess_data = function(years) {
  data = list()
  
  for (year in years) {
    base_dir = str_interp('~/emoryhealthcare/data/${year}/Dietary')
    files = list.files(base_dir)
    
    file_name = files[str_detect(files, 'DR1TOT_*')][1]
    path = str_interp('${base_dir}/${file_name}')
    data_ = preprocessing_util$read_and_preprocess_data(path, COLS)
    
    data = append(data, list(data_))
  }
  
  data = rbindlist(data, use.names = T)
  return (data)
}