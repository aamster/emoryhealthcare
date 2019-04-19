library(modules)
library(data.table)
library(magrittr)
library(ggplot2)
library(dplyr)
library(caret)
library(gridExtra)

preprocessing = import('preprocessing/preprocessing')

set.seed(1234)

data = preprocessing$read_and_preprocess_data(years = c(
  '2015-2016', '2013-2014', '2011-2012', '2009-2010',
  '2007-2008', '2005-2006', '2003-2004', '2001-2002',  '1999-2000'
))

# Join cluster labels
clusters = fread('clustering/seqn_cluster_map.csv')
data = data %>%
  merge(clusters, on = 'SEQN', all.x = T)

data[is.na(CLUSTER), CLUSTER := max(data$CLUSTER, na.rm = T)+1L]

# Remove join key
data[, SEQN := NULL]

# Drop rows with missing values
print(paste('data size before dropping null obs', nrow(data)))
# Drop any row with any missing value
data = data[complete.cases(data)]
print(paste('data size after dropping null obs', nrow(data)))

# Combine similar clusters
data[, CLUSTER_LABEL := case_when(
  CLUSTER == 0 | CLUSTER == 1 | CLUSTER == 5 | CLUSTER == 6 | CLUSTER == 9 ~ 'heart_problems',
  CLUSTER == 2 | CLUSTER == 8 ~ 'high_cholesterol',
  CLUSTER == 3 ~ 'thyroid_issues',
  CLUSTER == 4 ~ 'type_II_diabetes',
  CLUSTER == 7 ~ 'copd_asthma',
  T ~ 'no_medications'
)]

fit_control = trainControl(method = "cv",
                           number = 5,
                           classProbs = T,
                           summaryFunction = twoClassSummary,
                           search = 'random',
                           sampling = 'smote'
)

global_var_imp_plots = list()

for (cluster_label in unique(data$CLUSTER_LABEL)) {
  print(paste('CLUSTER', cluster_label))
  print('=======================')
  
  cluster = data[CLUSTER_LABEL == cluster_label]
  cluster[, CLUSTER := NULL]
  cluster[, CLUSTER_LABEL := NULL]
  
  # set boolean drug category cols as factor
  drug_category_cols = names(cluster)[startsWith(names(cluster), 'drug_category')]
  cluster[, (drug_category_cols) := lapply(.SD, as.factor), .SDcols=drug_category_cols]
  
  # Drop  near 0 variance features
  print(paste('n features before dropping near 0 var', dim(cluster)[2]))
  nzv = nearZeroVar(cluster, saveMetrics = T)
  class = cluster$questionnaire_overnight_hospital_patient_in_last_year
  cluster = cluster[, .SD, .SDcols = names(cluster)[!nzv$nzv]]
  print(paste('n features after dropping near 0 var', dim(cluster)[2]))
  cluster[, questionnaire_overnight_hospital_patient_in_last_year := class]
  
  model = train(
    questionnaire_overnight_hospital_patient_in_last_year ~ .,
    data = cluster,
    method = "rf",
    metric = 'ROC',
    trControl = fit_control
  )
  
  print(model)
  
  # Variable importances
  var_imp = varImp(model)$importance
  g = ggplot(
    data.table(
      variable = rownames(var_imp), 
      importance = var_imp$Overall
    )[order(-importance)][1:5], aes(reorder(variable, importance), importance)
  ) + 
    geom_bar(stat = "identity") + 
    coord_flip() + 
    labs(title = paste('Cluster', cluster_label), x = 'variable')
  
  global_var_imp_plots[[cluster_label]] = g
  
  # LIME local approximation
  X = cluster[, .SD, .SDcols=names(cluster)[names(cluster) != "questionnaire_overnight_hospital_patient_in_last_year"]]

  pred = predict(model, type="prob")[,1]
  test_idx = c(sample(which(pred < .1))[1], sample(which(pred > .8))[1])
  test_idx = test_idx[complete.cases(test_idx)]
  X_train = X[-test_idx]
  X_test = X[test_idx]

  explainer = lime::lime(X_train, model)
  explanation = lime::explain(X_test, explainer, labels="Yes", n_features=10, feature_select = "lasso_path")

  g = lime::plot_features(explanation, ncol=1) + labs(title = paste('Cluster', cluster_label))
  ggsave(paste0('plots/cluster/lime', cluster_label, '.png'), plot = g, width=10, height=10)
  
}

g = do.call("grid.arrange", c(global_var_imp_plots))
ggsave(paste0('plots/cluster/global_cluster_ftr_importance', '.png'), plot = g, width = 10, height = 10)


# Additional EDA
g = ggplot(data, aes(questionnaire_overnight_hospital_patient_in_last_year, lab_iron)) + geom_boxplot() + facet_wrap(~CLUSTER_LABEL)
ggsave(paste0('plots/cluster/eda/cluster_iron_boxplot', '.png'), plot = g, width = 10, height = 10)

g = ggplot(data, aes(questionnaire_overnight_hospital_patient_in_last_year, demographics_householdIncome)) + geom_boxplot() + facet_wrap(~CLUSTER_LABEL)
ggsave(paste0('plots/cluster/eda/cluster_income_boxplot', '.png'), plot = g, width = 10, height = 10)

ggplot(data, aes(questionnaire_overnight_hospital_patient_in_last_year, demographics_householdIncome)) + geom_boxplot()
ggsave(paste0('plots/cluster/eda/global_income', '.png'), plot = g, width = 10, height = 10)

ggplot(data, aes(questionnaire_overnight_hospital_patient_in_last_year, lab_a1c)) + geom_boxplot() + facet_wrap(~CLUSTER_LABEL)
ggsave(paste0('plots/cluster/eda/cluster_a1c_boxplot', '.png'), plot = g, width = 10, height = 10)

g = ggplot(data[, 
            .(anti_hypertensive_frac = mean(`drug_category_ANTIHYPERTENSIVE COMBINATIONS`)), 
            by = .(questionnaire_overnight_hospital_patient_in_last_year, CLUSTER_LABEL)],
       aes(questionnaire_overnight_hospital_patient_in_last_year, anti_hypertensive_frac)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~CLUSTER_LABEL) +
  labs(title = 'Fraction of anti hypertensive combination use', y = 'Fraction')
ggsave(paste0('plots/cluster/eda/antihypertensive_by_cluster', '.png'), plot = g, width = 10, height = 10)

ggplot(data, aes(questionnaire_overnight_hospital_patient_in_last_year, demographics_age)) + geom_boxplot() + facet_wrap(~CLUSTER_LABEL)
ggsave(paste0('plots/cluster/eda/cluster_a1c_boxplot', '.png'), plot = g, width = 10, height = 10)

g = ggplot(data[, 
                .(female_frac = mean(demographics_gender == "Female")), 
                by = .(questionnaire_overnight_hospital_patient_in_last_year, CLUSTER_LABEL)],
           aes(questionnaire_overnight_hospital_patient_in_last_year, female_frac)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~CLUSTER_LABEL) +
  labs(title = 'Fraction female', y = 'Fraction')
ggsave(paste0('plots/cluster/eda/fraction_female_by_cluster', '.png'), plot = g, width = 10, height = 10)

g = ggplot(data, aes(questionnaire_overnight_hospital_patient_in_last_year, examination_diastolic_blood_pressure)) + geom_boxplot() + facet_wrap(~CLUSTER_LABEL)
ggsave(paste0('plots/cluster/eda/cluster_diastolic_boxplot', '.png'), plot = g, width = 10, height = 10)

g = ggplot(data, aes(questionnaire_overnight_hospital_patient_in_last_year, examination_systolic_blood_pressure)) + geom_boxplot() + facet_wrap(~CLUSTER_LABEL)
ggsave(paste0('plots/cluster/eda/cluster_systolic_boxplot', '.png'), plot = g, width = 10, height = 10)



