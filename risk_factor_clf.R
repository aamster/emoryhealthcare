library(modules)
library(data.table)
library(magrittr)
library(ggplot2)
library(dplyr)
library(caret)
library(gridExtra)

preprocessing = import('preprocessing/preprocessing')
metrics = import('metrics')

set.seed(1234)

data = preprocessing$read_and_preprocess_data(years = c(
  '2015-2016', '2013-2014', '2011-2012', '2009-2010',
  '2007-2008', '2005-2006', '2003-2004', '2001-2002',  '1999-2000'
  ))

# Remove join key
data[, SEQN := NULL]

# Drop any drug categories which are reported by less than 1% of ppl who take medications
# Note logic placed here since we have already dropped age < 20
drug_category_frac = data[total_drug_count>0, lapply(.SD, sum), .SDcols = names(data)[str_detect(names(data), '^drug_category_')]] / nrow(data[total_drug_count>0])
least_frequent_drug_categories = names(drug_category_frac)[c(drug_category_frac < .01)]
data[, c(least_frequent_drug_categories) := NULL]

print(paste('data size before dropping null obs', nrow(data)))
# Drop any row with any missing value
data = data[complete.cases(data)]
print(paste('data size after dropping null obs', nrow(data)))

# create a testing and training set
data = data[sample(nrow(data))]
train_idx = createDataPartition(
  data$questionnaire_overnight_hospital_patient_in_last_year,
  p = .8,
  list = F
)

train = data[train_idx]
test = data[-train_idx]

# train_upsampled = upSample(
#   x = train[, .SD, .SDcols = !names(train) == 'questionnaire_overnight_hospital_patient_in_last_year'],
#   y = train$questionnaire_overnight_hospital_patient_in_last_year,
#   yname = 'questionnaire_overnight_hospital_patient_in_last_year'
# )

fit_control = trainControl(method = "cv",
                           number = 5,
                           classProbs = T,
                           summaryFunction = twoClassSummary,
                           search = 'random'
)

# model_xgbTree = train(
#   questionnaire_overnight_hospital_patient_in_last_year ~ .,
#   data = train,
#   method = "xgbTree",
#   metric = 'ROC',
#   trControl = fit_control
# )
# model_lasso = train(
#   questionnaire_overnight_hospital_patient_in_last_year ~ .,
#   data = train,
#   method = "glmnet",
#   family = "binomial",
#   metric = 'ROC',
#   trControl = fit_control
# )

fit_control$sampling = "smote"

model_xgbTree_smote = train(
  questionnaire_overnight_hospital_patient_in_last_year ~ .,
  data = train,
  method = "xgbTree",
  metric = 'ROC',
  trControl = fit_control
)
model_lasso_smote = train(
  questionnaire_overnight_hospital_patient_in_last_year ~ .,
  data = train,
  method = "glmnet",
  family = "binomial",
  metric = 'ROC',
  trControl = fit_control
)
resamps = resamples(
  list(
    xgbTree_smote = model_xgbTree_smote,
    lasso_smote = model_lasso_smote
    # xgbTree = model_xgbTree,
    # logReg = model_logReg
  )
)
summary(resamps)

# Plot cross validation metrics
g1 = ggplot(model_xgbTree_smote) + labs(title = 'xgboost')
g2 = ggplot(model_lasso_smote) + labs(title = 'Lasso logistic regression')
grid.arrange(g1, g2, nrow=1)

## Test performance
ROCR::performance(
  ROCR::prediction(predict(model_xgbTree_smote, type = "prob", newdata = test)[1], test$questionnaire_overnight_hospital_patient_in_last_year), 
  'auc'
  )@y.values[1]

ROCR::performance(
  ROCR::prediction(predict(model_lasso_smote, type = "prob", newdata = test)[1], test$questionnaire_overnight_hospital_patient_in_last_year), 
  'auc'
)@y.values[1]


# Lift plot
lift_results <- data.frame(
  questionnaire_overnight_hospital_patient_in_last_year = test$questionnaire_overnight_hospital_patient_in_last_year,
  xgbTree_smote = predict(model_xgbTree_smote, type = "prob", newdata = test)[[1]],
  lasso_smote = predict(model_lasso_smote, type = "prob", newdata = test)[[1]]
)
lift_obj = lift(questionnaire_overnight_hospital_patient_in_last_year ~ xgbTree_smote + lasso_smote, data = lift_results)
ggplot(lift_obj)

# Confusion matrix
fourfoldplot(
  confusionMatrix(
    predict(model_xgbTree_smote, newdata = test), test$questionnaire_overnight_hospital_patient_in_last_year
  )$table
)

fourfoldplot(
  confusionMatrix(
    predict(model_lasso_smote, newdata = test), test$questionnaire_overnight_hospital_patient_in_last_year
  )$table
) 

# Precision
precision(
  predict(model_xgbTree_smote, newdata = test), test$questionnaire_overnight_hospital_patient_in_last_year
)
precision(
  predict(model_lasso_smote, newdata = test), test$questionnaire_overnight_hospital_patient_in_last_year
)

# Variable importances
var_imp = varImp(model_xgbTree_smote)$importance
ggplot(
  data.table(
    variable = rownames(var_imp), 
    importance = var_imp$Overall
  )[1:10], aes(reorder(variable, importance), importance)
) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title = 'xgboost', x = 'variable')

var_imp = varImp(model_lasso_smote)$importance
ggplot(
  data.table(
    variable = rownames(var_imp), 
    importance = var_imp$Overall
  )[order(-importance)][1:10], aes(reorder(variable, importance), importance)
) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title = 'lasso', x = 'variable')

# Looking at individual predictions
# pred = predict(model_xgbTree_smote, type = "prob", newdata = test)[[1]]
# 
# high = sort(pred, decreasing = T, index.return=T)$ix[1]
# high_obs = test[high]
# high_pred = pred[high]
# 
# low = sort(pred, decreasing = T, index.return=T)$ix[length(pred)]
# low_obs = test[low]
# low_pred = pred[low]

# LIME local approximation
# X = train[, .SD, .SDcols=names(train)[names(train) != "questionnaire_overnight_hospital_patient_in_last_year"]]
# 
# pred = predict(model_xgbTree_smote, type="prob")[,1]
# test_idx = c(which.min(pred), which.max(pred))
# X_train = X[-test_idx]
# X_test = X[test_idx]
# 
# explainer = lime::lime(X_train, model_xgbTree_smote)
# explanation = lime::explain(X_test, explainer, labels="Yes", n_features=10, feature_select = "lasso_path")
# 
# g = lime::plot_features(explanation, ncol=1)

g = ggplot(train, aes(questionnaire_overnight_hospital_patient_in_last_year, total_drug_count)) + geom_boxplot()
g = ggplot(train, aes(questionnaire_overnight_hospital_patient_in_last_year, demographics_householdIncome)) + geom_boxplot()
g = ggplot(train, aes(questionnaire_overnight_hospital_patient_in_last_year, demographics_age)) + geom_boxplot()
g = ggplot(train[, 
                .(female_frac = mean(demographics_gender == "Female")), 
                by = .(questionnaire_overnight_hospital_patient_in_last_year)],
           aes(questionnaire_overnight_hospital_patient_in_last_year, female_frac)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Fraction female', y = 'Fraction')
g = ggplot(copy(train)[, age_bin := cut(demographics_age, 4)][, 
                 .(female_frac = mean(demographics_gender == "Female")), 
                 by = .(questionnaire_overnight_hospital_patient_in_last_year, age_bin)],
           aes(questionnaire_overnight_hospital_patient_in_last_year, female_frac)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~age_bin) +
  labs(title = 'Fraction female by age', y = 'Fraction')
# g = ggplot(copy(train)[, `:=`(age_bin = cut(demographics_age, 4), income_bin = cut(demographics_householdIncome, 4))], 
#            aes(questionnaire_overnight_hospital_patient_in_last_year, examination_diastolic_blood_pressure)) +
#   geom_boxplot() + 
#   facet_grid(age_bin~income_bin) +
#   labs(title = 'Diastolic blood pressure by age')
