library(modules)
library(data.table)
library(magrittr)
library(ggplot2)
library(dplyr)
library(caret)

preprocessing = import('preprocessing/preprocessing')
metrics = import('metrics')

set.seed(1234)

data = preprocessing$read_and_preprocess_data(years = c(
  '2015-2016', '2013-2014', '2011-2012', '2009-2010',
  '2007-2008', '2005-2006', '2003-2004', '2001-2002',  '1999-2000'
  ))

# Note, DMDEDUC2 and  DMDMARTL only apply to those ages 20+
# Note, RIDRETH3 includes non-hispanic asian code 5 which is not included prior to 2011
# Thought, exclude those < 20 yrs old since they tend to be healthy and some variables are not given values for them?

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
ggplot(model_xgbTree_smote) + labs(title = 'xgboost')
ggplot(model_lasso_smote) + labs(title = 'Lasso logistic regression')

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
pred = predict(model_xgbTree_smote, type = "prob", newdata = test)[[1]]

high = sort(pred, decreasing = T, index.return=T)$ix[1]
high_obs = test[high]
high_pred = pred[high]

low = sort(pred, decreasing = T, index.return=T)$ix[length(pred)]
low_obs = test[low]
low_pred = pred[low]
