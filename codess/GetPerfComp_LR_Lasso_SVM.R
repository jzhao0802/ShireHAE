#Developer - Jie Zhao
#Develope time - Dec 2015-Jan2016

rm(list=ls())
myTryCatch <- function(expr) {
    warn <- err <- NULL
    value <- withCallingHandlers(
        tryCatch(expr, error=function(e) {
            err <<- e
            NULL
        }), warning=function(w) {
            warn <<- w
            invokeRestart("muffleWarning")
        })
    list(value=value, warning=warn, error=err)
}


library(xlsx)
library(ROCR)
library(plyr)
library(caret)
library(dplyr)
library(glmnet)
library(snow)
library(snowfall)
library(caTools)
library(e1071)
source("D:\\Shire_project\\03_code\\Jie\\svm\\RBF\\svm_subFunc_Dec03_normByRow_rbf.R")


getPerf <- function(resp, pred){
    
    #recall_tar_onTs <- sort(seq(0.5, 0.05, -0.05), decreasing = T)
    recall_tar_onTs <- sort(0.3, decreasing = T)
    result_msOnTest <- msOnTest_sep_v2(pred, resp, recall_tar_onTs)
    ms_onTest <- result_msOnTest$ms
    return(ms_onTest)

}

#load in the pred value for svm model who has the optimum grid search by now
inPath <- 'D:\\Shire_project\\04_result\\svm\\CV\\iter1\\cost(9e-04,0.0012,0.0015)_wt(200)_rn'
load(paste(inPath, '\\pred_wt(200)cost(0.0015)Fold1.RData', sep=''))
svmFit_lin <- pred_model$model_onSubTr
resp_pred_val <- pred_model$resp_pred_ts

#load in the normalized train and test data for iteration 1(for SVM)
path_sepData <- 'D:\\Shire_project\\04_result\\data_sep\\v2\\iter1\\norm'
load(paste(path_sepData, '\\trTs_rn.RData', sep=''))
train_data_iter <- trTs_norm$tr
test_data_iter <- trTs_norm$ts
x_ts2 <- test_data_iter[, -match('response',names(test_data_iter))]

resp_ts <- test_data_iter$response
resp_tr <- train_data_iter$response
#generate the SVM performance on test data
pred_ts <- predict(svmFit_lin, x_ts2 , scale=F,decision.values = TRUE)
predscore <- attr(pred_ts, "decision.values")

ms_svm_lin <- getPerf(resp_ts, predscore)
#on validation data
ms_svm_lin_vl <-  getPerf(resp_pred_val[, 1], resp_pred_val[, 2])
names(ms_svm_lin_vl) <- paste(names(ms_svm_lin_vl), '_validation', sep='')




#load in the pred value for svm model who has the optimum grid search by now
inPath <- 'D:\\Shire_project\\04_result\\svm\\CV\\iter1\\cost(0.001)_gm(0.1,0.3,0.5)_wt(300)_ONE(FALSE)_rn'
load(paste(inPath, '\\pred_on_fullTest.RData', sep=''))
predscore <- attr(pred, "decision.values")
pred_svm <- predscore
rm(predscore)
load(paste(inPath, '\\pred_wt(300)cost(0.001)gm(0.5)Fold1.RData', sep=''))
resp_pred_val <- pred_model$resp_pred_ts

#load in the normalized train and test data for iteration 1(for SVM)
path_sepData <- 'D:\\Shire_project\\04_result\\data_sep\\v2\\iter1\\norm'
load(paste(path_sepData, '\\trTs_rn.RData', sep=''))
train_data_iter <- trTs_norm$tr
test_data_iter <- trTs_norm$ts
resp_ts <- test_data_iter$response
resp_tr <- train_data_iter$response
#generate the SVM performance on test data
ms_svm <- getPerf(resp_ts, pred_svm)
#on validation data
ms_svm_vl <-  getPerf(resp_pred_val[, 1], resp_pred_val[, 2])
names(ms_svm_vl) <- paste(names(ms_svm_vl), '_validation', sep='')

#load in the original train and test data for iteration 1
path_sepData <- 'D:\\Shire_project\\04_result\\data_sep\\v2\\iter1'
load(paste(path_sepData, '\\trTs.RData', sep=''))
train_data_iter <- trTs$tr
test_data_iter <- trTs$ts
resp_ts <- test_data_iter$response
x_test <- test_data_iter[, -match('response', names(test_data_iter))]
#logistci regression
trn_logit_fit = glm(response~., family=binomial(), data=train_data_iter)
tst_prob_logit = predict(trn_logit_fit, x_test, type="response")
ms_logistic <- getPerf(resp_ts, tst_prob_logit)


#load in the original train and test data for fold 1 of iteration 1
path_sepData <- 'D:\\Shire_project\\04_result\\data_sep\\v2\\iter1'
load(paste(path_sepData, '\\dataset_cv', 1, '.RData', sep=''))
cv_data_tr <- cv_data_trTs$cv_data[cv_data_trTs$cv_tr_flag,] #[1] 127032    242
cv_data_ts <- cv_data_trTs$cv_data[!cv_data_trTs$cv_tr_flag,] #[1] 31356   242
cv_resp_ts <- cv_data_ts$response
cv_x_ts <- cv_data_ts[, -match('response', names(cv_data_ts))]
#logistci regression
trn_logit_fit_vl = glm(response~., family=binomial(), data=cv_data_tr)
tst_prob_logit_vl = predict(trn_logit_fit_vl, cv_x_ts, type="response")
ms_logistic_vl <- getPerf(cv_resp_ts, tst_prob_logit_vl)
names(ms_logistic_vl) <- paste(names(ms_logistic_vl), '_validation', sep='')


#load in the pred from weighted lasso 
resp_pred_lasso <- read.csv(paste('D:\\Shire_project\\04_result\\Lasso\\fromHui\\pred_score_val_iter_1.csv', sep=''))
ms_lasso <- getPerf(resp_pred_lasso[, 1], resp_pred_lasso[, 2])


#get performance for validation data(left out fold)
opt_wt <- 0.1
opt_lmd_idx <- 84
lmd_length <- 500
load(paste(path_sepData, '\\trTs.RData', sep=''))
trTs_data_iter <- rbind(trTs$tr, trTs$ts)
trTs_matrix <- model.matrix(response~., data=trTs_data_iter)
tr_matrix <- trTs_matrix[1:nrow(trTs$tr),]
response_tr <- trTs$tr$response
initial_lambda<-glmnet(x=tr_matrix, y=response_tr, family="binomial", alpha=1, 
                       weights=ifelse(response_tr==1, 1, opt_wt), 
                       standardize=F)$lambda  # calculating the initial lambda
lambda_seq<- c(initial_lambda[-length(initial_lambda)] , 
               seq(initial_lambda[length(initial_lambda)] , 0 , length=lmd_length)) 




load(paste(path_sepData, '\\dataset_cv', 1, '.RData', sep=''))
cv_data <- cv_data_trTs$cv_data
cv_tr_flag <- cv_data_trTs$cv_tr_flag
cv_matrix <- model.matrix(response~., data = cv_data)
cv_training_matrix <- cv_matrix[cv_tr_flag,]
cv_test_matrix <- cv_matrix[!cv_tr_flag,]
cv_resp_tr <- cv_data$response[cv_tr_flag]
cv_resp_ts <- cv_data$response[!cv_tr_flag]
wt_vct <- ifelse(cv_resp_tr==1, 1, opt_wt)

#fit the lasso on 4folds
fit_lasso<- glmnet(cv_training_matrix, cv_resp_tr, 
                   lambda=lambda_seq, family="binomial", alpha=1, 
                   weights = wt_vct,
                   standardize=F)

#apply the model on validation data(left out fold)
test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
test_pred_lasso_vl <- test_pred[, opt_lmd_idx]
ms_lasso_vl <- getPerf(cv_resp_ts, test_pred_lasso_vl)
names(ms_lasso_vl) <- paste(names(ms_lasso_vl), '_validation', sep='')


#load in the pred value for svm model(ONE-Classification) who has the optimum grid search by now
inPath <- 'D:\\Shire_project\\04_result\\svm\\test_1fold\\iter1\\cost(0.5,0.7,0.9)_gm(0.001,0.01,0.5,10,100,1000,5000)_wt(NA)_ONE(TRUE)_rn'

load(paste(inPath, '\\ms_svm_ts_vl.RData', sep=''))


#for the random forest modeling with Dong's set

#load in the original train and test data for fold 1 of iteration 1
path_sepData <- 'D:\\Shire_project\\04_result\\data_sep\\v2\\iter1'
load(paste(path_sepData, '\\dataset_cv', 1, '.RData', sep=''))
fct <- apply(cv_data_trTs$cv_data[, c('region', 'gender')], 2, as.factor)
cv_data_trTs$cv_data[, c('region', 'gender')] <- as.data.frame(fct)
cv_data_tr <- cv_data_trTs$cv_data[cv_data_trTs$cv_tr_flag,] #[1] 127032    242
cv_data_ts <- cv_data_trTs$cv_data[!cv_data_trTs$cv_tr_flag,] #[1] 31356   242
cv_resp_tr <- as.factor(cv_data_tr$response)
cv_resp_ts <- as.factor(cv_data_ts$response)
cv_x_tr <- cv_data_tr[, -match('response', names(cv_data_tr))]
cv_x_ts <- cv_data_ts[, -match('response', names(cv_data_ts))]
#logistci regression
RF_fit <- randomForest(x=cv_x_tr,y=cv_resp_tr, 
                       ntree=50,
                       mtry=3,
                       nodesize=10, 
                       importance=F)
save(RF_fit, 'RF_fit_onSubTr(4folds).RData')
pred_rf_vl = predict(RF_fit, cv_x_ts, type='prob')
ms_rf_vl <- getPerf(cv_resp_ts, pred_rf_vl)
names(ms_rf_vl) <- paste(names(ms_rf_vl), '_validation', sep='')


#load in the original train and test data for iteration 1
path_sepData <- 'D:\\Shire_project\\04_result\\data_sep\\v2\\iter1'
load(paste(path_sepData, '\\trTs.RData', sep=''))
train_data_iter <- trTs$tr
test_data_iter <- trTs$ts
train_data_iter[, c('region', 'gender')] <- as.data.frame(
    apply(train_data_iter[, c('region', 'gender')]
          , 2, as.factor)
)

test_data_iter[, c('region', 'gender')] <- as.data.frame(
    apply(test_data_iter[, c('region', 'gender')]
          , 2, as.factor)
)
x_train <- train_data_iter[, -match('response', names(train_data_iter))]
resp_tr <- as.factor(train_data_iter$response)
resp_ts <- test_data_iter$response
x_test <- test_data_iter[, -match('response', names(test_data_iter))]
#random forest
pred_rf = predict(RF_fit, x_test, type='prob')

ms_rf <- getPerf(resp_ts, pred_rf)


#logistic regression after covariates selection
load('ms_logistic_rm_vl_ts.RData')







outPath <- 'D:\\Shire_project\\04_result\\svm\\test_1fold\\iter1'
setwd(outPath)
ms_summary_add <- rbind(
                    #c(ms_logistic, ms_logistic_vl)
                    c(ms_logistic_rm, ms_logistic_vl_rm)
                    #, c(ms_lasso, ms_lasso_vl)
                    , c(ms_rf, ms_rf_vl)
                    , c(ms_svm_lin, ms_svm_lin_vl)
                    #, c(ms_svm, ms_svm_vl)
                    #, ms_svm_ts_vl
                    )
                
rownames(ms_summary_add) <- c(
    #'Logistic'
    'Logistic after covariates selection'
    #, 'Weighted Lasso'
    ,'Random Forest'
    ,'Weighted SVM Linear(C-Classification)'
    #, 'Weighted SVM(ONE-Classification)'
    )
write.csv(ms_summary_add, 'performance_comparison_add.csv', row.names=T)
