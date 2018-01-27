rm(list=ls())
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
library(randomForest)
#source("D:\\Shire_project\\03_code\\Jie\\svm\\RBF\\svm_subFunc_Dec03_normByRow_rbf.R")
#source("D:\\Shire_project\\03_code\\Jie\\svm\\DR_constrain\\svm_subFunc_svm_withFpConstrain.R")

source("F:\\Jie\\Shire_project\\03_code\\svm\\DR_constrain\\svm_subFunc_svm_withFpConstrain.R")

getPerf <- function(resp, pred){
    
    #recall_tar_onTs <- sort(seq(0.5, 0.05, -0.05), decreasing = T)
    recall_tar_onTs <- sort(0.3, decreasing = T)
    result_msOnTest <- msOnTest_sep_v2_DR(pred, resp, recall_tar_onTs)
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
save(pred_ts, '')
ms_svm_lin <- getPerf(resp_ts, pred_ts)
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


#svm linear and rbf with DR constrain
#load in the pred value for svm model who has the optimum grid search by now
inPath <- 'F:\\Jie\\Shire_project\\04_result\\svm\\test_1fold\\iter1\\cost(5e-05,1e-04,5e-04,0.0013,0.007,0.01)_wt(200,300,400)_ONE(FALSE)_rn_DR'
load(paste(inPath, '\\pred_wt(200)cost(0.01)gm(NA)Fold1.RData', sep=''))
svmFit_lin <- pred_model$model_onSubTr
resp_pred_val <- pred_model$resp_pred_ts

#load in the normalized train and test data for iteration 1(for SVM)
path_sepData <- 'F:\\Jie\\Shire_project\\04_result\\data_sep\\v2\\iter1\\norm'
load(paste(path_sepData, '\\trTs_rn.RData', sep=''))
train_data_iter <- trTs_norm$tr
test_data_iter <- trTs_norm$ts
x_ts2 <- test_data_iter[, -match('response',names(test_data_iter))]

resp_ts <- test_data_iter$response
resp_tr <- train_data_iter$response
#generate the SVM performance on test data
pred_ts <- predict(svmFit_lin, x_ts2 , scale=F,decision.values = TRUE)
save(pred_ts, file=paste0(inPath, '\\opt_pred_onTs.RData'))
#get predscore
predscore <- attr(pred_ts, "decision.values")
pred_svm_ts <- predscore
rm(predscore)

ms_svm_lin <- getPerf(resp_ts, pred_svm_ts)
#on validation data
ms_svm_lin_vl <-  getPerf(resp_pred_val[, 1], resp_pred_val[, 2])
names(ms_svm_lin_vl) <- paste(names(ms_svm_lin_vl), '_validation', sep='')
ms_svm_lin_vl_ts_DR <- c(ms_svm_lin_vl, ms_svm_lin)
save(ms_svm_lin_vl_ts_DR, file=paste0(inPath, '\\ms_svm_lin_vl_ts_DR.RData'))



#load in the pred value for svm model who has the optimum grid search by now
inPath <- 'D:\\Shire_project\\04_result\\svm\\test_1fold\\iter1\\cost(2e-04,5e-05)_gm(0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85,0.95)_wt(300)_ONE(FALSE)_rn_DR0(FALSE)'
#load(paste(inPath, '\\pred_on_fullTest.RData', sep=''))

#predscore <- attr(pred, "decision.values")
#pred_svm <- predscore
#rm(predscore)
load(paste(inPath, '\\pred_wt(300)cost(2e-04)gm(0.85)Fold1.RData', sep=''))
resp_pred_val <- pred_model$resp_pred_ts

#load in the normalized train and test data for iteration 1(for SVM)
path_sepData <- 'D:\\Shire_project\\04_result\\data_sep\\v2\\iter1\\norm'
load(paste(path_sepData, '\\trTs_rn.RData', sep=''))
train_data_iter <- trTs_norm$tr
test_data_iter <- trTs_norm$ts
resp_ts <- test_data_iter$response
resp_tr <- train_data_iter$response
x_ts <- test_data_iter[, -match('response',names(test_data_iter))]

#get pred score on test data
pred <- predict(pred_model$model_onSubTr, x_ts , scale=F,decision.values = TRUE)
predscore <- attr(pred, "decision.values")
pred_svm <- predscore
rm(predscore)

#generate the SVM performance on test data
ms_svm <- getPerf(resp_ts, pred_svm)
#on validation data
ms_svm_vl <-  getPerf(resp_pred_val[, 1], resp_pred_val[, 2])
names(ms_svm_vl) <- paste(names(ms_svm_vl), '_validation', sep='')
#svm with DR constrain end



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
create_dummy <- function(data){
    temp_fct <- as.data.frame(apply(data[, cati_var], 2, as.factor))
    dummy <- 
        model.matrix( ~ ., data=temp_fct, contrasts.arg = 
                          lapply(temp_fct, contrasts, contrasts=FALSE))[, -1]
    cati_var_dummy <- colnames(dummy)
    data <- cbind(as.data.frame(dummy), data[, c(binary_var, conti_var, 'response')])
    temp <- list(data=data, dummy_nm = cati_var_dummy)
    return(temp)
}

path_sepData <- 'D:\\Shire_project\\04_result\\data_sep\\v2\\iter1'
load(paste(path_sepData, '\\dataset_cv', 1, '.RData', sep=''))


cv_data <- cv_data_trTs$cv_data
var_list <- names(cv_data)
fct_flag <- sapply(cv_data, is.character)
cati_var <- var_list[fct_flag]
binary_var <- grep('flag$', var_list, value=T)
conti_var <- grep('age|lookback_days|freq$', var_list, value=T)
length(cati_var)+length(binary_var)+length(conti_var)+1==length(var_list)

cv_data_result <- create_dummy(cv_data)
cv_data <- cv_data_result$data

fct <- apply(cv_data[, c(grep('region|gender', names(cv_data), value=T, perl=T), binary_var)]
             , 2, as.factor)
cv_data[, c(grep('region|gender', names(cv_data), value=T, perl=T), binary_var)] <- as.data.frame(fct)

cv_data_tr <- cv_data[cv_data_trTs$cv_tr_flag,] #[1] 127032    242
cv_data_ts <- cv_data[!cv_data_trTs$cv_tr_flag,] #[1] 31356   242

cv_resp_tr <- as.factor(cv_data_tr$response)
cv_resp_ts <- as.factor(cv_data_ts$response)
cv_x_tr <- cv_data_tr[, -match('response', names(cv_data_tr))]
cv_x_ts <- cv_data_ts[, -match('response', names(cv_data_ts))]

#load in the original train and test data for iteration 1
path_sepData <- 'D:\\Shire_project\\04_result\\data_sep\\v2\\iter1'
load(paste(path_sepData, '\\trTs.RData', sep=''))
train_data_iter <- trTs$tr
test_data_iter <- trTs$ts
tr_flag <- c(rep(1, nrow(train_data_iter)), rep(0, nrow(test_data_iter)))
trTs <- rbind(train_data_iter, test_data_iter)
trTs_result <- create_dummy(trTs)
trTs <- trTs_result$data
test_data_iter <- trTs[!tr_flag, ]
test_data_iter[, c(grep('region|gender', names(test_data_iter), value=T, perl=T), binary_var)] <- as.data.frame(
    apply(test_data_iter[, c(grep('region|gender', names(test_data_iter), value=T, perl=T), binary_var)]
          , 2, as.factor)
)
resp_tr <- as.factor(train_data_iter$response)
resp_ts <- as.factor(test_data_iter$response)
x_test <- test_data_iter[, -match('response', names(test_data_iter))]
#RF
#pred_rf = predict(RF_fit, x_test, type='prob')
#save(pred_rf, file='pred_rf_v7.RData')
#ms_rf <- getPerf(resp_ts, pred_rf[, 2])
#save(ms_rf, ms_rf_vl, file='ms_rf_vl_ts_v7.RData')

#randomForest for subtrain(4 folds)
grid_par <- function(r){
    mtry <- grid[r, 'mtry']
    rate <- grid[r,'rate' ]
    ns <- grid[r, 'nodesize']
    n_neg_smp <- n_pos*rate
    
    #start1 <- proc.time()
    #RF_fit <- randomForest(x=cv_x_tr,y=cv_resp_tr, 
    #                      ntree=30000/rate,
    #                      mtry=mtry,
    #                      nodesize=ns, 
    #                      importance=T,
    #                      sampsize=c(n_neg_smp, n_pos),
    #                      strata=cv_resp_tr,
    #                      keep.inbag=T
    #)
    suffix <- paste0('mtry(', mtry, ')rate(', rate, ')ns(', ns, ')')
    #cat(file=traceFile, append=T, suffix, ' train model time used:', (proc.time()-start1)[3]/60, 'min!\n')
    #save(RF_fit, file=paste0('RF_fit_onSubTr(4folds)_', suffix, '.RData'))
    load(paste0('RF_fit_onSubTr(4folds)_', suffix, '.RData'))
    pred_rf_vl = predict(RF_fit, cv_x_ts, type='prob')
    save(pred_rf_vl, file=paste0('pred_rf_vl_', suffix, '.RData'))
    ms_rf_vl <- getPerf(cv_resp_ts, pred_rf_vl[, 2])
    names(ms_rf_vl) <- paste(names(ms_rf_vl), '_validation', sep='')
    
    pred_rf = predict(RF_fit, x_test, type='prob')
    save(pred_rf, file=paste0('pred_rf_', suffix, '.RData'))
    ms_rf <- getPerf(resp_ts, pred_rf[, 2])
    save(ms_rf, ms_rf_vl, file=paste0('ms_rf_vl_ts_', suffix, '.RData'))
    return(c(ms_rf_vl, ms_rf))
    
}

model <- 'RF'
test_1fold <- T
iter <- 1
folder <- ifelse(test_1fold, 'test_1fold', 'CV')
n_pos <- as.vector(table(cv_resp_tr)[2])
n_neg <- as.vector(table(cv_resp_tr)[1])
rate_list <- c(20, 50, 80, 100)
mtry_list <- c(25)
ns_list <- c(300, 500, 1000)
grid <- expand.grid(mtry=mtry_list, rate=rate_list, nodesize=ns_list)


mtry_str <- paste(mtry_list, collapse=',')
rate_str <- paste(rate_list, collapse=',')
ns_str <- paste(ns_list, collapse=',')
rootPath <- "D:\\Shire_project\\"
outPath <- paste(rootPath, '04_result\\', model, '\\', folder, '\\iter', iter,  '\\mtry(', mtry_str, ')_rate(', rate_str, ')_ns(', ns_str, ')', sep='')

n.cpu <- nrow(grid)


if(!file.exists(outPath)){
    dir.create(outPath, recursive=T, showWarnings=T)
    setwd(outPath)
}else{
    setwd(outPath)
}
#define the traceFile to trace how the paralle process
traceFile <- paste(outPath, '\\traceFile.csv', sep='')



sfInit(parallel=TRUE, cpus=n.cpu, type='SOCK')
sfSource("D:\\Shire_project\\03_code\\Jie\\svm\\svm_subFunc_Dec03_normByRow_rbf.R")
cat(file=traceFile, append=TRUE, 'iter:', iter, ' sfExport running!\n')
sfExport('grid', 'traceFile', 'cv_x_tr', 'cv_resp_tr', 'cv_x_ts', 'cv_resp_ts', 'x_test', 'resp_ts', 'n_pos')
sfExport('msOnTest_sep_v2', 'getPerf', 'grid_par')
#sfClusterEval(library("glmnet"))
#sfClusterEval(library("e1071"))
sfClusterEval(library("ROCR"))
sfClusterEval(library("plyr"))
sfClusterEval(library("dplyr"))
sfClusterEval(library("caTools"))
#sfClusterEval(library('snowfall'))
sfClusterEval(library('randomForest'))
cat(file=traceFile, append=TRUE, 'iter:', iter, ' parallele grid search running!\n')

ms_allGrid <- sfClusterApplyLB(1:nrow(grid), grid_par)
sfStop()

ms_allGrid_df <- cbind(grid, ntree=30000/grid$rate, ldply(ms_allGrid, quickdf))
ms_ord <- ms_allGrid_df[order(ms_allGrid_df$`PPV(recall=0.3)_validation`, decreasing = T), ]
write.csv(ms_ord, 'ms_allGrid_ord.csv', row.names = F)

outPath <- 'D:\\Shire_project\\04_result\\svm\\test_1fold\\iter1'
setwd(outPath)
ms_summary <- rbind(c(ms_logistic, ms_logistic_vl), c(ms_lasso, ms_lasso_vl), c(ms_svm, ms_svm_vl), ms_svm_ts_vl)
rownames(ms_summary) <- c('Logistic', 'Weighted Lasso', 'Weighted SVM(C-Classification)', 'Weighted SVM(ONE-Classification)')
write.csv(ms_summary, 'performance_comparison.csv', row.names=T)

