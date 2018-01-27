lmd_length <- 500
optimum_wt <- 0.1
optimum_lambdaIdx <- 85
training_data_lasso <- rbind(dat_hae_1111_rf[trainIdxHAE, -1], dat_nonhae_1111_rf[nonHaeIdx_train, -1])
response_train <- training_data_lasso$response
# Calculating initial lambda and the lambda sequence
training_matrix<- model.matrix(response~., data=training_data_lasso)[,-1]     # removes intercept term
initial_lambda<-glmnet(x=training_matrix, y=training_data_lasso$response, family="binomial", alpha=1, standardize=F
                         ,weights = ifelse(training_data_lasso$response==1, 1, optimum_wt))$lambda  # calculating the initial lambda
lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=lmd_length)) # get a length=500 descending sequence from initial lambda to 0

total_model<- glmnet(x=training_matrix, y=training_data_lasso$response, 
                     lambda=lambda_seq, family="binomial", 
                     alpha=1, 
                     weights = ifelse(training_data_lasso$response==1, 1, optimum_wt),
                     standardize=F)


model_coef<- total_model$beta[, as.vector(optimum_lambdaIdx)]
odds_ratio<- exp(model_coef)[model_coef != 0]
non_zero_var<- names(model_coef)[as.vector(model_coef != 0)]
re_model_var<- c('response', non_zero_var)
#model<- data.frame(coefficient=model_coef[model_coef!=0], odds_ratio, p_value=p_value[match(non_zero_var, names(p_value))])
model_output <- data.frame(variable=non_zero_var, 
                           coefficient=as.vector(model_coef)[as.vector(model_coef) != 0], 
                           odds_ratio=odds_ratio)
write.csv(model_output, row.names=F)
#return(list(coef=model_output, optim_pmt=c(wt=optimum_wt, lmd=optimum_lmd, ms=auc_test_esti)))
train_pred <- predict(total_model, training_matrix, type="response")[, optimum_lambdaIdx]
test_pred <- predict(total_model, test_mtx, type="response")[, optimum_lambdaIdx]
test_resp_pred <- data.frame(resp=response_test, pred=test_pred)
write.csv(test_resp_pred, 'test_pred_forFullTrain.csv', row.names=F)
save(file='test_pred_forFullTrain.RData', test_resp_pred)
#generate the model performance on left-out test data
source("D:\\Shire_project\\03_code\\Jie\\sep\\subFunc_sep_Nov28.R")
outPath <- paste(rootPath, '04_result\\lasso\\test_wt(0.1)_lmd(500-85)', sep='')

if(!file.exists(outPath)){
    dir.create(outPath, recursive=T, showWarnings=T)
    setwd(outPath)
}else{
    setwd(outPath)
}

recall_tar_onTs <- sort(c(0.25, seq(0.5, 0.1, -0.1)), decreasing = T)
result_msOnTest <- msOnTest_sep_v2(test_pred, response_test, recall_tar_onTs)
result_msOnTrain <- msOnTest_sep_v2(train_pred, response_train, recall_tar_onTs)
ms_onTest <- result_msOnTest$ms
ms_onTrain <- result_msOnTrain$ms
ms_trTs <- data.frame(train=ms_onTrain, test=ms_onTest)
write.csv(ms_trTs, 'lasso_result_trTs_v2.csv', row.names = T)
write.csv(result_msOnTest$curve, 'curve_forTs.csv', quote=T, row.names = F)
rec_prec_tr <- result_msOnTrain$rec_prec
which(rec_prec_tr[, 1]==0.5)


