#Shire project
#01 generate statistic distribution for full data before modeling
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

#set some constants for lasso grid search and optimum parameter selection
#rootPath <- "D:\\Shire_project\\"
rootPath <- 'F:\\Jie\\Shire_project\\'
test_1fold <- T
k.folds= ifelse(test_1fold, 1, 5)
folder <- ifelse(test_1fold, 'test_1fold', 'CV')
n.iter=5
crit <- 0.3
result_list <- list()

#sampling 5 folds for HAE
#grid <- expand.grid(fold <- 1:k.folds, wt <- wt_list)
#n.cpu <- Sys.getenv('NUMBER_OF_PROCESSORS')
#wt_list <- c(seq(0.005, 1, 0.05), 1)
#wt_list <-  c(0.007, 0.015, 0.07, 0.09)
#wt_list <- c(0.01, 0.2, 0.6)
#wt_list <- c(0.1)
type <- 'v2'
model <- 'svm'
ONE <- F
kernel <- 'rbf'
#k.folds=1
wt_list <-  c(300)
wt_list <- ifelse(ONE, wt_list[1], wt_list)
cost_list <- c(0.001)
nu_list <- c(0.3, 0.5, 0.7) # a set of values for nu for one-class SVMs
if(ONE){
    cost_list <- nu_list
}
#cost_list <- c(0.001, 0.01, 0.1, 1, 10, 100)
gamma_list <-c(seq(0.001, 0.009, 0.003)
               , seq(0.01, 0.09, 0.03)
               , seq(0.1, 0.9, 0.1))

iter=1
clsMethod <- ifelse(ONE, 'one-classification', 'C-classification')
path_sepData <- paste(rootPath, '04_result\\data_sep\\', type, '\\iter', iter, '\\norm', sep='')
wt_str <- ifelse(ONE, NA, paste(wt_list, collapse=','))
cost_str <- paste(cost_list, collapse=',')
gamma_str <- paste(gamma_list, collapse=',')
if(kernel=='lin'){
    grid <- expand.grid(cost = cost_list, weights = wt_list)[,]
    outPath <- paste(rootPath, '04_result\\', model, '\\', folder, '\\iter', iter,  '\\cost(', cost_str, ')_wt(', wt_str, ')_ONE(', ONE, ')_rn', sep='')
    
}else if(kernel=='rbf'){
    grid <- expand.grid(cost = cost_list, gamma = gamma_list, weights = wt_list)[,]   
    outPath <- paste(rootPath, '04_result\\', model, '\\', folder, '\\iter', iter,  '\\cost(', cost_str, ')_gm(', gamma_str, ')_wt(', wt_str, ')_ONE(', ONE, ')_rn', sep='')
    
}
n.cpu <- nrow(grid)

if(!file.exists(outPath)){
    dir.create(outPath, recursive=T, showWarnings=T)
    setwd(outPath)
}else{
    setwd(outPath)
}
#define the traceFile to trace how the paralle process
traceFile <- paste(outPath, '\\traceFile.csv', sep='')
#source("D:\\Shire_project\\03_code\\Jie\\svm\\RBF\\svm_subFunc_Dec03_normByRow_rbf.R")
source("F:\\Jie\\Shire_project\\03_code\\svm\\svm_subFunc_Dec03_normByRow_rbf_1class.R")

for(iter in 1:n.iter){
    start <- proc.time()
    cat(file=traceFile, append=T, 'for the model performance\n', 'cost-', cost_list, ' gamma-', gamma_list , ' wt list-', wt_list,  ' k.folds-', k.folds, ' running....\n')
    cat('simulation:', iter, ' start!\n')
    #load in the separated data for iteration
    result_list[[iter]] <- get_optimum_model_iter_withCV(traceFile, path_sepData, n.cpu)
    save(file=paste('result_list_iter', iter, '.RData', sep=''), result_list)
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ': grid iteration end!\n') #added by Jie
    cat(append=TRUE, 'Iteration ', iter, ': grid iteration end!\n') #added by Jie
    
    end <- proc.time()
    execution_time<- (end-start)[3]/60
    cat('iteration:', iter, ' end!\n time used:', execution_time, ' min!\n')
    cat(file=traceFile, append=TRUE, 'iteration:', iter, ' end!\n time used:', execution_time, ' min!\n')
}





#summarize the results for 5 iterations;
pred_list <- lapply(result_list, function(X){
    return(X[[1]])
})
#pred_df <- t(ldply(pred_list, quickdf))
pred_df <- pred_list[[iter]]

save(pred_df, file="resp_pred_df_forTs.RData")
write.csv(pred_df, paste('resp_pred_df_forTs_iter', iter, '.csv', sep=''), row.names = F)

optimum_pmt_list <- lapply(result_list, function(X){
    return(X[[2]])
})
optimum_pmt_df <- ldply(optimum_pmt_list[iter], quickdf)
save(optimum_pmt_df, file="optimum_pmt_df.RData")
#outOfSmpCurve <- lapply(result_list, function(X){
#   return(X[[3]])
#})
#save(outOfSmpCurve, file='outOfSmpCurve.RData')

#lasso grid search and pred on test completed

#generate the performance measure across all iterations
recall_tar_onTs <- sort(c(0.25, seq(0.5, 0.1, -0.1)), decreasing = T)
result_msOnTest <- msOnTest_sep_v2(pred_df[, 2], pred_df[, 1], recall_tar_onTs)
ms_onTest <- result_msOnTest$ms
write.csv(ms_onTest, 'lasso_result.csv', row.names = T)
write.csv(result_msOnTest$curve, 'curve_forTs.csv', quote=T, row.names = F)
#end2 <- proc.time()
#execution_time2<- (end2-start2)[3]/60




