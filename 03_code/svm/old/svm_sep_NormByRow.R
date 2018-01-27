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
rootPath <- "F:\\Jie\\Shire_project\\"

k.folds=5
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
#k.folds=1
wt_list <-  c(200)
cost_list <- c(0.0009, 0.0012, 0.0015)
grid <- expand.grid(cost = cost_list, weights = wt_list)[,]
n.cpu <- nrow(grid)

iter=3

path_sepData <- paste(rootPath, '04_result\\data_sep\\', type, '\\iter', iter, '\\norm', sep='')
wt_str <- paste(wt_list, collapse=',')
cost_str <- paste(cost_list, collapse=',')
outPath <- paste(rootPath, '04_result\\', model, '\\CV\\iter', iter,  '\\cost(', cost_str, ')_wt(', wt_str, ')_rn', sep='')
if(!file.exists(outPath)){
    dir.create(outPath, recursive=T, showWarnings=T)
    setwd(outPath)
}else{
    setwd(outPath)
}
#define the traceFile to trace how the paralle process
traceFile <- paste(outPath, '\\traceFile.csv', sep='')
source("F:\\Jie\\Shire_project\\03_code\\svm\\svm_subFunc_Dec03_normByRow.R")

for(iter in 1:n.iter){
    start <- proc.time()
    cat(file=traceFile, append=T, 'for the model performance\n', 'wt list-', wt_list,  ' k.folds-', k.folds, ' running....\n')
    cat('simulation:', iter, ' start!\n')
    #load in the separated data for iteration
    result_list[[iter]] <- get_optimum_model_iter_withCV(traceFile, path_sepData, n.cpu)
    save(file=paste('result_list_iter', iter, '.RData', sep=''), result_list)
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ': wt iteration end!\n') #added by Jie
    cat(append=TRUE, 'Iteration ', iter, ': wt iteration end!\n') #added by Jie
    
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
#recall_tar_onTs <- sort(c(0.25, seq(0.5, 0.1, -0.1)), decreasing = T)
recall_tar_onTs <- sort(c(seq(0.5, 0.05, -0.05)), decreasing = T)
result_msOnTest <- msOnTest_sep_v2(pred_df[, 2], pred_df[, 1], recall_tar_onTs)
ms_onTest <- result_msOnTest$ms
write.csv(ms_onTest, 'lasso_result.csv', row.names = T)
write.csv(result_msOnTest$curve, 'curve_forTs.csv', quote=T, row.names = F)
#end2 <- proc.time()
#execution_time2<- (end2-start2)[3]/60




