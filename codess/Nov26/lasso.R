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
sfSource("D:\\Shire_project\\03_code\\subFunc.R")


rootPath <- "D:\\Shire_project\\"
inPath <- paste(rootPath, '02_data\\',sep='')
#outPath <- paste(inPath, 'preModel', sep='')
outPath <- paste(rootPath, '04_result\\Lasso\\test2', sep='')

if(!file.exists(outPath)){
    dir.create(outPath, recursive=T, showWarnings=T)
    setwd(outPath)
}else{
    setwd(outPath)
}
#define the traceFile to trace how the paralle process
traceFile <- paste(outPath, '\\traceFile.csv', sep='')
load(paste(inPath, 'dat_hae_1111_rf.RData', sep=''))
load(paste(inPath, 'dat_nonhae_1111_rf.RData', sep=''))
names(dat_hae_1111_rf) <- tolower(gsub("^hae$", "response", names(dat_hae_1111_rf), ignore.case = T))
names(dat_nonhae_1111_rf) <- tolower(gsub("^hae$", "response", names(dat_nonhae_1111_rf), ignore.case = T))
#load(paste(rootPath, "03_code\\Dong's\\trainIndex.RData", sep=''))
p_id_hae <- dat_hae_1111_rf[, 1]
p_id_nonHae <- dat_nonhae_1111_rf[, 1]

#sampling 80% HEA as training and extract the matching NonHAE, 
#meanwhile records the iterration id for each training NonHAE idx

set.seed(20)
n.iter<- 20
trainIdxHAE <- sample(1:nrow(dat_hae_1111_rf), 0.8*length(p_id_hae)) #986
trainIdxIterIDNonHAE <- lapply(trainIdxHAE, function(i){
                        p_id <- p_id_hae[i]
                        idx <- which(p_id_nonHae==p_id)
                        iterId <- numeric(length(idx))
                        iterId <- sample(rep(1:n.iter, length(iterId)/n.iter))
                        return(cbind(idx, iterId))
                    })
#check the nonHAE_train sampleing number
n.trainNonHAE <- length(trainIdxIterIDNonHAE)*nrow(trainIdxIterIDNonHAE[[1]]) #[1] 197200
n.train <- length(trainIdxHAE)+n.trainNonHAE #[1] 198186

testHAE <- dat_hae_1111_rf[-trainIdxHAE,]
trainPid <- p_id_hae[trainIdxHAE]
testNonHAE <- dat_nonhae_1111_rf[is.na(match(p_id_nonHae, trainPid)), ]
test_data <- rbind(testHAE[, -1], testNonHAE[, -1])
response_test <- test_data$response
n.test <- nrow(testHAE)+nrow(testNonHAE) #[1] 49647

test_mtx <- model.matrix(response~., data=test_data)[, -1] #[1] 49647   245
#986 elements
trainHAE <- dat_hae_1111_rf[trainIdxHAE,]

#set some constants for lasso grid search and optimum parameter selection
#wt_list <- seq(0.1, 1, 0.1)
wt_list <- c(seq(0.4, 0.5, 0.02), seq(0.6, 1, 0.2))
#wt_list <- c(0.1, 0.5, 1)
k.folds=5
n.iter=20
crit <- 0.3
#curve_outOfSmp <- list()


start1 <- proc.time()
#num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
num_pros <- n.iter
sfInit(parallel=TRUE, cpus=num_pros, type='SOCK')
sfSource("D:\\Shire_project\\03_code\\subFunc.R")

sfExport('trainHAE', 'trainIdxIterIDNonHAE', 'wt_list', 'k.folds', 'crit',
         'test_mtx', 'traceFile', 'dat_nonhae_1111_rf', "dat_nonhae_1111_rf",
         "curve_outOfSmp")
sfExport("getNonHAEIdx_iter", 'createCurve', 'run_lasso_iter', 'get_out_of_sample_curve')
sfClusterEval(library("glmnet"))
sfClusterEval(library("ROCR"))
sfClusterEval(library("plyr"))
sfClusterEval(library("dplyr"))

end1 <- proc.time()
execution_time1<- (end1-start1)[3]/60

start2 <- proc.time()
result_list <- sfClusterApplyLB(1:n.iter, run_lasso_iter,
                              trainHAE=trainHAE, trainIdxIterIDNonHAE=trainIdxIterIDNonHAE, 
                              wt_list=wt_list, k.folds=k.folds, crit=crit, traceFile=traceFile)
pred_list <- lapply(result_list, function(X){
    return(X[[1]])
})
pred_df <- t(ldply(pred_list, quickdf))
resp_pred_df_forTs <- data.frame(p_id=c(testHAE[, 1], testNonHAE[, 1]), resp=response_test, pred=pred_df[, 1])
save(resp_pred_df_forTs, file="resp_pred_df_forTs.RData")
#write.csv(pred_df, 'pred.csv', row.names = F)

optimum_pmt_list <- lapply(result_list, function(X){
    return(X[[2]])
})
optimum_pmt_df <- ldply(optimum_pmt_list, quickdf)
save(optimum_pmt_df, file="optimum_pmt_df.RData")
outOfSmpCurve <- lapply(result_list, function(X){
    return(X[[3]])
})
save(outOfSmpCurve, file='outOfSmpCurve.RData')

#lasso grid search and pred on test completed

#generate the performance measure across all iterations
recall_tar_onTs <- sort(c(0.25, seq(0.5, 0.1, -0.1)), decreasing = T)
result_msOnTest <- msOnTest_dong(pred_df, response_test, recall_tar_onTs)
ms_onTest <- result_msOnTest$ms
write.csv(ms_onTest, 'lasso_result.csv', row.names = T)
write.csv(result_msOnTest$curve, 'curve_forTs.csv', quote=T, row.names = F)
end2 <- proc.time()
execution_time2<- (end2-start2)[3]/60






