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
source("F:\\Jie\\Shire\\03_code\\subFunc_sep.R")


rootPath <- "F:\\Jie\\Shire\\"
inPath <- paste(rootPath, '02_data\\',sep='')

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
n.iter<- 5
#load("D:\\Shire_project\\03_code\\Dong's\\trainIndex.RData")
trainIdxHAE <- sample(1:nrow(dat_hae_1111_rf), 0.8*length(p_id_hae)) #986
trainIdxIterIDNonHAE <- lapply(trainIdxHAE, function(i){
    p_id <- p_id_hae[i]
    idx <- which(p_id_nonHae==p_id)
    iterId <- numeric(length(idx))
    iterId <- sample(rep(1:n.iter, length(iterId)/n.iter))
    return(cbind(idx, iterId))
})
check <- lapply(trainIdxIterIDNonHAE, function(X){
    length <- nrow(X)
    freq <- table(X[, 2])
    if(length(table(freq))==1 & freq[1]==200/n.iter){
        flag=T
    }else{
        flag=F
    }
    return(list(length=length, flag=flag))
})
check1 <- table(unlist(lapply(check, function(X)X$length)))
check2 <- table(unlist(lapply(check, function(X)X$flag)))
nonHaeIdx_train <- unlist(lapply(trainIdxIterIDNonHAE, function(X){X[, 1]})) #[1] 197200
#nonHaeIdx_test
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
#wt_list <- c(seq(0.005, 1, 0.05), 1)
wt_list <- c(0.5, 0.75, 1)
k.folds=5
n.iter=5
crit <- 0.3
lmd_length <- 500
result_list <- list()

#sampling 5 folds for HAE
#foldid_hae1 <- sample(rep(1:n.iter, length=nrow(trainHAE)))
set.seed(20)
foldid_hae <- createFolds(1:nrow(trainHAE), n.iter)
grid <- expand.grid(fold <- 1:k.folds, wt <- wt_list)
#train_data <- rbind(trainHAE[, -1], dat_nonhae_1111_rf[nonHaeIdx_train, -1]) #986*201=198186
#n.cpu <- Sys.getenv('NUMBER_OF_PROCESSORS')
#n.cpu <- nrow(grid)
#source("D:\\Shire_project\\03_code\\Jie\\subFunc_v2.R")
#or_coef <- get_or_coef(grid, train_data, n.cpu)
#write.csv(or_coef, 'coefficient_OR.csv', row.names = F)
#foldid_hae
#1   2   3   4   5 
#198 197 197 197 197 
iter=1
#outPath <- paste(inPath, 'preModel', sep='')
outPath <- paste(rootPath, '04_result\\lasso\\iter', iter, sep='')

if(!file.exists(outPath)){
    dir.create(outPath, recursive=T, showWarnings=T)
    setwd(outPath)
}else{
    setwd(outPath)
}
#define the traceFile to trace how the paralle process
traceFile <- paste(outPath, '\\traceFile.csv', sep='')
source("F:\\Jie\\Shire\\03_code\\subFunc_sep.R")

for(iter in 1:n.iter){
    start <- proc.time()
    cat(file=traceFile, append=T, 'for the model performance\n', 'wt list-', wt_list, ' lambda length-', lmd_length, 'k.folds-', k.folds, ' running....\n')
    cat('simulation:', iter, ' start!\n')
    haeIdx_iter <- foldid_hae[[iter]] #198
    
    nonhaeIdx_iter <- unlist(lapply(trainIdxIterIDNonHAE[haeIdx_iter], function(x)x[, 1])) #39600
    
    hae_test <- trainHAE[foldid_hae[[iter]],] #[1] 198 243
    hae_train <- trainHAE[setdiff(1:nrow(trainHAE), foldid_hae[[iter]]),] #[1] 788 243
    #nonHaeIdx_test_iter <- getNonHAEIdx_iter(iter, trainIdxIterIDNonHAE) #39440
    nonHae_test <- dat_nonhae_1111_rf[nonhaeIdx_iter,] #[1] 39600   243
    nonHae_train <- dat_nonhae_1111_rf[setdiff(nonHaeIdx_train, nonhaeIdx_iter),] #[1] 157600    243
    test_data_iter <- rbind(hae_test[, -1], nonHae_test[, -1]) #[1] 39798   242
    train_data_iter <- rbind(hae_train[, -1], nonHae_train[, -1])  #[1] 158388    242
    rm(list=c( 'nonHae_train',  'hae_train'))
    
    response_tr <- train_data_iter$response
    data_mtx <- model.matrix(response~., data=train_data_iter)[, -1]
    response_ts <- test_data_iter$response
    test_mtx <- model.matrix(response~., data=test_data_iter)[, -1]
    
    #[1] 10846   245
    #get initial lambda: because initial lambda  sequence only is related to x and y, but has no relationship with weight
    initial_lambda<-glmnet(x=data_mtx, y=response_tr, family="binomial", alpha=1, 
                           #weights=ifelse(response==1, 1, wt), 
                           standardize=F)$lambda  # calculating the initial lambda
    lambda_seq<- c(initial_lambda[-length(initial_lambda)] , 
                   seq(initial_lambda[length(initial_lambda)] , 0 , length=500)) 
    lambda_seq <- lambda_seq[1:5]
    # 599   get a length=500 descending sequence from initial lambda to 0
    
    #grid search for both weights and lambda
    
    #set.seed(1234) 10 folds cv (1:200)
    set.seed(20)
    foldid_hae_cv <- createFolds(haeIdx_iter, k.folds) #relavate index start with 1
    
    result_list[[iter]] <- get_optimum_model_iter_sep(haeIdx_iter, nonhaeIdx_iter, foldid_hae_cv, trainIdxIterIDNonHAE, traceFile, grid, lambda_seq)
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
pred_df <- pred_list[[1]][[1]]
resp_pred_df_forTs <- data.frame(p_id=c(hae_test[, 1], nonHae_test[, 1]), resp=response_ts, pred=pred_df[, 1])
save(resp_pred_df_forTs, file="resp_pred_df_forTs.RData")
#write.csv(pred_df, 'pred.csv', row.names = F)

optimum_pmt_list <- lapply(result_list, function(X){
    return(X[[2]])
})
optimum_pmt_df <- ldply(optimum_pmt_list, quickdf)
save(optimum_pmt_df, file="optimum_pmt_df.RData")
#outOfSmpCurve <- lapply(result_list, function(X){
 #   return(X[[3]])
#})
#save(outOfSmpCurve, file='outOfSmpCurve.RData')

#lasso grid search and pred on test completed

#generate the performance measure across all iterations
recall_tar_onTs <- sort(c(0.25, seq(0.5, 0.1, -0.1)), decreasing = T)
result_msOnTest <- msOnTest_sep(pred_df[, 2], response_ts, recall_tar_onTs)
ms_onTest <- result_msOnTest$ms
write.csv(ms_onTest, 'lasso_result.csv', row.names = T)
write.csv(result_msOnTest$curve, 'curve_forTs.csv', quote=T, row.names = F)
end2 <- proc.time()
execution_time2<- (end2-start2)[3]/60






