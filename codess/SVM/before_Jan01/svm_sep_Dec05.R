#Shire project
#01 generate statistic distribution for full data before modeling
rm(list=ls())
library(xlsx)
library(ROCR)
library(plyr)
library(caret)
library(dplyr)
library(e1071)
library(snow)
library(snowfall)
library(caTools)
library(doParallel)
library(pROC)
library(PRROC)



rootPath <- "D:\\Shire_project\\"
inPath <- paste(rootPath, '02_data\\',sep='')

load(paste(inPath, 'dat_hae_1111_rf_nov26.RData', sep='')) #update the hae data by Dong
load(paste(inPath, 'dat_nonhae_1111_rf.RData', sep=''))
dat_hae_1111_rf <- dat_hae_1111_rf_nov26
rm(list=c('dat_hae_1111_rf_nov26'))
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
wt_list <-  c(0.01, 0.001, 1, 100, 1000)
cost_list <- c(1)
#wt_list <- c(0.01, 0.2, 0.6)
#wt_list <- c(0.1)
k.folds=5
n.iter=5
crit <- 0.3
result_list <- list()

#sampling 5 folds for HAE
#foldid_hae1 <- sample(rep(1:n.iter, length=nrow(trainHAE)))
set.seed(20)
foldid_hae <- createFolds(1:nrow(trainHAE), n.iter)
grid <- expand.grid(cost = cost_list, weights = wt_list)[,]
#train_data <- rbind(trainHAE[, -1], dat_nonhae_1111_rf[nonHaeIdx_train, -1]) #986*201=198186
#n.cpu <- Sys.getenv('NUMBER_OF_PROCESSORS')
num_pros <- nrow(grid)
#source("D:\\Shire_project\\03_code\\Jie\\subFunc_v2.R")
#or_coef <- get_or_coef(grid, train_data, n.cpu)
#write.csv(or_coef, 'coefficient_OR.csv', row.names = F)
#foldid_hae
#1   2   3   4   5 
#198 197 197 197 197 
iter=1
#outPath <- paste(inPath, 'preModel', sep='')
wt_str <- paste(wt_list, collapse=',')
cost_str <- paste(cost_list, collapse=',')
outPath <- paste(rootPath, '04_result\\svm\\iter', iter, '\\cost(', cost_str, ')_wt(', wt_str, ')', sep='')

if(!file.exists(outPath)){
    dir.create(outPath, recursive=T, showWarnings=T)
    setwd(outPath)
}else{
    setwd(outPath)
}
#define the traceFile to trace how the paralle process
traceFile <- paste(outPath, '\\traceFile.csv', sep='')
source("D:\\Shire_project\\03_code\\Jie\\svm\\svm_subFunc_Dec03.R")

for(iter in 1:n.iter){
    start <- proc.time()
    cat(file=traceFile, append=T, 'for the model performance\n', 'wt list-', wt_list, 'k.folds-', k.folds, ' running....\n')
    cat('simulation:', iter, ' start!\n')
    haeIdx_iter <- foldid_hae[[iter]] #198
    haeIdx_iter_forCV <- unlist(foldid_hae[-iter]) #198
    
    nonhaeIdx_iter <- unlist(lapply(trainIdxIterIDNonHAE[haeIdx_iter], function(x)x[, 1])) #39600  
    nonhaeIdx_iter_forCV <- unlist(lapply(trainIdxIterIDNonHAE[haeIdx_iter_forCV], function(x)x[, 1])) #39600  
    
    hae_test <- trainHAE[foldid_hae[[iter]],] #[1] 198 243
    hae_train <- trainHAE[setdiff(1:nrow(trainHAE), foldid_hae[[iter]]),] #[1] 788 243
    #nonHaeIdx_test_iter <- getNonHAEIdx_iter(iter, trainIdxIterIDNonHAE) #39440
    nonHae_test <- dat_nonhae_1111_rf[nonhaeIdx_iter,] #[1] 39600   243
    nonHae_train <- dat_nonhae_1111_rf[setdiff(nonHaeIdx_train, nonhaeIdx_iter),] #[1] 157600    243
    test_data_iter <- rbind(hae_test[, -1], nonHae_test[, -1]) #[1] 39798   242
    train_data_iter <- rbind(hae_train[, -1], nonHae_train[, -1])  #[1] 158388    242
    train_data_iter$response <- as.factor(ifelse(train_data_iter$response==1,'H', 'NH'))
    train_data_iter$region <- as.numeric(as.factor(train_data_iter$region))
    train_data_iter$gender <- as.numeric(as.factor(train_data_iter$gender))
    
    set.seed(20)
    foldid_hae_cv <- createFolds(haeIdx_iter_forCV, k.folds) #relavate index start with 1
    i=1
    useCaret=F
    if(useCaret==T){
        registerDoParallel(cores=10)    #specify the number of workers to use
        
        ctrl <- trainControl(method='none', number=k.folds, classProbs=TRUE, 
                             summaryFunction = get_auprSummary
                             # , returnResamp='none'
        )
        wtsvmlinear <- get_svmWtModel()
        start <- proc.time()
        cat("train model start!\n")
        cat(file=traceFile, append = T, "train model start!\n")
        
        svmFit_lin <- train(response~., data=cv_data_tr, method=wtsvmlinear, 
                            tuneGrid=grid[1,],
                            trControl=ctrl,
                            metric='ROC',
                            maximize=T,
                            preProc=c('center', 'scale')
        )
        end <- proc.time()
        timeUsed <- (end-start)[3]/60
        cat('train model end!\n', 'time Used:', timeUsed, 'min!\n', 'end time:', date(), '!\n')
        cat(file=traceFile, append = T, 'train model end!\n', 'time Used:', timeUsed, 'min!\n', 'end time:', date(), '!\n')
        save(file='svmFit_lin_wt(0.01)_cost(0.001)_v2.RData', svmFit_lin) 
        #grid search for both weights and lambda
    }else{
        result_list[[iter]] <- get_optimum_model_iter(haeIdx_iter, nonhaeIdx_iter, foldid_hae_cv, trainIdxIterIDNonHAE, traceFile, grid, num_pros)
        
    }
    
    
    #set.seed(1234) 10 folds cv (1:200)
    
    result_list[[iter]] <- get_optimum_model_iter(haeIdx_iter, nonhaeIdx_iter, foldid_hae_cv, trainIdxIterIDNonHAE, traceFile, grid, num_pros)
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
resp_pred_df_forTs <- data.frame(p_id=c(hae_test[, 1], nonHae_test[, 1]), resp=response_ts, pred=pred_df[, 2])
resp_pred_df_forTs <- data.frame(resp=response_ts, pred=pred_df[, 2])

save(resp_pred_df_forTs, file="resp_pred_df_forTs.RData")
write.csv(resp_pred_df_forTs[, ], paste('pred_score_val_iter_', iter, '.csv', sep=''), row.names = F)

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
result_msOnTest <- msOnTest_sep(pred_df[, 2], response_ts, recall_tar_onTs)
ms_onTest <- result_msOnTest$ms
write.csv(ms_onTest, 'lasso_result.csv', row.names = T)
write.csv(result_msOnTest$curve, 'curve_forTs.csv', quote=T, row.names = F)
#end2 <- proc.time()
#execution_time2<- (end2-start2)[3]/60




