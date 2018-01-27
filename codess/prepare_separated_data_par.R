#Shire project
#01 get the data separated by iteration and folds for cv

#Developer - Jie Zhao
#Develope time - Dec 2015-Jan2016

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


rootPath <- "D:\\Shire_project\\"
source("D:\\Shire_project\\03_code\\Jie\\sep\\subFunc_sep_Dec08.R")

prepare_sep_data <- function(type, k.folds=5, n.iter=5){
    if(type %in% c('rx', 'dx')){
        first <- substr(type, start=1, stop=1)
        sec <- substr(type, start=2, stop=2)
        #dataFlag <- gsub('(^\\w{2})(only$)', '\\1', fileSuffix, perl=T)
        inPath <- paste(rootPath, '02_data\\', toupper(first), sec, '_only\\', sep='')
        load(paste(inPath, 'dat_hae_1111_', type, 'only.RData', sep='')) #update the hae data by Dong
        load(paste(inPath, 'dat_nonhae_1111_', type, 'only.RData', sep=''))
        
    }else{
        inPath <- paste(rootPath, '02_data\\', sep='')
        load(paste(inPath, 'dat_hae_1111_rf_nov26.RData', sep='')) #update the hae data by Dong
        load(paste(inPath, 'dat_nonhae_1111_rf.RData', sep=''))
        
    }
    haeFn <- grep('_hae_', ls(), value=T)
    nonhaeFn <- grep('_nonhae_', ls(), value=T)
    eval(parse(text=paste('dat_hae_1111_rf <- ', haeFn, sep='')))
    eval(parse(text=paste('dat_nonhae_1111_rf <- ', nonhaeFn, sep='')))
    rm_list <- setdiff(grep('_hae_|_nonhae_', ls(), value=T), c('dat_hae_1111_rf', 'dat_nonhae_1111_rf'))
    rm(list=rm_list)
    names(dat_hae_1111_rf) <- tolower(gsub("^hae$", "response", names(dat_hae_1111_rf), ignore.case = T))
    names(dat_nonhae_1111_rf) <- tolower(gsub("^hae$", "response", names(dat_nonhae_1111_rf), ignore.case = T))
    #load(paste(rootPath, "03_code\\Dong's\\trainIndex.RData", sep=''))
    p_id_hae <- dat_hae_1111_rf[, 1]
    p_id_nonHae <- dat_nonhae_1111_rf[, 1]
    
    #sampling 80% HEA as training and extract the matching NonHAE, 
    #meanwhile records the iterration id for each training NonHAE idx
    
    set.seed(20)
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
    
    
    #sampling 5 folds for HAE
    set.seed(20)
    foldid_hae <- createFolds(1:nrow(trainHAE), n.iter)
    outPath <- paste(rootPath, '04_result\\data_sep\\', type, sep='')
    get_sepData_v2(outPath, 1)
    get_sepData_v2(outPath, 2)
    get_sepData_v2(outPath, 3)
    get_sepData_v2(outPath, 4)
    get_sepData_v2(outPath, 5)
    
}
prepare_sep_data(type='rx')
prepare_sep_data(type='dx')


#normalize the data
rNorm <- function(data, contiIdx){
    pNorm <- apply(data, 1, function(x){
        sum1 <- sum(x[-contiIdx])
        sum2 <- sum(x[contiIdx]^2)
        pNorm <- sqrt(sum1+sum2)
        return(pNorm)
    }) 
    
    data_rNorm <- lapply(1:nrow(data), function(r){
        return(data[r, ]/pNorm[r])
    }) 
    data_rNorm1 <- ldply(data_rNorm, quickdf)
    return(data_rNorm1)
}

normalize_trTs <- function(tr, ts){
    trTs_data_iter <- rbind(tr, ts)
    var_list <- names(trTs_data_iter)
    fct_flag <- sapply(trTs_data_iter, is.character)
    cati_var <- var_list[fct_flag]
    binary_var <- grep('flag$', var_list, value=T)
    conti_var <- grep('age|lookback_days|freq$', var_list, value=T)
    length(cati_var)+length(binary_var)+length(conti_var)+1==length(var_list)
    #var_list[which(is.na(match(var_list, c(cati_var, binary_var, conti_var, 'response'))))]
    #generate dummy variables for catigorical variables
    dummy <- model.matrix(response~., data=trTs_data_iter[, c('response', cati_var)])[,-1]#6
    binary_var2 <- c(binary_var, colnames(dummy))
    #calculate the mean and sd for catigorical varials on training data
    conti_scale_tr <- scale(tr[, conti_var])
    mean_sd_tr <- data.frame(mean=attr(conti_scale_tr, 'scaled:center')
                             , sd=attr(conti_scale_tr, 'scaled:scale'))
    conti_scale_ts_list <- lapply(conti_var, function(v){
        var <- ts[, v]
        mean_sd <- mean_sd_tr[match(v, rownames(mean_sd_tr)), ]
        scaled <- (var-mean_sd[1, 1])/mean_sd[1, 2]
        return(scaled)
    })
    conti_scale_ts <- ldply(conti_scale_ts_list, quickdf)
    conti_scale_ts <- t(conti_scale_ts)
    forRnorm_tr <- as.data.frame(cbind(conti_scale_tr
                                       , tr[, binary_var]
                                       , dummy[1:nrow(tr), ]))
    forRnorm_ts <- as.data.frame(cbind(conti_scale_ts
                                       , ts[, binary_var]
                                       , dummy[-(1:nrow(tr)), ]))
    names(forRnorm_ts) <- names(forRnorm_tr)
    
    contiIdx <- 1:length(conti_var)
    rm(trTs, dummy, conti_scale_tr, conti_scale_ts)
    tr_nr <- rNorm(forRnorm_tr, contiIdx)
    ts_nr <- rNorm(forRnorm_ts, contiIdx)
    tr_nr$response <- tr$response
    ts_nr$response <- ts$response
    trTs_rn <- list(tr=tr_nr, ts=ts_nr)
    return(trTs_rn)
    
}

norm_byIter <- function(iter){
    path_sepData <- paste(rootPath, '04_result\\data_sep\\', type, '\\iter', iter, sep='')
    path_sepData_norm <- paste(path_sepData, '\\norm', sep='')
    if(!file.exists(path_sepData_norm)){
        dir.create(path_sepData_norm, recursive=T, showWarnings=T)
        setwd(path_sepData_norm)
    }else{
        setwd(path_sepData_norm)
    }
    traceFile <- 'traceFile_forNorm.csv'
    #load(paste(path_sepData, '\\trTs.RData', sep=''))
    #get the normalized training and test data
    #start1 <- proc.time()
    #cat(file=traceFile, append=T, 'iter-', iter, ' normalize on training and test start!\n')
    #trTs_norm <- normalize_trTs(trTs$tr, trTs$ts)
    #time1 <- (proc.time()-start1)[3]/60
    #cat(file=traceFile, append=T, 'iter-', iter, ' normalize on training and test used time:'
     #   , time1, 'min!\n'
      #  )
    
    #save(file='trTs_rn.RData', trTs_norm)
    #get the normalized cv_tr and cv_ts data
    for(i in 1:k.folds){
        load(paste(path_sepData, '\\dataset_cv', i, '.RData', sep=''))
        cv_data <- cv_data_trTs$cv_data
        cv_tr_flag <- cv_data_trTs$cv_tr_flag
        start2 <- proc.time()
        cat(file=traceFile, append=T, 'iter-', iter, ' fold-', i, ' normalize on training and test start!\n')
        
        cv_trTs_norm <- normalize_trTs(cv_data[cv_tr_flag,], cv_data[!cv_tr_flag,])
        save(file=paste0('dataset_cv', i, '.RData'), cv_trTs_norm)
        time2 <- (proc.time()-start2)[3]/60
        cat(file=traceFile, append=T, 'iter-', iter, ' fold', i, ' normalize on training and test used time:'
            , time2, 'min!\n'
        )
        
    }
}
n.iter=5
k.folds <- 5
type='v2'
n.cpu=5
#num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
sfInit(parallel=TRUE, cpus=n.cpu, type='SOCK')
sfSource("D:\\Shire_project\\03_code\\Jie\\prepare_separated_data_par.R")
sfExport('k.folds', 'type', 'rootPath')
sfExport('rNorm', 'normalize_trTs', 'norm_byIter')
sfClusterEval(library("glmnet"))
sfClusterEval(library("ROCR"))
sfClusterEval(library("plyr"))
sfClusterEval(library("dplyr"))

result_list <- sfClusterApplyLB(1:n.iter, norm_byIter)
sfStop()


