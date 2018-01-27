#some functions
get_sepData_v1 <- function(iter){
    outPath <- paste(rootPath, '04_result\\data_sep\\iter', iter, sep='')
    if(!file.exists(outPath)){
        dir.create(outPath, recursive=T, showWarnings=T)
        setwd(outPath)
    }else{
        setwd(outPath)
    }
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
    trTs <- list(tr=train_data_iter, ts=test_data_iter)
    save(file='trTs.RData', trTs)
    rm(list=c( 'nonHae_train',  'hae_train'))
    set.seed(20)
    foldid_hae_cv <- createFolds(haeIdx_iter, k.folds) #relavate index start with 1
    data_forCV <- lapply(1:k.folds, function(i){
        dataset_cvi <- create_tr_ts_forCVi(haeIdx_iter_forCV, nonhaeIdx_iter_forCV, foldid_hae_cv, trainIdxIterIDNonHAE, i)
        #cv_training_data_lasso <- dataset_cvi[[2]]
        #cv_test_data_lasso <- dataset_cvi[[1]]
        cv_data <- dataset_cvi$cv_data_lasso
        cv_tr_flag <- dataset_cvi$tr_flag
        cv_data_tr <- cv_data[cv_tr_flag,] #[1] 127032    242
        cv_data_ts <- cv_data[!cv_tr_flag,] #[1] 31356   242
        cv_data_trTs <- list(cvTr=cv_data_tr, cvTs=cv_data_ts)
        save(file=paste('dataset_cv', i, '.RData', sep=''), cv_data_trTs)
        
    })
}

get_sepData_v2 <- function(path, iter){
    outPath <- paste(path, '\\iter', iter, sep='')
    if(!file.exists(outPath)){
        dir.create(outPath, recursive=T, showWarnings=T)
        setwd(outPath)
    }else{
        setwd(outPath)
    }
    haeIdx_iter <- foldid_hae[[iter]] #198
    haeIdx_iter_forCV <- unlist(foldid_hae[-iter]) #790
    
    nonhaeIdx_iter <- unlist(lapply(trainIdxIterIDNonHAE[haeIdx_iter], function(x)x[, 1])) #39600
    nonhaeIdx_iter_forCV <- unlist(lapply(trainIdxIterIDNonHAE[haeIdx_iter_forCV], function(x)x[, 1])) #39600  
    
    hae_test <- trainHAE[foldid_hae[[iter]],] #[1] 198 243
    hae_train <- trainHAE[setdiff(1:nrow(trainHAE), foldid_hae[[iter]]),] #[1] 788 243
    #nonHaeIdx_test_iter <- getNonHAEIdx_iter(iter, trainIdxIterIDNonHAE) #39440
    nonHae_test <- dat_nonhae_1111_rf[nonhaeIdx_iter,] #[1] 39600   243
    nonHae_train <- dat_nonhae_1111_rf[setdiff(nonHaeIdx_train, nonhaeIdx_iter),] #[1] 157600    243
    test_data_iter <- rbind(hae_test[, -1], nonHae_test[, -1]) #[1] 39798   242
    train_data_iter <- rbind(hae_train[, -1], nonHae_train[, -1])  #[1] 158388    242
    trTs <- list(tr=train_data_iter, ts=test_data_iter)
    save(file='trTs.RData', trTs)
    rm(list=c( 'nonHae_train',  'hae_train'))
    set.seed(20)
    foldid_hae_cv <- createFolds(haeIdx_iter_forCV, k.folds) #relavate index start with 1
    data_forCV <- lapply(1:k.folds, function(i){
        dataset_cvi <- create_tr_ts_forCVi(haeIdx_iter_forCV, nonhaeIdx_iter_forCV, foldid_hae_cv, trainIdxIterIDNonHAE, i)
        #cv_training_data_lasso <- dataset_cvi[[2]]
        #cv_test_data_lasso <- dataset_cvi[[1]]
        cv_data <- dataset_cvi$cv_data_lasso
        cv_tr_flag <- dataset_cvi$tr_flag
        cv_data_tr <- cv_data[cv_tr_flag,] #[1] 127032    242
        cv_data_ts <- cv_data[!cv_tr_flag,] #[1] 31356   242
        cv_data_trTs <- list(cv_data = cv_data, cv_tr_flag=cv_tr_flag)
        save(file=paste('dataset_cv', i, '.RData', sep=''), cv_data_trTs)
        
    })
}

getNonHAEIdx_iter <- function(iter, trainIdxIterIDNonHAE){
    nonHAE_idx <- unlist(lapply(trainIdxIterIDNonHAE, function(x){
        idx_iter <- x[x[, 2]==iter, 1]
        return(idx_iter)
    }))
    #9860
    return(nonHAE_idx)
}

createCurve <- function(resp, pred, recall_tar){
    predobj <- prediction(pred, resp)
    #add plot
    perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
    recall <- perf@x.values[[1]]
    precision <- perf@y.values[[1]]
    auc <- performance(predobj, 'auc')@y.values[[1]]
    
    rePrec <- cbind(recall, precision)
    bucket <- cut(recall, breaks=seq(0, 1, 0.005), include.lowest=T,right=F)
    rec_prec_byBucket <- aggregate(rePrec, by=list(bucket), function(i)mean(i, na.rm=T))
    
    ##in simulation
    #recall<- c(0.05, 0.1, 0.25, 0.5)
    
    temp4 <- unlist(lapply(recall_tar, function(X){
        idx <- which(abs(rec_prec_byBucket[, 2]-X)==min(abs(rec_prec_byBucket[, 2]-X), na.rm=T))
        prec_sel <- rec_prec_byBucket[idx, 3]
        return(prec_sel)
    }))    
    ##end 
    return(temp4)
    
}

msOnTest <- function(pred, response, recall_tar){
    temp3 <- lapply(1:nrow(pred), function(i){
        predobj <- prediction(pred[i,], response)
        #add plot
        perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
        recall <- perf@x.values[[1]]
        precision <- perf@y.values[[1]]
        auc <- performance(predobj, 'auc')@y.values[[1]]
        
        bucket <- cut(recall, breaks=seq(0, 1, 0.005), include.lowest=T,right=F)
        rec_prec <- cbind(recall, precision)
        rec_prec_byBucket <- aggregate(rec_prec, by=list(bucket), function(i)mean(i, na.rm=T))
        write.xlsx(rec_prec_result, paste('Curve_sepIter.xlsx', sep=''), 
                   sheetName=paste("Iter_",i), row.names=F, append=T)
        
        ##in simulation
        temp4 <- unlist(lapply(recall_tar, function(X){
            idx <- which(abs(rec_prec_byBucket[, 2]-X)==min(abs(rec_prec_byBucket[, 2]-X), na.rm=T))
            prec_sel <- rec_prec_byBucket[idx, 3]
            return(prec_sel)
        }))    
        
        ##end 
        
        return(list(auc=auc, rec_prec_byBucket=rec_prec_byBucket, ppv=temp4))
        
    })
    auc_mean <- mean(unlist(lapply(1:10, function(X)temp3[[X]][[1]])), na.rm=T)
    ppv_list <- lapply(temp3, function(X){
        return(X[[3]]) 
    })
    ppv_df <- ldply(ppv_mean, quickdf)
    ppv_mean <- apply(ppv_df, 2, mean, na.rm=T)
    ms <- c(auc_mean, ppv_mean)
    names(ms) <- c('auc', paste("PPV(recall=", recall_tar,')', sep=''))
    return(ms)
    
}
msOnTest_dong <- function(pred, response, recall_tar){
    pred <- apply(pred, 1, mean, na.rm=T)
    predobj <- prediction(pred, response)
    #add plot
    perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
    recall <- perf@x.values[[1]]
    precision <- perf@y.values[[1]]
    auc <- performance(predobj, 'auc')@y.values[[1]]
    rec_prec <- data.frame(recall=recall, precision=precision)
    rec_prec_omitMiss <- rec_prec[complete.cases(rec_prec),]
    aupr <- trapz(rec_prec_omitMiss$recall, rec_prec_omitMiss$precision)
    bucket <- cut(recall, breaks=seq(0, 1, 0.005), include.lowest=T,right=F)
    rec_prec_byBucket <- aggregate(rec_prec, by=list(bucket), function(i)mean(i, na.rm=T))
    #write.csv(rec_prec_byBucket, paste('Curve_dong.csv', sep=''), 
    #         row.names=F, quote=T)
    #plot
    pdf(file=paste('recall-precision curve on test.pdf', sep=''))
    #plot(recall, precision, type='l', main=paste('recall-precision curve'))
    plot(perf)
    dev.off()
    
    ##in simulation
    temp4 <- unlist(lapply(recall_tar, function(X){
        idx <- which(abs(rec_prec_byBucket[, 2]-X)==min(abs(rec_prec_byBucket[, 2]-X), na.rm=T))
        prec_sel <- rec_prec_byBucket[idx, 3]
        return(prec_sel)
    }))    
    
    ##end
    ms <- c(auc, aupr, temp4)
    names(ms) <- c('auc',"aupr", paste("PPV(recall=", recall_tar,')', sep=''))
    
    return(list(ms=ms, curve=rec_prec_byBucket))
    
}


msOnTest_sep_v2 <- function(pred, response, recall_tar){
    #pred <- apply(pred, 1, mean, na.rm=T)
    predobj <- prediction(pred, response)
    #add plot
    perf <- performance(predobj, 'ppv', 'sens') # added by jie for recall-precision plot.
    recall <- perf@x.values[[1]]
    precision <- perf@y.values[[1]]
    auc <- performance(predobj, 'auc')@y.values[[1]]
    rec_prec <- data.frame(recall=recall, precision=precision)
    rec_prec_omitMiss <- rec_prec[complete.cases(rec_prec),]
    aupr <- trapz(rec_prec_omitMiss$recall, rec_prec_omitMiss$precision)
    bucket <- cut(recall, breaks=seq(0, 1, 0.01), include.lowest=T,right=F)
    rec_prec_byBucket <- aggregate(rec_prec, by=list(bucket), function(i)mean(i, na.rm=T))
    #write.csv(rec_prec_byBucket, paste('Curve_dong.csv', sep=''), 
    #         row.names=F, quote=T)
    #plot
    pdf(file=paste('recall-precision curve on test.pdf', sep=''))
    #plot(recall, precision, type='l', main=paste('recall-precision curve'))
    plot(perf)
    dev.off()
    
    ##in simulation
    temp4 <- unlist(lapply(recall_tar, function(X){
        #idx <- sample(rep(which(abs(rec_prec[, 1]-X)==min(abs(rec_prec[, 1]-X), na.rm=T)), 2), 1)
        idx=which(abs(rec_prec[, 1]-X)==min(abs(rec_prec[, 1]-X), na.rm=T))[1]
        prec_sel <- rec_prec[idx, 2]
        return(prec_sel)
    }))    
    
    ##end
    ms <- c(auc, aupr, temp4)
    names(ms) <- c('auc',"aupr", paste("PPV(recall=", recall_tar,')', sep=''))
    
    return(list(ms=ms, curve=rec_prec_byBucket, rec_prec=rec_prec))
    
}
msOnTest_sep <- function(pred, response, recall_tar){
    #pred <- apply(pred, 1, mean, na.rm=T)
    predobj <- prediction(pred, response)
    #add plot
    perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
    recall <- perf@x.values[[1]]
    precision <- perf@y.values[[1]]
    auc <- performance(predobj, 'auc')@y.values[[1]]
    rec_prec <- data.frame(recall=recall, precision=precision)
    rec_prec_omitMiss <- rec_prec[complete.cases(rec_prec),]
    aupr <- trapz(rec_prec_omitMiss$recall, rec_prec_omitMiss$precision)
    bucket <- cut(recall, breaks=seq(0, 1, 0.005), include.lowest=T,right=F)
    rec_prec_byBucket <- aggregate(rec_prec, by=list(bucket), function(i)mean(i, na.rm=T))
    #write.csv(rec_prec_byBucket, paste('Curve_dong.csv', sep=''), 
    #         row.names=F, quote=T)
    #plot
    pdf(file=paste('recall-precision curve on test.pdf', sep=''))
    #plot(recall, precision, type='l', main=paste('recall-precision curve'))
    plot(perf)
    dev.off()
    
    ##in simulation
    temp4 <- unlist(lapply(recall_tar, function(X){
        idx <- which(abs(rec_prec[, 1]-X)==min(abs(rec_prec[, 1]-X), na.rm=T))[1]
        prec_sel <- rec_prec[idx, 2]
        return(prec_sel)
    }))    
    
    ##end
    ms <- c(auc, aupr, temp4)
    names(ms) <- c('auc',"aupr", paste("PPV(recall=", recall_tar,')', sep=''))
    
    return(list(ms=ms, curve=rec_prec_byBucket))
    
}

getBkt <- function(pred, resp, n.bkt){
    predobj <- prediction(pred, resp)
    #add plot
    perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
    recall <- perf@x.values[[1]]
    precision <- perf@y.values[[1]]
    rec_prec <- cbind(recall, precision)
    auc <- performance(predobj, 'auc')@y.values[[1]]
    rec_prec_omitMiss <- rec_prec[complete.cases(rec_prec),]
    aupr <- trapz(rec_prec_omitMiss[, 1], rec_prec_omitMiss[, 2])
    
    bucket <- cut(recall, breaks=seq(0, 1, 1/n.bkt), include.lowest=T,right=F)
    rec_prec_byBucket <- aggregate(rec_prec, by=list(bucket), function(i)mean(i, na.rm=T))
    return(list(bkt <- rec_prec_byBucket, ms <- c(auc=auc, aupr=aupr)))
}

get_out_of_sample_curve <- function(wt, lmd, foldid, response,data_mtx){
    curve_fromCV <- lapply(1:k.folds, function(i){
        dataset_cvi <- create_tr_ts_forCVi(haeIdx_iter, nonhaeIdx_iter, foldid_hae_cv, trainIdxIterIDNonHAE, i)
        train_cvi <- dataset_cvi[[2]]
        test_cvi <- dataset_cvi[[1]]
        cv_training_resp <- train_cvi$response
        cv_training_matrix<- model.matrix(response~., data=train_cvi)[, -1]
        cv_test_resp<- test_cvi$response                                                # select 1 fold as test data
        cv_test_matrix<- model.matrix(response~., data=test_cvi)[, -1]
        wt_vct <- ifelse(cv_training_resp==1, 1, wt)
        
        fit_lasso<- glmnet(cv_training_matrix, cv_training_resp, weights=wt_vct,
                           lambda=lmd, family="binomial", alpha=1, standardize=F)
        test_pred<- predict(fit_lasso, cv_test_matrix, type="response") #2713 599
        curve <- data.frame(resp=cv_test_resp, pred=test_pred)
        return(curve)
    })
    return(curve_fromCV)
}


grid_search_v2 <- function(r, training_data_lasso){
    i <- grid[r, 2]
    j <- grid[r, 1]
    cv_training_data_lasso<- training_data_lasso[foldid!=i,]                                            # select 9 folds as training data
    cv_training_matrix<- model.matrix(response~., data=cv_training_data_lasso)[,-1]
    cv_test_data_lasso<- training_data_lasso[foldid==i,]                                                # select 1 fold as test data
    cv_test_matrix<- model.matrix(response~., data=cv_test_data_lasso)[,-1]
    
    fit_lasso<- glmnet(cv_training_matrix, cv_training_data_lasso$response, 
                       lambda=lambda_seq, family="binomial", alpha=1, 
                       weights = ifelse(cv_training_data_lasso$response==1, 1, j),
                       standardize=F)
    test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
    test_pred_avg<- apply(test_pred, 2, function(x){createCurve(cv_test_data_lasso$response , x, crit)})
    test_pred_avg<- c(test_pred_avg , rep(NA , length(lambda_seq) - length(test_pred_avg))) # some small lambda may not be reached
    
    cv_auc<- c(wt=j, fold=i, ms=test_pred_avg)                                                              # calculate the AUC based on left-out fold
    return(cv_auc)
}#end for grid



get_or_coef <- function(grid, training_data_lasso, num_pros){
    
    foldid<- nrow(training_data_lasso)
    foldid[training_data_lasso$response==1]<-sample(rep(1:k.folds, length=length(which(training_data_lasso$response==1))))
    foldid[training_data_lasso$response==0]<- sample(rep(1:k.folds, length=length(which(training_data_lasso$response==0))))
    table(training_data_lasso$response , foldid) # QC
    #list_lasso_foldid<- rbind(list_lasso_foldid , foldid)
    
    # Calculating initial lambda and the lambda sequence
    training_matrix<- model.matrix(response~., data=training_data_lasso)[,-1]     # removes intercept term
    initial_lambda<-glmnet(x=training_matrix, y=training_data_lasso$response, family="binomial", alpha=1, standardize=F)$lambda  # calculating the initial lambda
    lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=500)) # get a length=500 descending sequence from initial lambda to 0
    #lambda_seq <- lambda_seq[1:5]
    # Tenfold cross-validation
    #num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
    sfInit(parallel=TRUE, cpus=num_pros, type='SOCK')
    sfSource("F:\\Jie\\Shire\\03_code\\subFunc_v2.R")
    
    sfExport('training_data_lasso', 'k.folds', 'crit', 'foldid',
             'lambda_seq', 'grid')
    sfExport('createCurve', 'grid_search_v2')
    sfClusterEval(library("glmnet"))
    sfClusterEval(library("ROCR"))
    sfClusterEval(library("plyr"))
    sfClusterEval(library("dplyr"))
    ms_fromWtFd <- sfClusterApplyLB(1:nrow(grid), grid_search_v2)
    sfStop()
    cv_auc <- ldply(ms_fromWtFd, quickdf)
    
    
    #cv_auc_group <- aggregate(cv_auc[,-c(1, 2)],by=list(wt=cv_auc[, 1],fold=cv_auc[, 2]),mean)          #[1] 110 590
    cv_auc_byWt <- aggregate(cv_auc[,-c(1, 2)],by=list(wt=cv_auc[, 1]),mean, na.rm=T)[, ]                            #[1]  11 587
    wt_seq <- cv_auc_byWt[, 1]
    alWt_forAuc<- apply(cv_auc_byWt[, -1], 1, which.max)                         # allocate the maximum lambda for each alpha 
    maxAuc_byWt <- apply(cv_auc_byWt, 1, function(x) x[which.max(x)])
    optimum_wtIdx <- which.max(maxAuc_byWt)                                        # allocate which alpha the final maximum lambda fall into
    optimum_lambdaIdx <-  alWt_forAuc[optimum_wtIdx]
    auc_test_esti <- maxAuc_byWt[optimum_wtIdx]
    optimum_wt <- wt_seq[optimum_wtIdx]
    optimum_lmd <- lambda_seq[optimum_lambdaIdx]
    # calculate coeff, odds ratio and p-value
    optim_model<- glmnet(x=training_matrix, y=training_data_lasso$response, 
                         lambda=optimum_lmd, family="binomial", 
                         alpha=1, 
                         weights = ifelse(training_data_lasso$response==1, 1, optimum_wt),
                         standardize=F)
    model_coef<- optim_model$beta
    odds_ratio<- exp(model_coef)[model_coef != 0]
    non_zero_var<- rownames(model_coef)[as.vector(model_coef != 0)]
    re_model_var<- c('response', non_zero_var)
    #model<- data.frame(coefficient=model_coef[model_coef!=0], odds_ratio, p_value=p_value[match(non_zero_var, names(p_value))])
    model_output <- data.frame(variable=non_zero_var, 
                               coefficient=as.vector(model_coef)[as.vector(model_coef) != 0], 
                               odds_ratio=odds_ratio)
    return(model_output)
}


grid_search_v1_sep_parOnWt <- function( j, traceFile, path_sepData, test){
    optMs_allWt <- numeric()
    load(paste(path_sepData, '\\trTs.RData', sep=''))
    trTs_data_iter <- rbind(trTs$tr, trTs$ts)
    trTs_matrix <- model.matrix(response~., data=trTs_data_iter)
    tr_matrix <- trTs_matrix[1:nrow(trTs$tr),]
    response_tr <- trTs$tr$response
    cat(file=traceFile, append=TRUE, 'model performance running\n', 'wt:', j, 'start!\n') #added by Jie
    initial_lambda<-glmnet(x=tr_matrix, y=response_tr, family="binomial", alpha=1, 
                           weights=ifelse(response_tr==1, 1, j), 
                           standardize=F)$lambda  # calculating the initial lambda
    lambda_seq<- c(initial_lambda[-length(initial_lambda)] , 
                   seq(initial_lambda[length(initial_lambda)] , 0 , length=lmd_length)) 
    if(test==T){
        lambda_seq <- lambda_seq[1:5]
    }
    rm(trTs, trTs_data_iter, trTs_matrix, tr_matrix)
    #lambda_seq <- lambda_seq[1:5]
    # 599   get a length=500 descending sequence from initial lambda to 0
    
    auc_fromCV <- lapply(1:k.folds, function(i){
        load(paste(path_sepData, '\\dataset_cv', i, '.RData', sep=''))
        cv_data <- cv_data_trTs$cv_data
        cv_tr_flag <- cv_data_trTs$cv_tr_flag
        cv_matrix <- model.matrix(response~., data = cv_data)
        cv_training_matrix <- cv_matrix[cv_tr_flag,]
        cv_test_matrix <- cv_matrix[!cv_tr_flag,]
        cv_resp_tr <- cv_data$response[cv_tr_flag]
        cv_resp_ts <- cv_data$response[!cv_tr_flag]
        
        start1 <- proc.time()
        cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, ' start!\n')
        cat("wt-", j, ' fold-', i, ' start!\n')
        cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, ' lasso start!\n')
        cat("wt-", j, ' fold-', i, ' lasso start!\n')
        wt_vct <- ifelse(cv_resp_tr==1, 1, j)
        
        fit_lasso<- glmnet(cv_training_matrix, cv_resp_tr, 
                           lambda=lambda_seq, family="binomial", alpha=1, 
                           weights = wt_vct,
                           standardize=F)
        test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
        rm(cv_data, cv_matrix, cv_training_matrix, cv_test_matrix)
        cat(file=traceFile, append=T, 'wt-', j, ' fold-', i, ' lasso and pred and rm object end!\n')
        
        test_pred_avg<- apply(test_pred, 2, function(x){createCurve(cv_resp_ts , x, crit)})
        test_pred_avg<- c(test_pred_avg , rep(NA , length(lambda_seq) - length(test_pred_avg))) # some small lambda may not be reached
        optN_cvi <- which(test_pred_avg==max(test_pred_avg, na.rm = T))
        optMs_cvi <- max(test_pred_avg, na.rm = T)
        optLmd_cvi <- lambda_seq[optN_cvi]
        cat("wt-", j, ' fold-', i, 'optN_cvi:', optN_cvi, 'optLmd_cvi', optLmd_cvi, 'maximum ms-', optMs_cvi, ' \n')
        cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, 'optN_cvi:', optN_cvi, 'optLmd_cvi', optLmd_cvi,optLmd_cvi, 'maximum ms-', optMs_cvi, ' \n')
        cat("wt-", j, ' fold-', i, ' end!\n')
        cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, ' time:', (proc.time()-start1)[3]/60, 'min!\n')
        cat("wt-", j, ' fold-', i, ' time:', (proc.time()-start1)[3]/60, 'min!\n')
        cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, ' end time:', date(), '!\n\n')
        cat("wt-", j, ' fold-', i, ' end time:', date(), '!\n\n')
        
        return(test_pred_avg)
    })
    ms_fromCV_df <- ldply(auc_fromCV, quickdf)
    ms_wtj <- apply(ms_fromCV_df, 2, mean,na.rm=T)
    optN <- which(ms_wtj==max(ms_wtj, na.rm=T))
    if(length(optN)>1){
        optN <- sample(optN, 1)
    }
    optLmd <- lambda_seq[optN]
    maxMs <- ms_wtj[optN]
    optMs_1wt <-  c(wt=j, idx=optN, ms=maxMs)
    
    cat(file=traceFile, append=TRUE, "wt-", j,' optimum lambda idx-', optN, ' optimum lambda-', optLmd, ' maximum(corresponding) ms-', maxMs, ' !\n')
    cat("wt-", j,' optimum lambda idx-', optN, ' optimum lambda-', optLmd, ' maximum(corresponding) ms-', maxMs, ' !\n')
    
    return(list(optMs_1wt=optMs_1wt, lambda_seq=lambda_seq))
}



grid_search_v1_sep <- function( traceFile, grid, lambda_seq, path_sepData){
    optMs_allWt <- numeric()
    for(j in wt_list){
        load(paste(path_sepData, '\\trTs.RData', sep=''))
        train_data_iter <- trTs$tr
        train_matrix <- model.matrix(response~., data=train_data_iter)
        response_tr <- train_data_iter$response
        cat(file=traceFile, append=TRUE, 'model performance running\n', 'wt:', j, 'start!\n') #added by Jie
        initial_lambda<-glmnet(x=train_matrix, y=response_tr, family="binomial", alpha=1, 
                               weights=ifelse(response_tr==1, 1, j), 
                               standardize=F)$lambda  # calculating the initial lambda
        lambda_seq<- c(initial_lambda[-length(initial_lambda)] , 
                       seq(initial_lambda[length(initial_lambda)] , 0 , length=lmd_length)) 
        rm(trTs, train_data_iter, train_matrix)
        #lambda_seq <- lambda_seq[1:5]
        # 599   get a length=500 descending sequence from initial lambda to 0
        
        auc_fromCV <- lapply(1:k.folds, function(i){
            load(paste(path_sepData, '\\dataset_cv', i, '.RData', sep=''))
            cv_data_tr <- cv_data_trTs$cvTr
            cv_data_ts <- cv_data_trTs$cvTs
            cv_resp_tr <- cv_data_tr$response
            cv_resp_ts <- cv_data_ts$response
            
            start1 <- proc.time()
            cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, ' start!\n')
            cat("wt-", j, ' fold-', i, ' start!\n')
            #cv_training_matrix<- model.matrix(response~., data=cv_training_data_lasso)[,-1]
            #cv_test_matrix<- model.matrix(response~., data=cv_test_data_lasso)[,-1]
            cv_training_matrix <- model.matrix(response ~., data=cv_data_tr)
            cv_test_matrix <- model.matrix(response ~., data=cv_data_ts)
            cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, ' lasso start!\n')
            cat("wt-", j, ' fold-', i, ' lasso start!\n')
            wt_vct <- ifelse(cv_resp_tr, 1, j)
            
            fit_lasso<- glmnet(cv_training_matrix, cv_resp_tr, 
                               lambda=lambda_seq, family="binomial", alpha=1, 
                               weights = wt_vct,
                               standardize=F)
            test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
            rm(cv_data_trTs, cv_data_tr, cv_data_ts, cv_training_matrix, cv_test_matrix)
            
            test_pred_avg<- apply(test_pred, 2, function(x){createCurve(cv_resp_ts , x, crit)})
            test_pred_avg<- c(test_pred_avg , rep(NA , length(lambda_seq) - length(test_pred_avg))) # some small lambda may not be reached
            optN_cvi <- which(test_pred_avg==max(test_pred_avg, na.rm = T))
            optMs_cvi <- max(test_pred_avg, na.rm = T)
            optLmd_cvi <- lambda_seq[optN_cvi]
            cat("wt-", j, ' fold-', i, 'optN_cvi:', optN_cvi, 'optLmd_cvi', optLmd_cvi, 'maximum ms-', optMs_cvi, ' \n')
            cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, 'optN_cvi:', optN_cvi, 'optLmd_cvi', optLmd_cvi,optLmd_cvi, 'maximum ms-', optMs_cvi, ' \n')
            cat("wt-", j, ' fold-', i, ' end!\n')
            cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, ' time:', (proc.time()-start1)[3]/60, 'min!\n')
            cat("wt-", j, ' fold-', i, ' time:', (proc.time()-start1)[3]/60, 'min!\n')
            cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, ' end time:', date(), '!\n\n')
            cat("wt-", j, ' fold-', i, ' end time:', date(), '!\n\n')
            
            return(test_pred_avg)
        })
        ms_fromCV_df <- ldply(auc_fromCV, quickdf)
        ms_wtj <- apply(ms_fromCV_df, 2, mean,na.rm=T)
        optN <- which(ms_wtj==max(ms_wtj, na.rm=T))
        if(length(optN)>1){
            optN <- sample(optN, 1)
        }
        optLmd <- lambda_seq[optN]
        maxMs <- ms_wtj[optN]
        optMs_allWt <- rbind(optMs_allWt, c(idx=optN, ms=maxMs))
        
        cat(file=traceFile, append=TRUE, "wt-", j,' optimum lambda idx-', optN, 'optimum lambda-', optLmd, ' !\n')
        cat("wt-", j,' optimum lambda idx-', optN, 'maximum(corresponding) ms-', maxMs, ' !\n')
        
    }
    opt_wt <- wt_list[which(optMs_allWt[, 2]==max(optMs_allWt[, 2], na.rm=T))]
    optMs <-  optMs_allWt[optMs_allWt[, 2]==max(optMs_allWt[, 2], na.rm=T), ]
    return(c(opt_wt=opt_wt, optMs))
}


grid_search_v2_sep <- function(haeIdx_iter, nonhaeIdx_iter, foldid_hae_cv, trainIdxIterIDNonHAE, traceFile, grid, lambda_seq){
    
    temp1 <- lapply(1:nrow(grid), function(r){
        j <- grid[r, 2] #wt
        i <- grid[r, 1] #fold
        
        #get the corresponding nonHae index for each folds
        #for foldid i
        
        dataset_cvi <- create_tr_ts_forCVi(haeIdx_iter, nonhaeIdx_iter, foldid_hae_cv, trainIdxIterIDNonHAE, i)
        cv_training_data_lasso <- dataset_cvi[[2]]
        cv_test_data_lasso <- dataset_cvi[[1]]
        cvIdx <- list(haeIdx_iter=haeIdx_iter, nonhaeIdx_iter=nonhaeIdx_iter)
        save(file=paste('dataset_cv', i, '.RData', sep=''), cvIdx)
        
        start1 <- proc.time()
        cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, ' start!\n')
        cat("wt-", j, ' fold-', i, ' start!\n')
        cv_training_matrix<- model.matrix(response~., data=cv_training_data_lasso)[,-1]
        cv_test_matrix<- model.matrix(response~., data=cv_test_data_lasso)[,-1]
        cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, ' lasso start!\n')
        cat("wt-", j, ' fold-', i, ' lasso start!\n')
        fit_lasso<- glmnet(cv_training_matrix, cv_training_data_lasso$response, 
                           lambda=lambda_seq, family="binomial", alpha=1, 
                           weights = ifelse(cv_training_data_lasso$response==1, 1, j),
                           standardize=F)
        test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
        test_pred_avg<- apply(test_pred, 2, function(x){createCurve(cv_test_data_lasso$response , x, crit)})
        test_pred_avg<- c(test_pred_avg , rep(NA , length(lambda_seq) - length(test_pred_avg))) # some small lambda may not be reached
        
        cv_auc<- c(wt=j, fold=i, ms=test_pred_avg)  
        cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, ' ms-', test_pred_avg, '\n')
        cat("wt-", j, ' fold-', i, ' end!\n')
        cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, ' time:', (proc.time()-start1)[3]/60, 'min!\n')
        cat("wt-", j, ' fold-', i, ' time:', (proc.time()-start1)[3]/60, 'min!\n')
        cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, ' end time:', date(), '!\n\n')
        cat("wt-", j, ' fold-', i, ' end time:', date(), '!\n\n')
        
        # calculate the AUC based on left-out fold
        return(cv_auc)
    })
    return(temp1)
    
}#end for grid

get_or_coef_sep_v1 <- function(trainIdxHAE, nonHaeIdx_train, foldid_hae_cv, grid, lmd_length, test){
    
    training_data_lasso <- rbind(dat_hae_1111_rf[trainIdxHAE, -1], dat_nonhae_1111_rf[nonHaeIdx_train, -1])
    
    # Calculating initial lambda and the lambda sequence
    training_matrix<- model.matrix(response~., data=training_data_lasso)[,-1]     # removes intercept term
    initial_lambda<-glmnet(x=training_matrix, y=training_data_lasso$response, family="binomial", alpha=1, standardize=F)$lambda  # calculating the initial lambda
    lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=lmd_length)) # get a length=500 descending sequence from initial lambda to 0
    if(test){
        lambda_seq <- lambda_seq[1:5]    
    }
    
    # Tenfold cross-validation
    
    #haeIdx_iter, nonhaeIdx_iter
    ms_fromWtFd <- grid_search_v1_sep(trainIdxHAE, nonHaeIdx_train, foldid_hae_cv, trainIdxIterIDNonHAE, traceFile, grid, lambda_seq)
    cat(file=traceFile, append = T, 'grid search end!\n')
    cat( 'grid search end!\n')
    
    optimum_lambdaIdx <- ms_fromWtFd[2]
    auc_test_esti <- ms_fromWtFd[3]
    optimum_wt <- ms_fromWtFd[1]
    optimum_lmd <- lambda_seq[optimum_lambdaIdx]
    cat('optimum lmd index-', optimum_lambdaIdx, 'optimum lmd-', lambda_seq[optimum_lambdaIdx],
        ' optimum wt-', optimum_wt, ' maximum ms-', auc_test_esti, ' !\n')
    cat(file=traceFile, append = T, 'optimum lmd index-', optimum_lambdaIdx, 'optimum lmd-', lambda_seq[optimum_lambdaIdx],
        ' optimum wt-', optimum_wt, ' maximum ms-', auc_test_esti, ' !\n')
    
    # calculate coeff, odds ratio and p-value
    cat(file=traceFile, append = T, 'apply optimum model to the whole training data start!\n')
    cat('apply optimum model to the whole training data start!\n')
    
    total_model<- glmnet(x=training_matrix, y=training_data_lasso$response, 
                         lambda=lambda_seq, family="binomial", 
                         alpha=1, 
                         weights = ifelse(training_data_lasso$response==1, 1, optimum_wt),
                         standardize=F)
    cat(file=traceFile, append = T, 'apply optimum model to the whole training data end!\n')
    cat('apply optimum model to the whole training data end!\n')
    
    model_coef<- total_model$beta[, as.vector(optimum_lambdaIdx)]
    odds_ratio<- exp(model_coef)[model_coef != 0]
    non_zero_var<- names(model_coef)[as.vector(model_coef != 0)]
    re_model_var<- c('response', non_zero_var)
    #model<- data.frame(coefficient=model_coef[model_coef!=0], odds_ratio, p_value=p_value[match(non_zero_var, names(p_value))])
    model_output <- data.frame(variable=non_zero_var, 
                               coefficient=as.vector(model_coef)[as.vector(model_coef) != 0], 
                               odds_ratio=odds_ratio)
    return(list(coef=model_output, optim_pmt=c(wt=optimum_wt, lmd=optimum_lmd, ms=auc_test_esti)))
}


get_optimum_model_iter <- function(num_pros, iter, grid, crit, traceFile){
    
    #num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
    sfInit(parallel=TRUE, cpus=num_pros, type='SOCK')
    sfSource("F:\\Jie\\Shire\\03_code\\subFunc_v3.R")
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' sfExport running!\n')
    sfExport('train_data_iter', 'k.folds', 'crit', 'foldid',
             'lambda_seq', 'grid', 'iter', 'traceFile')
    sfExport('createCurve', 'grid_search_v2')
    sfClusterEval(library("glmnet"))
    sfClusterEval(library("ROCR"))
    sfClusterEval(library("plyr"))
    sfClusterEval(library("dplyr"))
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' parallele grid search running!\n')
    
    ms_fromWtFd <- sfClusterApplyLB(1:nrow(grid), grid_search_v2, train_data_iter)
    sfStop()
    cv_auc <- ldply(ms_fromWtFd, quickdf)
    
    
    #cv_auc_group <- aggregate(cv_auc[,-c(1, 2)],by=list(wt=cv_auc[, 1],fold=cv_auc[, 2]),mean)          #[1] 110 590
    cv_auc_byWt <- aggregate(cv_auc[,-c(1, 2)],by=list(wt=cv_auc[, 1]),mean, na.rm=T)[, ]                            #[1]  11 587
    wt_seq <- cv_auc_byWt[, 1]
    alWt_forAuc<- apply(cv_auc_byWt[, -1], 1, which.max)                         # allocate the maximum lambda for each alpha 
    maxAuc_byWt <- apply(cv_auc_byWt, 1, function(x) x[which.max(x)])
    optimum_wtIdx <- which.max(maxAuc_byWt)                                        # allocate which alpha the final maximum lambda fall into
    optimum_lambdaIdx <-  alWt_forAuc[optimum_wtIdx]
    auc_test_esti <- maxAuc_byWt[optimum_wtIdx]
    optimum_wt <- wt_seq[optimum_wtIdx]
    optimum_lmd <- lambda_seq[optimum_lambdaIdx]
    # calculate coeff, odds ratio and p-value
    optim_model<- glmnet(x=data_mtx, y=train_data_iter$response, 
                         lambda=optimum_lmd, family="binomial", 
                         alpha=1, 
                         weights = ifelse(train_data_iter$response==1, 1, optimum_wt),
                         standardize=F)
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ':optimum model selection end!\n') #added by Jie
    #generate the performance measure for out of samples
    curve_outOfSmp <- get_out_of_sample_curve(optimum_wt, optimum_lmd, foldid, response_tr, data_mtx)
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ':out of sample end!\n') #added by Jie
    
    #apply the optimum weight and lambda to the test data
    fit_lasso_eval<- glmnet(data_mtx, response_tr, weights=ifelse(response_tr==1, 1, optimum_wt),
                            lambda=optimum_lmd, family="binomial", alpha=1, standardize=F)
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ':fit_lasso_eval end!\n') #added by Jie
    
    test_pred <- predict(fit_lasso_eval, test_mtx, type="response")
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ': test_pred end!\n') #added by Jie
    
    return(list(test_pred=cbind(response_ts, test_pred), 
                optimum_pmt=c(iter=iter, optim_lambda=optimum_lmd, optim_wt=optimum_wt, max_ms=auc_test_esti),  
                curve_outOfSmp=curve_outOfSmp))
}




get_optimum_model_iter_sep_v1 <- function(traceFile, grid, lambda_seq, path_sepData){
    
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' parallele grid search running!\n')
    cat( 'iter:', iter, ' parallele grid search running!\n')
    
    ms_fromWtFd <- grid_search_v1_sep(traceFile, grid, lambda_seq, path_sepData)
    
    cat(file=traceFile, append = T, 'grid search end!\n')
    cat( 'grid search end!\n')
    
    optimum_lambdaIdx <- ms_fromWtFd[2]
    auc_test_esti <- ms_fromWtFd[3]
    optimum_wt <- ms_fromWtFd[1]
    optimum_lmd <- lambda_seq[optimum_lambdaIdx]
    cat('optimum lmd index-', optimum_lambdaIdx, 'optimum lmd-', lambda_seq[optimum_lambdaIdx],
        ' optimum wt-', optimum_wt, ' maximum ms-', auc_test_esti, ' !\n')
    cat(file=traceFile, append = T, 'optimum lmd index-', optimum_lambdaIdx, 'optimum lmd-', lambda_seq[optimum_lambdaIdx],
        ' optimum wt-', optimum_wt, ' maximum ms-', auc_test_esti, ' !\n')
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ':optimum model selection end!\n') #added by Jie
    cat(append=TRUE, 'Iteration ', iter, ':optimum model selection end!\n') #added by Jie
    
    cat(file=traceFile, append = T, 'get the total model to the whole training data start!\n')
    cat('get the total model to the whole training data start!\n')
    #reload the training and test data
    load(paste(path_sepData, '\\trTs.RData', sep=''))
    train_data_iter <- trTs$tr
    test_data_iter <- trTs$ts
    train_matrix <- model.matrix(response~., data=train_data_iter)
    test_matrix <- model.matrix(response~., data=test_data_iter)
    
    response_tr <- train_data_iter$response
    response_ts <- test_data_iter$response
    
    total_model<- glmnet(x=train_matrix, y=response_tr, 
                         lambda=lambda_seq, family="binomial", 
                         alpha=1, 
                         weights = ifelse(response_tr==1, 1, optimum_wt),
                         standardize=F)
    #apply the optimum weight and lambda to the test data
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ':total model end!\n') #added by Jie
    cat(append=TRUE, 'Iteration ', iter, ':total model end!\n') #added by Jie
    
    test_pred <- predict(total_model, test_matrix, type="response")[, optimum_lambdaIdx]
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ': test_pred end!\n') #added by Jie
    cat(append=TRUE, 'Iteration ', iter, ': test_pred end!\n') #added by Jie
    
    return(list(test_pred=cbind(response_ts, test_pred), 
                optimum_pmt=c(iter=iter, optim_lambda=optimum_lmd, optim_wt=optimum_wt, max_ms=auc_test_esti)
                #,  curve_outOfSmp=curve_outOfSmp
    ))
}

get_optimum_model_iter_sep_v1_parOnWt <- function(traceFile, path_sepData, test){
    
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' parallele grid search running!\n')
    cat( 'iter:', iter, ' parallele grid search running!\n')
    test <- test
    #num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
    sfInit(parallel=TRUE, cpus=n.cpu, type='SOCK')
    sfSource("D:\\Shire_project\\03_code\\Jie\\sep\\subFunc_sep_Dec08.R")
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' sfExport running!\n')
    cat('iter:', iter, ' sfExport running!\n')
    sfExport('k.folds', 'crit', 'path_sepData'
             , 'iter', 'traceFile', 'lmd_length'
             , 'test'
             )
    sfExport('createCurve', 'grid_search_v1_sep_parOnWt')
    sfClusterEval(library("glmnet"))
    sfClusterEval(library("ROCR"))
    sfClusterEval(library("plyr"))
    sfClusterEval(library("dplyr"))
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' parallele grid search running!\n')
    cat('iter:', iter, ' parallele grid search running!\n')
    
    result_list <- sfClusterApplyLB(wt_list, grid_search_v1_sep_parOnWt
                                         , traceFile, path_sepData, test
    )
    sfStop()
    
    cat(file=traceFile, append = T, 'grid search end!\n')
    cat( 'grid search end!\n')
    
    ms_fromWtFd_list <- lapply(result_list, function(X){
        X[[1]]
    })
    lmd_seq_list <- lapply(result_list, function(X){
        X[[2]]
    })
    
    ms_fromWtFd <- ldply(ms_fromWtFd_list, quickdf)
    
    idx <- which(ms_fromWtFd[,3]==max(ms_fromWtFd[, 3], na.rm=T))
    if(length(idx)>1){
        idx <- sample(idx, 1)
    }
    lambda_seq <- lmd_seq_list[[idx]]
    optimum_lambdaIdx <- ms_fromWtFd[idx,2]
    auc_test_esti <- ms_fromWtFd[idx,3]
    optimum_wt <- ms_fromWtFd[idx,1]
    optimum_lmd <- lambda_seq[optimum_lambdaIdx]
    cat('optimum lmd index-', optimum_lambdaIdx, 'optimum lmd-', lambda_seq[optimum_lambdaIdx],
        ' optimum wt-', optimum_wt, ' maximum ms-', auc_test_esti, ' !\n')
    cat(file=traceFile, append = T, 'optimum lmd index-', optimum_lambdaIdx, 'optimum lmd-', lambda_seq[optimum_lambdaIdx],
        ' optimum wt-', optimum_wt, ' maximum ms-', auc_test_esti, ' !\n')
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ':optimum model selection end!\n') #added by Jie
    cat('Iteration ', iter, ':optimum model selection end!\n') #added by Jie
    
    cat(file=traceFile, append = T, 'get the total model to the whole training data start!\n')
    cat('get the total model to the whole training data start!\n')
    #start to train the full model on the whole training data
    start2 <- proc.time()
    cat('iter:', iter, ' train the full model on the whole training data using optimum wt start!'
        , 'start time:', date(), '\n'
    )
    cat(file=traceFile, append=T
        , 'iter:', iter, 'train the full model on the whole training data using optimum wt start!'
        , 'start time:', date(), '\n'
    )
    #reload the data for full training model data
    cat(file=traceFile, append=T
        , 'iter-', iter, 'reload the full training and test data start!\n')
    cat('iter-', iter, 'reload the full training and test data start!\n')
    
    load(paste(path_sepData, '\\trTs.RData', sep=''))
    trTs_data_iter <- rbind(trTs$tr, trTs$ts)
    trTs_matrix <- model.matrix(response~., data=trTs_data_iter)
    tr_matrix <- trTs_matrix[1:nrow(trTs$tr),]
    response_tr <- trTs$tr$response
    response_ts <- trTs$ts$response
    
    total_model<- glmnet(x=tr_matrix, y=response_tr, 
                         lambda=lambda_seq, family="binomial", 
                         alpha=1, 
                         weights = ifelse(response_tr==1, 1, optimum_wt),
                         standardize=F)
    rm(tr_matrix)
    #apply the optimum weight and lambda to the test data
    cat(file=traceFile, append=TRUE
        , 'Iteration ', iter, ':total model end!'
        , 'time Used for training full model is:', (proc.time()-start2)[3]/60, 'min!\n'
    ) 
    cat( 'Iteration ', iter, ':total model end!'
         , 'time Used for training full model is:', (proc.time()-start2)[3]/60, 'min!\n'
    ) 
    ts_matrix <- trTs_matrix[-(1:nrow(trTs$tr)),]
    test_pred <- predict(total_model, ts_matrix, type="response")[, optimum_lambdaIdx]
    rm(trTs_matrix, ts_matrix)
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ': test_pred end!\n') #added by Jie
    cat('Iteration ', iter, ': test_pred end!\n') #added by Jie
    
    return(list(test_pred=cbind(response_ts, test_pred), 
                optimum_pmt=c(iter=iter, optim_lambda=optimum_lmd, optim_wt=optimum_wt, max_ms=auc_test_esti)
                ,  lmd_seq=lmd_seq_list
    ))
}



create_tr_ts_forCVi <- function(haeIdx_iter, nonhaeIdx_iter, foldid_hae_cv, trainIdxIterIDNonHAE, i){
    haeIdx_iter_cviTs <- haeIdx_iter[foldid_hae_cv[[i]]] #20
    nonhaeIdx_iter_cviTs <- unlist(lapply(trainIdxIterIDNonHAE[haeIdx_iter_cviTs], function(x)x[, 1])) #4000  #pay attention to 
    
    haeIdx_iter_cviTr <- setdiff(haeIdx_iter, haeIdx_iter_cviTs) #178
    nonhaeIdx_iter_cviTr <- setdiff(nonhaeIdx_iter, nonhaeIdx_iter_cviTs) # 35600 Right!
    
    idx_iter_cviTs <- c(haeIdx_iter_cviTs, nonhaeIdx_iter_cviTs) #4020
    idx_iter_cviTr <- c(haeIdx_iter_cviTr, nonhaeIdx_iter_cviTr) # 35778
    
    HAE_cviTs <- dat_hae_1111_rf[haeIdx_iter_cviTs, -1]
    HAE_cviTr <- dat_hae_1111_rf[haeIdx_iter_cviTr, -1]
    NonHae_cviTs <- dat_nonhae_1111_rf[nonhaeIdx_iter_cviTs, -1]
    NonHae_cviTr <- dat_nonhae_1111_rf[nonhaeIdx_iter_cviTr, -1]
    
    cv_test_data_lasso <- as.data.frame(rbind(HAE_cviTs, NonHae_cviTs)) #[1] 4020  242
    cv_training_data_lasso <- as.data.frame(rbind(HAE_cviTr, NonHae_cviTr)) #[1] 35778   242
    cv_data_lasso <- rbind(cv_training_data_lasso, cv_test_data_lasso)
    tr_flag <- c(rep(TRUE, nrow(cv_training_data_lasso)), rep(FALSE, nrow(cv_test_data_lasso)))
    #return(list(test <- cv_test_data_lasso, train <- cv_training_data_lasso))
    return(list(cv_data_lasso=cv_data_lasso, tr_flag=tr_flag))
    
}





