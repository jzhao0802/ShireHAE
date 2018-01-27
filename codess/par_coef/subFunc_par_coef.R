#subfunctions for k-folds cross validation and grid search(weighted + lambda) for lasso and ridge regression
#the functions can do parallel on k-folds CV and meanwhile can do parallel on grid search for optimum parameter selection

#Developter - Jie Zhao

#Develop time - Dec2015-Jan2016

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


grid_search_v1_sep <- function(haeIdx_iter, nonhaeIdx_iter, foldid_hae_cv, trainIdxIterIDNonHAE, traceFile, grid, lambda_seq){
    optMs_allWt <- numeric()
    for(j in wt_list){
        cat(file=traceFile, append=TRUE, 'coefficient separatly running\n', 'wt:', wt, 'start!\n') #added by Jie
        
        auc_fromCV <- lapply(1:k.folds, function(i){
            dataset_cvi <- create_tr_ts_forCVi(haeIdx_iter, nonhaeIdx_iter, foldid_hae_cv, trainIdxIterIDNonHAE, i)
            #cv_training_data_lasso <- dataset_cvi[[2]]
            #cv_test_data_lasso <- dataset_cvi[[1]]
            cv_data_lasso <- dataset_cvi$cv_data_lasso
            cv_tr_flag <- dataset_cvi$tr_flag
            cv_data_matrix <- model.matrix(response ~., data=cv_data_lasso)[, -1]
            cv_resp_tr <- cv_data_lasso$response[cv_tr_flag]
            cv_resp_ts <- cv_data_lasso$response[!cv_tr_flag]
            cvIdx <- list(haeIdx_iter=haeIdx_iter, nonhaeIdx_iter=nonhaeIdx_iter)
            save(file=paste('dataset_cv', i, '.RData', sep=''), cvIdx)
            
            start1 <- proc.time()
            cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, ' start!\n')
            cat("wt-", j, ' fold-', i, ' start!\n')
            #cv_training_matrix<- model.matrix(response~., data=cv_training_data_lasso)[,-1]
            #cv_test_matrix<- model.matrix(response~., data=cv_test_data_lasso)[,-1]
            cv_training_matrix <- cv_data_matrix[cv_tr_flag, ]
            cv_test_matrix <- cv_data_matrix[!cv_tr_flag,]
            cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, ' lasso start!\n')
            cat("wt-", j, ' fold-', i, ' lasso start!\n')
            wt_vct <- ifelse(cv_resp_tr, 1, j)
            
            fit_lasso<- glmnet(cv_training_matrix, cv_resp_tr, 
                               lambda=lambda_seq, family="binomial", alpha=1, 
                               weights = wt_vct,
                               standardize=F)
            test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
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
        optLmd <- ms_wtj[optN]
        optMs_allWt <- rbind(optMs_allWt, c(idx=optN, lmd=optLmd))
        
        cat(file=traceFile, append=TRUE, "wt-", j,' optimum lambda idx-', optN, 'optimum lambda-', optLmd, ' !\n')
        cat("wt-", j,' optimum lambda idx-', optN, 'optimum lambda-', optLmd, ' !\n')
        
    }
    opt_wt <- wt_list[which(optMs_allWt[, 2]==max(optMs_allWt[, 2], na.rm=T))]
    optMs <-  optMs_allWt[optMs_allWt[, 2]==max(optMs_allWt[, 2], na.rm=T), ]
    return(c(opt_wt=opt_wt, optMs))
}


grid_search_v1_par <- function(j,  haeIdx_iter, nonhaeIdx_iter){
    wt <- grid[j, 2]
    cat(file=traceFile, append=TRUE, 'coefficient separatly running\n', 'wt:', wt, 'start!\n') #added by Jie
    
    auc_fromCV <- lapply(1:k.folds, function(i){
        dataset_cvi <- create_tr_ts_forCVi(haeIdx_iter, nonhaeIdx_iter, foldid_hae_cv, trainIdxIterIDNonHAE, i)
        #cv_training_data_lasso <- dataset_cvi[[2]]
        #cv_test_data_lasso <- dataset_cvi[[1]]
        cv_data_lasso <- dataset_cvi$cv_data_lasso
        cv_tr_flag <- dataset_cvi$tr_flag
        cv_data_matrix <- model.matrix(response ~., data=cv_data_lasso)[, -1]
        cv_resp_tr <- cv_data_lasso$response[cv_tr_flag]
        cv_resp_ts <- cv_data_lasso$response[!cv_tr_flag]
        cvIdx <- list(haeIdx_iter=haeIdx_iter, nonhaeIdx_iter=nonhaeIdx_iter)
        save(file=paste('dataset_cv', i, '.RData', sep=''), cvIdx)
        
        start1 <- proc.time()
        cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, ' start!\n')
        cat("wt-", j, ' fold-', i, ' start!\n')
        #cv_training_matrix<- model.matrix(response~., data=cv_training_data_lasso)[,-1]
        #cv_test_matrix<- model.matrix(response~., data=cv_test_data_lasso)[,-1]
        cv_training_matrix <- cv_data_matrix[cv_tr_flag, ]
        cv_test_matrix <- cv_data_matrix[!cv_tr_flag,]
        cat(file=traceFile, append=TRUE, "wt-", j, ' fold-', i, ' lasso start!\n')
        cat("wt-", j, ' fold-', i, ' lasso start!\n')
        wt_vct <- ifelse(cv_resp_tr, 1, j)
        
        fit_lasso<- glmnet(cv_training_matrix, cv_resp_tr, 
                           lambda=lambda_seq, family="binomial", alpha=1, 
                           weights = wt_vct,
                           standardize=F)
        test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
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
    optLmd <- ms_wtj[optN]
    optMs_allWt <- rbind(optMs_allWt, c(idx=optN, lmd=optLmd))
    
    cat(file=traceFile, append=TRUE, "wt-", j,' optimum lambda idx-', optN, 'optimum lambda-', optLmd, ' !\n')
    cat("wt-", j,' optimum lambda idx-', optN, 'optimum lambda-', optLmd, ' !\n')
    opt_wt <- wt_list[which(optMs_allWt[, 2]==max(optMs_allWt[, 2], na.rm=T))]
    optMs <-  optMs_allWt[optMs_allWt[, 2]==max(optMs_allWt[, 2], na.rm=T), ]
    
    return(optMs_allWt)
}


get_or_coef_par <- function(trainIdxHAE, nonHaeIdx_train, wt_list, lmd_length, num_pros, test=F){
    training_data_lasso <- rbind(dat_hae_1111_rf[trainIdxHAE, -1], dat_nonhae_1111_rf[nonHaeIdx_train, -1])
    # Calculating initial lambda and the lambda sequence
    training_matrix<- model.matrix(response~., data=training_data_lasso)[,-1]     # removes intercept term
    initial_lambda<-glmnet(x=training_matrix, y=training_data_lasso$response, family="binomial", alpha=1, standardize=F)$lambda  # calculating the initial lambda
    lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=lmd_length)) # get a length=500 descending sequence from initial lambda to 0
    if(test){
        lambda_seq <- lambda_seq[1:5]    
    }
    # Tenfold cross-validation
    #num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
    sfInit(parallel=TRUE, cpus=num_pros, type='SOCK')
    sfSource("D:\\Shire_project\\03_code\\Jie\\par_coef\\subFunc_par_coef.R")

    cat('Export starts!\n')
    cat(file=traceFile, append = T, 'Export starts!\n')
    start_exp <- proc.time()
    
    sfExport('training_data_lasso', 'dat_hae_1111_rf', 'dat_nonhae_1111_rf'
             , 'trainIdxHAE', 'nonHaeIdx_train', 'foldid_hae', 'trainIdxIterIDNonHAE'
             , 'crit', 'lambda_seq', 'wt_list')
    
    sfExport('createCurve', 'grid_search_v1_par', 'create_tr_ts_forCVi', 'traceFile')
    sfClusterEval(library("glmnet"))
    sfClusterEval(library("ROCR"))
    sfClusterEval(library("plyr"))
    sfClusterEval(library("dplyr"))
    cat('Export ends! used time-', (proc.time()-start_exp)[3]/60, 'min\n')
    cat(file=traceFile, append = T, 'Export ends! used time-', (proc.time()-start_exp)[3]/60, 'min\n')
    
    ms_list <- sfClusterApplyLB(1:nrow(grid[4:7,]), grid_search_v1_par, trainIdxHAE, nonHaeIdx_train)
    sfStop()
    optMs_allWt <- ldply(ms_list, quickdf)
    
   
    
    
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



get_or_coef_sep <- function(trainIdxHAE, nonHaeIdx_train, grid, lmd_length, test){
    
    training_data_lasso <- rbind(dat_hae_1111_rf[trainIdxHAE, -1], dat_nonhae_1111_rf[nonHaeIdx_train, -1])
    
    # Calculating initial lambda and the lambda sequence
    training_matrix<- model.matrix(response~., data=training_data_lasso)[,-1]     # removes intercept term
    initial_lambda<-glmnet(x=training_matrix, y=training_data_lasso$response, family="binomial", alpha=1, standardize=F)$lambda  # calculating the initial lambda
    lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=lmd_length)) # get a length=500 descending sequence from initial lambda to 0
    if(test){
        lambda_seq <- lambda_seq[1:5]    
    }
    
    # Tenfold cross-validation
    set.seed(20)
    foldid_hae_cv <- createFolds(1:nrow(trainHAE), k.folds)
    
    #haeIdx_iter, nonhaeIdx_iter
    ms_fromWtFd <- grid_search_v2_sep(trainIdxHAE, nonHaeIdx_train, foldid_hae_cv, trainIdxIterIDNonHAE, traceFile, grid, lambda_seq)
    cv_auc <- ldply(ms_fromWtFd, quickdf)
    cat(file=traceFile, append = T, 'grid search end!\n')
    
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
    cat(file=traceFile, append = T, 'apply optimum model to the whole training data start!\n')
    cat('apply optimum model to the whole training data start!\n')
    
    optim_model<- glmnet(x=training_matrix, y=training_data_lasso$response, 
                         lambda=optimum_lmd, family="binomial", 
                         alpha=1, 
                         weights = ifelse(training_data_lasso$response==1, 1, optimum_wt),
                         standardize=F)
    cat(file=traceFile, append = T, 'apply optimum model to the whole training data end!\n')
    cat('apply optimum model to the whole training data end!\n')
    
    model_coef<- optim_model$beta
    odds_ratio<- exp(model_coef)[model_coef != 0]
    non_zero_var<- rownames(model_coef)[as.vector(model_coef != 0)]
    re_model_var<- c('response', non_zero_var)
    #model<- data.frame(coefficient=model_coef[model_coef!=0], odds_ratio, p_value=p_value[match(non_zero_var, names(p_value))])
    model_output <- data.frame(variable=non_zero_var, 
                               coefficient=as.vector(model_coef)[as.vector(model_coef) != 0], 
                               odds_ratio=odds_ratio)
    return(list(coef=model_output, optim_pmt=c(wt=optimum_wt, lmd=optimum_lmd, ms=auc_test_esti)))
}






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















