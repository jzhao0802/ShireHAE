#some functions
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
        cv_training_resp <- response[foldid!=i]
        cv_training_matrix<- data_mtx[foldid!=i,]
        cv_test_resp<- response[foldid==i]                                                # select 1 fold as test data
        cv_test_matrix<- data_mtx[foldid==i,]
        wt_vct <- ifelse(cv_training_resp==1, 1, wt)
        
        fit_lasso<- glmnet(cv_training_matrix, cv_training_resp, weights=wt_vct,
                           lambda=lmd, family="binomial", alpha=1, standardize=F)
        test_pred<- predict(fit_lasso, cv_test_matrix, type="response") #2713 599
        curve <- data.frame(resp=cv_test_resp, pred=test_pred)
        return(curve)
    })
    return(curve_fromCV)
}


grid_search_v2 <- function(r){
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
    sfSource("D:\\Shire_project\\03_code\\Jie\\subFunc_v2.R")
    
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




grid_search <- function(wt){
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ' wt:', wt, 'start!\n') #added by Jie
    
    auc_fromCV <- lapply(1:k.folds, function(i){
        cv_training_resp <- response_tr[foldid!=i]
        cv_training_matrix<- data_mtx[foldid!=i,]
        cv_test_resp<- response_tr[foldid==i]                                                # select 1 fold as test data
        cv_test_matrix<- data_mtx[foldid==i,]
        wt_vct <- ifelse(cv_training_resp==1, 1, wt)
        
        fit_lasso<- glmnet(cv_training_matrix, cv_training_resp, weights=wt_vct,
                           lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
        test_pred<- predict(fit_lasso, cv_test_matrix, type="response") #2713 599
        auc1_byLmd <- apply(test_pred, 2, function(x){
            createCurve(cv_test_resp, x, crit)
        })
        auc1_byLmd<- c(wt=wt, auc1_byLmd , rep(NA , length(lambda_seq) - length(auc1_byLmd))) # some small lambda may not be reached
        return(auc1_byLmd)
    })
    ms_fromCV_df <- ldply(auc_fromCV, quickdf)
    ms_forWt <- apply(ms_fromCV_df, 2, mean,na.rm=T)
    
    return(ms_forWt)
    
    
}

get_optimum_model_iter <- function( num_pros, iter, wt_list, k.folds, crit, traceFile){
    
    #num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
    sfInit(parallel=TRUE, cpus=num_pros, type='SOCK')
    sfSource("D:\\Shire_project\\03_code\\Jie\\subFunc_v3.R")
    
    sfExport('response_tr', 'data_mtx', 'k.folds', 'crit', 'foldid',
             'traceFile', 'iter', 'lambda_seq')
    sfExport('createCurve', 'grid_search')
    sfClusterEval(library("glmnet"))
    sfClusterEval(library("ROCR"))
    sfClusterEval(library("plyr"))
    sfClusterEval(library("dplyr"))
    ms_fromWt <- sfClusterApplyLB(wt_list, grid_search)
    
    ms_fromWt_df <- ldply(ms_fromWt, quickdf)
    #find the optimum lambda and wt
    forMax <- lapply(1:nrow(ms_fromWt_df), function(r){
        row <- ms_fromWt_df[r,-1]
        lmd_max <- lambda_seq[which(row==max(row, na.rm=T))]
        max_value <- max(row)
        tb <- data.frame(wt=wt_list[r], lambda=lmd_max, ms_avg=max_value)
        rownames(tb) <- NULL
        return(tb)
    })
    forMax1 <- ldply(forMax, rbind)
    #find the optimum wt and lambda
    #optim_row <- forMax1[forMax1$ms_avg==max(forMax1$ms_avg),]
    optim_row <- forMax1[which(forMax1$ms_avg==max(forMax1$ms_avg, na.rm=T)),]
    optim_row <- optim_row[sample(nrow(optim_row), 1),]
    optim_wt <- as.numeric(optim_row[1])
    optim_lambda <- as.numeric(optim_row[2])
    max_ms <- as.numeric(optim_row[3])
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ':optimum model selection end!\n') #added by Jie
    #generate the performance measure for out of samples
    curve_outOfSmp <- get_out_of_sample_curve(optim_wt, optim_lambda, foldid, response_tr, data_mtx)
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ':out of sample end!\n') #added by Jie
    
    #apply the optimum weight and lambda to the test data
    fit_lasso_eval<- glmnet(data_mtx, response_tr, weights=ifelse(response_tr==1, 1, optim_wt),
                            lambda=optim_lambda, family="binomial", alpha=1, standardize=F)
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ':fit_lasso_eval end!\n') #added by Jie
    
    test_pred <- predict(fit_lasso_eval, test_mtx, type="response")
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ': test_pred end!\n') #added by Jie
    
    return(list(test_pred=cbind(response_ts, test_pred), 
                optimum_pmt=c(iter=iter, optim_lambda=optim_lambda, optim_wt=optim_wt, max_ms=max_ms),  
                curve_outOfSmp=curve_outOfSmp))
}









