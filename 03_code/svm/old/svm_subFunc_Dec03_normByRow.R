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
createCurve_v2 <- function(resp, pred, recall_tar){
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
        idx <- which(abs(rePrec[, 1]-X)==min(abs(rePrec[, 1]-X), na.rm=T))[1]
        prec_sel <- rePrec[idx, 2]
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






grid_search_1fold_svm_findPred <- function(r, data, data_ts){
    cost <- grid[r, 1]
    wt <- grid[r, 2]
    wts <-  list( c("NH"=1,"H"=wt) )
    resp_ts <- ifelse(data_ts$response=='H', 1, 0)
    x_ts <- data_ts[, -match('response', names(data_ts))]
    cat(file = traceFile, append=T, 'iter-', iter , ' wt-', wt, ' cost-', cost, ' start!\n')
    start <- proc.time()
    load(paste('pred_wt(', wt,')cost(', cost, ').RData', sep=''))
    svmFit <- pred_model$model_onSubTr
    cat(file=traceFile, append=T, 'iter-', iter , ' wt-', wt, ' cost-', cost, ' svm training on subtrain loading end! '
        , 'time used-', (proc.time()-start)[3]/60, 'min! '
        , 'ending time-', date() ,'\n'
    )
    pred <- predict(svmFit, x_ts , sclae=T,decision.values = TRUE)
    cat(file=traceFile, append=T,'iter-', iter , ' wt-', wt, ' cost-', cost, ' pred on left out fold end! '
        , 'time used-', (proc.time()-start)[3]/60, 'min! '
        , 'ending time-', date() ,'\n'
    )
    predscore <- attr(pred, "decision.values")
    pred_model <- list(model_onSubTr=svmFit, pred_ts = predscore)
    save(file=paste('pred_wt(', wt,')cost(', cost, ')_v2.RData', sep=''), pred_model)
    ms <- createCurve(resp = resp_ts, pred=predscore, recall_tar = crit)
    grid_ms = c(cost=cost, weight=wt, ms=ms)
    cat(file=traceFile, append=T,'iter-', iter ,  ' wt-', wt, ' cost-', cost, ' ms-', ms, '\n')
    
    return(list(grid_ms = grid_ms, model_onSubTr=svmFit, pred_ts = pred))
    
}






grid_search_1fold_svm <- function(r, data, data_ts){
    cost <- grid[r, 1]
    wt <- grid[r, 2]
    wts <-  list( c("NH"=1,"H"=wt) )
    resp_ts <- ifelse(data_ts$response=='H', 1, 0)
    x_ts <- data_ts[, -match('response', names(data_ts))]
    cat(file = traceFile, append=T, 'iter-', iter , ' wt-', wt, ' cost-', cost, ' start!\n')
    start <- proc.time()
    svmFit <- svm(
        response~., data=data
        #TrainData, TrainClasses
        , cost=cost, class.weights = wts[[1]]
        , type="C-classification", kernel='linear'
        , scale=T
    )
    cat(file=traceFile, append=T, 'iter-', iter , ' wt-', wt, ' cost-', cost, ' svm training on subtrain end! '
        , 'time used-', (proc.time()-start)[3]/60, 'min! '
        , 'ending time-', date() ,'\n'
    )
    pred <- predict(svmFit, x_ts , sclae=T,decision.values = TRUE)
    cat(file=traceFile, append=T,'iter-', iter , ' wt-', wt, ' cost-', cost, ' pred on left out fold end! '
        , 'time used-', (proc.time()-start)[3]/60, 'min! '
        , 'ending time-', date() ,'\n'
    )
    predscore <- attr(pred, "decision.values")
    pred_model <- list(model_onSubTr=svmFit, pred_ts = predscore)
    save(file=paste('pred_wt(', wt,')cost(', cost, ').RData', sep=''), pred_model)
    ms <- createCurve(resp = resp_ts, pred=predscore, recall_tar = crit)
    grid_ms = c(cost=cost, weight=wt, ms=ms)
    cat(file=traceFile, append=T,'iter-', iter ,  ' wt-', wt, ' cost-', cost, ' ms-', ms, '\n')
    
    return(list(grid_ms = grid_ms, model_onSubTr=svmFit, pred_ts = pred))
    
}
get_optimum_model_iter <- function(haeIdx_iter, nonhaeIdx_iter, foldid_hae_cv, trainIdxIterIDNonHAE, traceFile, grid, num_pros){
    dataset_cvi <- create_tr_ts_forCVi(haeIdx_iter_forCV, nonhaeIdx_iter_forCV, foldid_hae_cv, trainIdxIterIDNonHAE, i)
    #cv_training_data_lasso <- dataset_cvi[[2]]
    #cv_test_data_lasso <- dataset_cvi[[1]]
    cv_data <- dataset_cvi$cv_data_lasso
    cv_tr_flag <- dataset_cvi$tr_flag
    cv_data$response <- as.factor(ifelse(cv_data$response==1,'H', 'NH'))
    cv_data$region <- as.numeric(as.factor(cv_data$region))
    cv_data$gender <- as.numeric(as.factor(cv_data$gender))
    
    cv_data_tr <- cv_data[cv_tr_flag,] #[1] 127032    242
    cv_data_ts <- cv_data[!cv_tr_flag,] #[1] 31356   242
    cv_resp_ts <- cv_data_ts$response
    #generate dummy bariables for catigorical varibles(i.e. gender and region)
    cati_var <- c('gender', 'region')
    dummy_df <- model.matrix(response~., data=cv_data_tr[, match(cati_var, names(cv_data_tr))] 
                             
                             )
    #scale the 4/5 folds data and save the mean and sd to apply on 1/5 fold scaling
    conti_var <- c('age', 'lookback_days', grep('freq$', names(cv_data_tr), value=T))
    mean_sd <- lapply(conti_var, function(v){
        var <- cv_data_tr[, v]
        var_scale <- scale(var)
        mean <- mean(var) 
        sd <- sd(var)
        return(c(mean=mean, sd=sd))
    })
    conti_scale_df <- scale(cv_data_tr[, match(conti_var, name(cv_data_cv))])
    cv_data_tr_scale <- cbind(cv_data_tr[, -match(c(conti_var, cati_var),names(cv_data_cv))], dummy_df, conti_scale_df)
    
    #num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
    sfInit(parallel=TRUE, cpus=num_pros, type='SOCK')
    sfSource("D:\\Shire_project\\03_code\\Jie\\svm\\svm_subFunc_Dec03.R")
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' sfExport running!\n')
    sfExport('cv_data_tr', 'crit', 'cv_data_ts'
             , 'grid', 'iter', 'traceFile')
    sfExport('createCurve', 'grid_search_1fold_svm')
    #sfClusterEval(library("glmnet"))
    sfClusterEval(library("e1071"))
    sfClusterEval(library("ROCR"))
    sfClusterEval(library("plyr"))
    sfClusterEval(library("dplyr"))
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' parallele grid search running!\n')
    
    ms_fromGrid <- sfClusterApplyLB(1:nrow(grid), grid_search_1fold_svm, cv_data_tr, cv_data_ts)
    sfStop()
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' parallele grid search end!\n')
    
    save(file='fullRecords.RData', ms_fromGrid)
    ms_list <- lapply(ms_fromGrid, function(X)X[[1]])
    ms_df <- ldply(ms_list, quickdf)
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' run over for initial grid search!\n')
    
    return(ms_df)
}

grid_search_cv_svm <- function(r){
    cost <- grid[r, 1]
    wt <- grid[r, 2]
    wts <-  list( c("NH"=1,"H"=wt) )
    cat(file = traceFile, append=T, 'iter-', iter , ' wt-', wt, ' cost-', cost, ' start!\n')
    start1 <- proc.time()
    ms_onCV <- lapply(1:k.folds, function(i){
        #load the cv data for  cv i
        load(paste(path_sepData, '\\dataset_cv', i, '.RData', sep=''))
        
        cv_data_tr <- cv_trTs_norm$tr #[1] 127032    242
        cv_data_tr$response <- as.factor(ifelse(cv_data_tr$response==1, 'H', 'NH'))
        cv_data_ts <- cv_trTs_norm$ts #[1] 31356   242
        cv_resp_ts <- cv_data_ts$response
        cv_x_ts <- cv_data_ts[, -match('response', names(cv_data_ts))]
        cat(file=traceFile, append=T, 'iter-', iter , ' wt-', wt, ' cost-', cost, ' Fold-', i, ' svm training on subtrain start! \n')
        start <- proc.time()
        svmFit <- svm(
            response~., data=cv_data_tr
            #TrainData, TrainClasses
            , cost=cost, class.weights = wts[[1]]
            , type="C-classification", kernel='linear'
            , scale=F
        )
        save(file=paste('svmModel_cost_', cost, '_wt_', wt, '_fold_', i, '.RData', sep=''), svmFit)
        cat(file=traceFile, append=T, 'iter-', iter , ' wt-', wt, ' cost-', cost, ' Fold-', i, ' svm training on subtrain end! '
            , 'time used-', (proc.time()-start1)[3]/60, 'min! '
            , 'ending time-', date() ,'\n'
        )
        
        pred <- predict(svmFit, cv_x_ts , scale=F,decision.values = TRUE)
        cat(file=traceFile, append=T,'iter-', iter , ' wt-', wt, ' cost-', cost, ' Fold-', i, ' pred on left out fold end! '
            , 'time used-', (proc.time()-start1)[3]/60, 'min! '
            , 'ending time-', date() ,'\n'
        )
        
        predscore <- attr(pred, "decision.values")
        pred_model <- list(model_onSubTr=svmFit, resp_pred_ts = data.frame(resp=cv_resp_ts, pred=predscore))
        save(file=paste('pred_wt(', wt,')cost(', cost, ')Fold',i, '.RData', sep=''), pred_model)
        ms <- createCurve_v2(resp = cv_resp_ts, pred=predscore, recall_tar = crit)
        grid_ms = c(cost=cost, weight=wt, fold=i, ms=ms)
        cat(file=traceFile, append=T,'iter-', iter ,  ' wt-', wt, ' cost-', cost, ' Fold-', i, ' ms-', ms, '\n')
        
        return( grid_ms)
    })
    ms_onCV_df <- ldply(ms_onCV, quickdf)
    ms_avg <- aggregate(ms_onCV_df$ms, by=list(cost=ms_onCV_df$cost, wt=ms_onCV_df$weight), mean, na.rm=T)
    names(ms_avg) <- c('cost', 'wt', 'ms')
    cat(file=traceFile, append=T,'iter-', iter ,  ' wt-', wt, ' cost-', cost, ' ms_avg-', ms_avg[1, 3], '\n')
    
    return(ms_avg)
}

par_onCV <- function(i){
    #load the cv data for  cv i
    load(paste(path_sepData, '\\dataset_cv', i, '.RData', sep=''))
    
    cv_data_tr <- cv_trTs_norm$tr #[1] 127032    242
    cv_data_tr$response <- as.factor(ifelse(cv_data_tr$response==1, 'H', 'NH'))
    cv_data_ts <- cv_trTs_norm$ts #[1] 31356   242
    cv_resp_ts <- cv_data_ts$response
    cv_x_ts <- cv_data_ts[, -match('response', names(cv_data_ts))]
    cat(file=traceFile, append=T, 'iter-', iter , ' wt-', wt, ' cost-', cost, ' Fold-', i, ' svm training on subtrain start! \n')
    start1 <- proc.time()
    svmFit <- svm(
        response~., data=cv_data_tr
        #TrainData, TrainClasses
        , cost=cost, class.weights = wts[[1]]
        , type="C-classification", kernel='linear'
        , scale=F
    )
    #load(file=paste('svmModel_cost_', cost, '_wt_', wt, '_fold_', i, '.RData', sep=''))
    cat(file=traceFile, append=T, 'iter-', iter , ' wt-', wt, ' cost-', cost, ' Fold-', i, ' svm training on subtrain end! '
        , 'time used-', (proc.time()-start1)[3]/60, 'min! '
        , 'ending time-', date() ,'\n'
    )
    
    pred <- predict(svmFit, cv_x_ts , scale=F,decision.values = TRUE)
    cat(file=traceFile, append=T,'iter-', iter , ' wt-', wt, ' cost-', cost, ' Fold-', i, ' pred on left out fold end! '
        , 'time used-', (proc.time()-start1)[3]/60, 'min! '
        , 'ending time-', date() ,'\n'
    )
    
    predscore <- attr(pred, "decision.values")
    pred_model <- list(model_onSubTr=svmFit, resp_pred_ts = data.frame(resp=cv_resp_ts, pred=predscore))
    save(file=paste('pred_wt(', wt,')cost(', cost, ')Fold',i, '.RData', sep=''), pred_model)
    ms <- createCurve(resp = cv_resp_ts, pred=predscore, recall_tar = crit)
    grid_ms = c(cost=cost, weight=wt, fold=i, ms=ms)
    cat(file=traceFile, append=T,'iter-', iter ,  ' wt-', wt, ' cost-', cost, ' Fold-', i, ' ms-', ms, '\n')
    
    return( grid_ms)
}

grid_search_cv_svm_par <- function(r){
    cost <- grid[r, 1]
    wt <- grid[r, 2]
    wts <-  list( c("NH"=1,"H"=wt) )
    cat(file = traceFile, append=T, 'iter-', iter , ' wt-', wt, ' cost-', cost, ' start!\n')
    start2 <- proc.time()
    #num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
    sfInit(parallel=TRUE, cpus=k.folds, type='SOCK')
    sfSource("F:\\Jie\\Shire_project\\03_code\\svm\\svm_subFunc_Dec03_normByRow.R")
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' cost-', cost, '-wt', wt, ' sfExport running!\n')
    sfExport('grid', 'cost', 'wt', 'wts', 'iter', 'traceFile', 'crit', 'k.folds', 'path_sepData')
    sfExport('createCurve_v2', 'par_onCV')
    #sfClusterEval(library("glmnet"))
    sfClusterEval(library("e1071"))
    sfClusterEval(library("ROCR"))
    sfClusterEval(library("plyr"))
    sfClusterEval(library("dplyr"))
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' cost-', cost, 'wt-', wt, ' parallele onCV running start!\n')
    
    ms_onCV <- sfClusterApplyLB(1:k.folds, par_onCV)
    sfStop()
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' cost-', cost, 'wt-', wt, ' parallele onCV running end!', 'usedTime-', (proc.time()-start2)[3]/60, 'min\n')
    
    ms_onCV_df <- ldply(ms_onCV, quickdf)
    ms_avg <- aggregate(ms_onCV_df$ms, by=list(cost=ms_onCV_df$cost, wt=ms_onCV_df$weight), mean, na.rm=T)
    names(ms_avg) <- c('cost', 'wt', 'ms')
    cat(file=traceFile, append=T,'iter-', iter ,  ' wt-', wt, ' cost-', cost,' ms_avg-', ms_avg[1, 3], '\n')
    
    return(ms_avg)
}


get_optimum_model_iter_withCV <- function(traceFile, path_sepData, num_pros){
    
    #num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
    sfInit(parallel=TRUE, cpus=num_pros, type='SOCK')
    sfSource("F:\\Jie\\Shire_project\\03_code\\svm\\svm_subFunc_Dec03_normByRow.R")
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' sfExport running!\n')
    sfExport('grid', 'iter', 'traceFile', 'crit', 'k.folds', 'path_sepData')
    sfExport('createCurve_v2', 'grid_search_cv_svm_par')
    #sfClusterEval(library("glmnet"))
    sfClusterEval(library("e1071"))
    sfClusterEval(library("ROCR"))
    sfClusterEval(library("plyr"))
    sfClusterEval(library("dplyr"))
    sfClusterEval(library('snowfall'))
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' parallele grid search running!\n')
    
    ms_allGrid <- sfClusterApplyLB(1:nrow(grid), grid_search_cv_svm_par)
    sfStop()
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' parallele grid search end!\n')
    
    save(file='fullRecords.RData', ms_allGrid)
    ms_allGrid <- ldply(ms_allGrid, quickdf)
    #choose the best parameter
    opt_lines <- ms_allGrid[ms_allGrid$ms==max(ms_allGrid$ms, na.rm = T),]
    opt_line <- opt_lines[sample(rep(1:nrow(opt_lines), 2), 1), ]
    opt_cost <- opt_line$cost
    opt_wt <- opt_line$wt
    max_ms <- opt_line$ms 
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' run over for all grid search!\n'
        ,'opt_cost-',opt_cost, ' opt_wt-', opt_wt, ' max_ms-', max_ms, '\n'
        )
    
    #applythe optimal parameter on the test data
    cat(file=traceFile, append=T, 'iter-', iter , ' opt_wt-', opt_wt
        , ' opt_cost-', opt_cost, ' svm training on subtrain start! \n')
    start3 <- proc.time()
    load(paste(path_sepData, '\\trTs_rn.RData', sep=''))
    train_data_iter <- trTs_norm$tr
    test_data_iter <- trTs_norm$ts
    rm(trTs_norm)
    train_data_iter$response <- ifelse(train_data_iter$response==1, 'H', 'NH')
    resp_ts2 <- test_data_iter$response
    x_ts2 <- test_data_iter[, -match('response',names(test_data_iter))]
    rm(test_data_iter)
    wts <-  list( c("NH"=1,"H"=opt_wt) )
    svmFit <- svm(
        response~., data=train_data_iter
        #TrainData, TrainClasses
        , cost=opt_cost, class.weights = wts[[1]]
        , type="C-classification", kernel='linear'
        , scale=F
    )
    save(file='svm_model_on_fullTrain.RData', svmFit)
    rm(train_data_iter)
    cat(file=traceFile, append=T, 'iter-', iter , ' opt_wt-', opt_wt
        , ' opt_cost-', opt_cost, ' svm training on training end! '
        , 'time used-', (proc.time()-start3)[3]/60, 'min! '
        , 'ending time-', date() ,'\n'
    )
    
    pred <- predict(svmFit, x_ts2 , scale=F,decision.values = TRUE)
    save(file='pred_on_fullTest.RData', pred)
    rm(x_ts2)
    cat(file=traceFile, append=T,'iter-', iter , ' opt_wt-', opt_wt
        , ' opt_cost-', opt_cost, ' pred on left out test end! '
        , 'time used-', (proc.time()-start3)[3]/60, 'min! '
        , 'ending time-', date() ,'\n'
    )
    
    predscore <- attr(pred, "decision.values")
    pred_model <- list(model_onFullTr=svmFit
                       , resp_pred_ts = data.frame(resp=resp_ts2,pred=predscore))
    save(file=paste('Model&Pred_optWt(', opt_wt,')optCost(', opt_cost, ').RData', sep='')
         , pred_model)
    ms <- createCurve_v2(resp = resp_ts2, pred=predscore, recall_tar = crit)
    grid_ms = c(opt_cost=opt_cost, opt_wt=opt_wt, max_ms=ms)
    result_list <- list(resp_pred =data.frame(resp=resp_ts2,pred=predscore)
                        , ms=c(opt_cost=opt_cost, opt_wt=opt_wt, max_ms=ms))
    return(result_list)
}

get_optimum_model_iter_withCV_temp <- function(traceFile, path_sepData, num_pros){
    
    #num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
    sfInit(parallel=TRUE, cpus=num_pros, type='SOCK')
    sfSource("F:\\Jie\\Shire_project\\03_code\\svm\\svm_subFunc_Dec03_normByRow.R")
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' sfExport running!\n')
    sfExport('grid', 'iter', 'traceFile', 'crit', 'k.folds', 'path_sepData')
    sfExport('createCurve', 'grid_search_cv_svm_temp')
    #sfClusterEval(library("glmnet"))
    sfClusterEval(library("e1071"))
    sfClusterEval(library("ROCR"))
    sfClusterEval(library("plyr"))
    sfClusterEval(library("dplyr"))
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' parallele grid search running!\n')
    
    ms_allGrid <- sfClusterApplyLB(1:nrow(grid), grid_search_cv_svm_temp)
    sfStop()
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' parallele grid search end!\n')
    
    save(file='fullRecords.RData', ms_allGrid)
    ms_allGrid <- ldply(ms_allGrid, quickdf)
    #choose the best parameter
    opt_lines <- ms_allGrid[ms_allGrid$ms==max(ms_allGrid$ms, na.rm = T),]
    opt_line <- opt_lines[sample(rep(1:nrow(opt_lines), 2), 1), ]
    opt_cost <- opt_line$cost
    opt_wt <- opt_line$wt
    max_ms <- opt_line$ms 
    cat(file=traceFile, append=TRUE, 'iter:', iter, ' run over for all grid search!\n'
        ,'opt_cost-',opt_cost, ' opt_wt-', opt_wt, ' max_ms-', max_ms, '\n'
    )
    
    #applythe optimal parameter on the test data
    cat(file=traceFile, append=T, 'iter-', iter , ' opt_wt-', opt_wt
        , ' opt_cost-', opt_cost, ' svm training on subtrain start! \n')
    start1 <- proc.time()
    load(paste(path_sepData, '\\trTs_rn.RData', sep=''))
    train_data_iter <- trTs_norm$tr
    test_data_iter <- trTs_norm$ts
    rm(trTs_norm)
    train_data_iter$response <- ifelse(train_data_iter$response==1, 'H', 'NH')
    resp_ts2 <- test_data_iter$response
    x_ts2 <- test_data_iter[, -match('response',names(test_data_iter))]
    rm(test_data_iter)
    wts <-  list( c("NH"=1,"H"=opt_wt) )
    svmFit <- svm(
        response~., data=train_data_iter
        #TrainData, TrainClasses
        , cost=opt_cost, class.weights = wts[[1]]
        , type="C-classification", kernel='linear'
        , scale=F
    )
    rm(train_data_iter)
    cat(file=traceFile, append=T, 'iter-', iter , ' opt_wt-', opt_wt
        , ' opt_cost-', opt_cost, ' svm training on training end! '
        , 'time used-', (proc.time()-start1)[3]/60, 'min! '
        , 'ending time-', date() ,'\n'
    )
    
    pred <- predict(svmFit, x_ts2 , sclae=T,decision.values = TRUE)
    rm(x_ts2)
    cat(file=traceFile, append=T,'iter-', iter , ' opt_wt-', opt_wt
        , ' opt_cost-', opt_cost, ' pred on left out test end! '
        , 'time used-', (proc.time()-start1)[3]/60, 'min! '
        , 'ending time-', date() ,'\n'
    )
    
    predscore <- attr(pred, "decision.values")
    pred_model <- list(model_onFullTr=svmFit
                       , resp_pred_ts = data.frame(resp=resp_ts2,pred=predscore))
    save(file=paste('Model&Pred_optWt(', opt_wt,')optCost(', opt_cost, ').RData', sep='')
         , pred_model)
    ms <- createCurve(resp = resp_ts2, pred=predscore, recall_tar = crit)
    grid_ms = c(opt_cost=opt_cost, opt_wt=opt_wt, max_ms=ms)
    result_list <- list(resp_pred =data.frame(resp=resp_ts2,pred=predscore)
                        , ms=c(opt_cost=opt_cost, opt_wt=opt_wt, max_ms=ms))
    return(result_list)
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


get_auprSummary <- function (data, lev = NULL, model = NULL) {
    if (length(levels(data$obs)) > 2) 
        stop(paste("Your outcome has", length(levels(data$obs)), 
                   "levels. The twoClassSummary() function isn't appropriate."))
    library("pROC")
    #browser()
    if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
        stop("levels of observed and predicted data do not match")
    rocObject <- try(pROC::roc(data$obs, data[, lev[1]]), silent = TRUE)
    rocAUC <- if (class(rocObject)[1] == "try-error") 
        NA
    else rocObject$auc
    
    library("PRROC")
    obs_ori <- ifelse(data$obs=='H', 1, 0)
    prObject <- try(PRROC::pr.curve(scores.class0 = data[, lev[1]],
                                    weights.class0 = obs_ori), silent = TRUE)
    prAUPR <- if (class(prObject)[1] == "try-error") 
        NA
    else prObject$auc.integral
    
    DR <- table(data[, 'obs'], data[, 'pred'])[2, 1]
    out <- c(DR, prAUPR, rocAUC, sensitivity(data[, "pred"], data[, "obs"],  lev[1])
             , specificity(data[, "pred"], data[, "obs"], lev[2])
             , posPredValue(data[, "pred"], data[, "obs"], lev[1], na.rm=T)
    )
    names(out) <- c('DR', "AUPR","ROC", "Sens", "Spec", "PPV")
    out
} 


#create own functions to pass cost and weights to svm
get_svmWtModel <- function(){
    com_name <- getModelInfo(model='svmLinear2', regex=FALSE)[[1]]
    wtsvmlinear <- com_name
    
    #parameter
    prm <- data.frame(parameter = c('cost', 'weights'),
                      class = rep('numeric',2),
                      label = c('cost', 'weights'))
    
    wtsvmlinear$parameters <- prm
    
    #train the model
    wtSVMfit <- function(x, y, wts, param, lev, last, weights, classProbs, ...){
        
        if(param$weights !=1){
            wts <- c(param$weights,1)
            names(wts)<-levels(y)
        }
        else wts <- NULL
        
        svm(x = as.matrix(x), y = y,
            kernel = 'linear',
            cost = param$cost, 
            class.weights = wts, 
            probability=classProbs, ...)
    }
    
    wtsvmlinear$fit <- wtSVMfit
    return(wtsvmlinear)
}







