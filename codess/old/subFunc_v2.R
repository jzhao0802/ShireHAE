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

get_out_of_sample_curve <- function(wt, lmd, foldid, response,data_mtx, is.scale){
    curve_fromCV <- lapply(1:k.folds, function(i){
        cv_training_resp <- response[foldid!=i]
        cv_training_matrix<- data_mtx[foldid!=i,]
        cv_test_resp<- response[foldid==i]                                                # select 1 fold as test data
        cv_test_matrix<- data_mtx[foldid==i,]
        wt_vct <- ifelse(cv_training_resp==1, 1, wt)
        
        fit_lasso<- glmnet(cv_training_matrix, cv_training_resp, weights=wt_vct,
                           lambda=lmd, family="binomial", alpha=1, standardize=is.scale)
        test_pred<- predict(fit_lasso, cv_test_matrix, type="response") #2713 599
        curve <- data.frame(resp=cv_test_resp, pred=test_pred)
        return(curve)
    })
    return(curve_fromCV)
}


run_lasso_iter <- function(iter, trainHAE, trainIdxIterIDNonHAE, wt_list, k.folds, crit, traceFile, is.scale){
    #extract nonHAE samples for each iteration
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ': start!\n') #added by Jie
    
    trainNonHAEIdx <- getNonHAEIdx_iter(iter, trainIdxIterIDNonHAE)
    trainNonHAE <- dat_nonhae_1111_rf[trainNonHAEIdx, ]
    data <- rbind(trainHAE[, -1], trainNonHAE[, -1])
    response <- data$response
    data_mtx <- model.matrix(response~., data=data)[, -1]
    #[1] 10846   245
    #get initial lambda: because initial lambda  sequence only is related to x and y, but has no relationship with weight
    initial_lambda<-glmnet(x=data_mtx, y=response, family="binomial", alpha=1, 
                           #weights=ifelse(response==1, 1, wt), 
                           standardize=is.scale)$lambda  # calculating the initial lambda
    lambda_seq<- c(initial_lambda[-length(initial_lambda)] , 
                   seq(initial_lambda[length(initial_lambda)] , 0 , length=200)) 
    lambda_seq <- lambda_seq[1:2]
    # 599   get a length=500 descending sequence from initial lambda to 0
    
    #grid search for both weights and lambda
    
    #set.seed(1234)
    foldid<- rep(0, nrow(data))
    foldid[data$response==1]<- sample(rep(1:k.folds, length=length(which(data$response==1))))
    foldid[data$response==0]<- sample(rep(1:k.folds, length=length(which(data$response==0))))
    table(data$response , foldid) # QC
    #foldid
    #1    2    3    4    5
    #0 1972 1972 1972 1972 1972
    #1  198  197  197  197  197
    ms_fromWt <- lapply(wt_list, function(wt){
        cat(file=traceFile, append=TRUE, 'Iteration ', iter, ' wt:', wt, 'start!\n') #added by Jie
        
        auc_fromCV <- lapply(1:k.folds, function(i){
            cv_training_resp <- response[foldid!=i]
            cv_training_matrix<- data_mtx[foldid!=i,]
            cv_test_resp<- response[foldid==i]                                                # select 1 fold as test data
            cv_test_matrix<- data_mtx[foldid==i,]
            wt_vct <- ifelse(cv_training_resp==1, 1, wt)
            
            fit_lasso<- glmnet(cv_training_matrix, cv_training_resp, weights=wt_vct,
                               lambda=lambda_seq, family="binomial", alpha=1, standardize=is.scale)
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
        
        
    })
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ': wt iteration end!\n') #added by Jie
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
    curve_outOfSmp <- get_out_of_sample_curve(optim_wt, optim_lambda, foldid, response, data_mtx, is.scale)
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ':out of sample end!\n') #added by Jie
    
    #apply the optimum weight and lambda to the test data
    fit_lasso_eval<- glmnet(data_mtx, response, weights=ifelse(response==1, 1, optim_wt),
                            lambda=optim_lambda, family="binomial", alpha=1, standardize=is.scale)
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ':fit_lasso_eval end!\n') #added by Jie
    
    test_pred <- predict(fit_lasso_eval, test_mtx, type="response")
    cat(file=traceFile, append=TRUE, 'Iteration ', iter, ': test_pred end!\n') #added by Jie
    
    return(list(test_pred=test_pred, 
                optimum_pmt=c(iter=iter, optim_lambda=optim_lambda, optim_wt=optim_wt, max_ms=max_ms),  
                curve_outOfSmp=curve_outOfSmp))
}









