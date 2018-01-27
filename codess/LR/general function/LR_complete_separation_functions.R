#Developer - Jie Zhao
#Develope time - Dec 2015-Jan2016

#function to trace back the warning and error message
myTryCatch <- function(expr) {
    warn <- err <- NULL
    value <- withCallingHandlers(
        tryCatch(expr, error=function(e) {
            err <<- e
            NULL
        }), warning=function(w) {
            warn <<- w
            invokeRestart("muffleWarning")
        })
    list(value=value, warning=warn, error=err)
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



getPerf <- function(resp, pred){
    
    #recall_tar_onTs <- sort(seq(0.5, 0.05, -0.05), decreasing = T)
    recall_tar_onTs <- sort(0.3, decreasing = T)
    result_msOnTest <- msOnTest_sep_v2(pred, resp, recall_tar_onTs)
    ms_onTest <- result_msOnTest$ms
    return(ms_onTest)
    
}

create_crossTb <- function(var_list, resp_v, data){
    crossTb <- lapply(var_list, function(v){
        var <- data[, v]
        tb <- table(var, resp_v)
        tb1 <-cbind(rownames(tb), tb)
        colnames(tb1)[1] <- 'var'
        tb2 <- rbind(c(v, paste('resp_', colnames(tb1)[-1], sep='')), tb1)
        return(tb2)
    })
    
    crossTb_df <- ldply(crossTb, rbind)
    
    return(crossTb_df) 
  
}
create_stat <- function(var_list, resp_v, data){
    stat <- lapply(var_list, function(v){
        var <- data[, v]
        var_0 <- var[resp_v==0]
        var_1 <- var[resp_v==1]
        n_0 <- sum(resp_v==0)
        mean_0 <- mean(var_0, na.rm=T)
        sd_0 <- sd(var_0, na.rm=T)
        n_1 <- sum(resp_v==1)
        mean_1 <- mean(var_1, na.rm=T)
        sd_1 <- sd(var_1, na.rm=T)
        istat <- c(n_0, mean_0, sd_0, n_1, mean_1, sd_1)
        return(istat)
    })
    stat1 <- ldply(stat, quickdf)
    stat2 <- as.data.frame(cbind(var_list, stat1))
    names(stat2)<- c('Varaible', '# of positive', 'Mean of positive', 'SD of positive'
                     , '# of negative', 'Mean of negative', 'SD of negative')
    return(stat2)
}
create_stat1 <- function(var_list, resp_v, data){
    stat <- lapply(var_list, function(v){
        var <- data[, v]
        mean <- mean(var, na.rm=T)
        sd <- sd(var, na.rm=T)
        min <- min(var, na.rm=T)
        max <- max(var, na.rm=T)
        istat <- c(mean, sd, min, max)
        return(istat)
    })
    stat1 <- ldply(stat, quickdf)
    stat2 <- as.data.frame(cbind(var_list, stat1))
    names(stat2)<- c('Varaible', 'Mean', 'SD'
                     , 'Max', 'Min')
    return(stat2)
}
create_OR_forBinary <- function(var_list, resp_v, data){
    OR <- lapply(var_list, function(v){
        var <- data[, v]
        
        tb <- table(var, resp_v)
        if(dim(tb)[1] ==1 & '0' %in% rownames(tb)){
            pos <- 0
            neg <- sum(tb)
            covar1_response1 <- 0
            covar0_response1 <- tb[1,2]
            covar1_response0 <- 0
            covar0_response0 <- tb[1,1]
            odds_ratio_my <- ((covar1_response1/pos)/(covar1_response0/pos))/((covar0_response1/neg)/(covar0_response0/neg))    #odds_ratio=odds(x==1)/odds(x==0) = ((p(Y=1|x=1)/p(Y=0|x=1))/(p(Y=1|x=0)/p(Y=0|x=0))
            
        }else if(dim(tb)[1] ==1 & '1' %in% rownames(tb)){
            neg <- 0
            pos <- sum(tb)
            covar1_response1 <- tb[1, 2]
            covar0_response1 <- 0
            covar1_response0 <- tb[1, 1]
            covar0_response0 <- 0
            odds_ratio_my <- ((covar1_response1/pos)/(covar1_response0/pos))/((covar0_response1/neg)/(covar0_response0/neg))    #odds_ratio=odds(x==1)/odds(x==0) = ((p(Y=1|x=1)/p(Y=0|x=1))/(p(Y=1|x=0)/p(Y=0|x=0))
            
        }else{
            pos <- sum(tb[2,])
            neg <- sum(tb[1,])
            pos_pos <- tb[2,2]
            covar1_response1 <- tb[2,2]
            covar0_response1 <- tb[1,2]
            covar1_response0 <- tb[2,1]
            covar0_response0 <- tb[1,1]
            odds_ratio_my <- ((covar1_response1/sum(tb[2, ]))/(covar1_response0/sum(tb[2, ])))/((covar0_response1/sum(tb[1, ]))/(covar0_response0/sum(tb[1, ])))    
            #odds_ratio=odds(x==1)/odds(x==0) = ((p(Y=1|x=1)/p(Y=0|x=1))/(p(Y=1|x=0)/p(Y=0|x=0))
        }
        contigency_table<- matrix(c(covar1_response1, covar0_response1, covar1_response0, covar0_response0), nc=2, byrow=T)
        association_test<- fisher.test(contigency_table , alternative = "two.sided")
        p_value<- association_test$p.value
        odds_ratio<- as.vector(association_test$estimate)
        temp <- c(covar1_response1, covar0_response1, covar1_response0, covar0_response0
                  , odds_ratio_my, odds_ratio, p_value)
        return(temp)
        
    })
    OR_df <- ldply(OR, quickdf)
    OR_1 <-as.data.frame(cbind(var_list, OR_df))
    names(OR_1) <-c('Variable', 'covar1_response1', 'covar0_response1', 'covar1_response0'
                    , "covar0_response0"
                    , "odds_ratio_my", "odds_ratio", "p_value")
    return(OR_1)
}

check_sep_forConti <- function(var_list, resp_v, data){
    sep_result <- lapply(var_list, function(v){
        var <- data[, v]
        min_resp0 <- min(var[resp_v == 0])
        min_resp1 <- min(var[resp_v == 1])
        max_resp0 <- max(var[resp_v == 0])
        max_resp1 <- max(var[resp_v == 1])
        if(max_resp0 <= min_resp1 | max_resp1 <= min_resp0){
            complete_separate <- 1
        }else{
            complete_separate <- 0
        }
        result <- c(min_resp0, min_resp1, max_resp0, max_resp1, complete_separate)
        return(result)
    })
    sep_result_df <- ldply(sep_result, quickdf)
    sep_result1 <- cbind(var_list, sep_result_df)
    names(sep_result1) <- c('Variable', 'Min_resp0', 'Min_resp1'
                            , 'Max_resp0', 'Max_resp1', 'Comp_sep flag')
    return(sep_result1)
}


#variables selection 
##for catigorical variables
create_dummy <- function(data){
    temp_fct <- as.data.frame(apply(data[, cati_var], 2, as.factor))
    dummy <- 
        model.matrix( ~ ., data=temp_fct, contrasts.arg = 
                          lapply(temp_fct, contrasts, contrasts=FALSE))[, -1]
    cati_var_dummy <- colnames(dummy)
    data <- cbind(as.data.frame(dummy), data[, c(binary_var, conti_var, 'response')])
    temp <- list(data=data, dummy_nm = cati_var_dummy)
    return(temp)
}

#re-run logistci regression
remove_in_iteration <- function(i=n.iter,covar_rm, coef_cutoff, or_cutoff, covar_rm_2_list){
    covar_rm <- c(binary_var_or_rm1, conti_var_sep_rm)
    covar_rm_2_list <- list()
    coef_cutoff <- 5
    or_cutoff <- 5
    start <- proc.time()
    for(i in 1:10){
        cat('iteration-', i, ' start!\n')
        if(i == 1){
            trn_logit_fit = myTryCatch(
                glm(response~., family=binomial()
                    , data=train_data_iter[, -match(covar_rm, names(train_data_iter))])
            )
            covar_rm_1 <- covar_rm
        }else{
            covar_rm_1 <- c(covar_rm_2 )
            trn_logit_fit = myTryCatch(
                glm(response~., family=binomial(), data=train_data_iter[, -match(covar_rm_1, names(train_data_iter))])
            )
            
        }
        coef_or <- data.frame(coef=coef(trn_logit_fit$value), or=exp(coef(trn_logit_fit$value)))
        coef_or <- coef_or[order(coef_or$or, decreasing = T), ]
        if(i==1){
            save(coef_or, file='LR_coef_or_org.RData')
        }
        if(is.null(trn_logit_fit$warning)){
            save(coef_or, file=paste('LR_coef_or_delVar_coef', coef_cutoff, '_or', or_cutoff, '.RData', sep=''))
            stop('issue covariates have been moved all and without warning!\n')
        }else{
            covar_rm_2 <- rownames(coef_or)[abs(coef_or$coef) >= coef_cutoff | is.na(abs(coef_or$coef)) | coef_or$or >= or_cutoff]
            if(length(covar_rm_2)==0){
                save(coef_or, file=paste('LR_coef_or_delVar_coef', coef_cutoff, '_or', or_cutoff, '.RData', sep=''))
                save(covar_rm_2_list, file=paste('covar_rm_2_list_coef', coef_cutoff, '_or', or_cutoff, '.RData', sep=''))
                stop('issue covariates have been moved all, but still with warning-', trn_logit_fit$warning, '\n')
            }else{
                
                covar_rm_2 <- c(covar_rm_1, covar_rm_2)
                covar_rm_2_list[[i]] <- covar_rm_2   
            }
        }
    }
    cat('time used:', (proc.time()-start)[3]/60)
    result_temp <- list(covar_rm_list=covar_rm_2_list, fit_final=trn_logit_fit$value)
    return(result_temp)
}

#3. using Yan's method to delete covariates using VIF
fit_1<- trn_logit_fit$value
alias_res <- alias(fit_1)
del_multiCorr <- rownames(alias_res$Complete)
is.null(del_multiCorr)
#drop the ' ' in the names string
if(!is.null(del_multiCorr)){
    del_multiCorr <- gsub("(^`)(.+)(`$)", "\\2", del_multiCorr, perl=T)
    notFind <- del_multiCorr[!(del_multiCorr %in% names(training_data))] #character(0)
    var_del_all <- unique(c(const_var, var_del_or, del_multiCorr))
    
    
}

#4. check the std.Error
check_stdErr <- function(train_data_iter, trn_logit_fit, var_kept){
    fit_summary <- summary(trn_logit_fit$value)$coefficients
    stdErr_max <- max(fit_summary[, 2], na.rm=T)
    fit_summary_ord <- fit_summary[order(fit_summary[, 2], decreasing = T),]
    var_del_stderr <- rownames(fit_summary)[fit_summary[, 2]>10]
    trn_logit_fit2 = myTryCatch(
        glm(response~., family=binomial()
            , data=train_data_iter[, match(c(var_kept, 'response'), names(train_data_iter))])
    )
    result_temp <- list(var_del_stderr=var_del_stderr, fit=trn_logit_fit2)
    return(var_del_stderr)
}

#5. check the correlation
check_correlation <- function(data, thresh){
    var_list <- names(data)
    covariance_matrix <- cor(data)
    write.xlsx(covariance_matrix , 'covariance_matrix.xlsx', sheetNam='var_kept', row.names=T, append=T, showNA=T)
    pdf("covariance_matrix.pdf", height=6, width=8, pointsize=12)
    heatmap(covariance_matrix)
    dev.off()
    
    high_corr <- numeric()
    for(i in 1:(length(var_list)-1)){
        for(j in (i+1):length(var_list)){
            if(abs(covariance_matrix[i,j]) > thresh){
                #print(paste(covar_forCorr[i] , covar_forCorr[j] , sep=' : '))
                high_corr <- rbind(high_corr, c(Var1=var_list[i], Var2=var_list[j], Corr=covariance_matrix[i,j]))
            }
        }
    }
    high_corr <- as.data.frame(high_corr)
    
}

grid_par <- function(r){
    mtry <- grid[r, 'mtry']
    rate <- grid[r,'rate' ]
    ns <- grid[r, 'nodesize']
    n_neg_smp <- n_pos*rate
    
    start1 <- proc.time()
    if(is.na(rate)){
        
        RF_fit <- randomForest(x=cv_x_tr,y=cv_resp_tr, 
                               ntree=300,
                               mtry=mtry,
                               nodesize=ns, 
                               importance=T,
                               #sampsize=c(n_neg_smp, n_pos),
                               #strata=cv_resp_tr,
                               keep.inbag=T
        )
    }else{
        
        RF_fit <- randomForest(x=cv_x_tr,y=cv_resp_tr, 
                               ntree=30000/rate,
                               mtry=mtry,
                               nodesize=ns, 
                               importance=T,
                               sampsize=c(n_neg_smp, n_pos),
                               strata=cv_resp_tr,
                               keep.inbag=T
        )   
    }
    suffix <- paste0('mtry(', mtry, ')rate(', rate, ')ns(', ns, ')')
    cat(file=traceFile, append=T, suffix, ' train model time used:', (proc.time()-start1)[3]/60, 'min!\n')
    save(RF_fit, file=paste0('RF_fit_onSubTr(4folds)_', suffix, '.RData'))
    load(paste0('RF_fit_onSubTr(4folds)_', suffix, '.RData'))
    pred_rf_vl = predict(RF_fit, cv_x_ts, type='prob')
    save(pred_rf_vl, file=paste0('pred_rf_vl_', suffix, '.RData'))
    ms_rf_vl <- getPerf(cv_resp_ts, pred_rf_vl[, 2])
    names(ms_rf_vl) <- paste(names(ms_rf_vl), '_validation', sep='')
    
    pred_rf = predict(RF_fit, x_test, type='prob')
    save(pred_rf, file=paste0('pred_rf_', suffix, '.RData'))
    ms_rf <- getPerf(resp_ts, pred_rf[, 2])
    save(ms_rf, ms_rf_vl, file=paste0('ms_rf_vl_ts_', suffix, '.RData'))
    return(c(ms_rf_vl, ms_rf))
    
}

