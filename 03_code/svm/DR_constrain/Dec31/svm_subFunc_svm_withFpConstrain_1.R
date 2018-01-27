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
  
  #hui added
  resp_pred <- ifelse(pred>=0, 1, 0)
  con_tb <- table(resp_pred, resp)
  ifp <- con_tb[2,1]
  
  ##end 
  return(c(temp4, ifp))
  
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



par_onCV <- function(i){
  #load the cv data for  cv i
  load(paste(path_sepData, '\\dataset_cv', i, '.RData', sep=''))
  
  cv_data_tr <- cv_trTs_norm$tr #[1] 127032    242
  cv_data_tr$response <- as.factor(ifelse(cv_data_tr$response==1, 'H', 'NH'))
  cv_data_ts <- cv_trTs_norm$ts #[1] 31356   242
  cv_resp_ts <- cv_data_ts$response
  cv_x_ts <- cv_data_ts[, -match('response', names(cv_data_ts))]
  cat(file=traceFile, append=T, 'iter-', iter , ' wt-', wt, ' cost-', cost, ' gamma-', gamma,' Fold-', i, ' svm training on subtrain start! \n')
  start1 <- proc.time()
  if(kernel=='lin'){
        svmFit <- svm(
          response~., data=cv_data_tr
          #TrainData, TrainClasses
          , cost=cost, class.weights = wts[[1]]
          , type=clsMethod, kernel='linear'
          , scale=F
        )
  }else if(kernel=='rbf'){
        svmFit <- svm(
          response~., data=cv_data_tr
          #TrainData, TrainClasses
          , cost=cost, class.weights = wts[[1]]
          , gamma=gamma
          , type=clsMethod, kernel='radial'
          , scale=F
        )
        
  }
  
  #load(file=paste('svmModel_cost_', cost, '_wt_', wt, '_fold_', i, '.RData', sep=''))
  cat(file=traceFile, append=T, 'iter-', iter , ' wt-', wt, ' cost-', cost,' gamma-', gamma, ' Fold-', i, ' svm training on subtrain end! '
      , 'time used-', (proc.time()-start1)[3]/60, 'min! '
      , 'ending time-', date() ,'\n'
  )
  
  pred <- predict(svmFit, cv_x_ts , scale=F,decision.values = TRUE)
  cat(file=traceFile, append=T,'iter-', iter , ' wt-', wt, ' cost-', cost, ' gamma-', gamma, ' Fold-', i, ' pred on left out fold end! '
      , 'time used-', (proc.time()-start1)[3]/60, 'min! '
      , 'ending time-', date() ,'\n'
  )
  
  predscore <- attr(pred, "decision.values")
  pred_model <- list(model_onSubTr=svmFit, resp_pred_ts = data.frame(resp=cv_resp_ts, pred=predscore))
  save(file=paste('pred_wt(', wt,')cost(', cost, ')gm(', gamma, ')Fold',i, '.RData', sep=''), pred_model)
  ms <- createCurve_v2(resp = cv_resp_ts, pred=predscore, recall_tar = crit)
  if(kernel=='lin'){
    grid_ms = c(cost=cost, weight=wt, fold=i, ms=ms[1], fp=ms[2])#hui adjusted
  }else if(kernel=='rbf'){
    grid_ms = c(cost=cost, gamma=gamma, weight=wt, fold=i, ms=ms[1], fp=ms[2]) #hui adjusted   
  }
  
  cat(file=traceFile, append=T,'iter-', iter ,  ' wt-', wt, ' cost-', cost
      ,' gamma-', gamma, ' Fold-', i, ' ms-', ms[1], ' fp-', ms[2], '\n')
  
  return( grid_ms)
}

grid_search_cv_svm_par <- function(r){
  if(kernel=='lin'){
    cost <- grid[r, 1]
    wt <- grid[r, 2]
    gamma <- 'NA'
  }else if(kernel=='rbf'){
    cost <- grid[r, 1]
    gamma <- grid[r, 2]
    wt <- grid[r, 3]
  }
  wts <-  list( c("NH"=1,"H"=wt) )
  cat(file = traceFile, append=T, 'iter-', iter , ' wt-', wt, ' cost-', cost, ' gamma-', gamma, ' start!\n')
  start1 <- proc.time()
  #num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
  sfInit(parallel=TRUE, cpus=k.folds, type='SOCK')
  #sfSource("D:\\Shire_project\\03_code\\Jie\\svm\\svm_subFunc_svm_withFpConstrain.R")
  sfSource("F:\\Jie\\Shire_project\\03_code\\svm\\DR_constrain\\svm_subFunc_svm_withFpConstrain.R")
  
  cat(file=traceFile, append=TRUE, 'iter:', iter, ' cost-', cost, '-wt', wt, ' sfExport running!\n')
  sfExport('grid', 'cost', 'wt', 'wts', 'iter', 'traceFile', 'crit', 'k.folds', 'path_sepData'
           , 'ONE', 'kernel', 'clsMethod', 'gamma')
  sfExport('createCurve_v2', 'par_onCV')
  #sfClusterEval(library("glmnet"))
  sfClusterEval(library("e1071"))
  sfClusterEval(library("ROCR"))
  sfClusterEval(library("plyr"))
  sfClusterEval(library("dplyr"))
  cat(file=traceFile, append=TRUE, 'iter:', iter, ' cost-', cost, 'wt-', wt, ' gamma-', gamma, ' parallele onCV running start!\n')
  
  ms_onCV <- sfClusterApplyLB(1:k.folds, par_onCV)
  sfStop()
  cat(file=traceFile, append=TRUE, 'iter:', iter, ' cost-', cost, 'wt-', wt,' gamma-', gamma, ' parallele onCV running end!', 'usedTime-', (proc.time()-start1)[3]/60, 'min\n')
  
  ms_onCV_df <- ldply(ms_onCV, quickdf)
  if(kernel=='lin'){
    ms_avg <- aggregate(data.frame(ms_onCV_df$ms, ms_onCV_df$fp), 
                        by=list(cost=ms_onCV_df$cost, wt=ms_onCV_df$weight), mean, na.rm=T) 
    names(ms_avg) <- c('cost', 'wt', 'ms', 'fp')#hui adjusted
    
  }else if(kernle=='rbf'){
    ms_avg <- aggregate(data.frame(ms_onCV_df$ms, ms_onCV_df$fp),
                        by=list(cost=ms_onCV_df$cost, gamma=ms_onCV_df$gamma, wt=ms_onCV_df$weight), mean, na.rm=T)
    names(ms_avg) <- c('cost', 'gamma', 'wt', 'ms', 'fp')#hui adjusted
    
  }
  
  cat(file=traceFile, append=T,'iter-', iter ,  ' wt-', wt, ' cost-', cost,' gamma-', gamma
      , ' ms_avg-', ms_avg[1, (ncol(ms_avg)-1)], ' fp-', ms_avg[1, ncol(ms_avg)], '\n')
  
  return(ms_avg)
}


get_optimum_model_iter_withCV <- function(traceFile, path_sepData, num_pros){
  
  #num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
  sfInit(parallel=TRUE, cpus=num_pros, type='SOCK')
  #sfSource("D:\\Shire_project\\03_code\\Jie\\svm\\svm_subFunc_svm_withFpConstrain.R")
  sfSource("F:\\Jie\\Shire_project\\03_code\\svm\\DR_constrain\\svm_subFunc_svm_withFpConstrain.R")
  cat(file=traceFile, append=TRUE, 'iter:', iter, ' sfExport running!\n')
  sfExport('grid', 'iter', 'traceFile', 'crit', 'k.folds', 'path_sepData'
           , 'ONE', 'kernel', 'clsMethod')
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
  #fp is less than DR, hui added
  cat(file=traceFile, append=TRUE, 'iter:', iter
      , ' minimum fp(in all the grid search)-', min(ms_allGrid$fp), '\n')
  
  if(min(ms_allGrid$fp)>DR){
    least_fp <- ms_allGrid[ms_allGrid$fp==min(ms_allGrid$fp, na.rm = T),]
  }else{
    least_fp <- ms_allGrid[ms_allGrid$fp <= DR,]
  }
  
  opt_lines <- least_fp[least_fp$ms==max(least_fp$ms, na.rm = T),]
  opt_line <- opt_lines[sample(rep(1:nrow(opt_lines), 2), 1), ]
  opt_cost <- opt_line$cost
  opt_wt <- opt_line$wt
  max_ms <- opt_line$ms
  opt_fp <- opt_line$fp
  if(kernel=='rbf'){
    opt_gamma <- opt_line$gamma
  }else if(kernel=='lin'){
    opt_gamma <- 'NA'
  }
  cat(file=traceFile, append=TRUE, 'iter:', iter, ' run over for all grid search!\n'
      ,'opt_cost-',opt_cost, ' opt_gamma-',opt_gamma , ' opt_wt-', opt_wt
      , ' max_ms-', max_ms, ' opt_fp-', opt_fp, '\n'
  )
  
  if(test_1fold){
      stop('no need to run the svm in full training data for testing 1 fold!\n')
  }
  #applythe optimal parameter on the test data
  cat(file=traceFile, append=T, 'iter-', iter , ' opt_wt-', opt_wt
      , ' opt_cost-', opt_cost, ' opt_gamma-',opt_gamma ,' svm training on subtrain start! \n')
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
  if(kernel=='lin'){
    svmFit <- svm(
      response~., data=train_data_iter
      #TrainData, TrainClasses
      , cost=opt_cost, class.weights = wts[[1]]
      , type=clsMethod, kernel='linear'
      , scale=F
    )
    
  }else if(kernel=='rbf'){
    svmFit <- svm(
      response~., data=train_data_iter
      #TrainData, TrainClasses
      , cost=opt_cost, class.weights = wts[[1]]
      , gamma=opt_gamma
      , type=clsMethod, kernel='radial'
      , scale=F
    )
    
  }
  save(file='svm_model_on_fullTrain.RData', svmFit)
  rm(train_data_iter)
  cat(file=traceFile, append=T, 'iter-', iter , ' opt_wt-', opt_wt
      , ' opt_cost-', opt_cost, ' opt_gamma-',opt_gamma ,' svm training on training end! '
      , 'time used-', (proc.time()-start1)[3]/60, 'min! '
      , 'ending time-', date() ,'\n'
  )
  
  pred <- predict(svmFit, x_ts2 , scale=F,decision.values = TRUE)
  save(file='pred_on_fullTest.RData', pred)
  rm(x_ts2)
  cat(file=traceFile, append=T,'iter-', iter , ' opt_wt-', opt_wt
      , ' opt_cost-', opt_cost, ' opt_gamma-',opt_gamma ,' pred on left out test end! '
      , 'time used-', (proc.time()-start1)[3]/60, 'min! '
      , 'ending time-', date() ,'\n'
  )
  
  predscore <- attr(pred, "decision.values")
  pred_model <- list(model_onFullTr=svmFit
                     , resp_pred_ts = data.frame(resp=resp_ts2,pred=predscore))
  save(file=paste('Model&Pred_optWt(', opt_wt,')optCost(', opt_cost, ')optGm(', opt_gamma,').RData', sep='')
       , pred_model)
  ms <- createCurve_v2(resp = resp_ts2, pred=predscore, recall_tar = crit)
  if(kernel=='lin'){
    grid_ms = c(opt_cost=opt_cost, opt_wt=opt_wt, max_ms=ms[1], fp=ms[2])#hui adjusted
  }else if(kernel=='rbf'){
    grid_ms = c(opt_cost=opt_cost, opt_gamma=opt_gamma, opt_wt=opt_wt, max_ms=ms[1], fp=ms[2])#hui adjusted    
  }
  
  result_list <- list(resp_pred =data.frame(resp=resp_ts2,pred=predscore)
                      , ms=grid_ms)
  return(result_list)
}
