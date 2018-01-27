#Logistic regression with Lasso
#Created on 11/19/2015
#Developed by Hui Jin

library(dplyr)
library(glmnet)
library(caret)
library(ROCR)
library(pROC)
library(snowfall)

#some constant
inpath <- 'F:/Hui/Shire_project/02_data'
outpath <- 'F:/Hui/Shire_project/04_result/LR_LASSO'
iterations <- 20
kfold <- 5
sens_levl <- 0.3

#reading in the data
load(paste(inpath, '/model_data.RData', sep=''))
#input hae_tr_model_data, nonhae_tr_model_data, tr_x, tr_y, patid_hae_tr, 
#patid_nonhae_tr, iters

#2. Logistic model with LASSO
weight_grid <- seq(0.1, 1, 0.1)

final_ppv <- numeric()
f_pred_ts <- numeric()

for(it in 1:iterations){
  cat('iteration ', it, 'runing \n')
  tm_it <- Sys.time()
  
  inonhae_tr_model_data <- nonhae_tr_model_data[iters[[it]], ]
  model_data <- rbind(hae_tr_model_data, inonhae_tr_model_data)

  #training set, 1 of 20 iterations
  tr_x <- model.matrix(response~., data=model_data)[,-1]
  tr_y <- model_data$response
  
  opti_ppv <- numeric()
  #opti_pred_tr <- list()
  opti_pred_ts <- list()
  #loop for weight
  for(iwt in 1:length(weight_grid)){
    
    tm_cv <- Sys.time()
    
    wt_vec <- ifelse(tr_y==1, 1, weight_grid[iwt])
      
    ini_lambda <- glmnet(x=tr_x, y=tr_y, weights=wt_vec, family='binomial', 
                         alpha=1, standardize=F)$lambda
    lambda_seq<- c(ini_lambda[-length(ini_lambda)] , 
                   seq(ini_lambda[length(ini_lambda)] , 0 , length=500)) 
    
    foldid<- nrow(model_data)
    set.seed(10)
    foldid[model_data$response==0]<-sample(rep(1:kfold, length=length(which(model_data$response==0))))
    foldid[model_data$response==1]<- sample(rep(1:kfold, length=length(which(model_data$response==1))))
    
    foldid <- createFolds(patid_nonhae_tr, k=kfold)
    
    # 5-fold cross-validation
    cv_ppv <- matrix(nr=kfold , nc=length(lambda_seq))
    #pred_cv <- numeric()
    
    for(k in 1:kfold){
      tm1<-Sys.time()
      cat(k, 'fold runing \n')
      
      cv_patid <- data.frame(hae_paitent=patid_nonhae_tr[foldid[[k]]])
      cv_patid$hae_patient <- as.character(cv_patid$hae_patient)
      
      cv_hae_tr <- inner_join(data.frame(cbind(patid_hae_tr, hae_tr_model_data)), 
                              cv_patid, by = 'hae_patient')
      cv_hae_tr <- cv_hae_tr[, -1]
      
      cv_nonhae_tr <- inonhae_tr_model_data[foldid[[k]], ] 
      
      
      
      
      
      cv_training_x <- tr_x[foldid!=k, ]
      cv_training_y <- tr_y[foldid!=k]
      cv_test_x <- tr_x[foldid==k, ]
      cv_test_y <- tr_y[foldid == k ]

      iwt_vec <- wt_vec[foldid!=k]
      
      fit_lasso<- glmnet(cv_training_x, cv_training_y, weights=iwt_vec,
                         lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
      cv_test_pred<- predict(fit_lasso, cv_test_x, type="response")
      
      cv_pred_re <- apply(cv_test_pred, 2, function(x){prediction(x, cv_test_y)})
      ppv_rec <- lapply(cv_pred_re, function(x)performance(x, 'ppv', 'tpr'))
      
      #find the ppv at 30% recall
      ppv_50rec <- lapply(ppv_rec, function(x){x@y.values[[1]][which.min(abs(x@x.values[[1]]-sens_levl))]})
      
      ippv <- unlist(ppv_50rec)
      cv_ppv[k, ] <- ippv
      
      #save the predicted score for each left-out fold
      #pred_cv <- rbind(pred_cv, cbind(cv_test_y, cv_test_pred))
      cat(k, 'fold finished \n')
      print(Sys.time() - tm1)
    }

    #select optimal lambda
    cv_ppv_mean<- apply(cv_ppv , 2 , function(x) mean(x[!is.na(x)]))                           
    optimum_model<- which.max(cv_ppv_mean)
    
    #optimal ppv
    iopti_ppv <- cv_ppv_mean[optimum_model]
    #optimal lambda
    iopti_lambda <- lambda_seq[optimum_model]
    
    #the predicted score for left out fold, the rank is optimum_model +1 since the first column is response
    #iopti_pred_score_tr <- pred_cv[, c(1, (optimum_model+1))]
    
    total_model<- glmnet(x=tr_x, y=tr_y, lambda=lambda_seq, weights=wt_vec,
                         family="binomial", alpha=1, standardize=F)
    
    iopti_pred_score_ts <- predict(total_model, ts_x, type='response')[,optimum_model]
    
    #save grid search result
    temp_ppv <- c(iwt, weight_grid[iwt],optimum_model, iopti_lambda, iopti_ppv)
    opti_ppv <- rbind(opti_ppv, temp_ppv)
    
    #save predicted value
    #eval(parse(text = paste('opti_pred_tr$wt', iwt, '<- iopti_pred_score_tr' , sep='')))
    
    #save predicted value on test
    eval(parse(text = paste('opti_pred_ts$wt', iwt, '<- iopti_pred_score_ts' , sep='')))
    
    cat('weight number', iwt, ' finished; total number of weight: ', length(weight_grid), '\n')
    print(Sys.time() - tm_cv)
    
  }
  
  #select best weight
  best_nr <- opti_ppv[which.max(opti_ppv[,5]), 1]
  
  #best estimate PPV
  ibest_ppv <- opti_ppv[best_nr, ]
  write.csv(temp_ppv, paste(ibest_ppv, '/ppv_result_iter_', it,'.csv', sep=''), quote = T, row.names = F)
  
  #best predicted score on validation samples
  #eval(parse(text = paste('ibest_pred_re_tr <- opti_pred_tr$wt', best_nr, sep='')))
  #write.csv(ibest_pred_re_tr, paste(outpath, '/pred_score_tr_iter_', it, '.csv', sep=''), quote = T, row.names = F)
  
  #best predicted score on test set
  
  eval(parse(text = paste('ibest_pred_re_ts <- c(ts_y, opti_pred_ts$wt', best_nr, ')',sep='')))
  write.csv(ibest_pred_re_ts, paste(outpath, '/pred_score_ts_iter_', it, '.csv', sep=''), quote = T, row.names = F)
  

  final_ppv <- rbind(final_ppv, c(it, ibest_ppv))
  f_pred_ts <- cbind(f_pred_ts, ibest_pred_re_ts)
  #eval(parse(text = paste('final_pred_re$iter', iwt, '<- ibest_pred_re' , sep='')))
  
  cat('iteration number', it, ' finished \n')
  print(Sys.time() - tm_it)
  
  
}

colnames(final_ppv) <- c('iteration', 'best_wt_nr', 'best_wt', 'best_lambda', 'best_ppv')
write.csv(final_ppv, paste(outpath, '/final_ppv.csv', sep=''), quote = T, row.names = F)

f_pred_ts2 <- apply(f_pred_ts, 1, mean)
final_pred_ts <- c(ts_y, f_pred_ts2)

colnames(final_pred_ts) <- c('true_response', 'pred_value')
wirte.csv(final_pred_ts, paste(outpath, '/final_pred_re.csv', sep=''), quote = T, row.names = F)


#performance for the model
#1. AUC & AUPR
calAUC <- function(y_pred, y_true)
{
  roc_obj <- 
    roc(response=as.vector(y_true), 
        predictor=as.vector(y_pred),
        direction="<")
  
  return(roc_obj$auc)
}

test_auc <- calAUC(final_pred_ts$pred_value, final_pred_ts$true_response)
test_auprObj <- pr.curve(scores.class0 = final_pred_ts$pred_value, 
                      weights.class0 = final_pred_ts$true_response, curve=TRUE)
test_aupr <- test_auprObj$auc.integral








#calculate the odss ratios, number of significant times retain
model_coef<- total_model$beta[,optimum_model]
model_coef[model_coef==0] <- NA 
odds_ratio<- exp(model_coef)

test_re_lr <- data.frame(cbind(test_obs, Test_ori[, response]))
colnames(test_re_lr) <- c('pred_value', 'true_value')
write.csv(test_re_lr, paste(output_path, '/test_pred_', isim,'.csv', sep=''), quote=T, row.names=T)


test_re_lr <- test_re_lr %>% mutate(buckets = ntile(pred_value, buck))

eval(parse(text=paste('iresult_lr <- test_re_lr %>% 
                      group_by(buckets) %>%
                      summarise(Percentage_', isim,'= mean(true_value, na.rm = TRUE),
                      Pos_', isim, '= sum(true_value, na.rm=TRUE),
                      Neg_', isim, '= n()-Pos_',isim, ')', sep=''))) 

iperc <- iresult_lr[, 2]
ipos <- iresult_lr[, 3]
ineg <- iresult_lr[, 4]

