#check the classwt in randomForest
root <- "D:\\Shire_project\\04_result\\RF\\test_1fold\\iter1\\"
path <- paste0(root
               , "ntree(300)_mtry(25)_wt(0.3,0.5,0.6,0.03,0.05,0.07)_ns(300)_seed(1)withLassoCovar(TRUE)"
)
setwd(path)
model_vl <- "pred_rf_vl_ntree(300)mtry(25)wt(0.95&0.05)ns(300)se(1)rep(FALSE)"
model_ts <- "pred_rf_ntree(300)mtry(25)wt(0.95&0.05)ns(300)se(1)rep(FALSE)"
load(file=paste0(model_vl, '.RData'))
load( file=paste0(model_ts, '.RData'))

#for the random forest modeling with Dong's set
with_lassoCovar=T
#load in the original train and test data for fold 1 of iteration 1
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

path_sepData <- 'D:\\Shire_project\\04_result\\data_sep\\v2\\iter1'
load(paste(path_sepData, '\\dataset_cv', 1, '.RData', sep=''))


cv_data <- cv_data_trTs$cv_data
var_list <- names(cv_data)
fct_flag <- sapply(cv_data, is.character)
cati_var <- var_list[fct_flag]
binary_var <- grep('flag$', var_list, value=T)
conti_var <- grep('age|lookback_days|freq$', var_list, value=T)
length(cati_var)+length(binary_var)+length(conti_var)+1==length(var_list)

cv_data_result <- create_dummy(cv_data)
cv_data <- cv_data_result$data

fct <- apply(cv_data[, c(grep('region|gender', names(cv_data), value=T, perl=T), binary_var)]
             , 2, as.factor)
cv_data[, c(grep('region|gender', names(cv_data), value=T, perl=T), binary_var)] <- as.data.frame(fct)

cv_data_tr <- cv_data[cv_data_trTs$cv_tr_flag,] #[1] 127032    242
cv_data_ts <- cv_data[!cv_data_trTs$cv_tr_flag,] #[1] 31356   242

cv_resp_tr <- as.factor(cv_data_tr$response)
cv_resp_ts <- as.factor(cv_data_ts$response)
if(with_lassoCovar){
    load('D:\\Shire_project\\04_result\\Lasso\\fromHui\\opt_lasso_retained_var.RData')
    cv_data_tr <- cv_data_tr[, re_model_var]
    cv_data_ts <- cv_data_ts[, re_model_var]
}

cv_x_tr <- cv_data_tr[, -match('response', names(cv_data_tr))]
cv_x_ts <- cv_data_ts[, -match('response', names(cv_data_ts))]

#load in the original train and test data for iteration 1
path_sepData <- 'D:\\Shire_project\\04_result\\data_sep\\v2\\iter1'
load(paste(path_sepData, '\\trTs.RData', sep=''))
train_data_iter <- trTs$tr
test_data_iter <- trTs$ts
tr_flag <- c(rep(1, nrow(train_data_iter)), rep(0, nrow(test_data_iter)))
trTs <- rbind(train_data_iter, test_data_iter)
trTs_result <- create_dummy(trTs)
trTs <- trTs_result$data
test_data_iter <- trTs[!tr_flag, ]
test_data_iter[, c(grep('region|gender', names(test_data_iter), value=T, perl=T), binary_var)] <- as.data.frame(
    apply(test_data_iter[, c(grep('region|gender', names(test_data_iter), value=T, perl=T), binary_var)]
          , 2, as.factor)
)
resp_tr <- as.factor(train_data_iter$response)
resp_ts <- as.factor(test_data_iter$response)
if(with_lassoCovar){
    load('D:\\Shire_project\\04_result\\Lasso\\fromHui\\opt_lasso_retained_var.RData')
    test_data_iter <- test_data_iter[, re_model_var]
    #train_data_iter <- train_data_iter[, re_model_var]
}
x_test <- test_data_iter[, -match('response', names(test_data_iter))]

table(cv_resp_ts)
table(resp_ts)

length(pred_rf[, 2])
length(pred_rf_vl[, 2])

check_classwt <- function(recall_tar, data, classwt){
    if(data=='validation'){
        model <- paste0("pred_rf_vl_ntree(300)mtry(25)wt(", 1-classwt, "&", classwt, ")ns(300)se(1)rep(FALSE)")
        load(file=paste0(model, '.RData'))
        pred <- pred_rf_vl[, 2]
        response <- cv_resp_ts
    }else{
        
        model <- paste0("pred_rf_ntree(300)mtry(25)wt(", 1-classwt, "&", classwt, ")ns(300)se(1)rep(FALSE)")
        load(file=paste0(model, '.RData'))
        pred <- pred_rf[, 2]
        response <- resp_ts
    }#model_ts <- paste0("pred_rf_", "ntree(300)mtry(25)wt(", 1-classwt, "&", classwt, ")ns(300)se(1)rep(FALSE)")
    #load( file=paste0(model_ts, '.RData'))
    
    
    #pred <- apply(pred, 1, mean, na.rm=T)
    predobj <- prediction(pred, response)
    #add plot
    perf <- performance(predobj, 'ppv', 'sens') # added by jie for recall-precision plot.
    x <- cbind(perf@alpha.values[[1]], perf@x.values[[1]], perf@y.values[[1]])
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
    #pdf(file=paste('recall-precision curve on test.pdf', sep=''))
    plot(recall, precision, type='l', main=paste0('classwt=', classwt, ' for ', data))
    #plot(perf)
    #dev.off()
    
    return(x)
    
}
pdf(file=paste('recall-precision curve for classwt checking.pdf', sep='')
    , width=15, height=10)
par(mfrow=c(2,2))
par(pty='m')
par(cex.main=1.3, cex.lab=1.2, cex.axis=1)
perf_0.6_vl <- check_classwt(0.3, 'validation', 0.6)
perf_0.6_ts <- check_classwt(0.3, 'test', 0.6)
perf_0.05_vl <- check_classwt(0.3, 'validation', 0.05)
perf_0.05_ts <- check_classwt(0.3, 'test', 0.05)
dev.off()
save(perf_0.6_vl, file='perf_0.6_vl.RData')
save(perf_0.6_ts, file='perf_0.6_ts.RData')
save(perf_0.05_vl, file='perf_0.05_vl.RData')
save(perf_0.05_ts, file='perf_0.05_ts.RData')

