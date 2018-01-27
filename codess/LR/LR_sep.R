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
source("D:\\Shire_project\\03_code\\Jie\\sep\\subFunc_sep_Dec08.R")

outPath <- 'D:\\Shire_project\\04_result\\LR\\Jan07'
inPath <- outPath
if(!file.exists(outPath)){
    dir.create(outPath, recursive=T, showWarnings=T)
    setwd(outPath)
}else{
    setwd(outPath)
}
load('lgFit_final.RData')

trn_logit_fit3$warning
LR_fit <- trn_logit_fit3$value
tr_data_cv <- LR_fit$data
train_data_iter <- LR_fit$data
test_data_iter <- test_data_iter[, match(names(train_data_iter), names(test_data_iter))]

conti_var_flag <- sapply(data[, setdiff(names(data), 'response')], function(x)length(table(x)) > 2)
conti_var <- setdiff(names(data), 'response')[conti_var_flag]

#for conti_var, let's do the logistic regression fit for every n(0 < n < length(conti_var)) covariates with response
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

fit_list <- list()
k=1
for(i in 1:(length(conti_var)-1)){
    for (j in (i+1):length(conti_var)){
        v1 <- conti_var[i]
        v2 <- conti_var[j]
        eval(parse(text=paste0("temp_fit <- myTryCatch(glm(response ~ ", v1, " + ", v2, ", data=data, family=binomial())) ")))
        if(!is.null(temp_fit$warning)){
            print(temp_fit$warning)
            coef_or <- data.frame(coef=coef(temp_fit$value), or=exp(coef(temp_fit$value)))
            print(coef_or)
            fit_list[[k]] <- list(fit=temp_fit, coef_or=coef_or)
            k <- k+1
        }
    }
}


fit_rm <- myTryCatch(glm(response ~., data=data[, setdiff(names(data), c('rx_73_freq', 'diag_32_freq'))], family = binomial()))
issue_covar <- rownames(coef_or)[-1]
corr <- cor(data[, c("response", issue_covar)])
crossTable <- sapply(data[, issue_covar], function(x){table(x=x, response=data$response)})
save(crossTable, corr, fit_rm, file='result_Jan07.RData')

x_levels <- sort(as.numeric(unique(c(rownames(tb1), rownames(tb2)))))

x1_levels <- as.numeric(rownames(tb1))
x2_levels <- as.numeric(rownames(tb2))

x1_umtc <- setdiff(x_levels, x1_levels)
x2_umtc <- setdiff(x_levels, x2_levels)


tb11 <- cbind(x=x1_levels, tb1)
tb21 <- cbind(x=x2_levels, tb2)

tb12 <- rbind(tb11, c(x1_umtc, 0, 0))
tb22 <- rbind(tb21, cbind(x2_umtc, 0, 0))

tb_agg <- data.frame(tb12[order(tb12[, 1]),], tb22[order(tb22[, 1]), ])

tb_agg1 <- apply(tb_agg, 1, sum)

data$count1<- 1

a_summary<- aggregate(cbind(response,count1)~rx_73_freq+diag_32_freq, data, FUN=sum)
a_summary$nonresponse<-a_summary$count1 - a_summary$response
library(dplyr)
arrange(a_summary,desc(response))

fit_rm <- myTryCatch(glm(response ~., data=data[, setdiff(names(data), c('diag_32_freq'))], family = binomial()))


train_data_iter <- train_data_iter[, setdiff(names(train_data_iter), c('diag_32_freq'))]
test_data_iter <- test_data_iter[, setdiff(names(test_data_iter), c('diag_32_freq'))]

fit_rm <- myTryCatch(glm(response ~., data=train_data_iter, family = binomial()))

start <- proc.time()
tst_prob_logit_vl = predict(fit_rm$value
                         , test_data_iter[, setdiff(names(test_data_iter), 'response')]
                         , type="response")
ms_logistic_vl_rm <- getPerf(test_data_iter$response, tst_prob_logit_vl)
#load in the original train and test data for iteration 1
path_sepData <- 'D:\\Shire_project\\04_result\\data_sep\\v2\\iter1'
load(paste(path_sepData, '\\trTs.RData', sep=''))
tr_full <- trTs$tr
ts_full <- trTs$ts
resp_tr <- tr_full$response
resp_ts <- ts_full$response
tr_full <- create_dummy(tr_full)$data
ts_full <- create_dummy(ts_full)$data
x_test <- ts_full[, -match('response', names(ts_full))]

#logistci regression
tst_prob_logit = predict(fit_rm$value
                            , x_test[, setdiff(names(train_data_iter), 'response')]
                            , type="response")
ms_logistic_rm <- getPerf(resp_ts, tst_prob_logit)
names(ms_logistic_vl_rm) <- paste(names(ms_logistic_vl_rm), '_validation', sep='')

save(ms_logistic_rm, ms_logistic_vl_rm, file='ms_logistic_rm_vl_ts.RData')
write.csv(c(ms_logistic_vl_rm, ms_logistic_rm), 'LR.csv')
#save(ms_logistic_rm, ms_logistic_vl_rm
 #    , file=
  #       paste0('D:\\Shire_project\\04_result\\svm\\test_1fold\\iter1\\ms_logistic_rm_vl_ts.RData'))



