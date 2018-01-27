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
data <- LR_fit$data

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


save(fit_list, file='fit_list_for_issuePairLR.RData')

fit_rm <- myTryCatch(glm(response ~., data=data[, setdiff(names(data), c('rx_73_freq', 'diag_32_freq'))], family = binomial()))
issue_covar <- rownames(coef_or)[-1]
corr <- cor(data[, c("response", issue_covar)])
crossTable <- sapply(data[, issue_covar], function(x){table(x=x, response=data$response)})
save(crossTable, corr, fit_rm, file='result_Jan07.RData')


data$count1<- 1

a_summary<- aggregate(cbind(response,count1)~rx_73_freq+diag_32_freq, data, FUN=sum)
a_summary$nonresponse<-a_summary$count1 - a_summary$response
library(dplyr)
arrange(a_summary,desc(response))
