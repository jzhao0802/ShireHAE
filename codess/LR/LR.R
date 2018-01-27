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

rootPath <- "D:\\Shire_project\\"
type='v2'

iter=1
path_sepData <- paste(rootPath, '04_result\\data_sep\\', type, '\\iter', iter, sep='')

i=1
load(paste(path_sepData, '\\dataset_cv', i, '.RData', sep=''))
cv_data <- cv_data_trTs$cv_data
cv_tr_flag <- cv_data_trTs$cv_tr_flag
tr <- cv_data[cv_tr_flag, ]
ts <- cv_data[!cv_tr_flag, ]

#covariates selection
data <- ts
var_list <- names(data)
fct_flag <- sapply(data, is.character)
cati_var <- var_list[fct_flag]
binary_var <- grep('flag$', var_list, value=T)
conti_var <- grep('age|lookback_days|freq$', var_list, value=T)
length(cati_var)+length(binary_var)+length(conti_var)+1==length(var_list)

#for binray and contigorical covariables, let's create the cross table with response
outPath <- 'D:\\Shire_project\\04_result\\LR'
setwd(outPath)
resp_v <- data[, 'response']
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



path_sepData <- 'D:\\Shire_project\\04_result\\data_sep\\v2\\iter1'
load(paste(path_sepData, '\\trTs.RData', sep=''))
train_data_iter <- trTs$tr
test_data_iter <- trTs$ts
resp_ts <- test_data_iter$response
resp_tr <- train_data_iter$response
x_test <- test_data_iter[, -match('response', names(test_data_iter))]
#check the missing value
miss_num <- apply(apply(train_data_iter, 2, is.na), 2, sum)
sum(miss_num>0) #0
#varaible descriptive statistics
##for catigorical and binary variables
crossTb <- create_crossTb(c(cati_var, binary_var), resp_tr, train_data_iter)
write.csv(crossTb, 'Cross Table for non-continous variables.csv', row.names = F)
##for continous variables
stat <- create_stat(conti_var, resp_tr, train_data_iter)
write.csv(stat, 'Descriptive Statistics Table for continous variables.csv', row.names = F)

#variables selection 
##for binary variables
OR_forBinary <- create_OR_forBinary(binary_var, resp_tr, train_data_iter)
write.csv(OR_forBinary, 'Odds ratio check table for binary variables.csv', row.names = F)
binary_var_or_rm <- binary_var[OR_forBinary$odds_ratio>=10 
                               | OR_forBinary$odds_ratio==Inf
                               | OR_forBinary$odds_ratio==0
                               | is.na(OR_forBinary$odds_ratio)]
##for continous variables
sep_result_forConti <- check_sep_forConti(conti_var, resp_tr, train_data_iter)
write.csv(sep_result_forConti, 'Complete separation check table for continous variables.csv', row.names = F)

conti_var_sep_rm <- conti_var[sep_result_forConti$`Comp_sep flag`==1]


#re-run logistci regression
covar_rm <- c(binary_var_or_rm, conti_var_sep_rm)
covar_rm_2_list <- list()
start <- proc.time()
for(i in 1:10){
    cat('iteration-', i, ' start!\n')
    if(i == 1){
        trn_logit_fit = myTryCatch(
            glm(response~., family=binomial(), data=train_data_iter)
        )

    }else{
        covar_rm_1 <- c(covar_rm_2 )
        trn_logit_fit = myTryCatch(
            glm(response~., family=binomial(), data=train_data_iter[, -match(covar_rm_1, names(train_data_iter))])
        )
        
    }
    coef_or <- data.frame(coef=coef(trn_logit_fit$value), or=exp(coef(trn_logit_fit$value)))
    if(i==1){
        save(coef_or, file='LR_coef_or_org.R')
    }
    if(is.null(trn_logit_fit$warning)){
        save(coef_or, file='LR_coef_or_delVar.R')
        stop('issue covariates have been moved all and without warning!\n')
    }else{
        covar_rm_2 <- rownames(coef_or)[abs(coef_or$coef) >= 5 | is.na(abs(coef_or$coef)) | coef_or$or >= 10]
        if(length(covar_rm_2)==0){
            save(coef_or, file='LR_coef_or_delVar.R')
            stop('issue covariates have been moved all, but still with warning-', trn_logit_fit$warning, '\n')
        }else{
            
            covar_rm_2 <- c(covar_rm_1, covar_rm_2)
            covar_rm_2_list[[i]] <- covar_rm_2   
        }
    }
}
cat('time used:', (proc.time()-start)[3]/60)

start <- proc.time()
trn_logit_fit = glm(response~., family=binomial(), data=train_data_iter[, -match(covar_rm, names(train_data_iter))])
cat('time used:', (proc.time()-start)[3]/60)
coef_or <- data.frame(coef=coef(trn_logit_fit), or=exp(coef(trn_logit_fit)))
covar_rm2 <- rownames(coef_or)[abs(coef_or$coef) >= 5]
trn_logit_fit2 <- myTryCatch(
                        glm(response~., family=binomial()
                          , data=train_data_iter[, -match(c(covar_rm, covar_rm2), names(train_data_iter))]))

tst_prob_logit = predict(trn_logit_fit, x_test[, -match(c(covar_rm, covar_rm2), names(x_test))], type="response")
ms_logistic <- getPerf(resp_ts, tst_prob_logit)


#load in the original train and test data for fold 1 of iteration 1
path_sepData <- 'D:\\Shire_project\\04_result\\data_sep\\v2\\iter1'
load(paste(path_sepData, '\\dataset_cv', 1, '.RData', sep=''))
cv_data_tr <- cv_data_trTs$cv_data[cv_data_trTs$cv_tr_flag,] #[1] 127032    242
cv_data_ts <- cv_data_trTs$cv_data[!cv_data_trTs$cv_tr_flag,] #[1] 31356   242
cv_resp_ts <- cv_data_ts$response
cv_x_ts <- cv_data_ts[, -match('response', names(cv_data_ts))]
#logistci regression
trn_logit_fit_vl = glm(response~., family=binomial(), data=cv_data_tr)
tst_prob_logit_vl = predict(trn_logit_fit_vl, cv_x_ts, type="response")
ms_logistic_vl <- getPerf(cv_resp_ts, tst_prob_logit_vl)
names(ms_logistic_vl) <- paste(names(ms_logistic_vl), '_validation', sep='')




































OUT<- file('cross_table.txt' , 'w')
for(v in var_list){
    var <- data[, v]
    tb<- table(var, resp_v)
    v_nm<- rownames(tb)
    rsp_nm <- colnames(tb)
    writeLines(paste(v, paste('Frequency', rsp_nm, sep='_'), sep='\t') , OUT)
    writeLines(paste(rep('-' , 100) , collapse='') , OUT)
    for(j in 1:nrow(tb)){
        writeLines(paste(v_nm[j] , tb[j, ] , sep='\t') , OUT)
    }
    writeLines('' , OUT)
}
close(OUT)

    
