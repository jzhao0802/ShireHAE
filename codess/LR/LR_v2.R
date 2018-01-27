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
resp_tr <- tr$response
resp_ts <- ts$response

#covariates selection
data <- ts
var_list <- names(data)
fct_flag <- sapply(data, is.character)
cati_var <- var_list[fct_flag]
binary_var <- grep('flag$', var_list, value=T)
conti_var <- grep('age|lookback_days|freq$', var_list, value=T)
length(cati_var)+length(binary_var)+length(conti_var)+1==length(var_list)

#for binray and contigorical covariables, let's create the cross table with response
#outPath <- 'D:\\Shire_project\\04_result\\LR'
outPath <- 'D:\\Shire_project\\04_result\\LR\\Jan08'
if(!file.exists(outPath)){
    dir.create(outPath, recursive=T, showWarnings=T)
    setwd(outPath)
}else{
    setwd(outPath)
}
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
        
        tb <- table(var, resp_v)
        n.row0 <- sum(tb[, 2]==0)
        n.pat <- tb[, 2]
        complete_separate <- ifelse(max_resp0 <= min_resp1 | max_resp1 <= min_resp0, 1, 
                                    ifelse(n.overlaped_pat <= n & n.overlaped_row <= m, 0.5, 0))
        result <- c(min_resp0, min_resp1, max_resp0, max_resp1, complete_separate)
        return(result)
    })
    sep_result_df <- ldply(sep_result, quickdf)
    sep_result1 <- cbind(var_list, sep_result_df)
    names(sep_result1) <- c('Variable', 'Min_resp0', 'Min_resp1'
                            , 'Max_resp0', 'Max_resp1', 'Comp_sep flag')
    return(sep_result1)
}

train_data_iter <- tr
test_data_iter <- ts
#check the missing value
miss_num <- apply(apply(train_data_iter, 2, is.na), 2, sum)
sum(miss_num>0) #0
#varaible descriptive statistics
##for catigorical and binary variables
crossTb <- create_crossTb(c(cati_var, binary_var), resp_tr, train_data_iter)
write.csv(crossTb, 'Cross Table for non-continous variables.csv', row.names = F)
##for continous variables
stat <- create_stat1(conti_var, resp_tr, train_data_iter)
write.csv(stat, 'Descriptive Statistics Table for continous variables v2.csv', row.names = F)

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

train_data_iter_result <- create_dummy(train_data_iter)
train_data_iter <- train_data_iter_result$data
cati_var_dummy <- train_data_iter_result$dummy_nm

test_data_iter <- create_dummy(test_data_iter)$data
##for binary+catigorial(turn to dummy) variables
OR_forBinary <- create_OR_forBinary(c(cati_var_dummy, binary_var), resp_tr, train_data_iter)
write.csv(OR_forBinary, 'Odds ratio check table for binary variables.csv', row.names = F)
binary_var_or_rm <- c(cati_var_dummy, binary_var)[OR_forBinary$odds_ratio>=10 
                               | OR_forBinary$odds_ratio==Inf
                               | OR_forBinary$odds_ratio==0
                               | is.na(OR_forBinary$odds_ratio)]
binary_var_or_rm1 <- c(cati_var_dummy, binary_var)[OR_forBinary$covar1_response1 <= 50]
##for continous variables
levels <- sapply(train_data_iter[, conti_var], function(x){length(table(x))})
conti_var_bin <- conti_var[levels <= 2] # 'prc_62_freq
OR_for_Conti_var_bin <- create_OR_forBinary(conti_var_bin, resp_tr, train_data_iter)
conti_var_bin_or_rm <- conti_var_bin[OR_for_Conti_var_bin$odds_ratio>=10 
                                                  | OR_for_Conti_var_bin$odds_ratio==Inf
                                                  | OR_for_Conti_var_bin$odds_ratio==0
                                                  | is.na(OR_for_Conti_var_bin$odds_ratio)]
conti_var_bin_or_rm1 <- conti_var_bin[OR_for_Conti_var_bin$covar1_response1 <= 50]
sep_result_forConti <- check_sep_forConti(conti_var, resp_tr, train_data_iter)
write.csv(sep_result_forConti, 'Complete separation check table for continous variables.csv', row.names = F)

conti_var_sep_rm <- conti_var[sep_result_forConti$`Comp_sep flag`==1]
conti_var_sep_rm <- c(conti_var_sep_rm, conti_var_bin_or_rm1)

#re-run logistci regression
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

head(coef_or)
var_kept <- setdiff(rownames(coef_or), '(Intercept)')
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
fit_summary <- summary(trn_logit_fit$value)$coefficients
stdErr_max <- max(fit_summary[, 2], na.rm=T)
fit_summary_ord <- fit_summary[order(fit_summary[, 2], decreasing = T),]
var_del_stderr <- rownames(fit_summary)[fit_summary[, 2]>10]
var_kept <- setdiff(var_kept, var_del_stderr)
trn_logit_fit2 = myTryCatch(
    glm(response~., family=binomial()
        , data=train_data_iter[, match(c(var_kept, 'response'), names(train_data_iter))])
)
#try bayesglm
trn_logit_fit2_bay = myTryCatch(
    bayesglm( .~response, family="binomial"
        , data=train_data_iter[, match(c(var_kept, 'response'), names(train_data_iter))])
)

var_del_all <- c(covar_rm_1)
var_del_bin <- var_del_all[var_del_all %in% c(binary_var, cati_var_dummy)]
var_del_conti <- var_del_all[var_del_all %in% conti_var]
#create_crossTb create_stat create_stat1 create_OR_forBinary check_sep_forConti
crossTb_forRemoved <- create_crossTb(var_del_bin, resp_tr, train_data_iter)
stat_forRemoved <- create_stat(var_del_conti, resp_tr, train_data_iter)
stat1_forRemoved <- create_stat1(var_del_conti, resp_tr, train_data_iter)
OR_forRemoved <- create_OR_forBinary(var_del_bin, resp_tr, train_data_iter)
separation_forRemoved <- check_sep_forConti(var_del_conti, resp_tr, train_data_iter)
write.csv(crossTb_forRemoved, 'crossTb_forRemoved.csv', row.names = F)
write.csv(stat_forRemoved, 'stat_forRemoved.csv', row.names = F)
write.csv(stat1_forRemoved, 'stat1_forRemoved.csv', row.names = F)
write.csv(OR_forRemoved, 'OR_forRemoved.csv', row.names = F)
write.csv(separation_forRemoved, 'separation_forRemoved.csv', row.names = F)

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
correlation_kept <- check_correlation(train_data_iter[, c('response', var_kept)], 0.8)
correlation_kept <- correlation_kept[order(abs(as.numeric(as.character(correlation_kept$Corr))), decreasing = T),]
write.xlsx(correlation_kept , 'covariance_matrix.xlsx', sheetNam='abs(corr)>0.8', row.names=T, append=T, showNA=T)
issue_covar_1 <- correlation_kept$Var1
issue_covar_2 <- correlation_kept$Var2

coef_or_issue1 <- coef_or[match(issue_covar_1, rownames(coef_or)), ]
coef_or_issue2 <- coef_or[match(issue_covar_2, rownames(coef_or)), ]

var_kept_2 <- setdiff(var_kept, issue_covar_1)
trn_logit_fit3 = myTryCatch(
    glm(response~., family=binomial()
        , data=train_data_iter[, match(c(var_kept_2, 'response'), names(train_data_iter))])
)

save(trn_logit_fit3, file='lgFit_final.RData')
trn_logit_fits$warning
start <- proc.time()
tst_prob_logit = predict(trn_logit_fit$value
                         , test_data_iter[, match(var_kept, names(test_data_iter))]
                         , type="response")
ms_logistic_rm <- getPerf(resp_ts, tst_prob_logit)
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
tst_prob_logit_vl = predict(fit_1
                            , x_test[, match(var_kept, names(x_test))]
                            , type="response")
ms_logistic_vl_rm <- getPerf(resp_ts, tst_prob_logit_vl)
names(ms_logistic_vl_rm) <- paste(names(ms_logistic_vl_rm), '_validation', sep='')

save(ms_logistic_rm, ms_logistic_vl_rm, file='ms_logistic_rm_vl_ts.RData')

save(ms_logistic_rm, ms_logistic_vl_rm
     , file=
         paste0('D:\\Shire_project\\04_result\\svm\\test_1fold\\iter1\\ms_logistic_rm_vl_ts.RData'))




































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

    
