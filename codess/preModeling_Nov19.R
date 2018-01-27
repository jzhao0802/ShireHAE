#Shire project
#01 generate statistic distribution for full data before modeling
#Developer - Jie Zhao
#Develope time - Dec 2015-Jan2016

library(xlsx)
library(plyr)
library(dplyr)
library(glmnet)
rootPath <- "D:\\Shire_project\\"
inPath <- paste(rootPath, '02_data\\',sep='')
outPath <- paste(inPath, 'preModel', sep='')


if(!file.exists(outPath)){
    dir.create(outPath, recursive=T, showWarnings=T)
    setwd(outPath)
}else{
    setwd(outPath)
}

load(paste(inPath, 'dat_hae_1111_rf.RData', sep=''))
load(paste(inPath, 'dat_nonhae_1111_rf.RData', sep=''))

p_id_hae <- dat_hae_1111_rf[, 1]
p_id_nonHae <- dat_nonhae_1111_rf[, 1]
fullData <- rbind(dat_hae_1111_rf[, -1], dat_nonhae_1111_rf[, -1]) #[1] 247833    242
names(fullData) <- tolower(names(fullData))
table(fullData$hae)

str(fullData[, 1:10])
var_list <- names(fullData)
#check the missing value
miss_cnt <- apply(apply(fullData, 2, is.na), 2, sum)
var_list[miss_cnt >0] #0

cha_flag <- sapply(fullData, is.character)
cha_var <- var_list[cha_flag]

freqTb <- lapply(cha_var, function(v){
    var <- fullData[, v]
    table(var)
})

#check the constant variables
var_levels <- unlist(lapply(var_list, function(v){
    var <- fullData[, v]
    tb <- table(var)
    return(dim(tb))
}))
cbind(var_list, var_levels)
constant_var <- var_list[var_levels==1]

#check the frequency table for variables Dong mentioned in email
dong_tb <- read.xlsx(paste(inPath, 'Copy of HAE vs Non HAE flags.xlsx', sep=''),
                     sheetIndex = 2, as.data.frame=T, header=T, 
                     colClasses=c('integer', "character", "character", "numeric", 'numeric'),
                     stringsToFactors=F)
dong_tb2 <- read.xlsx(paste(inPath, 'Copy of HAE vs Non HAE flags.xlsx', sep=''),
                     sheetIndex = 1, as.data.frame=T, header=T, 
                     colClasses=c('integer', "character", "character", "numeric", 'numeric'),
                     stringsToFactors=F)

create_freqTb <- function(dong_tb){
    var_focus <- grep("^[rx|diag|prc]", as.character(dong_tb[, 3]), value=T, ignore.case=T)
    var_focus1 <- tolower(paste(var_focus, '_flag', sep='') )
    
    resp <- fullData$hae
    freq_list <- lapply(var_focus1, function(v){
        var <- fullData[, v]
        tb <- table(var, resp)
        return(c(v, prop.table(tb,2)[2, ]))
    })
    freq_df <- ldply(freq_list, quickdf)[, c(1,3,2)]
    return(freq_df)
}
freq_df <- create_freqTb(dong_tb)
write.csv(freq_df, "\\freq_check_with_dong.csv", row.names=F)

#create dummy baraibles for catigorical variables "gender" "region"
to_dummy_full <- as.data.frame(model.matrix(~., data=as.factor(fullData[, sapply(fullData, is.character)])
                                       , contrasts.arg=lapply(cha_var, function(v){
                                           return(contrasts(as.factor(fullData[, v]), contrasts=F))
                                       })))
to_dummy <- as.data.frame(model.matrix(~ gender + region, data=fullData))[, -1]
fullData1 <- cbind(fullData[, -match(cha_var, var_list)], to_dummy) # 247833    246
names(fullData1)[match('hae', names(fullData1))] <- 'response'
#lasso regression
#with scaling predictors

set.seed(20)

#separate mutual exclusive simulations for all the nonHAE patients
simulationNum <- 20
foldNum <- 5
n_hae <- sum(fullData1$response==1)
n_nonHae <- sum(fullData1$response==0)
HAE <- subset(fullData1, response==1)
nonHAE <- subset(fullData1, response==0)

#split the HAE into 5 mutual independent sample sets
foldid<- numeric(n_nonHae)
foldid<- sample(rep(1:simulationNum, length=n_hae))
table(foldid) # QC
#fetch the matching nonHAE samples for each set of HAE samples

#split into traing and test
sim=1

idx_HAE_iRep_test <- which(foldid==sim)
idx_nonHAE_iRep_test <- which(!is.na(match( p_id_nonHae, p_id_hae[idx_HAE_iRep])))
HAE_sim_test <- HAE[idx_HAE_iRep_test, ]
NonHAE_sim_test <- nonHAE[idx_nonHAE_iRep_test,]
fullDt_iRep_test <- rbind(HAE_sim_test, NonHAE_sim_test)

HAE_sim_training <- HAE[-idx_HAE_iRep_test, ]
NonHAE_sim_training <- nonHAE[-idx_nonHAE_iRep_test,]
fullDt_iRep_training <- rbind(HAE_sim_training, NonHAE_sim_training)
#separate those nonHAE patients into 5 folds to carray out the cross validation
#idxListForFolds <-  createFolds(fullDt_iRep$hae, k=foldNum, list=TRUE, 
#                               returnTrain=F)
training <- fullDt_iRep_training
test <- fullDt_iRep_test

#split the training data into 20 mutual parts for 20 iterations

foldid <- numeric(nrow(NonHAE_sim_training))
foldid <- sample(rep(1:simulationNum, length=nrow(NonHAE_sim_training)))
table(foldid)
# QC each fold has 9860

run_lasso_eachSim <- function(sim){
    fullDt_iRep_training <- rbind(HAE_sim_training, NonHAE_sim_training[foldid==sim,])
    
    # Calculating initial lambda and the lambda sequence
    model_matrix<- model.matrix(response~., data=fullDt_iRep_training)[,-1]     # removes intercept term
    #test_matrix<- model.matrix(response~., data=fullDt_iRep_test)[,-1]     # removes intercept term
    model_matrix1 <- scale(model_matrix)
    #test_matrix1 <- scale(test_matrix)
    response_sim_training <- fullDt_iRep_training$response
    response_sim_test <- fullDt_iRep_test$response
    wt_list <- seq(1:2)
    auc_fromCV_wt <- numeric()
    for(wt in wt_list){
        initial_lambda<-glmnet(x=model_matrix1, y=response_sim_training, family="binomial", alpha=0, 
                               weights=ifelse(response_sim_training==0, 1, wt), 
                               standardize=F)$lambda  # calculating the initial lambda
        lambda_seq<- c(initial_lambda[-length(initial_lambda)] , 
                       seq(initial_lambda[length(initial_lambda)] , 0 , length=500)) # get a length=500 descending sequence from initial lambda to 0
        
        training_data <- fullDt_iRep_training
        resp_training <- training_data$response
        k.folds<- 5
        foldid<- nrow(training_data)
        foldid[training_data$response==1]<- sample(rep(1:k.folds, length=length(which(training_data$response==1))))
        foldid[training_data$response==0]<- sample(rep(1:k.folds, length=length(which(training_data$response==0))))
        table(training_data$response , foldid) # QC
        
        auc_fromCV <- lapply(1:2, function(i){
            cv_training_resp <- resp_training[foldid!=i]
            cv_training_matrix<- model_matrix1[foldid!=i,]
            cv_test_resp<- resp_training[foldid==i]                                                # select 1 fold as test data
            cv_test_matrix<- model_matrix1[foldid==i,]
            wt_vct <- ifelse(cv_training_resp==0, 1, wt)
            
            fit_lasso<- glmnet(cv_training_matrix, cv_training_resp, weights=wt_vct,
                               lambda=lambda_seq, family="binomial", alpha=0, standardize=F)
            test_pred<- predict(fit_lasso, cv_test_matrix, type="response") #2713 599
            auc1_byLmd <- apply(test_pred, 2, function(x){
                createCurve(cv_test_resp, x)
            })
            auc1_byLmd<- c(wt=wt, auc1_byLmd , rep(NA , length(lambda_seq) - length(auc1_byLmd))) # some small lambda may not be reached
            return(auc1_byLmd)
        })
        auc_fromCV_df <- ldply(auc_fromCV, quickdf)
        auc_fromCV_avg <- aggregate(auc_fromCV_df[, -1], by=list(auc_fromCV_df[, 1]), mean, na.rm=T)
        auc_fromCV_wt <- rbind(auc_fromCV_wt, auc_fromCV_avg)
        for_curve_cvTest[[sim]] <- cbind(resp=cv_test_resp, pred=)
        
    }
    #find the optimum lambda and wt
    forMax <- lapply(1:nrow(auc_fromCV_wt), function(r){
                row <- auc_fromCV_wt[r,-1]
                lmd_max <- lambda_seq[which(row==max(row))]
                max_value <- max(row)
                tb <- data.frame(wt=wt_list[r], lambda=lmd_max, auc_avg=max_value)
                return(tb)
            })
    forMax1 <- ldply(forMax, rbind)
    #find the optimum wt and lambda
    optim_row <- forMax1[forMax1$auc_avg==max(forMax1$auc_avg),]
    optim_wt <- as.numeric(optim_row[1])
    optim_lambda <- as.numeric(optim_row[2])
    #apply the optimum weight and lambda to the test data
    fit_lasso_eval<- glmnet(model_matrix1, response_sim_training, weights=ifelse(response_sim_training==0, 1, optim_wt),
                       lambda=optim_lambda, family="binomial", alpha=0, standardize=F)
    test_obs <- predict(fit_lasso_eval, test_matrix1, type="response")
    
}






