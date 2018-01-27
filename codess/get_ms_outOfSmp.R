#calculate performance matrics of out of sample for test1
#Developer - Jie Zhao
#Develope time - Dec 2015-Jan2016
library(ROCR)
library(plyr)
library(dplyr)
library(caTools)
source("D:\\Shire_project\\03_code\\subFunc.R")

#inPath <- 'D:\\Shire_project\\04_result\\Lasso\\test1(wt-0.1, 0.4, 0.7, 1)\\iteration1-10\\'
#inPath <- 'D:\\Shire_project\\04_result\\Lasso\\test1(wt-0.1, 0.4, 0.7, 1)\\iteration11-20\\'
inPath <- 'D:\\Shire_project\\04_result\\Lasso\\test2\\'
outPath <- paste0(inPath, "out of sample")
if(!file.exists(outPath)){
    dir.create(outPath, recursive=T, showWarnings=T)
    setwd(outPath)
}else{
    setwd(outPath)
}

get_ms_onOutOfSmp <- function(inPath, crit){
    load(paste0(inPath, 'outOfSmpCurve.RData'))
    crit <- 0.3
    
    #bkt_all <- lapply(outOfSmpCurve, function(X){
    temp1 <- list()
    for(i in 1:length(outOfSmpCurve)){
        err_cat <- tryCatch(
            {temp1[[i]] <- lapply(outOfSmpCurve[[i]], function(X1){
                temp11 <- getBkt(X1[, 2], X1[, 1], n.bkt=200)
                curve <- temp11[[1]]
                ms <- temp11[[2]]
                idx <- which(abs(curve[, 2]-crit)==min(abs(curve[, 2]-crit), na.rm=T))
                prec_sel <- curve[idx, 3]
                ms_f <- c(ms, ppv_tar=prec_sel)
                return(ms_f)
            })
            temp1_df <- ldply(temp1, quickdf)
            ms_iter <- apply(temp1_df, 2, mean,na.rm=T)
            return(ms_iter)},
            error = function(e) e
        )
        if (inherits(err_cat, "error")) next
        
    }
    
    temp2 <- lapply(temp1, function(X){
        error_cat <- tryCatch(
            {df <- ldply(X, quickdf)
            }, 
            error=function(e) e)
        if(inherits(error_cat, "error")){
            df <- rep(NA, 3)
        }
        mean <- apply(df, 2, mean, na.rm=T)
    })
    na_flag <- unlist(lapply(temp2, length))==0
    ms_allSim <- data.frame(cbind(seq(1, length(outOfSmpCurve))[!na_flag], 
                                  tryCatch(
                                      {df <- ldply(temp2[!na_flag], quickdf)},
                                      error=function(e) e)))
    names(ms_allSim)[1]<- "iter"
    #add optimum wt and lambda
    load(paste0(inPath, 'optimum_pmt_df.RData'))
    ms_allSim_v2 <- full_join(ms_allSim, optimum_pmt_df, by=c("iter"='iter') )
    ms_allSim_v2 <- ms_allSim_v2[, c( 1, 6, 5, 4, 2, 3)]
    ms_allSim_v2 <- ms_allSim_v2[order(ms_allSim_v2[, 1], decreasing = F), ]
    
}
test2_outOfSmp_ms <- get_ms_onOutOfSmp(inPath, 0.3)
write.csv(test2_outOfSmp_ms, 'test2_outOfSmp_ms.csv', row.names = F)

