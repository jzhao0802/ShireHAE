#Shire project
#01 generate statistic distribution for full data before modeling
#Developer - Jie Zhao
#Develope time - Dec 2015-Jan2016


library(xlsx)
library(plyr)
library(dplyr)
rootPath <- "D:\\Shire_project\\"
inPath <- paste(rootPath, '02_data\\',sep='')
outPath <- paste(inPath, 'preModel\\', sep='')


if(!file.exists(outPath)){
    dir.create(outPath, recursive=T, showWarnings=T)
    setwd(outPath)
}else{
    setwd(outPath)
}

load(paste(inPath, 'dat_hae_1111_rf.RData', sep=''))
load(paste(inPath, 'dat_nonhae_1111_rf.RData', sep=''))

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
write.csv(freq_df, "freq_check_with_dong.csv", row.names=F)

#create freq table for comulative %
var_focus <- as.character(dong_tb2[, 2])
var_focus1 <- gsub('(^\\+)(\\w+$)', '\\2',  var_focus, perl=T)
for(i in 1:length(var_focus1)){
    for(j in 1:i){
        eval(parse(paste()))
    }
}
row1 <- unlist(lapply(1:0, function(x){
    c <- sum((fullData$rx_72_flag==1) & fullData$hae==x)/sum(fullData$hae==x)
    return(c)
}))
row1

row2 <- unlist(lapply(1:0, function(x){
            c <- sum((fullData$rx_72_flag==1 | fullData$rx_75_flag==1) & fullData$hae==x)/sum(fullData$hae==x)
            return(c)
        }))
row2
row3 <- unlist(lapply(1:0, function(x){
    c <- sum((fullData$rx_72_flag==1 | fullData$rx_75_flag==1 | fullData$diag_6_flag==1) & fullData$hae==x)/sum(fullData$hae==x)
    return(c)
}))
row3
row4 <- unlist(lapply(1:0, function(x){
    c <- sum((fullData$rx_72_flag==1 | fullData$rx_75_flag==1 | fullData$diag_6_flag==1 |
                  fullData$rx_77_flag==1) 
             & fullData$hae==x)/sum(fullData$hae==x)
    return(c)
}))
row4
row5 <- unlist(lapply(1:0, function(x){
    c <- sum((fullData$rx_72_flag==1 | fullData$rx_75_flag==1 | fullData$diag_6_flag==1 |
                  fullData$rx_77_flag==1 | fullData$rx_73_flag==1) 
             & fullData$hae==x)/sum(fullData$hae==x)
    return(c)
}))
row5
row6 <- unlist(lapply(1:0, function(x){
    c <- sum((fullData$rx_72_flag==1 | fullData$rx_75_flag==1 | fullData$diag_6_flag==1 |
                  fullData$rx_77_flag==1 | fullData$rx_73_flag==1|fullData$rx_74_flag==1) 
             & fullData$hae==x)/sum(fullData$hae==x)
    return(c)
}))
row6
row7 <- unlist(lapply(1:0, function(x){
    c <- sum((fullData$rx_72_flag==1 | fullData$rx_75_flag==1 | fullData$diag_6_flag==1 |
                  fullData$rx_77_flag==1 | fullData$rx_73_flag==1|fullData$rx_74_flag==1|
                  fullData$diag_1_flag==1) 
             & fullData$hae==x)/sum(fullData$hae==x)
    return(c)
}))
row7
row8 <- unlist(lapply(1:0, function(x){
    c <- sum((fullData$rx_72_flag==1 | fullData$rx_75_flag==1 | fullData$diag_6_flag==1 |
                  fullData$rx_77_flag==1 | fullData$rx_73_flag==1|fullData$rx_74_flag==1|
                  fullData$diag_1_flag==1 | fullData$rx_67_flag==1) 
             & fullData$hae==x)/sum(fullData$hae==x)
    return(c)
}))
row8
row9 <- unlist(lapply(1:0, function(x){
    c <- sum((fullData$rx_72_flag==1 | fullData$rx_75_flag==1 | fullData$diag_6_flag==1 |
                  fullData$rx_77_flag==1 | fullData$rx_73_flag==1|fullData$rx_74_flag==1|
                  fullData$diag_1_flag==1 | fullData$rx_67_flag==1| fullData$rx_68_flag==1) 
             & fullData$hae==x)/sum(fullData$hae==x)
    return(c)
}))
row9
row10 <- unlist(lapply(1:0, function(x){
    c <- sum((fullData$rx_72_flag==1 | fullData$rx_75_flag==1 | fullData$diag_6_flag==1 |
                  fullData$rx_77_flag==1 | fullData$rx_73_flag==1|fullData$rx_74_flag==1|
                  fullData$diag_1_flag==1 | fullData$rx_67_flag==1| fullData$rx_68_flag==1|
                  fullData$rx_71_flag==1) 
             & fullData$hae==x)/sum(fullData$hae==x)
    return(c)
}))
row10
row11 <- unlist(lapply(1:0, function(x){
    c <- sum((fullData$rx_72_flag==1 | fullData$rx_75_flag==1 | fullData$diag_6_flag==1 |
                  fullData$rx_77_flag==1 | fullData$rx_73_flag==1|fullData$rx_74_flag==1|
                  fullData$diag_1_flag==1 | fullData$rx_67_flag==1| fullData$rx_68_flag==1|
                  fullData$rx_71_flag==1 | fullData$rx_70_flag==1) 
             & fullData$hae==x)/sum(fullData$hae==x)
    return(c)
}))
row11
row12 <- unlist(lapply(1:0, function(x){
    c <- sum((fullData$rx_72_flag==1 | fullData$rx_75_flag==1 | fullData$diag_6_flag==1 |
                  fullData$rx_77_flag==1 | fullData$rx_73_flag==1|fullData$rx_74_flag==1|
                  fullData$diag_1_flag==1 | fullData$rx_67_flag==1| fullData$rx_68_flag==1|
                  fullData$rx_71_flag==1 | fullData$rx_70_flag==1|fullData$diag_5_flag==1) 
             & fullData$hae==x)/sum(fullData$hae==x)
    return(c)
}))
row12
row13 <- unlist(lapply(1:0, function(x){
    c <- sum((fullData$rx_72_flag==1 | fullData$rx_75_flag==1 | fullData$diag_6_flag==1 |
                  fullData$rx_77_flag==1 | fullData$rx_73_flag==1|fullData$rx_74_flag==1|
                  fullData$diag_1_flag==1 | fullData$rx_67_flag==1| fullData$rx_68_flag==1|
                  fullData$rx_71_flag==1 | fullData$rx_70_flag==1|fullData$diag_5_flag==1|
                  fullData$diag_56_flag==1) 
             & fullData$hae==x)/sum(fullData$hae==x)
    return(c)
}))
row13
row14 <- unlist(lapply(1:0, function(x){
    c <- sum((fullData$rx_72_flag==1 | fullData$rx_75_flag==1 | fullData$diag_6_flag==1 |
                  fullData$rx_77_flag==1 | fullData$rx_73_flag==1|fullData$rx_74_flag==1|
                  fullData$diag_1_flag==1 | fullData$rx_67_flag==1| fullData$rx_68_flag==1|
                  fullData$rx_71_flag==1 | fullData$rx_70_flag==1|fullData$diag_5_flag==1|
                  fullData$diag_56_flag==1|fullData$diag_53_flag==1) 
             & fullData$hae==x)/sum(fullData$hae==x)
    return(c)
}))
row14
row15 <- unlist(lapply(1:0, function(x){
    c <- sum((fullData$rx_72_flag==1 | fullData$rx_75_flag==1 | fullData$diag_6_flag==1 |
                  fullData$rx_77_flag==1 | fullData$rx_73_flag==1|fullData$rx_74_flag==1|
                  fullData$diag_1_flag==1 | fullData$rx_67_flag==1| fullData$rx_68_flag==1|
                  fullData$rx_71_flag==1 | fullData$rx_70_flag==1|fullData$diag_5_flag==1|
                  fullData$diag_56_flag==1|fullData$diag_53_flag==1|fullData$diag_49_flag==1) 
             & fullData$hae==x)/sum(fullData$hae==x)
    return(c)
}))
row15
full <- numeric()
for(i in 1:15){
    eval(parse(paste("full <- rbind(full, row", i, ")", sep='')))
    
}
freq_cumulative <- rbind(row1,row2, row3, row4, row5, row6, row7, row8, row9, row10, row11, row12, row13, row14, row15)
freq_cumulative <- cbind(var_focus, freq_cumulative)
colnames(freq_cumulative)<- c('Variable',	'HAE',	'NON-HAE')
write.csv(freq_cumulative, 'freq_table_compare_with_dong_comulative.csv', row.names=F)
#create freq table as Danels's
cell1 <- unlist(lapply(1:0, function(x){
    cell <- sum(
        fullData$rx_75_flag==1 
        & 
        (fullData$diag_6_flag==1 | fullData$rx_71_flag==1 | 
         fullData$rx_72_flag==1 | fullData$rx_73_flag==1 | fullData$diag_5_flag==1 | fullData$diag_48_flag==1
        )
        & fullData$hae==x)
    ratio <- cell/sum(fullData$hae==x)
    
}))

cell2 <- unlist(lapply(1:0, function(x){
    cell <- sum(
        (fullData$diag_1_flag==1 | fullData$diag_40_flag==1 |fullData$diag_56_flag==1)
        & 
            (fullData$diag_6_flag==1 | fullData$rx_71_flag==1 | 
                 fullData$rx_72_flag==1 | fullData$rx_73_flag==1 
            )
        & fullData$hae==x)
    ratio <- cell/sum(fullData$hae==x)
    
}))
cell2

cell3 <- unlist(lapply(1:0, function(x){
    cell <- sum(
        (fullData$rx_77_flag==1)
        & 
        (fullData$diag_6_flag==1 | fullData$rx_71_flag==1 | 
                 fullData$rx_72_flag==1 | fullData$rx_73_flag==1 | fullData$diag_5_flag==1 | fullData$diag_48_flag==1
        )
        & 
        fullData$hae==x)
    ratio <- cell/sum(fullData$hae==x)
    
}))
cell3

cell4 <- unlist(lapply(1:0, function(x){
    cell <- sum(
        (fullData$diag_50_flag==1 | fullData$diag_53_flag==1 | fullData$diag_58_flag==1 | fullData$prc_62_flag==1|
             fullData$diag_39_flag==1 | fullData$diag_49_flag==1 | fullData$diag_41_flag==1 | fullData$prc_65_flag==1|
             fullData$prc_63_flag==1 | fullData$diag_2_flag==1 |fullData$diag_3_flag==1
         )
        & 
            (fullData$diag_6_flag==1 | fullData$rx_71_flag==1 | 
                 fullData$rx_72_flag==1 | fullData$rx_73_flag==1 | fullData$diag_5_flag==1 | fullData$diag_48_flag==1
            )
        & 
            fullData$hae==x)
    ratio <- cell/sum(fullData$hae==x)
    
}))
cell4

cell5 <- unlist(lapply(1:0, function(x){
    cell <- sum(
        (fullData$rx_74_flag==1
        )
        & 
            (fullData$diag_6_flag==1 | fullData$rx_71_flag==1 | 
                 fullData$rx_72_flag==1 | fullData$rx_73_flag==1 | fullData$diag_5_flag==1 | fullData$diag_48_flag==1
            )
        & 
            fullData$hae==x)
    ratio <- cell/sum(fullData$hae==x)
    
}))
cell5
cell_result <- rbind(cell1, cell2, cell3, cell4, cell5)
colnames(cell_result) <- c('% of HAE population with this combination', '% of non-HAE population with this combination')
write.csv(cell_result, 'freq_table_compare.csv', row.names=F)



ratioInHAE <- cell1/sum(fullData$hae==1)
ratioInNonHAE <- cell1/sum(fullData$hae==0)
cell2 <- unlist(lapply(1:0, function(x){
    cell <- sum(fullData$rx_75_freq==1 & fullData$diag_1_freq==1)/sum(fullData$hae==x)
    ratio <- cell/sum(fullData$hae==x)
}))
cell2

table(fullData[fullData$hae==0, 'gender'])/sum(fullData$hae==0)






