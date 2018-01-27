#cost-sensitive SVM for HAE project
#Created on 18/11/2015
#Devloped by Hui Jin

library(dplyr)
library(klaR)
library(ROCR)
library(PRROC)

#constance
inpath <- 'D:/Shire_project/02_data'
outpath <- 'D:/Shire_project/04_result'

iter <- 20
kfold <- 5


#1. reading the data
load(paste(inpath, "/dat_hae_1111_rf.RData", sep=''))
load(paste(inpath, "/dat_nonhae_1111_rf.RData", sep=''))
hae_data <- dat_hae_1111_rf
nonhae_data <- dat_nonhae_1111_rf

names(hae_data) <- tolower(names(hae_data))
names(nonhae_data) <- tolower(names(nonhae_data))

#1. tidy the data
names(hae_data) <- tolower(names(hae_data))
names(nonhae_data) <- tolower(names(nonhae_data))

#add the iteration number for non-hae data
iters <- rep(1:20, each=10, 1233)
nonhae_data <- cbind(nonhae_data, iters)

#create dummy variables for region
gen_reg_haecol <- data.frame(gender=as.factor(hae_data$gender), region=as.factor(hae_data$region))
gen_reg_hae <- model.matrix(~gender + region, gen_reg_haecol, contrasts.arg = lapply(gen_reg_haecol, contrasts, contrasts=F))[, -1]
gen_reg_hae <- data.frame(gen_reg_hae)

gen_reg_nonhaecol <- data.frame(gender=as.factor(nonhae_data$gender), region=as.factor(nonhae_data$region))
gen_reg_nonhae <- model.matrix(~gender + region, gen_reg_nonhaecol, contrasts.arg = lapply(gen_reg_nonhaecol, contrasts, contrasts=F))[, -1]
gen_reg_nonhae <- data.frame(gen_reg_nonhae)


#combine to have the total model data
hae_data2 <- hae_data %>% 
  select(-c(patient_id, region, gender)) %>%
  bind_cols(gen_reg_hae) %>%
  rename(response = hae)

nonhae_data2 <- nonhae_data %>% 
  select(-c(hae_patient, region, gender)) %>%
  bind_cols(gen_reg_nonhae) %>%
  rename(response = hae)

#save the model data for the following steps
write.csv(hae_data2, paste(inpath, '/hae_model_data.csv', sep=''), quote = T, row.names = F)
write.csv(nonhae_data2, paste(inpath, '/nonhae_model_data.csv', sep=''), quote = T, row.names = F)

#reading in the model data
hae_model_data <- read.csv(paste(inpath, '/hae_model_data.csv', sep=''), header = T)
nonhae_model_data <- read.csv(paste(inpath, '/nonhae_model_data.csv', sep=''), header = T)

