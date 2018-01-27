#Rading in and tidy the data
#created on 11/20/2015
#developed by Hui JIn

#some constant
inpath <- 'F:/Hui/Shire_project/02_data'
outpath <- 'F:/Hui/Shire_project/04_result/LR_LASSO'
iterations <- 20
kfold <- 5
sens_levl <- 0.3

#reading in the data
load(paste(inpath, '/dat_hae_1111_rf.RData', sep=''))
load(paste(inpath, '/dat_nonhae_1111_rf.RData', sep=''))
#load(paste(inpath, '/trainIndex.RData', sep=''))

hae_data <- dat_hae_1111_rf
nonhae_data <- dat_nonhae_1111_rf
#inTrain <- trainIndex

#1. tidy the data
names(hae_data) <- tolower(names(hae_data))
names(nonhae_data) <- tolower(names(nonhae_data))

#select training and test data
set.seed(20)
inTrain <- sample(1:dim(hae_data)[1], size= dim(hae_data)[1]*0.8)

hae_tr <- hae_data[inTrain, ]
hae_ts <- hae_data[-inTrain, ]

pat_tr <- data.frame(hae_patient=hae_tr$patient_id)
pat_tr$hae_patient <- as.character(pat_tr$hae_patient)
pat_ts <- data.frame(hae_patient=hae_ts$patient_id)
pat_ts$hae_patient <- as.character(pat_ts$hae_patient)

nonhae_tr <- inner_join(nonhae_data, pat_tr, by='hae_patient')
nonhae_ts <- inner_join(nonhae_data, pat_ts, by='hae_patient')

#add the iteration number for non-hae data
set.seed(10)
iters <- createFolds(nonhae_tr$hae_patient, iterations)

hae_tr_model_data <- hae_tr %>% 
  select(-c(patient_id)) %>%
  rename(response = hae)

nonhae_tr_model_data <- nonhae_tr %>%  
  select(-c(hae_patient)) %>%
  rename(response = hae)

hae_ts_model_data <- hae_ts %>% 
  select(-c(patient_id)) %>%
  rename(response = hae)

nonhae_ts_model_data <- nonhae_ts %>% 
  select(-c(hae_patient)) %>%
  rename(response = hae)

#test set, the model would apply on the whole test set
ts_set <- rbind(hae_ts_model_data, nonhae_ts_model_data)
ts_x <- model.matrix(response~., data=ts_set)[,-1]
ts_y <- ts_set$response

patid_hae_tr <- data.frame(hae_patient=hae_tr$patient_id)
patid_nonhae_tr <- data.frame(hae_patient=nonhae_tr$hae_patient)

save(hae_tr_model_data, nonhae_tr_model_data, patid_hae_tr, patid_nonhae_tr, ts_x, ts_y,iters, file=paste(inpath, '/model_data.RData', sep='' ) )








