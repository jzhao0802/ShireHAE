setwd('D:\\Shire_project\\04_result\\svm\\iter1\\cost(0.001,0.01,1,10,100)_wt(0.01,100)')
source("D:\\Shire_project\\03_code\\Jie\\svm\\svm_subFunc_Dec03.R")
library(ROCR)
load('pred_wt(0.01)cost(0.001).RData')
pred <- pred_model$pred_ts
load('D:\\Shire_project\\04_result\\svm\\iter1\\resp_ts_fold1.RData')
resp <- ifelse(cv_resp_ts=='H', 1, 0)
ms5 <- createCurve(resp = resp, pred=pred, recall_tar = 0.3)

line1 <- c(wt=0.01, cost=0.01, ms=ms1)
line2 <- c(wt=0.01, cost=1, ms=ms2)
line3 <- c(wt=1, cost=0.01, ms=ms3)
line4 <- c(wt=0.001, cost=1, ms=ms4)
line5 <- c(wt=0.01, cost=0.001, ms=ms5)
line6 <- c(wt=0.01, cost=10, ms=ms6)
line7 <- c(wt=100, cost=0.001, ms=ms7)
line8 <- c(wt=100, cost=0.01, ms=ms8)

temp <- rbind(line1,line2, line3, line4, line5, line6, line7, line8)
temp <- temp[order(temp[, 1], temp[, 2]),]
write.csv(temp, 'D:\\Shire_project\\04_result\\svm\\iter1\\msOnFold1.csv', row.names = F)
