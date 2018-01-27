get_auprSummary <- function (data, lev = NULL, model = NULL) {
    if (length(levels(data$obs)) > 2) 
        stop(paste("Your outcome has", length(levels(data$obs)), 
                   "levels. The twoClassSummary() function isn't appropriate."))
    library("pROC")
    #browser()
    if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
        stop("levels of observed and predicted data do not match")
    rocObject <- try(pROC::roc(data$obs, data[, lev[1]]), silent = TRUE)
    rocAUC <- if (class(rocObject)[1] == "try-error") 
        NA
    else rocObject$auc
    
    library("PRROC")
    obs_ori <- ifelse(data$obs=='H', 1, 0)
    prObject <- try(PRROC::pr.curve(scores.class0 = data[, lev[1]],
                                    weights.class0 = obs_ori), silent = TRUE)
    prAUPR <- if (class(prObject)[1] == "try-error") 
        NA
    else prObject$auc.integral
    
    DR <- table(data[, 'obs'], data[, 'pred'])[2, 1]
    out <- c(DR, prAUPR, rocAUC, sensitivity(data[, "pred"], data[, "obs"],  lev[1])
             , specificity(data[, "pred"], data[, "obs"], lev[2])
             , posPredValue(data[, "pred"], data[, "obs"], lev[1], na.rm=T)
    )
    names(out) <- c('DR', "AUPR","ROC", "Sens", "Spec", "PPV")
    out
} 


#create own functions to pass cost and weights to svm
get_svmWtModel <- function(){
    com_name <- getModelInfo(model='svmLinear2', regex=FALSE)[[1]]
    wtsvmlinear <- com_name
    
    #parameter
    prm <- data.frame(parameter = c('cost', 'weights'),
                      class = rep('numeric',2),
                      label = c('cost', 'weights'))
    
    wtsvmlinear$parameters <- prm
    
    #train the model
    wtSVMfit <- function(x, y, wts, param, lev, last, weights, classProbs, ...){
        
        if(param$weights !=1){
            wts <- c(param$weights,1)
            names(wts)<-levels(y)
        }
        else wts <- NULL
        
        svm(x = as.matrix(x), y = y,
            kernel = 'linear',
            cost = param$cost, 
            class.weights = wts, 
            probability=classProbs, ...)
    }
    
    wtsvmlinear$fit <- wtSVMfit
    return(wtsvmlinear)
}


data(iris)
TrainData <- iris[,1:4]
#TrainClasses <- iris[,5]
TrainClasses <- as.factor(c(rep('H', 50), rep('NH', 100)))

ctrl <- trainControl(method='none'
                     #, number=k.folds, 
                     ,classProbs=T 
                     ,summaryFunction = get_auprSummary
                     #, returnResamp='all'
                     )

grid <- grid[1,]
start <- proc.time()
svmFit_lin <- train(TrainData, TrainClasses, method=wtsvmlinear, 
                    tuneGrid=grid,
                    trControl=ctrl,
                    metric='ROC',
                    maximize=T,
                    preProc=c('center', 'scale'))
#end <- proc.time()
#timeUsed <- (end-start)[3]/60
#cat(date(), '\n')

svm(TrainData, TrainClasses, cost=0.001, weights=)
