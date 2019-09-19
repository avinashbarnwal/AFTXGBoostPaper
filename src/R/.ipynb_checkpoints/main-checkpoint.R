library("tbm")
library(mboost)
library(survival)
library(penaltyLearning)


data_import <-function(dataname){
  
  filename = paste('https://raw.githubusercontent.com/avinashbarnwal/GSOC-2019/master/AFT/test/data/neuroblastoma-data-master/data/',dataname,'/',sep="")
  inputFileName = paste(filename,'inputs.csv',sep="")
  labelFileName = paste(filename,'outputs.csv',sep="")
  foldsFileName = paste(filename,'cv/equal_labels/folds.csv',sep="")
  inputs = read.table(inputFileName,sep=",",header=T,stringsAsFactors = F)
  labels = read.table(labelFileName,sep=",",header=T,stringsAsFactors = F)
  folds  = read.table(foldsFileName,sep=",",header=T,stringsAsFactors = F) 
  
  res        = list()
  res$inputs = inputs
  res$labels = labels
  res$folds  = folds
  
  return(res)
}

data_massage <- function(inputs,labels){
  
  naColumns = colnames(inputs)[colSums(is.na(inputs))>0]
  inputs    = inputs[ , !(names(inputs) %in% naColumns)]
  labels$min.log.lambda = lapply(labels$min.log.lambda,exp)
  labels$max.log.lambda = lapply(labels$max.log.lambda,exp)
  res = list()
  res$inputs = inputs
  res$labels = labels
  return(res)
}

getXY<-function(foldNo,folds,inputs,labels){
  
  test_id      = folds[folds$fold==foldNo,'sequenceID']
  train_id     = folds[folds$fold!=foldNo,'sequenceID']
  X            = inputs[inputs$sequenceID %in% train_id,]
  X            = X[,-which(names(X) %in% c("sequenceID"))]
  X            = as.matrix(X)
  X_val        = inputs[inputs$sequenceID %in% test_id,]
  X_val        = X_val[,-which(names(X_val) %in% c("sequenceID"))]
  X_val        = as.matrix(X_val)
  
  y_label       = labels[labels$sequenceID %in% train_id,]
  y_label_test  = labels[labels$sequenceID %in% test_id,]
  y_lower       = as.matrix(y_label$min.log.lambda)
  y_upper       = as.matrix(y_label$max.log.lambda)
  y_lower_val   = as.matrix(y_label_test$min.log.lambda)
  y_upper_val   = as.matrix(y_label_test$max.log.lambda)
  
  res   = list()
  res$X = X
  res$X_val    = X_val
  
  res$y_lower      = y_lower
  res$y_lower_val  = y_lower_val
  
  res$y_upper     = y_upper
  res$y_upper_val = y_upper_val
  
  return(res)
}

getParam <- function(sigma,distribution,learning_rate){
  eval_metric = paste("aft-nloglik@",distribution,",",sigma,sep="") 
  param       = list(learning_rate=learning_rate, aft_noise_distribution=distribution, 
                     nthread = 4, verbosity=0, aft_sigma= sigma,
                     eval_metric  = eval_metric,
                     objective  = "aft:survival")
  return(param)
}


trainModel <- function(foldNo,X,X_val,y_lower,y_lower_val,y_upper,y_upper_val,param,num_round){
  
  dtrain = xgb.DMatrix(X)
  setinfo(dtrain,'label_lower_bound', y_lower)
  setinfo(dtrain,'label_upper_bound', y_upper)
  
  dtest = xgb.DMatrix(X_val)
  setinfo(dtest,'label_lower_bound', y_lower_val)
  setinfo(dtest,'label_upper_bound', y_upper_val)
  
  watchlist <- list(eval = dtest, train = dtrain)
  bst       <- xgb.train(param, dtrain, num_round, watchlist,verbose = 0)
  
  return(bst)
}

createPlot <- function(dataname,data,distribution){
  
  title = paste("Data=",dataname,"   Distribution=",distribution,sep="")
  p     = ggplot(data=data,environment = environment()) +
    geom_line(aes(x=iter,y=error,colour=type),
              data=data,size=1)  +
    ylab("Error")+xlab("Number of Iteration") + ggtitle(title) + facet_grid(fold~sigma ,scales="free",labeller=label_both)
  print(p)
}

createData <- function(bst,Fold,distribution,sigma){
  colnames(bst$evaluation_log) = c('iter','eval','train')
  df_eval           = data.frame(bst$evaluation_log$iter,bst$evaluation_log$eval)
  df_eval$paramter  = rep('eval',nrow(df_eval))
  df_eval$sigma     = rep(sigma,nrow(df_eval))
  df_eval$distribution  = rep(distribution,nrow(df_eval))
  df_eval$fold       = rep(Fold,nrow(df_eval))
  
  colnames(df_eval) = c('iter','error','type','sigma','distribution','fold')
  df_train = data.frame(bst$evaluation_log$iter,bst$evaluation_log$train)
  df_train$paramter = rep('train',nrow(df_train))
  df_train$sigma     = rep(sigma,nrow(df_train))
  df_train$distribution  = rep(distribution,nrow(df_train))
  df_train$fold       = rep(Fold,nrow(df_train))
  
  colnames(df_train) = c('iter','error','type','sigma','distribution','fold')
  df = rbind(df_eval,df_train)
  return(df)
}

createDataPlot <- function(dataName,distribution,folds_iter,sigma_range,folds,inputs,labels,result){
  
  for(sigma in sigma_range) {
    for(i in folds_iter){
      res           = getXY(i,folds,inputs,labels)
      X             = res$X
      X_val         = res$X_val
      y_lower       = res$y_lower
      y_lower_val   = res$y_lower_val
      y_upper       = res$y_upper
      y_upper_val   = res$y_upper_val
      param = getParam(sigma,distribution,learning_rate)
      bst   = trainModel(i,X,X_val,y_lower,y_lower_val,y_upper,y_upper_val,param,num_round)
      dataIter = createData(bst,i,distribution,sigma)
      result   = rbind(result,dataIter)
    }
  }
  createPlot(dataName,result,distribution)
}

dataNameRange = c('ATAC_JV_adipose','CTCF_TDH_ENCODE','H3K27ac-H3K4me3_TDHAM_BP','H3K27ac_TDH_some','H3K36me3_AM_immune')
# Set Parameters
sigma_range         = c(1,2,5,10,100)
distribution_range  = c('normal','logistic','extreme')
learning_rate       = 0.1
num_round           = 200

for(dataName in dataNameRange){
  res      = data_import(dataName)
  inputs   = res$inputs
  labels   = res$labels
  folds    = res$folds
  result         = data.frame()
  resDataMassage = data_massage(inputs,labels)
  inputs         = resDataMassage$inputs
  labels         = resDataMassage$labels
  folds_iter     = unique(folds$fold)
  for(distribution in distribution_range){
    createDataPlot(dataName,distribution,folds_iter,sigma_range,folds,inputs,labels,result)
  }
}

