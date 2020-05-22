library(survival)
library(penaltyLearning)
library(rjson)

accuracy_datasets = list()
data_names      = list()
#data_names[[1]] = 'simulated.abs'
#data_names[[2]] = 'simulated.linear'
#data_names[[3]] = 'simulated.sin'
data_names[[1]] = 'simulated.model.1'
data_names[[2]] = 'simulated.model.2'
data_names[[3]] = 'simulated.model.3'

path            = '/Users/avinashbarnwal/Desktop/aftXgboostPaper/data/simulate/'

get.accuracy=function(pred,y){
  res = (pred>=y[,"min.log.penalty"] & pred<=y[,"max.log.penalty"])
  accurcy = sum(res)/length(res) 
  return(accurcy)
}

get.data = function(data_folder){
  X_path = paste(path,data_folder,'/features.csv',sep="")
  y_path = paste(path,data_folder,'/targets.csv',sep="")
  folds_path = paste(path,data_folder,'/folds.csv',sep="")
  X        = read.table(X_path,sep=",",header=T,stringsAsFactors = F)
  y        = read.table(y_path,sep=",",header=T,stringsAsFactors = F)
  folds    = read.table(folds_path,sep=",",header=T,stringsAsFactors = F)
  result   = list()
  result$X = X
  result$y = y
  result$folds = folds
  return(result)
}

get.train.test<-function(folds,i,X,y){
  X.train      = X[folds!= i,]
  X.test       = X[folds == i,]
  y.train      = y[folds != i,]
  y.test       = y[folds == i,]
  res          = list()
  res$X.train  = X.train
  res$X.test   = X.test
  res$y.train  = y.train
  res$y.test   = y.test
  return(res)
}

for(dataset in data_names){
  print(dataset)
  result = get.data(dataset)
  X = result$X
  y = result$y
  folds         = result$folds
  folds         = folds$fold
  fold_iter     = unique(folds)
  accuracy_fold = list()
  run_time = list()
  for(i in fold_iter){
    start_time          = Sys.time()
    res                 = get.train.test(folds,i,X,y)
    X.train             = res$X.train
    X.test              = res$X.test
    y.train             = res$y.train
    y.test              = res$y.test
    if(class(X.train)=='data.frame'){
      X.train = as.matrix(X.train)
    }
    if(class(y.train)=='data.frame'){
      y.train = as.matrix(y.train)
    }
    if(class(X.test)=='data.frame'){
      X.test = as.matrix(X.test)
    }
    if(class(y.test)=='data.frame'){
      y.test = as.matrix(y.test)
    }
    fit                 = IntervalRegressionCV(X.train, y.train)  
    y.pred.test         = predict(fit, X.test)
    accuracy_fold[[i]]  = get.accuracy(y.pred.test,y.test)
    end_time      = Sys.time()
    time_taken    = as.numeric(end_time-start_time)
    run_time[[i]] = time_taken
  }
  names(accuracy_fold) = fold_iter
  names(run_time) = fold_iter
  jsonAccuracy  = toJSON(accuracy_fold)
  jsonRunTime   = toJSON(run_time)
  file_name            = paste('/Users/avinashbarnwal/Desktop/aftXgboostPaper/result/simulated/',dataset,'/accuracy_intervalCV.json',sep="")
  write(jsonAccuracy, file = file_name)
  file_name            = paste('/Users/avinashbarnwal/Desktop/aftXgboostPaper/result/simulated/',dataset,'/runtime_intervalCV.json',sep="")
  write(jsonRunTime, file = file_name)
}