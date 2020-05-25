
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

sr.predict = function(clf,X){
  pred = predict(clf, X)
  return(pred)
}

sr.fit = function(X,y){
  data            = data.frame(X,y)
  my.surv         = Surv(y[,"min.log.penalty"],y[,"max.log.penalty"],type='interval2')
  formula         = as.formula(paste("my.surv ~", paste(colnames(X),collapse="+")))
  fit             = try(survreg(formula,data=data,dist='gaussian', control = list(maxiter=1000)))
  return(fit)
}
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
y.test        = y[folds == i,]
res           = list()
res$X.train   = X.train
res$X.test    = X.test
res$y.train   = y.train
res$y.test    = y.test
return(res)
}
get.pca.transform = function(pca,data){
pred.pr         = predict(pca,newdata=data)
return(pred.pr)
}
sr.pca=function(train,test,y.trn,y.tst){
  train.pr     = prcomp(train, center = TRUE, scale = TRUE)
  nPr          = length(train.pr$sdev)
  accuracy     = numeric(nPr)
  for(j in 1:nPr){
    train.sub  = as.matrix(train.pr$x[,c(1:j)])
    colnames(train.sub) = paste("PC",c(1:j),sep="")
    sr.pca.fit = sr.fit(train.sub,y.trn)
    if(class(sr.pca.fit) != "try-error"){
      pred.pr         = get.pca.transform(train.pr,test)
      pred.pr.sub     = data.frame(pred.pr[,c(1:j)])
      colnames(pred.pr.sub) = paste("PC",c(1:j),sep="")
      pred.y          = predict(sr.pca.fit,pred.pr.sub)
      accuracy[j]     = get.accuracy(pred.y,y.tst)
    }
  }
  return(accuracy)
}
sr.cv = function(X,y){
  train.folds         = cut(seq(1,nrow(X)),breaks=5,labels=FALSE)
  res                 = list()
  for(j in 1:5){
    testIndexes = which(train.folds==j,arr.ind=TRUE)
    X.tst  = X[testIndexes, ]
    X.trn  = X[-testIndexes, ]
    y.trn  = y[-testIndexes,]
    y.tst  = y[testIndexes,]
    res[[j]] = sr.pca(X.trn,X.tst,y.trn,y.tst)
  }
  result = do.call(cbind,res)
  result = cbind(result,apply(result,1,mean))
  pr_sel = which.max(result[,6])
  X.pr   = prcomp(X, center = TRUE, scale = TRUE)
  X.sub  = X.pr$x[,1:pr_sel]
  if(class(X.sub)=='matrix'|class(X.sub)=='numeric'){
    X.sub = data.frame(X.sub)
  }
  colnames(X.sub) = paste("PC",c(1:pr_sel),sep="")
  print(pr_sel)
  sr.cv.fit = sr.fit(X.sub,y)
  result = list()
  result$fit = sr.cv.fit
  result$pr_sel = pr_sel
  result$pr = X.pr
  return(result)
}
for(dataset in data_names){
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
  X.test               = res$X.test
  y.train             = res$y.train
  y.test              = res$y.test
  result = sr.cv(X.train,y.train)
  pr = result$pr
  sr.cv.fit = result$fit
  pr_sel = result$pr_sel
  X.test.pr = get.pca.transform(pr,X.test)
  X.test.pr = X.test.pr[,1:pr_sel]
  if(class(X.test.pr)=='matrix'|class(X.test.pr)=='numeric'){
    X.test.pr = data.frame(X.test.pr)
  }
  colnames(X.test.pr) = paste("PC",c(1:pr_sel),sep="")
  y.pred.test = sr.predict(sr.cv.fit,X.test.pr)
  accuracy_fold[[i]] = get.accuracy(y.pred.test,y.test)
  end_time      = Sys.time()
  time_taken    = as.numeric(end_time-start_time)
  run_time[[i]] = time_taken
  }
  names(accuracy_fold) = fold_iter
  names(run_time) = fold_iter
  jsonAccuracy  = toJSON(accuracy_fold)
  jsonRunTime   = toJSON(run_time)
  file_name            = paste('/Users/avinashbarnwal/Desktop/aftXgboostPaper/result/simulated/',dataset,'/accuracy_survreg_tuned.json',sep="")
  write(jsonAccuracy, file = file_name)
  file_name            = paste('/Users/avinashbarnwal/Desktop/aftXgboostPaper/result/simulated/',dataset,'/runtime_survreg_tuned.json',sep="")
  write(jsonRunTime, file = file_name)
}