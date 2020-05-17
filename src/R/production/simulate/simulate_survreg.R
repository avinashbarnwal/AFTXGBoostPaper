library(survival)
library(penaltyLearning)
library(rjson)
source("utils.R")

data_names      = list()
data_names[[1]] = 'simulated.abs'
data_names[[2]] = 'simulated.linear'
data_names[[3]] = 'simulated.sin'
path            = '/Users/avinashbarnwal/Desktop/aftXgboostPaper/data/simulate/'

set.seed(1)

sr.fit = function(X,y){
    data            = data.frame(X,y)
    my.surv         = Surv(y[,"min.log.penalty"],y[,"max.log.penalty"],type='interval2')
    formula         = as.formula(paste("my.surv ~", paste(colnames(X),collapse="+")))
    fit             = try(survreg(formula,data=data,dist='gaussian', control = list(maxiter=1000)))
    return(fit)
}

sr.predict = function(clf,X){
    pred = predict(clf, X)
    return(pred)
}

main = function(X,y){
    X = set_X_type(X)
    y = set_y_type(y,y_type='log')
    data_split = get_train_test_split(X,y,test_frac=0.5)
    train_X  = data_split[['train_x']]
    test_X   = data.frame(data_split[['test_x']])
    train_y  = data_split[['train_y']]
    test_y   = data_split[['test_y']]
    clf      = sr.fit(train_X,train_y)
    pred     = sr.predict(clf,test_X)
    accuracy = get.accuracy(pred,train_y)
    return(accuracy)
}

accuracy_datasets = list()
for(data_folder in data_names){
    X_path = paste(path,data_folder,'/features.csv',sep="")
    y_path = paste(path,data_folder,'/targets.csv',sep="")
    folds_path = paste(path,data_folder,'/folds.csv',sep="")
    X        = read.table(X_path,sep=",",header=T,stringsAsFactors = F)
    y        = read.table(y_path,sep=",",header=T,stringsAsFactors = F)
    folds    = read.table(folds_path,sep=",",header=T,stringsAsFactors = F)
    accuracy = main(X,y)
    accuracy_datasets[[data_folder]] = accuracy
}

jsonAccuracy         = toJSON(accuracy_datasets)
file_name            = paste('/Users/avinashbarnwal/Desktop/aftXgboostPaper/result/simulated/accuracy_survreg.JSON',sep="")
write(jsonAccuracy, file = file_name)