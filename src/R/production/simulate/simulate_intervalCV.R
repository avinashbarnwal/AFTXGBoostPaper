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

i.CV.fit = function(X,y){
    clf = IntervalRegressionCV(X, y)
    return(clf)
}

i.CV.predict = function(clf,X){
    pred = predict(clf, X)
    return(pred)
}

main = function(X,y){

    X = set_X_type(X)
    y = set_y_type(y,y_type='log')

    data_split = get_train_test_split(X,y,test_frac=0.5)
    train_X = data_split[['train_x']]
    test_X  = data_split[['test_x']]
    train_y  = data_split[['train_y']]
    test_y  = data_split[['test_y']]
    clf = i.CV.fit(train_X,train_y)
    pred = i.CV.predict(clf,test_X)
    accuracy = getaccuracy(pred,train_y[,"min.log.penalty"],train_y[,"max.log.penalty"])
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
file_name            = paste('/Users/avinashbarnwal/Desktop/aftXgboostPaper/result/simulated/accuracy_intervalCV.JSON',sep="")
write(jsonAccuracy, file = file_name)