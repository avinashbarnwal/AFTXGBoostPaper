library(survival)
library(penaltyLearning)
library(rjson)
source("utils.R")
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

get.pca.transform = function(pca,data){
    pred.pr         = predict(pca,newdata=data)
    return(pred.pr)
}


sr.pca=function(train,test,y.trn,y.tst){
    train.pr     = prcomp(train, center = TRUE, scale = TRUE)
    nPr          = length(train.pr$sdev)
    accuracy     = numeric(nPr)
    for(j in 1:nPr){
        train.sub  = train.pr$x[,c(1:j)]
        sr.pca.fit = sr.fit(train.sub,y.trn)
        if(class(sr.fit) != "try-error"){
            pred.pr         = get.pca.transform(train.pr,test)
            pred.pr.sub     = data.frame(pred.pr[,c(1:j)])
            colnames(pred.pr.sub) = colnames(train.pr$x)[1:j]
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
        y.trn  = y.lower[-testIndexes,] 
        y.tst  = y.upper[testIndexes,]
        res[[j]] = sr.pca(X.trn,X.tst,y.trn,y.tst)
    }
    result = do.call(cbind,res)
    result = cbind(result,apply(result,1,mean))
    pr_sel = which.max(result[,6])
    X.pr   = prcomp(X, center = TRUE, scale = TRUE)
    X.sub  = X.pr$x[,c(1:pr_sel)]
    sr.net.fit = sr.fit(X.sub,y.trn)
}



get.train.test<-function(folds,fold_id,inputs,labels){
    X            = inputs[folds!= test_fold_id]
    X.test       = inputs[folds == test_fold_id]
    y      = labels[folds != test_fold_id]
    y.test = labels[folds == test_fold_id]
    res           = list()
    res$X         = X
    res$X.val     = X.val
    res$y.lower      = y.lower
    res$y.lower.val  = y.lower.val
    res$y.upper      = y.upper
    res$y.upper.val  = y.upper.val
    return(res)
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
    accuracy = getaccuracy(pred,train_y[,"min.log.penalty"],train_y[,"max.log.penalty"])
    return(accuracy)
}

accuracy_datasets = list()
data_names      = list()
# data_names[[1]] = 'simulated.abs'
# data_names[[2]] = 'simulated.linear'
# data_names[[3]] = 'simulated.sin'
data_names[[1]] = 'simulated.model.1'
data_names[[2]] = 'simulated.model.2'
data_names[[3]] = 'simulated.model.3'

for(dataset in data_names){
    result = get.data(dataset)
    X = result$X
    y = result$y
    folds = result$folds

    result        = create_data(data_name)
    folds         = result$folds
    fold_iter     = result$fold_iter
    inputs        = result$inputs
    labels        = result$labels
    accuracy_fold = list()
    
    for(i in fold_iter){
        
        start_time          = Sys.time()
        res                 = getXY(i,folds,inputs,labels)
        X                   = res$X
        X.val               = res$X.val
        y.lower             = res$y.lower
        y.lower.val         = res$y.lower.val
        y.upper             = res$y.upper
        y.upper.val         = res$y.upper.val
        train.folds         = cut(seq(1,nrow(X)),breaks=5,labels=FALSE)
        res                 = list()
        
        for(j in 1:5){
            testIndexes = which(train.folds==j,arr.ind=TRUE)
            X.tst        = X[testIndexes, ]
            X.trn        = X[-testIndexes, ]
            y.lower.trn  = y.lower[-testIndexes,] 
            y.upper.trn  = y.upper[-testIndexes,]
            y.lower.tst  = y.lower[testIndexes,]
            y.upper.tst  = y.upper[testIndexes,]
            res[[j]]     = getBestPCACv(X.trn,X.tst,y.lower.trn,y.upper.trn,y.lower.tst,y.upper.tst)
        }
        
        result = do.call(cbind,res)
        result = cbind(result,apply(result,1,mean))
        pr_sel = which.max(result[,6])
        X.pr   = prcomp(X, center = TRUE, scale = TRUE)
        X.sub  = X.pr$x[,c(1:pr_sel)]
        data            = data.frame(X.sub,y.lower,y.upper)
        colnames(data)  = c(colnames(X.pr$x)[1:pr_sel],"y_lower","y_upper")
        my.surv         = Surv(y.lower,y.upper,type='interval2')
        formula         = as.formula(paste("my.surv ~", paste(colnames(X.pr$x)[1:pr_sel],collapse="+")))
        sr.fit          = survreg(formula,data=data,dist='lognormal', control = list(maxiter=500))
        pred.val.pr     = predict(X.pr,newdata=X.val)
        pred.val.pr.sub = data.frame(pred.val.pr[,c(1:pr_sel)])
        colnames(pred.val.pr.sub) = colnames(X.pr$x)[1:pr_sel]
        pred.y.val          = predict(sr.fit,pred.val.pr.sub)
        accuracy_fold[[i]]  = sum(mapply(getaccuracy,pred.y.val,y.lower.val,y.upper.val))/length(pred.y.val)
        pred_data           = data.frame(pred.y.val,y.lower.val,y.upper.val)
        colnames(pred_data) = c("predict","y.lower","y.upper")
        file_name           = paste("../../../../result/",data_name,"/survreg/",i,".csv",sep="")
        write.table(pred_data,file_name,sep=",",col.names=NA)
        end_time      = Sys.time()
        time_taken    = as.numeric(end_time-start_time)
        run_time[[i]] = time_taken
    }
    names(run_time)      = fold_iter
    names(accuracy_fold) = fold_iter
    json_accuracy = toJSON(accuracy_fold)
    file_name    = paste("../../../../result/",data_name,"/survreg/accuracy.JSON",sep="")
    write(json_accuracy, file=file_name)
    json_run_time = toJSON(run_time)
    file_name     = paste("../../../../result/",data_name,"/survreg/run_time.JSON",sep="")
    write(json_run_time, file=file_name)
    
}

jsonAccuracy         = toJSON(accuracy_datasets)
file_name            = paste('/Users/avinashbarnwal/Desktop/aftXgboostPaper/result/simulated/accuracy_survreg.JSON',sep="")
write(jsonAccuracy, file = file_name)