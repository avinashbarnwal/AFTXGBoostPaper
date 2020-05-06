library(survival)
library(penaltyLearning)
library(rjson)


set.seed(1)

set_X_type = function(X){
    return(as.matrix(X))    
}

set_y_type = function(y,y_type='antilog'){
    y = as.matrix(y)
    if(y_type=='antilog'){
        y[,"min.log.penalty"] = unlist(lapply(y[,"min.log.penalty"],exp))
        y[,"max.log.penalty"] = unlist(lapply(y[,"max.log.penalty"],exp))
        return(y)
    }
    else if(y_type=='log'){
        return(y)
    }
}

getaccuracy=function(pred,y_lower,y_higher){
    res = (pred>=y_lower & pred<=y_higher)
    accurcy = sum(res)/length(res) 
    return(accurcy)
}




get_train_test_split =function(X,y,test_frac=0.5){

    n = dim(X)[1]
    n.sel  = as.integer(n*test_frac)

    select = sample(n, n.sel)
    train_x = X[select,]
    test_x = X[-select,]

    #sub-setting the y data
    train_y = y[select,]
    test_y  = y[-select,]
    result = list()

    result[['train_x']] = train_x
    result[['test_x']] = test_x
    result[['train_y']] = train_y
    result[['test_y']] = test_y
    return(result)

}
