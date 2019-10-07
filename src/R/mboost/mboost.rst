
.. code:: r

    library(mboost)
    library(survival)
    library(penaltyLearning)
    library(Hmisc)
    library(caret)
    library(tbm)
    library(tram)

.. code:: r

    data_import =function(dataname){
      filename = paste('https://raw.githubusercontent.com/avinashbarnwal/GSOC-2019/master/AFT/test/data/neuroblastoma-data-master/data/',dataname,'/',sep="")
      inputFileName = paste(filename,'inputs.csv',sep="")
      labelFileName = paste(filename,'outputs.csv',sep="")
      foldsFileName = paste(filename,'cv/equal_labels/folds.csv',sep="")
      inputs        = read.table(inputFileName,sep=",",header=T,stringsAsFactors = F,row.names=1)
      labels        = read.table(labelFileName,sep=",",header=T,stringsAsFactors = F,row.names=1)
      folds         = read.table(foldsFileName,sep=",",header=T,stringsAsFactors = F,row.names=1)
      res           = list()
      res$inputs    = inputs
      res$labels    = labels
      res$folds     = folds
      return(res)
    }

.. code:: r

    data_massage = function(inputs,labels){
        rownamesInput = rownames(inputs)
        inputs        = do.call(data.frame,lapply(inputs, function(x) replace(x, is.infinite(x),NA)))
        naColumns     = colnames(inputs)[colSums(is.na(inputs))>0]
        noVarCol      = getNonVarCols(inputs)
        removeCols    = c(naColumns,noVarCol)
        inputs        = inputs[ , !(colnames(inputs) %in% removeCols)]
        rownames(inputs) = rownamesInput
        labels$min.log.lambda = unlist(lapply(labels$min.log.lambda,exp))
        labels$max.log.lambda = unlist(lapply(labels$max.log.lambda,exp))
        res        = list()
        res$inputs = inputs
        res$labels = labels
        return(res)
    }

.. code:: r

    getXY<-function(foldNo,folds,inputs,labels){
        test.id       = rownames(subset(folds,fold==foldNo))
        train.id      = rownames(subset(folds,fold!=foldNo))
        X             = subset(inputs,rownames(inputs) %in% train.id)
        X             = as.matrix(X)
        X.val         = subset(inputs,rownames(inputs) %in% test.id)
        X.val         = as.matrix(X.val)
        y.label       = subset(labels,rownames(labels) %in% train.id)
        y.label.test  = subset(labels,rownames(labels) %in% test.id)
        y.lower       = as.matrix(y.label$min.log.lambda)
        y.upper       = as.matrix(y.label$max.log.lambda)
        y.lower.val   = as.matrix(y.label.test$min.log.lambda)
        y.upper.val   = as.matrix(y.label.test$max.log.lambda)
        res           = list()
        res$X         = X
        res$X.val     = X.val
        res$y.lower      = y.lower
        res$y.lower.val  = y.lower.val
        res$y.upper      = y.upper
        res$y.upper.val  = y.upper.val
        return(res)
    }

.. code:: r

    getNonVarCols<-function(data){
        var_columns    = apply(inputs,2,var)
        resCol         = names(var_columns[var_columns==0.0])
        return(resCol)
    }

.. code:: r

    # Set Parameters
    dataNameRange       = c('ATAC_JV_adipose','CTCF_TDH_ENCODE','H3K27ac-H3K4me3_TDHAM_BP','H3K27ac_TDH_some','H3K36me3_AM_immune')
    sigma_range         = c(1,2,5,10,100)
    distribution_range  = c('normal','logistic','extreme')
    learning_rate       = 0.1
    num_round           = 200

.. code:: r

    res                 = data_import(dataNameRange[1])
    inputs              = res$inputs
    labels              = res$labels
    folds               = res$folds
    resDataMassage      = data_massage(inputs,labels)
    inputs              = resDataMassage$inputs
    labels              = resDataMassage$labels
    fold_iter           = unique(folds$fold)
    accuracy_fold       = numeric(length(fold_iter))
    coef_model          = list()

.. code:: r

    getaccuracy=function(pred,y_lower,y_higher){
        res = (pred>=y_lower & pred<=y_higher)
        return(res)
    }

.. code:: r

    i = 1
    res                 = getXY(fold_iter[i],folds,inputs,labels)
    X                   = res$X
    X.val               = res$X.val
    y.lower             = res$y.lower
    y.lower.val         = res$y.lower.val
    y.upper             = res$y.upper
    y.upper.val         = res$y.upper.val
    train.folds         = cut(seq(1,nrow(X)),breaks=5,labels=FALSE)
    res                 = list()
    my.surv             = Surv(y.lower,y.upper,type='interval2')
    formula             = as.formula(paste("my.surv ~", paste(colnames(X),collapse="+")))
    trn.data            = data.frame(X,y.lower,y.upper)
    glm = glmboost(formula,data=trn.data,family=Lognormal(),control=boost_control(mstop=200,nu=0.001,trace=TRUE))
    plot(glm, off2int = TRUE)
    plot(glm, ylim = range(coef(glm)))
    coef_model[i]       = coef(glm)
    tst.data            = data.frame(X.val)
    pred.y.val          = predict(glm,tst.data,type="response")
    accuracy_fold[1]    = sum(mapply(getaccuracy,pred.y.val,y.lower.val,y.upper.val))/length(pred.y.val)


.. parsed-literal::

    Warning message in optimize(risk, interval = c(0, max(log(y[, 1]), na.rm = TRUE)), :
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(risk, interval = c(0, max(log(y[, 1]), na.rm = TRUE)), :
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(risk, interval = c(0, max(log(y[, 1]), na.rm = TRUE)), :
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(risk, interval = c(0, max(log(y[, 1]), na.rm = TRUE)), :
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(risk, interval = c(0, max(log(y[, 1]), na.rm = TRUE)), :
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(risk, interval = c(0, max(log(y[, 1]), na.rm = TRUE)), :
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(risk, interval = c(0, max(log(y[, 1]), na.rm = TRUE)), :
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(risk, interval = c(0, max(log(y[, 1]), na.rm = TRUE)), :
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(risk, interval = c(0, max(log(y[, 1]), na.rm = TRUE)), :
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(risk, interval = c(0, max(log(y[, 1]), na.rm = TRUE)), :
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(risk, interval = c(0, max(log(y[, 1]), na.rm = TRUE)), :
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    [   1] 

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

     -- risk: 8757635 


.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    [  41] 

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

     -- risk: 27689168 


.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    [  81] 

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

     -- risk: 42906038 


.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    [ 121] 

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

     -- risk: 48289442 


.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    [ 161] 

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    .

.. parsed-literal::

    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”
    Warning message in optimize(riskS, interval = nuirange, y = y, fit = f, w = w):
    “NA/Inf replaced by maximum positive value”


.. parsed-literal::

    
    Final risk: 51503808 



.. image:: output_8_400.png
   :width: 420px
   :height: 420px


.. parsed-literal::

    Warning message in coef_model[i] = coef(glm):
    “number of items to replace is not a multiple of replacement length”



.. image:: output_8_402.png
   :width: 420px
   :height: 420px


.. code:: r

    ### With STM - Mboost (Additive Smooth Models)
    trn.data$y.lower = trn.data$y.upper = NULL
    trn.data$my.surv = my.surv
    m_mlt = Survreg(formula, data = trn.data, dist = "lognormal")
    bm = stmboost(m_mlt, formula = formula, data = trn.data,control = boost_control(mstop=200,nu=0.001,trace=TRUE),method = quote(mboost::mboost))
    ### look at in-sample performance
    logLik(m_mlt)
    plot(risk(bm)) ### this is the negative log-lik


.. parsed-literal::

    Warning message in c.basis(bresponse = response, bshifting = shifting):
    “more than one basis contains an intercept term”



.. parsed-literal::

    'log Lik.' -409.3729 (df=29)



.. image:: output_9_2.png
   :width: 420px
   :height: 420px


.. code:: r

    ### With STM - Mboost (Additive Smooth Models)
    trn.data$y.lower = trn.data$y.upper = NULL
    trn.data$my.surv = my.surv
    m_mlt            = Survreg(formula, data = trn.data, dist = "lognormal")
    bm               = stmboost(m_mlt, formula = formula, control = boost_control(mstop=10000,nu=0.001,trace=TRUE),data = trn.data,method = quote(mboost::blackboost))
    ### look at in-sample performance
    logLik(m_mlt)
    plot(risk(bm)) ### this is the negative log-lik


.. parsed-literal::

    [     1] ...................................... -- risk: 407.416 
    [    41] ...................................... -- risk: 406.2208 
    [    81] ...................................... -- risk: 405.0974 
    [   121] ...................................... -- risk: 404.0444 
    [   161] ...................................... -- risk: 403.0574 
    [   201] ...................................... -- risk: 402.2382 
    [   241] ...................................... -- risk: 401.5108 
    [   281] ...................................... -- risk: 400.8032 
    [   321] ...................................... -- risk: 399.0674 
    [   361] ...................................... -- risk: 398.8001 
    [   401] ...................................... -- risk: 398.5091 
    [   441] ...................................... -- risk: 397.5129 
    [   481] ...................................... -- risk: 396.5478 
    [   521] ...................................... -- risk: 395.6577 
    [   561] ...................................... -- risk: 394.8223 
    [   601] ...................................... -- risk: 394.0852 
    [   641] ...................................... -- risk: 393.4411 
    [   681] ...................................... -- risk: 392.8146 
    [   721] ...................................... -- risk: 391.1897 
    [   761] ...................................... -- risk: 390.4014 
    [   801] ...................................... -- risk: 389.6387 
    [   841] ...................................... -- risk: 388.9934 
    [   881] ...................................... -- risk: 388.3982 
    [   921] ...................................... -- risk: 387.8637 
    [   961] ...................................... -- risk: 387.3426 
    [ 1'001] ...................................... -- risk: 386.9043 
    [ 1'041] ...................................... -- risk: 386.5903 
    [ 1'081] ...................................... -- risk: 386.1742 
    [ 1'121] ...................................... -- risk: 385.7857 
    [ 1'161] ...................................... -- risk: 385.3588 
    [ 1'201] ...................................... -- risk: 384.2498 
    [ 1'241] ...................................... -- risk: 383.6051 
    [ 1'281] ...................................... -- risk: 383.0868 
    [ 1'321] ...................................... -- risk: 382.61 
    [ 1'361] ...................................... -- risk: 382.1491 
    [ 1'401] ...................................... -- risk: 381.8402 
    [ 1'441] ...................................... -- risk: 381.4961 
    [ 1'481] ...................................... -- risk: 381.1483 
    [ 1'521] ...................................... -- risk: 380.8039 
    [ 1'561] ...................................... -- risk: 380.4751 
    [ 1'601] ...................................... -- risk: 380.1691 
    [ 1'641] ...................................... -- risk: 379.9881 
    [ 1'681] ...................................... -- risk: 379.8382 
    [ 1'721] ...................................... -- risk: 379.6903 
    [ 1'761] ...................................... -- risk: 379.5248 
    [ 1'801] ...................................... -- risk: 379.3591 
    [ 1'841] ...................................... -- risk: 379.1363 
    [ 1'881] ...................................... -- risk: 378.9029 
    [ 1'921] ...................................... -- risk: 378.6796 
    [ 1'961] ...................................... -- risk: 378.4685 
    [ 2'001] ...................................... -- risk: 378.2672 
    [ 2'041] ...................................... -- risk: 378.0282 
    [ 2'081] ...................................... -- risk: 377.7965 
    [ 2'121] ...................................... -- risk: 377.5718 
    [ 2'161] ...................................... -- risk: 377.3538 
    [ 2'201] ...................................... -- risk: 377.1423 
    [ 2'241] ...................................... -- risk: 376.9393 
    [ 2'281] ...................................... -- risk: 376.7403 
    [ 2'321] ...................................... -- risk: 376.5152 
    [ 2'361] ...................................... -- risk: 376.2594 
    [ 2'401] ...................................... -- risk: 376.0136 
    [ 2'441] ...................................... -- risk: 375.7893 
    [ 2'481] ...................................... -- risk: 375.6384 
    [ 2'521] ...................................... -- risk: 375.4337 
    [ 2'561] ...................................... -- risk: 375.2261 
    [ 2'601] ...................................... -- risk: 375.0301 
    [ 2'641] ...................................... -- risk: 374.8419 
    [ 2'681] ...................................... -- risk: 374.6436 
    [ 2'721] ...................................... -- risk: 374.45 
    [ 2'761] ...................................... -- risk: 374.2943 
    [ 2'801] ...................................... -- risk: 373.9765 
    [ 2'841] ...................................... -- risk: 373.1543 
    [ 2'881] ...................................... -- risk: 372.3883 
    [ 2'921] ...................................... -- risk: 371.5911 
    [ 2'961] ...................................... -- risk: 370.4801 
    [ 3'001] ...................................... -- risk: 370.3554 
    [ 3'041] ...................................... -- risk: 370.0691 
    [ 3'081] ...................................... -- risk: 369.8842 
    [ 3'121] ...................................... -- risk: 369.6861 
    [ 3'161] ...................................... -- risk: 369.4916 
    [ 3'201] ...................................... -- risk: 369.2759 
    [ 3'241] ...................................... -- risk: 368.9374 
    [ 3'281] ...................................... -- risk: 368.5495 
    [ 3'321] ...................................... -- risk: 368.3002 
    [ 3'361] ...................................... -- risk: 368.1322 
    [ 3'401] ...................................... -- risk: 367.9802 
    [ 3'441] ...................................... -- risk: 367.8109 
    [ 3'481] ...................................... -- risk: 367.6174 
    [ 3'521] ...................................... -- risk: 367.4715 
    [ 3'561] ...................................... -- risk: 367.3247 
    [ 3'601] ...................................... -- risk: 367.1029 
    [ 3'641] ...................................... -- risk: 366.8556 
    [ 3'681] ...................................... -- risk: 366.6411 
    [ 3'721] ...................................... -- risk: 366.4202 
    [ 3'761] ...................................... -- risk: 366.2027 
    [ 3'801] ...................................... -- risk: 365.8381 
    [ 3'841] ...................................... -- risk: 365.1108 
    [ 3'881] ...................................... -- risk: 364.3501 
    [ 3'921] ...................................... -- risk: 363.6353 
    [ 3'961] ...................................... -- risk: 362.9676 
    [ 4'001] ...................................... -- risk: 362.3406 
    [ 4'041] ...................................... -- risk: 361.7709 
    [ 4'081] ...................................... -- risk: 361.1878 
    [ 4'121] ...................................... -- risk: 360.3122 
    [ 4'161] ...................................... -- risk: 359.9071 
    [ 4'201] ...................................... -- risk: 359.4882 
    [ 4'241] ...................................... -- risk: 359.0814 
    [ 4'281] ...................................... -- risk: 358.7032 
    [ 4'321] ...................................... -- risk: 358.3084 
    [ 4'361] ...................................... -- risk: 357.9478 
    [ 4'401] ...................................... -- risk: 357.5779 
    [ 4'441] ...................................... -- risk: 357.2165 
    [ 4'481] ...................................... -- risk: 356.8655 
    [ 4'521] ...................................... -- risk: 356.5156 
    [ 4'561] ...................................... -- risk: 356.174 
    [ 4'601] ...................................... -- risk: 355.124 
    [ 4'641] ...................................... -- risk: 354.6991 
    [ 4'681] ...................................... -- risk: 354.5257 
    [ 4'721] ...................................... -- risk: 354.413 
    [ 4'761] ...................................... -- risk: 354.2676 
    [ 4'801] ...................................... -- risk: 354.1039 
    [ 4'841] ...................................... -- risk: 353.9637 
    [ 4'881] ...................................... -- risk: 353.8987 
    [ 4'921] ...................................... -- risk: 353.8472 
    [ 4'961] ...................................... -- risk: 353.8331 
    [ 5'001] ...................................... -- risk: 353.4872 
    [ 5'041] ...................................... -- risk: 353.0402 
    [ 5'081] ...................................... -- risk: 352.5045 
    [ 5'121] ...................................... -- risk: 351.9707 
    [ 5'161] ...................................... -- risk: 351.566 
    [ 5'201] ...................................... -- risk: 351.0208 
    [ 5'241] ...................................... -- risk: 350.9583 
    [ 5'281] ...................................... -- risk: 350.9426 
    [ 5'321] ...................................... -- risk: 350.9263 
    [ 5'361] ...................................... -- risk: 350.9108 
    [ 5'401] ...................................... -- risk: 350.8945 
    [ 5'441] ...................................... -- risk: 350.8788 
    [ 5'481] ...................................... -- risk: 350.8621 
    [ 5'521] ...................................... -- risk: 350.8474 
    [ 5'561] ...................................... -- risk: 350.6994 
    [ 5'601] ...................................... -- risk: 350.5437 
    [ 5'641] ...................................... -- risk: 350.2935 
    [ 5'681] ...................................... -- risk: 350.047 
    [ 5'721] ...................................... -- risk: 349.8212 
    [ 5'761] ...................................... -- risk: 349.6068 
    [ 5'801] ...................................... -- risk: 349.4009 
    [ 5'841] ...................................... -- risk: 348.9505 
    [ 5'881] ...................................... -- risk: 348.1265 
    [ 5'921] ...................................... -- risk: 347.6687 
    [ 5'961] ...................................... -- risk: 347.2945 
    [ 6'001] ...................................... -- risk: 346.8959 
    [ 6'041] ...................................... -- risk: 346.501 
    [ 6'081] ...................................... -- risk: 346.126 
    [ 6'121] ...................................... -- risk: 345.7684 
    [ 6'161] ...................................... -- risk: 345.4288 
    [ 6'201] ...................................... -- risk: 345.0899 
    [ 6'241] ...................................... -- risk: 344.7544 
    [ 6'281] ...................................... -- risk: 344.4335 
    [ 6'321] ...................................... -- risk: 344.3261 
    [ 6'361] ...................................... -- risk: 344.3192 
    [ 6'401] ...................................... -- risk: 344.3128 
    [ 6'441] ...................................... -- risk: 344.3068 
    [ 6'481] ...................................... -- risk: 344.3012 
    [ 6'521] ...................................... -- risk: 344.2959 
    [ 6'561] ...................................... -- risk: 344.2875 
    [ 6'601] ...................................... -- risk: 344.2758 
    [ 6'641] ...................................... -- risk: 344.2643 
    [ 6'681] ...................................... -- risk: 344.2459 
    [ 6'721] ...................................... -- risk: 344.2173 
    [ 6'761] ...................................... -- risk: 344.1943 
    [ 6'801] ...................................... -- risk: 344.165 
    [ 6'841] ...................................... -- risk: 344.1201 
    [ 6'881] ...................................... -- risk: 344.0793 
    [ 6'921] ...................................... -- risk: 344.0406 
    [ 6'961] ...................................... -- risk: 344.0069 
    [ 7'001] ...................................... -- risk: 343.9708 
    [ 7'041] ...................................... -- risk: 343.9379 
    [ 7'081] ...................................... -- risk: 343.9026 
    [ 7'121] ...................................... -- risk: 343.8706 
    [ 7'161] ...................................... -- risk: 343.8389 
    [ 7'201] ...................................... -- risk: 343.8049 
    [ 7'241] ...................................... -- risk: 343.7739 
    [ 7'281] ...................................... -- risk: 343.7444 
    [ 7'321] ...................................... -- risk: 343.7043 
    [ 7'361] ...................................... -- risk: 343.6714 
    [ 7'401] ...................................... -- risk: 343.6334 
    [ 7'441] ...................................... -- risk: 343.58 
    [ 7'481] ...................................... -- risk: 343.5219 
    [ 7'521] ...................................... -- risk: 343.4487 
    [ 7'561] ...................................... -- risk: 343.3831 
    [ 7'601] ...................................... -- risk: 343.3212 
    [ 7'641] ...................................... -- risk: 343.2579 
    [ 7'681] ...................................... -- risk: 343.1913 
    [ 7'721] ...................................... -- risk: 343.1252 
    [ 7'761] ...................................... -- risk: 343.0621 
    [ 7'801] ...................................... -- risk: 343.005 
    [ 7'841] ...................................... -- risk: 342.9362 
    [ 7'881] ...................................... -- risk: 342.8734 
    [ 7'921] ...................................... -- risk: 342.811 
    [ 7'961] ...................................... -- risk: 342.7581 
    [ 8'001] ...................................... -- risk: 342.696 
    [ 8'041] ...................................... -- risk: 342.6474 
    [ 8'081] ...................................... -- risk: 342.5252 
    [ 8'121] ...................................... -- risk: 342.4105 
    [ 8'161] ...................................... -- risk: 342.2913 
    [ 8'201] ...................................... -- risk: 342.1811 
    [ 8'241] ...................................... -- risk: 342.0653 
    [ 8'281] ...................................... -- risk: 341.9631 
    [ 8'321] ...................................... -- risk: 341.8587 
    [ 8'361] ...................................... -- risk: 341.7763 
    [ 8'401] ...................................... -- risk: 341.6764 
    [ 8'441] ...................................... -- risk: 341.6023 
    [ 8'481] ...................................... -- risk: 341.5002 
    [ 8'521] ...................................... -- risk: 341.3292 
    [ 8'561] ...................................... -- risk: 341.1652 
    [ 8'601] ...................................... -- risk: 341.0068 
    [ 8'641] ...................................... -- risk: 340.8478 
    [ 8'681] ...................................... -- risk: 340.6968 
    [ 8'721] ...................................... -- risk: 340.5052 
    [ 8'761] ...................................... -- risk: 340.3747 
    [ 8'801] ...................................... -- risk: 340.3092 
    [ 8'841] ...................................... -- risk: 340.2566 
    [ 8'881] ...................................... -- risk: 340.2088 
    [ 8'921] ...................................... -- risk: 340.1562 
    [ 8'961] ...................................... -- risk: 340.1058 
    [ 9'001] ...................................... -- risk: 340.0544 
    [ 9'041] ...................................... -- risk: 340.0049 
    [ 9'081] ...................................... -- risk: 339.9544 
    [ 9'121] ...................................... -- risk: 339.8933 
    [ 9'161] ...................................... -- risk: 339.8196 
    [ 9'201] ...................................... -- risk: 339.7253 
    [ 9'241] ...................................... -- risk: 339.581 
    [ 9'281] ...................................... -- risk: 339.4437 
    [ 9'321] ...................................... -- risk: 339.3317 
    [ 9'361] ...................................... -- risk: 339.2741 
    [ 9'401] ...................................... -- risk: 339.1531 
    [ 9'441] ...................................... -- risk: 339.061 
    [ 9'481] ...................................... -- risk: 338.9745 
    [ 9'521] ...................................... -- risk: 338.8917 
    [ 9'561] ...................................... -- risk: 338.8077 
    [ 9'601] ...................................... -- risk: 338.7498 
    [ 9'641] ...................................... -- risk: 338.6648 
    [ 9'681] ...................................... -- risk: 338.5324 
    [ 9'721] ...................................... -- risk: 338.3979 
    [ 9'761] ...................................... -- risk: 338.2664 
    [ 9'801] ...................................... -- risk: 338.1388 
    [ 9'841] ...................................... -- risk: 338.0125 
    [ 9'881] ...................................... -- risk: 337.8905 
    [ 9'921] ...................................... -- risk: 337.8168 
    [ 9'961] ......................................
    Final risk: 337.7669 


.. parsed-literal::

    Warning message in c.basis(bresponse = response, bshifting = shifting):
    “more than one basis contains an intercept term”



.. parsed-literal::

    'log Lik.' -409.3729 (df=29)



.. image:: output_10_3.png
   :width: 420px
   :height: 420px


.. code:: r

    tst.data            = data.frame(X.val)
    pred.y.val          = predict(bm,newdata=tst.data)


::


    Error in .const_array(dim, nd, lp): length(x) == prod(dim[subdim]) is not TRUE
    Traceback:


    1. predict(bm, newdata = tst.data)

    2. predict.stmboost(bm, newdata = tst.data)

    3. cbind(ret, predict(tmpm, newdata = nd, ...))

    4. predict(tmpm, newdata = nd, ...)

    5. predict.ctm(tmpm, newdata = nd, ...)

    6. tmlt(object = object, newdata = newdata, q = q, terms = terms, 
     .     ...)

    7. predict(object$model, newdata = newdata, coef = coef(object), 
     .     dim = dim, ...)

    8. predict.cbind_bases(object$model, newdata = newdata, coef = coef(object), 
     .     dim = dim, ...)

    9. predict(object[[b]], newdata = newdata, coef = cf, dim = dim, 
     .     terms = tm, ...)

    10. predict.cbind_bases(object[[b]], newdata = newdata, coef = cf, 
      .     dim = dim, terms = tm, ...)

    11. predict(object[[b]], newdata = newdata, coef = cf, dim = dim, 
      .     terms = tm, ...)

    12. predict.basis(object[[b]], newdata = newdata, coef = cf, dim = dim, 
      .     terms = tm, ...)

    13. .const_array(dim, nd, lp)

    14. stopifnot(length(x) == prod(dim[subdim]))


.. code:: r

    sessionInfo()



.. parsed-literal::

    R version 3.6.1 (2019-07-05)
    Platform: x86_64-apple-darwin15.6.0 (64-bit)
    Running under: macOS Sierra 10.12.6
    
    Matrix products: default
    BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
    LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
    
    locale:
    [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    
    attached base packages:
    [1] parallel  stats     graphics  grDevices utils     datasets  methods  
    [8] base     
    
    other attached packages:
     [1] tram_0.2-6                tbm_0.3-1                
     [3] mlt_1.0-6                 basefun_1.0-5            
     [5] variables_1.0-2           caret_6.0-84             
     [7] Hmisc_4.2-0               ggplot2_3.2.1            
     [9] Formula_1.2-3             lattice_0.20-38          
    [11] penaltyLearning_2019.5.29 survival_2.44-1.1        
    [13] mboost_2.9-1              stabs_0.6-3              
    
    loaded via a namespace (and not attached):
     [1] nlme_3.1-140        lubridate_1.7.4     RColorBrewer_1.1-2 
     [4] repr_1.0.1          numDeriv_2016.8-1.1 tools_3.6.1        
     [7] backports_1.1.4     R6_2.4.0            rpart_4.1-15       
    [10] lazyeval_0.2.2      colorspace_1.4-1    nnet_7.3-12        
    [13] withr_2.1.2         tidyselect_0.2.5    gridExtra_2.3      
    [16] compiler_3.6.1      orthopolynom_1.0-5  htmlTable_1.13.1   
    [19] alabama_2015.3-1    sandwich_2.5-1      scales_1.0.0       
    [22] checkmate_1.9.4     nnls_1.4            mvtnorm_1.0-11     
    [25] quadprog_1.5-7      pbdZMQ_0.3-3        stringr_1.4.0      
    [28] digest_0.6.20       foreign_0.8-71      base64enc_0.1-3    
    [31] pkgconfig_2.0.2     htmltools_0.3.6     htmlwidgets_1.3    
    [34] rlang_0.4.0         rstudioapi_0.10     generics_0.0.2     
    [37] zoo_1.8-6           jsonlite_1.6        acepack_1.4.1      
    [40] dplyr_0.8.3         ModelMetrics_1.2.2  magrittr_1.5       
    [43] polynom_1.4-0       coneproj_1.14       Matrix_1.2-17      
    [46] Rcpp_1.0.2          IRkernel_1.0.2.9000 munsell_0.5.0      
    [49] abind_1.4-5         partykit_1.2-5      multcomp_1.4-10    
    [52] stringi_1.4.3       inum_1.0-1          MASS_7.3-51.4      
    [55] BB_2014.10-1        plyr_1.8.4          recipes_0.1.7      
    [58] grid_3.6.1          crayon_1.3.4        IRdisplay_0.7.0    
    [61] splines_3.6.1       knitr_1.25          pillar_1.4.2       
    [64] uuid_0.1-2          reshape2_1.4.3      codetools_0.2-16   
    [67] stats4_3.6.1        magic_1.5-9         glue_1.3.1         
    [70] evaluate_0.14       latticeExtra_0.6-28 data.table_1.12.2  
    [73] foreach_1.4.7       gtable_0.3.0        purrr_0.3.2        
    [76] assertthat_0.2.1    xfun_0.9            gower_0.2.1        
    [79] prodlim_2018.04.18  libcoin_1.0-5       class_7.3-15       
    [82] geometry_0.4.4      timeDate_3043.102   tibble_2.1.3       
    [85] iterators_1.0.12    cluster_2.1.0       lava_1.6.6         
    [88] TH.data_1.0-10      ipred_0.9-9        

