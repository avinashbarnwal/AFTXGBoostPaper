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
inputs     = do.call(data.frame,lapply(inputs, function(x) replace(x, is.infinite(x),NA)))
naColumns = colnames(inputs)[colSums(is.na(inputs))>0]
# infiniteColumns = colnames(inputs)[colSums(is.infinite(inputs))>0]
removeColumns   = c(naColumns,infiniteColumns)
inputs    = inputs[ , !(names(inputs) %in% naColumns)]
labels$min.log.lambda = unlist(lapply(labels$min.log.lambda,exp))
labels$max.log.lambda = unlist(lapply(labels$max.log.lambda,exp))
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
res       = list()
res$X     = X
res$X_val = X_val
res$y_lower      = y_lower
res$y_lower_val  = y_lower_val
res$y_upper     = y_upper
res$y_upper_val = y_upper_val
return(res)
}
dataNameRange = c('ATAC_JV_adipose','CTCF_TDH_ENCODE','H3K27ac-H3K4me3_TDHAM_BP','H3K27ac_TDH_some','H3K36me3_AM_immune')
# Set Parameters
sigma_range         = c(1,2,5,10,100)
distribution_range  = c('normal','logistic','extreme')
learning_rate       = 0.1
num_round           = 200
res      = data_import(dataNameRange[1])
inputs   = res$inputs
labels   = res$labels
folds    = res$folds
resDataMassage = data_massage(inputs,labels)
inputs         = resDataMassage$inputs
labels         = resDataMassage$labels
res           = getXY(1,folds,inputs,labels)
X             = res$X
X_val         = res$X_val
y_lower       = res$y_lower
y_lower_val   = res$y_lower_val
y_upper       = res$y_upper
y_upper_val   = res$y_upper_val
firstdata     = data.frame(X,y_lower,y_upper)
my.surv       = Surv(y_lower,y_upper,type='interval2')
formula       = as.formula(paste("my.surv ~", paste(colnames(X),collapse="+")))
sr.fit        = survreg(formula,data=firstdata,dist='lognormal', control = list(maxiter=500))
summary(sr.fit)