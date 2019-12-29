library("actuar")


set.seed(1)
### Supports 3 diff ###
#Weibull
#Loglogistic
#Lognormal
#qweibull
#qlnorm
#qllogis
model_1<-function(n=5000,d=100){
  Z <- rnorm(n,0,0.5)
  X <- data.frame(t(replicate(n, runif(d,-1,1))))
  colnames(X) <- paste("X",seq(1,d),sep=".")
  X.B         <- X[,"X.1"]*X[,"X.2"]+X[,"X.3"]**2-X[,"X.4"]*X[,"X.7"]+X["X.8"]*X[,"X.10"]-X[,"X.6"]**2+Z
  result     <- list()
  result$X.B <- X.B
  result$X   <- X
  return(result)
}

simEventTime <- function(linPred = 0, model = 'aft', 
                         dist = qweibull, 
                         paramList = list(shape = 1, scale = 1)){
  n <- length(linPred)
  rawP <- runif(n)
  nu <- exp(linPred)
  if(model == 'aft'){
    paramList$p <- rawP
    rawTimes <- do.call(dist, paramList)
    ans <- rawTimes * nu
    return(ans)
  }
}
  
  
simIC_weib <- function(n = 1000, d=100, model = "aft", dist = "weibull", inspections = 2, 
                       inspectLength = 2.5,prob_cen = 1, model_type="1", shape = 2,
                       scale = 2, meanlog = 0, sdlog = 1, rndDigits = NULL){
  
  ### Create XB ###
  
  if(model_type=="1"){
    linPred <- model_1(n=n,d=d)$X.B
    X       <- model_1(n=n,d=d)$X
  }
  
  if(dist=="weibull"){
    dist = qweibull
    paramList = list(shape = shape, scale = scale)
  }
  else if(dist=="lognormal")
  {
    dist      = qlnorm
    paramList = list(meanlog = meanlog, sdlog = sdlog)
  }
  else if(dist=="loglogistic"){
    dist = qllogis
    paramList = list(shape = shape, scale = scale)
  }
  
  
  trueTimes <- simEventTime(linPred, model = model, dist = dist, 
                              paramList = paramList)
  
  
  obsTimes <- runif(n = n, max = inspectLength)
  if (!is.null(rndDigits)) 
    obsTimes <- round(obsTimes, rndDigits)
  l <- rep(0, n)
  u <- rep(0, n)
  caught <- trueTimes < obsTimes
  u[caught] <- obsTimes[caught]
  l[!caught] <- obsTimes[!caught]
  if (inspections > 1) {
    for (i in 2:inspections) {
      oldObsTimes <- obsTimes
      obsTimes <- oldObsTimes + runif(n, max = inspectLength)
      if (!is.null(rndDigits)) 
        obsTimes <- round(obsTimes, rndDigits)
      caught <- trueTimes >= oldObsTimes & trueTimes < obsTimes
      needsCatch <- trueTimes > obsTimes
      u[caught] <- obsTimes[caught]
      l[needsCatch] <- obsTimes[needsCatch]
    }
  }
  else {
    needsCatch <- !caught
  }
  u[needsCatch] <- Inf
  if (sum(l > u) > 0) 
    stop("warning: l > u! Bug in code")
  isCensored <- rbinom(n = n, size = 1, prob = prob_cen) == 1
  l[!isCensored] <- trueTimes[!isCensored]
  u[!isCensored] <- trueTimes[!isCensored]
  if (sum(l == Inf) > 0) {
    allTimes <- c(l, u)
    allFiniteTimes <- allTimes[allTimes < Inf]
    maxFiniteTime <- max(allFiniteTimes)
    l[l == Inf] <- maxFiniteTime
  }
  result        = list()
  result$target = data.frame(as.numeric(l), as.numeric(u)) 
  colnames(result$target) = c("l","u")
  result$input = X
  return(result)
  }
  
analyze_target <- function(target){
  
  left_censored     = 0
  right_censored    = 0
  interval_censored = 0
  uncensored        = 0
  n = length(target$l)
  
  uncensored     = sum(target$l==target$u)
  left_censored  = sum(target$l==Inf)
  right_censored = sum(target$u==Inf)
  interval_censored = n - uncensored - left_censored- right_censored
  distribution = list()
  distribution$uncensored        = uncensored
  distribution$left_censored     = left_censored
  distribution$right_censored    = right_censored
  distribution$interval_censored = interval_censored
  return(distribution)
}

### Draw Samples ###

