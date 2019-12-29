source("simIC.R")

set.seed(1)
sim_data = simIC_weib(n = 5000, d=100, dist='weibull', inspections = 2, inspectLength = 2.5)
target   = sim_data$target
input    = sim_data$input
par(mar = c(5,5,2,5))
plot(target$l,type='l',ylab='lower level',col='blue')
par(new = T)
plot(target$u,type='l' ,pch=16, axes=F, xlab=NA, ylab=NA, cex=1.2,col='red')
axis(side = 4)
mtext(side = 4, line = 3, 'upper level')
distribution = analyze_target(target)

### print(distribution) ###

n         <- 5000
target$id <- seq(1,n)
input$id  <- seq(1,n)

write.table(target,"/Users/avinashbarnwal/Desktop/Personal/aftXgboostPaper/data/simulate/sim_data_1/target.csv",sep=",",row.names = FALSE)
write.table(input,"/Users/avinashbarnwal/Desktop/Personal/aftXgboostPaper/data/simulate/sim_data_1/input.csv",sep=",",row.names = FALSE)

fold      <- sample(1:4,size=n,replace=TRUE,prob=c(1/4,1/4,1/4,1/4))
id        <- seq(1,n)
fold_data <- data.frame(fold = fold,id=id)

write.table(fold_data,"/Users/avinashbarnwal/Desktop/Personal/aftXgboostPaper/data/simulate/sim_data_1/folds.csv",sep=",",row.names = FALSE)







