library(MST)
set.seed(186117)
data <- rmultime(N = 2000, K = 4, beta = c(-1, 0.8, 0.8, 0, 0),cutoff = c(0.5, 0.3, 0, 0), model = "marginal.multivariate.exponential",rho = 0.65)$dat
write.table(data,"simulate_survival.csv",sep=",",row.names = FALSE)