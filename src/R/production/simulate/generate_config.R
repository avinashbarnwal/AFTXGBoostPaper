library(configr)

### Generate a JSON format configuration file ###

param       <- list(n=5000,d=100,dist="weibull",model=1,shape=2,scale=2,inspections=2,inspectLength=2.5,seed=1,
                    path="/Users/avinashbarnwal/Desktop/Personal/aftXgboostPaper/data/simulate/")
input_param <- list("sim_data_1"=param)
path        <- paste(getwd(),"/config",sep="")
out.fn      <- sprintf("%s/config.json",path)
write.config(config.dat = input_param, file.path = out.fn, write.type = "json")

file_name = paste(path,"/config.json",sep="")

### config.1   <- read.config(file = file_name) ###
