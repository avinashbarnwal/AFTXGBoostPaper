# Survival - Cox-PH Model
#  Create 2000 clusters each with four patients
#   Failure time follow a marginal multivariate exponential distribution
#  ij = exp( 0 +  1I(xij   c)) (4.15)
# i = 1; :::; 2000; j = 1; :::; 4 (4.16)
#  0 =   1;  1 = (0:8; 0:8; 0; 0)T (4.17)
# Failure times have a 0.65 correlation within the cluster
# Approximately half the failure times are censored.
# Each patient has four covariates(x1; x2; x3; x4)
# First two covariates having cutpoints at 0.5 and 0.3 respectively
# Last two covariates having cutpoints at 0 and 0 respectively
# First two covariates have coe cients at 0.8
# Last two covariates have no e ect on failure.
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr 
from rpy2.robjects import pandas2ri

robjects.r.source("simulate_survival.R")

