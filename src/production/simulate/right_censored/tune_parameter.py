import xgboost as xgb
import pandas  as pd
import numpy   as np
from   sklearn.model_selection import KFold
import optuna
import time
import json
from   optuna.samplers import TPESampler
import functools
import sys
sys.path.insert(0,'../../../../utility')
from 

optuna.logging.set_verbosity(optuna.logging.WARNING)

for distribution in ['normal','logistic','extreme']:
    print(distribution)
    sampler = TPESampler(seed=1)  # Make the sampler behave in a deterministic way.
    study = optuna.create_study(sampler=sampler)
    study.optimize(functools.partial(objective,distribution,input_data), n_trials=100)
    trial         = study.best_trial
    json_filename = "../../../../result/"+distribution+'_param.json'
    with open(json_filename, "w") as write_file:
        json.dump(trial.params, write_file)

