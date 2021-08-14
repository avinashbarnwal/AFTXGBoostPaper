import xgboost as xgb
import pandas  as pd
import numpy   as np
from   sklearn.model_selection import KFold
import optuna
import time
import json
from   optuna.samplers import TPESampler
import functools
import pickle
import sys
sys.path.insert(0,'../../../../utility')
from model_utils import objective
import time
from joblib import Parallel, delayed

start = time.process_time()
optuna.logging.set_verbosity(optuna.logging.WARNING)
fp = open(b'../data/right_censored/aft/simulated_train_input_data.pkl',"rb")
input_data = pickle.load(fp)


def main(distribution,input_data,n_trials):
    study = optuna.load_study(study_name='survival', storage=sqllite_filename)
    # study.optimize(objective, distribution,input_data,n_trials=n_trials)
    study.optimize(lambda trial: objective(trial,distribution, input_data), n_trials=n_trials)

# trial = study.best_trial
# with open(json_filename, "w") as write_file:
#     json.dump(trial.params, write_file)


for distribution in ['normal','logistic','extreme']:
    print(distribution)
    sampler = TPESampler(seed=1)  # Make the sampler behave in a deterministic way.
    # study = optuna.create_study(sampler=sampler)
    sqllite_filename = 'sqlite:///'+distribution+'.db'
    study = optuna.create_study(sampler=sampler,direction='maximize',study_name='survival', storage=sqllite_filename)
    # study.optimize(functools.partial(objective,distribution,input_data), n_trials=10)
    r = Parallel(n_jobs=-1)([delayed(main)(distribution, input_data,1) for _ in range(10)])
    trial         = study.best_trial
    json_filename = "../../../../result/simulated/right_censored/"+distribution+'_param.json'
    with open(json_filename, "w") as write_file:
        json.dump(trial.params, write_file)

end_time = time.process_time() - start
print(end_time)