#!/usr/bin/env python
# coding: utf-8

# In[1]:


import xgboost as xgb
import pandas  as pd
import numpy   as np
from   sklearn.model_selection import KFold
import optuna
import time
import json
from   optuna.samplers import TPESampler
import functools
import os


# In[2]:


data_name = "sim_data_1"


# In[3]:


def data_import(data_name):
    filename = "../../../../data/simulate/"+data_name+"/"
    inputFileName  = filename+'input.csv'
    targetFileName = filename+'target.csv'
    foldFileName   = filename+'folds.csv'
    inputs         = pd.read_csv(inputFileName,index_col='id')
    targets        = pd.read_csv(targetFileName,index_col='id')
    folds          = pd.read_csv(foldFileName,index_col='id')
    res            = {}
    res['inputs']  = inputs
    res['targets'] = targets
    res['folds']   = folds
    return(res)


# In[4]:


res = data_import(data_name)


# In[5]:


folds   = res['folds']
targets = res['targets']
inputs  = res['inputs']


# In[6]:


def getXY(foldNo,folds,inputs,targets):
    test_id       = list(folds[folds['fold']==foldNo].index)
    train_id      = list(folds[folds['fold']!=foldNo].index)
    X             = inputs[inputs.index.isin(train_id)]
    X_val         = inputs[inputs.index.isin(test_id)]
    y_label       = targets[targets.index.isin(train_id)]
    y_label_test  = targets[targets.index.isin(test_id)]
    y_lower       = y_label['l']
    y_upper       = y_label['u']
    y_lower_val   = y_label_test['l']
    y_upper_val   = y_label_test['u']
    res                 = {}
    res['X']            = X
    res['X_val']        = X_val
    res['y_lower']      = y_lower
    res['y_lower_val']  = y_lower_val
    res['y_upper']      = y_upper
    res['y_upper_val']  = y_upper_val
    return res


# In[7]:


def trainModel(X,X_val,y_lower,y_upper,y_lower_val,y_upper_val,params,num_round,distributionCol):
    
    res    = {}
    dtrain = xgb.DMatrix(X)
    dtrain.set_float_info("label_lower_bound",y_lower.values)
    dtrain.set_float_info("label_upper_bound",y_upper.values)

    dtest  = xgb.DMatrix(X_val)
    dtest.set_float_info("label_lower_bound",y_lower_val.values)
    dtest.set_float_info("label_upper_bound",y_upper_val.values)
    
    bst    = xgb.train(params,dtrain,num_boost_round=num_round,evals=[(dtrain,"train"),(dtest,"test")],evals_result=res,verbose_eval=False)
    min_val_error = round(np.min(res['test'][distributionCol]),4)
    return(min_val_error)


# In[8]:


def objective(distribution,trial):
    SEED         = 1
    Kfolds       = KFold(n_splits=5,shuffle=True,random_state=SEED)
    num_round    = 5000
    res          = 0
    # Discrete-uniform parameter
    eta              = trial.suggest_discrete_uniform('eta',0.001,1.001,0.1)
    max_depth        = trial.suggest_discrete_uniform('max_depth',2, 10,2)
    min_child_weight = trial.suggest_discrete_uniform('min_child_weight',0.1,100.1,10)
    reg_alpha        = trial.suggest_loguniform('reg_alpha',0.0001,100)
    reg_lambda       = trial.suggest_loguniform('reg_lambda',0.0001,100)
#     sigma            = trial.suggest_discrete_uniform('sigma',1,100,1)
#     distribution     = trial.suggest_categorical('distribution',['normal','logistic','extreme'])
    sigma            = 1
    
    distribution_sigma = distribution+ ',' + str(sigma)
    eval_metric     = 'aft-nloglik@'+distribution_sigma
    base_score      = 0.5
    
    params   = {
                'eta':eta,
                'max_depth':int(max_depth),
                'min_child_weight':min_child_weight,
                'subsample':0.7,
                'reg_alpha':reg_alpha,
                'reg_lambda':reg_lambda,
                'aft_noise_distribution' : distribution, 
                'aft_sigma': sigma,
                'eval_metric':eval_metric,
                'base_score':base_score,
                'objective':"aft:survival",
                'verbosity': 0,
                'nthread':-1
                }
    
    for fold_, (trn_idx, val_idx) in enumerate(Kfolds.split(X, y_lower,y_upper)):
        tr_x, tr_y_lower,tr_y_upper = X.iloc[trn_idx,:],y_lower.iloc[trn_idx],y_upper.iloc[trn_idx]
        vl_x, vl_y_lower,vl_y_upper = X.iloc[val_idx,:], y_lower.iloc[val_idx],y_upper.iloc[val_idx]
        res = res + trainModel(tr_x,vl_x,tr_y_lower,tr_y_upper,vl_y_lower,vl_y_upper,params,num_round,distribution_sigma)
        
    return res


# In[9]:


global X
global X_val
global y_lower
global y_upper
global y_upper_val


# In[10]:


run_time = {}


# In[ ]:


#for fold in np.unique(folds['fold'].values):

for fold in [2,3,4]:
    
    start        = time.time()
    res          = getXY(fold,folds,inputs,targets)
    X            = res['X']        
    X_val        = res['X_val']
    y_lower      = res['y_lower']
    y_lower_val  = res['y_lower_val']
    y_upper      = res['y_upper']
    y_upper_val  = res['y_upper_val']
    
    for distribution in ['logistic','extreme']:
        
        print(fold,distribution)
        sampler = TPESampler(seed=1)  # Make the sampler behave in a deterministic way.
        database_name = 'sqlite:///'+str(fold)+"_"+distribution+".db"
        study = optuna.create_study(sampler=sampler,storage=database_name)
        study.optimize(functools.partial(objective,distribution), n_trials=100)
        trial         = study.best_trial
        json_filename = "../../../../result/simulated/"+data_name+"/xgboost/fold"+str(fold)+'_'+distribution+'_param.json'
        with open(json_filename, "w") as write_file:
            json.dump(trial.params, write_file)
    
    end            = time.time()
    time_taken     = end - start
    run_time[fold] = time_taken


# In[ ]:




