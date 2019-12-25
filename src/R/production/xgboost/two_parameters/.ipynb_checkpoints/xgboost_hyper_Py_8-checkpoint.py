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


# In[4]:


def data_import(data_name):
    filename = '../../../../../data/'+data_name+'/'
    inputFileName = filename+'inputs.csv'
    labelFileName = filename+'outputs.csv'
    foldsFileName = filename+'cv/equal_labels/folds.csv'
    inputs        = pd.read_csv(inputFileName,index_col='sequenceID')
    labels        = pd.read_csv(labelFileName,index_col='sequenceID')
    folds         = pd.read_csv(foldsFileName,index_col='sequenceID')
    res           = {}
    res['inputs'] = inputs
    res['labels'] = labels
    res['folds']  = folds
    return(res)


# In[3]:


def data_massage(inputs,labels):
    inputs.replace([-float('inf'),float('inf')],np.nan,inplace=True)
    missingCols = inputs.isnull().sum()
    missingCols = list(missingCols[missingCols>0].index)
    inputs.drop(missingCols,axis=1,inplace=True)
    varCols     = inputs.apply(lambda x: np.var(x))
    zeroVarCols = list(varCols[varCols==0].index)
    inputs.drop(zeroVarCols,axis=1,inplace=True)
    labels['min.log.lambda'] = labels['min.log.lambda'].apply(lambda x: np.exp(x))
    labels['max.log.lambda'] = labels['max.log.lambda'].apply(lambda x: np.exp(x))
    return inputs,labels


# In[4]:


def getXY(foldNo,folds,inputs,labels):
    test_id       = list(folds[folds['fold']==foldNo].index)
    train_id      = list(folds[folds['fold']!=foldNo].index)
    X             = inputs[inputs.index.isin(train_id)]
    X_val         = inputs[inputs.index.isin(test_id)]
    y_label       = labels[labels.index.isin(train_id)]
    y_label_test  = labels[labels.index.isin(test_id)]
    y_lower       = y_label['min.log.lambda']
    y_upper       = y_label['max.log.lambda']
    y_lower_val   = y_label_test['min.log.lambda']
    y_upper_val   = y_label_test['max.log.lambda']
    res           = {}
    res['X']         = X
    res['X_val']     = X_val
    res['y_lower']      = y_lower
    res['y_lower_val']  = y_lower_val
    res['y_upper']      = y_upper
    res['y_upper_val']  = y_upper_val
    return res


# In[5]:


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


# In[6]:


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
                'verbosity': 0
                }
    
    for fold_, (trn_idx, val_idx) in enumerate(Kfolds.split(X, y_lower,y_upper)):
        tr_x, tr_y_lower,tr_y_upper = X.iloc[trn_idx,:],y_lower.iloc[trn_idx],y_upper.iloc[trn_idx]
        vl_x, vl_y_lower,vl_y_upper = X.iloc[val_idx,:], y_lower.iloc[val_idx],y_upper.iloc[val_idx]
        res = res + trainModel(tr_x,vl_x,tr_y_lower,tr_y_upper,vl_y_lower,vl_y_upper,params,num_round,distribution_sigma)
    return res


# In[7]:


def best_iter(eta,max_depth,min_child_weight,reg_alpha,reg_lambda,sigma,distribution): 
    SEED          = 1
    Kfolds        = KFold(n_splits=5,shuffle=True,random_state=SEED)
    num_round     = 5000
    # Discrete-uniform parameter
    distributionCol = distribution+ ',' + str(sigma)
    eval_metric     = 'aft-nloglik@'+distributionCol
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
                'verbosity': 0
                }

    res_data = pd.DataFrame()
    for fold_, (trn_idx, val_idx) in enumerate(Kfolds.split(X, y_lower,y_upper)):
        tr_x, tr_y_lower,tr_y_upper = X.iloc[trn_idx,:],y_lower.iloc[trn_idx],y_upper.iloc[trn_idx]
        vl_x, vl_y_lower,vl_y_upper = X.iloc[val_idx,:], y_lower.iloc[val_idx],y_upper.iloc[val_idx]
        res_data[fold_] = trainModelIter(tr_x,vl_x,tr_y_lower,tr_y_upper,vl_y_lower,vl_y_upper,params,num_round,distributionCol)
    res_data['total'] = res_data.sum(axis=1)
    res = {}
    num_round = res_data.idxmin(axis=0, skipna=True)['total']
    res['num_round'] = num_round
    res['min_val_error'] = min(res_data['total'])
    return res


# In[2]:


data_name_domain = ['ATAC_JV_adipose','CTCF_TDH_ENCODE','H3K27ac-H3K4me3_TDHAM_BP','H3K27ac_TDH_some','H3K36me3_AM_immune','H3K27me3_RL_cancer','H3K27me3_TDH_some','H3K36me3_TDH_ENCODE','H3K36me3_TDH_immune','H3K36me3_TDH_other']


# In[5]:


data      = data_import(data_name_domain[7])
data_name = data_name_domain[7]


# In[10]:


inputs = data['inputs']
labels = data['labels']
folds  = data['folds']


# In[11]:


inputs,labels = data_massage(inputs,labels)


# In[12]:


global X
global X_val
global y_lower
global y_upper
global y_upper_val


# In[13]:


run_time = {}


# In[14]:


optuna.logging.set_verbosity(optuna.logging.WARNING)


# In[33]:


for fold in np.unique(folds['fold'].values):
    start        = time.time()
    res          = getXY(fold,folds,inputs,labels)
    X            = res['X']        
    X_val        = res['X_val']
    y_lower      = res['y_lower']
    y_lower_val  = res['y_lower_val']
    y_upper      = res['y_upper']
    y_upper_val  = res['y_upper_val']
    
    for distribution in ['normal','logistic','extreme']:
        
        print(fold,distribution)
        sampler = TPESampler(seed=1)  # Make the sampler behave in a deterministic way.
        study = optuna.create_study(sampler=sampler)
        study.optimize(functools.partial(objective,distribution), n_trials=100)
        trial         = study.best_trial
        json_filename = "../../../../result/"+data_name+"/xgboost/fold"+str(fold)+'_'+distribution+'_param.json'
        with open(json_filename, "w") as write_file:
            json.dump(trial.params, write_file)
            
    end            = time.time()
    time_taken     = end - start
    run_time[fold] = time_taken


# In[28]:


run_time1 = {}
for key in run_time.keys():
    run_time1[str(key)] = run_time[key]


# In[29]:


json_filename = "../../../../result/"+data_name+"/xgboost/run_time_tuning1.json"
with open(json_filename, "w") as write_file:
    json.dump(run_time1, write_file)


# In[30]:


def trainModelIter(X,X_val,y_lower,y_upper,y_lower_val,y_upper_val,params,num_round,distributionCol):
    
    res    = {}
    dtrain = xgb.DMatrix(X)
    dtrain.set_float_info("label_lower_bound",y_lower)
    dtrain.set_float_info("label_upper_bound",y_upper)

    dtest  = xgb.DMatrix(X_val)
    dtest.set_float_info("label_lower_bound",y_lower_val)
    dtest.set_float_info("label_upper_bound",y_upper_val)

    bst    = xgb.train(params,dtrain,num_boost_round=num_round,evals=[(dtrain,"train"),(dtest,"test")],evals_result=res,verbose_eval=False)
    val_error = res['test'][distributionCol]
    
    return(val_error)


# In[34]:


run_time2 = {}


# In[35]:


#for fold in range(2,3):
for fold in np.unique(folds['fold'].values):
    start_time   = time.time()
    res = getXY(fold,folds,inputs,labels)
    X            = res['X']        
    X_val        = res['X_val']
    y_lower      = res['y_lower']
    y_lower_val  = res['y_lower_val']
    y_upper      = res['y_upper']
    y_upper_val  = res['y_upper_val']
    
    for distribution in ['normal','logistic','extreme']:
        json_filename = "../../../../result/"+data_name+"/xgboost/fold"+str(fold)+'_'+distribution+'_param.json'
        with open(json_filename, errors='ignore') as json_data:
            json_fold = json.load(json_data, strict=False)
        eta = json_fold['eta']
        max_depth = json_fold['max_depth']
        min_child_weight = json_fold['min_child_weight']
        reg_alpha = json_fold['reg_alpha']
        reg_lambda = json_fold['reg_lambda']
        sigma= 1
        res      = best_iter(eta,max_depth,min_child_weight,reg_alpha,reg_lambda,sigma,distribution)
        new_json = {}
        new_json['eta'] = eta
        new_json['max_depth'] = max_depth
        new_json['min_child_weight'] = min_child_weight
        new_json['reg_alpha']     = reg_alpha
        new_json['reg_lambda']    = reg_lambda
        new_json['sigma']         = sigma
        new_json['distribution']  = distribution
        new_json['num_round']     = int(res['num_round'])
        if res['min_val_error'] == float('inf'):
            res['min_val_error'] = 10**8
        new_json['min_val_error'] = res['min_val_error']
        json_filename = "../../../../result/"+data_name+"/xgboost/fold_new"+str(fold)+'_'+distribution+'_param.json'
        with open(json_filename, "w") as write_file:
             json.dump(new_json, write_file)
    end_time        = time.time()
    time_taken      = end_time - start_time
    run_time2[fold] = time_taken


# ### Choosing best hyperparameter

# In[36]:


#for fold in range(2,3):
for fold in np.unique(folds['fold'].values):
    fold_data = pd.DataFrame()
    for distribution in ['normal','logistic','extreme']:
        json_filename = "../../../../result/"+data_name+"/xgboost/fold_new"+str(fold)+'_'+distribution+'_param.json'
        with open(json_filename, errors='ignore') as json_data:
            json_fold = json.load(json_data, strict=False)
        dist_data = pd.DataFrame.from_dict(json_fold,orient='index',columns=[distribution])
        fold_data = pd.concat([fold_data,dist_data],axis=1)
    fold_data = fold_data.transpose()
    fold_data['min_val_error'] = fold_data['min_val_error'].astype('float')
    best_dis   = fold_data['min_val_error'].idxmin()
    best_param = fold_data.loc[best_dis]
    json_filename = "../../../../result/"+data_name+"/xgboost/fold_new"+str(fold)+'_dis'+'_param.json'
    best_param = best_param.to_dict()
    with open(json_filename, "w") as write_file:
        json.dump(best_param, write_file)


# In[37]:


run_time3 ={}
for key in run_time2.keys():
    run_time3[str(key)] = run_time2[key]


# In[38]:


json_filename = "../../../../result/"+data_name+"/xgboost/run_time_tuning2.json"
with open(json_filename, "w") as write_file:
    json.dump(run_time3, write_file)

