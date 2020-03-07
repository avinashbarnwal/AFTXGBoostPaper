#!/usr/bin/env python
# coding: utf-8

# In[22]:


import xgboost as xgb
import pandas  as pd
import numpy   as np
from   sklearn.model_selection import KFold
import optuna
import time
import json
from   optuna.samplers import TPESampler
import functools
import itertools
import os


# ### Sigma Distribution

# | Data Name                |Normal|Logistic| Extreme | 
# |--------------------------|------|--------|---------|
# |ATAC_JV_adipose           | 1    | 1      |    10   |
# |CTCF_TDH_ENCODE           | 1    | 1      |    10   |
# |H3K27ac-H3K4me3_TDHAM_BP  | 1    | 1      |    10   |
# |H3K27ac_TDH_some          | 1    | 1      |    10   |
# |H3K36me3_AM_immune        | 1    | 1      |    10   |
# |H3K27me3_RL_cancer        | 10   | 1      |    10   |
# |H3K27me3_TDH_some         | 15   | 1      |    10   |
# |H3K36me3_TDH_ENCODE       | 1    | 1      |    10   |
# |H3K36me3_TDH_immune       | 10   | 1      |    10   |
# |H3K36me3_TDH_other        | 10   | 1      |    10   |

# In[2]:


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


def grid_search_cv_step(X, y_lower, y_upper, Kfolds, params, num_round, distribution_sigma):
    res = 0
    for fold_, (trn_idx, val_idx) in enumerate(Kfolds.split(X, y_lower,y_upper)):
        tr_x, tr_y_lower,tr_y_upper = X.iloc[trn_idx,:],y_lower.iloc[trn_idx],y_upper.iloc[trn_idx]
        vl_x, vl_y_lower,vl_y_upper = X.iloc[val_idx,:], y_lower.iloc[val_idx],y_upper.iloc[val_idx]
        res = res + trainModel(tr_x,vl_x,tr_y_lower,tr_y_upper,vl_y_lower,vl_y_upper,params,num_round,distribution_sigma)
    return res


# In[7]:


global min_child_weight 
global reg_alpha
global reg_lambda
global subsample
global num_round
global base_score
global eta


# In[8]:


min_child_weight = 0.1
reg_alpha        = 0.005
reg_lambda       = 0.5
subsample        = 0.7
num_round        = 5000
base_score       = 0.5
eta              = 0.01

# In[27]:


def grid_search_cv(X, y_lower, y_upper, distribution, sigma):
    
    SEED         = 1
    Kfolds       = KFold(n_splits=5,shuffle=True,random_state=SEED)
    res          = 0
    max_depth_range  = np.arange(2,10,2)
    
    
    distribution_sigma = distribution+ ',' + str(sigma)
    eval_metric  = 'aft-nloglik@'+distribution_sigma

    error_results = []
    max_depths    = []
    

    for max_depth in max_depth_range:
        params   = {
                    'eta':eta,
                    'max_depth':int(max_depth),
                    'min_child_weight':min_child_weight,
                    'subsample':subsample,
                    'reg_alpha':reg_alpha,
                    'reg_lambda':reg_lambda,
                    'aft_noise_distribution' : distribution, 
                    'aft_sigma': sigma,
                    'eval_metric':eval_metric,
                    'base_score':base_score,
                    'objective':"aft:survival",
                    'verbosity': 0
                    }
        error_result = grid_search_cv_step(X, y_lower, y_upper, Kfolds, params, num_round, distribution_sigma)
        error_results.append(error_result)
        max_depths.append(max_depth)

    return max_depths,error_results


# In[46]:


def best_iter(eta,max_depth,min_child_weight,reg_alpha,reg_lambda,sigma,distribution, X, y_lower,y_upper): 
    
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


# In[29]:


def get_data(data_name):
    data   = data_import(data_name)
    inputs = data['inputs']
    labels = data['labels']
    folds  = data['folds']
    inputs,labels = data_massage(inputs,labels)
    return folds,inputs,labels


# In[30]:


def get_best_param(max_depths,error_results):
    para_result    = pd.DataFrame()
    para_result['max_depths']    = max_depths
    para_result['error_results'] = error_results
    min_index                    = para_result['error_results'].idxmin()
    best_max_depth = para_result.loc[min_index,'max_depths']
    return best_max_depth


# In[42]:


def get_best_parameters(data_name,sigmas):
    folds,inputs,labels = get_data(data_name)
    run_time = {}
    for fold in np.unique(folds['fold'].values):
        start          = time.time()
        res          = getXY(fold,folds,inputs,labels)
        X            = res['X']       
        X_val        = res['X_val']
        y_lower      = res['y_lower']
        y_lower_val  = res['y_lower_val']
        y_upper      = res['y_upper']
        y_upper_val  = res['y_upper_val']
        for distribution in ['normal','logistic','extreme']:
            if distribution=='normal':
                sigma = sigmas['normal']
            elif distribution=='logistic':
                sigma = sigmas['logistic']
            elif distribution=='extreme':
                sigma = sigmas['extreme']
            
            max_depths,error_results = grid_search_cv(X, y_lower, y_upper, distribution,sigma)
            best_max_depth = get_best_param(max_depths,error_results)
            end            = time.time()
            time_taken     = end - start
            print(time_taken)
            key           = str(fold)+"_"+distribution
            run_time[key] = time_taken
            best_param = {}
            best_param['eta']       = eta
            best_param['max_depth'] = int(best_max_depth)
            best_param['min_child_weight'] = min_child_weight
            best_param['reg_alpha']        = reg_alpha
            best_param['reg_lambda']       = reg_lambda
            best_param['sigma']            = sigma
            best_param['distribution']     = distribution
            best_param['num_round']        = num_round
            json_filename = "../../../../../result/"+data_name+"/xgboost/fold"+str(fold)+'_'+distribution+'_param_1_grid_search.json'
            with open(json_filename, "w") as write_file:
                 json.dump(best_param, write_file)
    return run_time


# In[32]:


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


# In[48]:


def get_best_num_round(data_name,sigmas,run_time):
    
    folds,inputs,labels = get_data(data_name)
    
    for fold in np.unique(folds['fold'].values):
        start_time          = time.time()
        res          = getXY(fold,folds,inputs,labels)
        X            = res['X']       
        X_val        = res['X_val']
        y_lower      = res['y_lower']
        y_lower_val  = res['y_lower_val']
        y_upper      = res['y_upper']
        y_upper_val  = res['y_upper_val']
        for distribution in ['normal','logistic','extreme']:
            if distribution=='normal':
                sigma = sigmas['normal']
            elif distribution=='logistic':
                sigma = sigmas['logistic']
            elif distribution=='extreme':
                sigma = sigmas['extreme']
            
            json_filename = "../../../../../result/"+data_name+"/xgboost/fold"+str(fold)+'_'+distribution+'_param_1_grid_search.json'
            with open(json_filename, errors='ignore') as json_data:
                json_fold = json.load(json_data, strict=False)
            eta = json_fold['eta']
            max_depth = json_fold['max_depth']
            res      = best_iter(eta,max_depth,min_child_weight,reg_alpha,reg_lambda,sigma,distribution,X, y_lower,y_upper)
            res_param = {}
            res_param['eta'] = eta
            res_param['max_depth'] = max_depth
            res_param['min_child_weight'] = min_child_weight
            res_param['reg_alpha']     = reg_alpha
            res_param['reg_lambda']    = reg_lambda
            res_param['sigma']         = sigma
            res_param['distribution']  = distribution
            res_param['num_round']     = int(res['num_round'])
            if res['min_val_error'] == float('inf'):
                res['min_val_error'] = 10**8
            res_param['min_val_error'] = res['min_val_error']
            json_filename = "../../../../../result/"+data_name+"/xgboost/fold_new"+str(fold)+'_'+distribution+'_param_1_grid_search.json'
            with open(json_filename, "w") as write_file:
                 json.dump(res_param, write_file)
            end_time        = time.time()
            time_taken      = end_time - start_time
            key             = str(fold)+"_"+distribution
            run_time[key] = run_time[key] + time_taken
    return run_time


# In[34]:


data_name_domain = ['ATAC_JV_adipose','CTCF_TDH_ENCODE','H3K27ac-H3K4me3_TDHAM_BP',
                    'H3K27ac_TDH_some','H3K36me3_AM_immune','H3K27me3_RL_cancer',
                    'H3K27me3_TDH_some','H3K36me3_TDH_ENCODE','H3K36me3_TDH_immune','H3K36me3_TDH_other']


# In[35]:


data_name = data_name_domain[6]
sigmas = {}
sigmas['normal'] = 15
sigmas['logistic'] = 1
sigmas['extreme'] = 10


# In[43]:


run_time = get_best_parameters(data_name,sigmas)


# In[49]:


run_time = get_best_num_round(data_name,sigmas,run_time)


# In[50]:


json_filename = "../../../../../result/"+data_name+"/xgboost/run_dis_time_1_param_grid_search.json"
with open(json_filename, "w") as write_file:
    json.dump(run_time, write_file)

