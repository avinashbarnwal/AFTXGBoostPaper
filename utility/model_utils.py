import xgboost as xgb
import pandas  as pd
import numpy   as np
from   sklearn.model_selection import KFold
import optuna
import time
import json
from   optuna.samplers import TPESampler
import functools

global X
global X_val
global y_lower
global y_upper
global y_upper_val

def c_statistic_harrell(pred, labels):
    total = 0
    matches = 0
    for i in range(len(labels)):
        for j in range(len(labels)):
            if labels[j] > 0 and abs(labels[i]) > labels[j]:
                total += 1
                if pred[j] > pred[i]:
                    matches += 1
    return matches/total

def train_model(dtrain,dtest,params):
    model_train = xgb.train(params, dtrain, num_boost_round=10000,evals=[(dtest, "test")], verbose_eval=1000)
    y_test_pred = model_train.predict(dtest)
    return y_test_pred


def get_xgb_dataframe(X_train=None,y_train=None,X_test=None,y_test=None,y_train_lower_bound=None,
                    y_train_upper_bound=None,y_test_lower_bound=None,y_test_upper_bound=None,model_type='coxPh'):
    if model_type=='coxPh':
        dtrain = xgb.DMatrix(X_train, label=y_train)
        dtest = xgb.DMatrix(X_test, label=y_test)
    if model_type=='aft':
        dtrain = xgb.DMatrix(X_train)
        dtrain.set_float_info('label_lower_bound', y_train_lower_bound)
        dtrain.set_float_info('label_upper_bound', y_train_upper_bound)
        dtest = xgb.DMatrix(X_test)
        dtest.set_float_info('label_lower_bound', y_test_lower_bound)
        dtest.set_float_info('label_upper_bound', y_test_upper_bound)
    return dtrain,dtest



def objective(distribution,trial,model_type='aft'):
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
    # sigma            = trial.suggest_discrete_uniform('sigma',1,100,1)
    # distribution     = trial.suggest_categorical('distribution',['normal','logistic','extreme'])
    if model_type=='aft':
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
            dtrain,dtest = get_xgb_dataframe(X_train=None,y_train=None,X_test=None,y_test=None,y_train_lower_bound=None,
                    y_train_upper_bound=None,y_test_lower_bound=None,y_test_upper_bound=None,model_type='coxPh')
            y_test_pred = train_model(dtrain,dtest,params)
            c_index = c_statistic_harrell(list(y_test_pred), list(df_test['y']))

    return res


optuna.logging.set_verbosity(optuna.logging.WARNING)



    
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



run_time1 = {}
for key in run_time.keys():
    run_time1[str(key)] = run_time[key]

json_filename = "../../../../result/"+data_name+"/xgboost/run_time_tuning1.json"
with open(json_filename, "w") as write_file:
    json.dump(run_time1, write_file)




run_time2 = {}

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

run_time3 ={}
for key in run_time2.keys():
    run_time3[str(key)] = run_time2[key]

json_filename = "../../../../result/"+data_name+"/xgboost/run_time_tuning2.json"
with open(json_filename, "w") as write_file:
    json.dump(run_time3, write_file)