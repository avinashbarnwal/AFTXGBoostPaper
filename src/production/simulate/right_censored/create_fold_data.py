import xgboost as xgb
from sklearn.model_selection import train_test_split
import numpy as np
import pandas as pd
import sys
sys.path.insert(0,'../../../../utility')
from model_utils import get_xgb_dataframe
from sklearn.model_selection import KFold
import json
import pickle

def get_train_test_split(df,test_frac=0.2):
    df_test=df.sample(frac=test_frac,random_state=200)
    df_train=df.drop(df_test.index)
    df_train.reset_index(drop=True,inplace=True)
    df_test.reset_index(drop=True,inplace=True)
    return df_train,df_test

def get_X_y(model_type='aft'):
    df = pd.read_csv('simulate_survival.csv')
    df_X = df[['x1','x2','x3','x4']]
    df_y = df['time'].tolist()
    df_status = df['status'].tolist()
    if model_type=='coxPh':
        df_y = [x if y==1 else -x for (x,y) in zip(df_y,df_status)]
        return df_X,df_y
    elif model_type=='aft':
        df_y_lower = np.array(df_y)
        df_y_upper = np.array([x if y==1 else +np.inf for (x,y) in zip(df_y,df_status)])
        df_y = [x if y==1 else -x for (x,y) in zip(df_y,df_status)]
        return df_X,df_y_lower,df_y_upper,df_y
    

def get_kfolds(X,y=None,y_lower=None,y_upper=None,model_type='aft',n_folds=5):
    SEED         = 1
    Kfolds       = KFold(n_splits=5,shuffle=True,random_state=SEED)
    input_data = {}
    input_data['data']={}
    input_data['data']['fold_data']=[]
    if model_type=='aft':
        for fold_, (trn_idx, val_idx) in enumerate(Kfolds.split(X, y_lower,y_upper)):
            tr_x,tr_y,tr_y_lower,tr_y_upper = X.iloc[trn_idx,:],y.iloc[trn_idx],y_lower.iloc[trn_idx],y_upper.iloc[trn_idx]
            vl_x,vl_y,vl_y_lower,vl_y_upper = X.iloc[val_idx,:],y.iloc[val_idx], y_lower.iloc[val_idx],y_upper.iloc[val_idx]
            fold = {}
            fold['fold']=fold_
            fold['X_train']=tr_x
            fold['y_lower_train']=tr_y_lower
            fold['y_upper_train']=tr_y_upper
            fold['y_train']=tr_y
            fold['X_val']=vl_x
            fold['y_lower_val']=vl_y_lower
            fold['y_upper_val']=vl_y_upper
            fold['y_val']=vl_y
            input_data['data']['fold_data'].append(fold)
            print(len(input_data['data']['fold_data']))
    elif model_type=='coxph':
        for fold_, (trn_idx, val_idx) in enumerate(Kfolds.split(X, y)):
            tr_x, tr_y = X.iloc[trn_idx,:],y.iloc[trn_idx]
            vl_x, vl_y = X.iloc[val_idx,:], y.iloc[val_idx]
            input_data['data']={}
            input_data['data']['fold_data']=[]
            fold['fold']=fold_
            fold['X_train']=tr_x
            fold['y_train']=tr_y
            fold['X_val']=vl_x
            fold['y_val']=vl_y
            input_data['data']['fold_data'].append(fold)
    return input_data

# fp = open(b'../data/right_censored/aft/simulated_train_input_data.pkl',"rb")  
# input_train_data = pickle.load(fp)  


def main(model_type='aft'):
    if model_type=='aft':
        df_X,df_y_lower,df_y_upper,df_y = get_X_y(model_type='aft')
        df=df_X
        df['y_lower'] = df_y_lower
        df['y_upper'] = df_y_upper
        df['y'] = df_y
        df_train,df_test = get_train_test_split(df,test_frac=0.2)
        input_train_data = get_kfolds(X=df_train[['x1','x2','x3','x4']],y=df_train['y'],y_lower=df_train['y_lower'] ,y_upper=df_train['y_lower'],model_type='aft',n_folds=5)
        input_test_data = df_test.to_json('../data/right_censored/aft/simulated_test_input_data.json',orient="columns")  
        fp = open(b'../data/right_censored/aft/simulated_train_input_data.pkl',"wb")
        pickle.dump(input_train_data,fp)
    elif model_type=='coxPh':
        df_X,df_y = get_X_y(model_type='coxPh')
        df=df_X
        df['y'] = df_y
        df_train,df_test = get_train_test_split(df,test_frac=0.2)
        input_train_data = get_kfolds(X=df_train[['x1','x2','x3','x4']],y=df_train['y'],model_type='coxPh',n_folds=5)
        input_test_data = df_test.to_json('../data/right_censored/cox_ph/simulated_test_input_data.json',orient="columns")  
        fp = open(b'../data/right_censored/cox_ph/simulated_train_input_data.pkl',"wb")
        pickle.dump(input_train_data,fp)

if __name__ == '__main__':
    main(model_type='aft')
    main(model_type='coxPh')