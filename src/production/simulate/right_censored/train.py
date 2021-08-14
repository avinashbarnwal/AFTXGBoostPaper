import xgboost as xgb
from sklearn.model_selection import train_test_split
import numpy as np
import pandas as pd
import sys
sys.path.insert(0,'../../../../utility')
from model_utils import get_xgb_dataframe
from sklearn.model_selection import KFold
import pickle
import json

def get_data(model_type='aft'):
    if model_type=='aft':
        fp = open(b'../data/right_censored/aft/simulated_train_input_data.pkl',"rb")  
        input_train_data = pickle.load(fp)
        input_train_data['data']['fold_data']
        X = pd.concat([input_train_data['data']['fold_data'][0]['X_train'],input_train_data['data']['fold_data'][0]['X_val']],axis=0)
        y_lower = pd.concat([input_train_data['data']['fold_data'][0]['y_lower_train'],input_train_data['data']['fold_data'][0]['y_lower_val']],axis=0)
        y_upper = pd.concat([input_train_data['data']['fold_data'][0]['y_upper_train'],input_train_data['data']['fold_data'][0]['y_upper_val']],axis=0)
        y = pd.concat([input_train_data['data']['fold_data'][0]['y_train'],input_train_data['data']['fold_data'][0]['y_val']],axis=0)
        df_train = pd.concat([X,y_lower,y_upper,y_lower,y],axis=1)
        with open('../data/right_censored/aft/simulated_test_input_data.json','r') as fp:
            input_test_data = json.load(fp)
        df_test = pd.DataFrame(input_test_data)
        return df_train,df_test


def main(model_type='aft'):
    if model_type=='aft':
        dtrain,dtest = get_xgb_dataframe(df_train[['x1','x2','x3','x4']],y_train=df_train[['y']],X_test=df_test[['x1','x2','x3','x4']],y_test=df_test[['y']],
        y_train_lower_bound=None,y_train_upper_bound=None,y_test_lower_bound=None,y_test_upper_bound=None,model_type='coxPh')
        y_test_pred=train_model(dtrain,dtest,model_type='aft')
        c_index = c_statistic_harrell(list(y_test_pred), list(df_test['y']))
        print(c_index)
    elif model_type=='coxPh':
        df_X,df_y = get_X_y(model_type='coxPh')
        df=df_X
        df['y'] = df_y
        df_train,df_test = get_train_test_split(df,test_frac=0.2)
        dtrain,dtest = get_xgb_dataframe(df_train[['x1','x2','x3','x4']],y_train=df_train[['y']],X_test=df_test[['x1','x2','x3','x4']],y_test=df_test[['y']],
        y_train_lower_bound=None,y_train_upper_bound=None,y_test_lower_bound=None,y_test_upper_bound=None,model_type='coxPh')
        y_test_pred=train_model(dtrain,dtest,model_type='coxPh')
        c_index = c_statistic_harrell(list(y_test_pred), list(df_test['y']))
        print(c_index)

if __name__ == '__main__':
    main(model_type='aft')
    main(model_type='coxPh')