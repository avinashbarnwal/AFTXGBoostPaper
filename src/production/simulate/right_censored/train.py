import xgboost as xgb
from sklearn.model_selection import train_test_split
import numpy as np
import pandas as pd
import sys
sys.path.insert(0,'../../../../utility')
from model_utils import get_xgb_dataframe



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
    

def get_train_test_split(df,test_frac=0.2):
    df_train, df_test = train_test_split(df,test_frac,random_state=42)
    return df_train,df_test


def main(model_type='aft'):
    if model_type=='aft':
        df_X,df_y_lower,df_y_upper,df_y = get_X_y(model_type='aft')
        df=df_X
        df['y_lower'] = df_y_lower
        df['y_upper'] = df_y_upper
        df['y'] = df_y
        df_train,df_test = get_train_test_split(df,test_frac=0.2)
        dtrain,dtest = get_xgb_dataframe(df_train[['x1','x2','x3','x4']],y_train=None,X_test=df_test[['x1','x2','x3','x4']],y_test=None,y_train_lower_bound=df_train['y_lower'],y_train_upper_bound=df_train['y_upper'],
        y_test_lower_bound=df_test['y_lower'],y_test_upper_bound=df_test['y_upper'],model_type='aft')
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