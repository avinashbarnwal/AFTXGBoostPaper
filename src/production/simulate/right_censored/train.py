import xgboost as xgb
from sklearn.model_selection import train_test_split
import numpy as np
import pandas as pd

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
    
def get_train_test_split(df,val_frac=0.2,test_frac=0.2):
    train_frac = 1-val_frac-test_frac
    df_train, df_val, df_test = \
              np.split(df.sample(frac=1, random_state=42), 
                       [int(train_frac*len(df)), int((train_frac+val_frac)*len(df))])
    return df_train,df_val,df_test

def get_xgb_dataframe(X_train,y_train=None,X_test=None,y_test=None,X_val=None,y_val=None,y_train_lower_bound=None,
                    y_train_upper_bound=None,y_test_lower_bound=None,y_test_upper_bound=None,y_val_lower_bound=None,
                    y_val_upper_bound=None,model_type='coxPh'):
    if model_type=='coxPh':
        dtrain = xgb.DMatrix(X_train, label=y_train)
        dval = xgb.DMatrix(X_val, label=y_val)
        dtest = xgb.DMatrix(X_test, label=y_test)
    if model_type=='aft':
        dtrain = xgb.DMatrix(X_train)
        dtrain.set_float_info('label_lower_bound', y_train_lower_bound)
        dtrain.set_float_info('label_upper_bound', y_train_upper_bound)
        dval = xgb.DMatrix(X_val)
        dval.set_float_info('label_lower_bound', y_val_lower_bound)
        dval.set_float_info('label_upper_bound', y_val_upper_bound)
        dtest = xgb.DMatrix(X_test)
        dtest.set_float_info('label_lower_bound', y_test_lower_bound)
        dtest.set_float_info('label_upper_bound', y_test_upper_bound)
    return dtrain,dval,dtest



def train_model(dtrain,dval,dtest,model_type='aft'):
    if model_type=='coxPh':
        params = {
        "eta": 0.002,
        "max_depth": 3,
        "objective": "survival:cox",
        "subsample": 0.5
        }
        model_train = xgb.train(params, dtrain, 10000, evals = [(dval, "test")], verbose_eval=1000)
        y_test_pred = model_train.predict(dtest)
    elif model_type=='aft':
        params = {'objective': 'survival:aft',
                  'eval_metric': 'aft-nloglik',
                  'aft_loss_distribution': 'extreme',
                  'aft_loss_distribution_scale': 1.0, 'learning_rate': 0.01, 
                  'max_depth': 5}
        model_train = xgb.train(params, dtrain, num_boost_round=10000,evals=[(dval, "test")], verbose_eval=1000)
        y_test_pred = model_train.predict(dtest)
    return y_test_pred

def main(model_type='aft'):
    if model_type=='aft':
        df_X,df_y_lower,df_y_upper,df_y = get_X_y(model_type='aft')
        df=df_X
        df['y_lower'] = df_y_lower
        df['y_upper'] = df_y_upper
        df['y'] = df_y
        df_train,df_val,df_test = get_train_test_split(df,val_frac=0.2,test_frac=0.2)
        dtrain,dval,dtest = get_xgb_dataframe(df_train[['x1','x2','x3','x4']],y_train=None,X_test=df_test[['x1','x2','x3','x4']],y_test=None,X_val=df_val[['x1','x2','x3','x4']],y_val=None,
        y_train_lower_bound=df_train['y_lower'],y_train_upper_bound=df_train['y_upper'],
        y_test_lower_bound=df_test['y_lower'],y_test_upper_bound=df_test['y_upper'],
        y_val_lower_bound=df_val['y_lower'],y_val_upper_bound=df_val['y_upper'],model_type='aft')
        y_test_pred=train_model(dtrain,dval,dtest,model_type='aft')
        c_index = c_statistic_harrell(list(y_test_pred), list(df_test['y']))
        print(c_index)
    elif model_type=='coxPh':
        df_X,df_y = get_X_y(model_type='coxPh')
        df=df_X
        df['y'] = df_y
        df_train,df_val,df_test = get_train_test_split(df,val_frac=0.2,test_frac=0.2)
        dtrain,dval,dtest = get_xgb_dataframe(df_train[['x1','x2','x3','x4']],y_train=df_train[['y']],X_test=df_test[['x1','x2','x3','x4']],y_test=df_test[['y']],X_val=df_val[['x1','x2','x3','x4']],y_val=df_val[['y']],
        y_train_lower_bound=None,y_train_upper_bound=None,
        y_test_lower_bound=None,y_test_upper_bound=None,
        y_val_lower_bound=None,y_val_upper_bound=None,model_type='coxPh')
        y_test_pred=train_model(dtrain,dval,dtest,model_type='coxPh')
        c_index = c_statistic_harrell(list(y_test_pred), list(df_test['y']))
        print(c_index)

if __name__ == '__main__':
    main(model_type='aft')
    main(model_type='coxPh')