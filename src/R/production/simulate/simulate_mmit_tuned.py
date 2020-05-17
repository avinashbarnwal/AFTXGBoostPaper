
import csv,math,copy
import numpy as np
import mmit,time
from mmit import MaxMarginIntervalTree
from mmit.pruning import min_cost_complexity_pruning
from mmit.model_selection import GridSearchCV
from mmit.metrics import mean_squared_error
from mmit.model import _latex_export
from os import listdir
from os.path import exists,abspath,join
import sys
sys.path.insert(0, "/Users/avinashbarnwal/Desktop/aftXgboostPaper/src/R/production/mmit")
from mmit_predictions        import Dataset
from sklearn.model_selection import train_test_split
from utils import *
import json
import argparse
from sklearn.model_selection import KFold

path_dir = '/Users/avinashbarnwal/Desktop/aftXgboostPaper/data/simulate/'

#data  = ['simulated.linear','simulated.abs','simulated.sin']

def get_range_max_min(X,y,nature='margin'):
    if nature=='margin':
        sorted_limits = y.flatten()
        sorted_limits = sorted_limits[~np.isinf(sorted_limits)]
        sorted_limits.sort()
        range_max = sorted_limits.max() - sorted_limits.min()
        range_min = np.diff(sorted_limits)
        range_min = range_min[range_min > 0].min()
    elif nature == 'min_sample_split':
        range_min = 2
        range_max = X.shape[0]
    return range_min,range_max

def get_margin_range(range_min,range_max,n_margin_values=10):
    margin = [0.] + np.logspace(np.log10(range_min), np.log10(range_max), n_margin_values).tolist()
    return margin

def get_min_sample_split_sample(range_min,range_max,n_min_samples_split_values=10):
    min_samples_split = np.logspace(np.log10(range_min), np.log10(range_max), n_min_samples_split_values).astype(np.uint).tolist()
    return min_samples_split

def get_data(path_dir,folder_name):
    return Dataset(abspath(join(path_dir, folder_name)))

def get_train_valid_test_splits(folds, test_fold_id, inputs, labels):
    X            = inputs[folds!= test_fold_id]
    X_test       = inputs[folds == test_fold_id]
    y      = labels[folds != test_fold_id]
    y_test = labels[folds == test_fold_id]
    return X,X_test,y,y_test

def run_nested_cv(inputs, labels, folds, seed):
    fold_ids = np.unique(folds)
    accuracy = {}
    run_time = {}
    for fold_id in fold_ids:
        start = time.time() 
        X,X_test,y,y_test  \
              = get_train_valid_test_splits(folds, fold_id, inputs, labels)
        cv = mmit_fit(X,y,seed)
        y_pred = cv.predict(X_test)
        accuracy[str(fold_id)] = get_accuracy(y_pred,y_test[:,0],y_test[:,1])
        end        = time.time()
        time_taken = end - start
        run_time[str(fold_id)] = time_taken
        print(f'Time elapsed = {time_taken}')
    return accuracy,run_time
        
def mmit_fit(X,y,seed):
    range_min,range_max = get_range_max_min(X,y,nature='margin')
    margin = get_margin_range(range_min,range_max,n_margin_values=10)
    range_min,range_max = get_range_max_min(X,y,nature='min_sample_split')
    min_samples_split = get_min_sample_split_sample(range_min,range_max,n_min_samples_split_values=10)
    cv_protocol = KFold(n_splits=5, shuffle=True, random_state=seed)
    param_grid = {"margin": margin, "loss":["linear_hinge"], "max_depth":[1,2,4,6], "min_samples_split":min_samples_split}
    estimator  = MaxMarginIntervalTree()
    cv         = GridSearchCV(estimator, param_grid, cv=cv_protocol, n_jobs=-1)
    cv.fit(X, y)
    return cv

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--dataset',required=True)
    args = parser.parse_args()
    data_folder =  get_data(path_dir,args.dataset)
    inputs = data_folder.X
    labels = data_folder.y
    folds  = data_folder.folds
    result_folder = '../../../../result/simulated/'+args.dataset+'/'
    accuracy,run_time = run_nested_cv(inputs, labels, folds, 42)
    acc_file = result_folder+"accuracy_mmit_cv.json"
    run_file = result_folder+"run_mmit_cv.json"
    with open(acc_file, "w") as f:
        json.dump(accuracy,f)
    with open(run_file, "w") as f:
        json.dump(run_time,f)
if __name__ == '__main__':
    main()
