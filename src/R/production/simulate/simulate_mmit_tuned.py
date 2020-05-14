
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

def preprocess_data(labels):
    labels = labels.copy()
    labels[:,0] = list(map(lambda x: np.exp(x),labels[:,0]))
    labels[:,1] = list(map(lambda x: np.exp(x),labels[:,1]))
    return labels


def get_train_valid_test_splits(folds, test_fold_id, inputs, labels, kfold_gen):

    # Split data into train and test
    X            = inputs[folds!= test_fold_id]
    X_test       = inputs[folds == test_fold_id]
    y_label      = labels[folds != test_fold_id]
    y_label_test = labels[folds == test_fold_id]
    return X,X_test,y_label,y_label_test



def mmit_fit(X,y):
    range_min,range_max = get_range_max_min(X,y,nature='margin')
    margin = get_margin_range(range_min,range_max,n_margin_values=10)
    range_min,range_max = get_range_max_min(X,y,nature='min_sample_split')
    min_samples_split = get_min_sample_split_sample(range_min,range_max,n_min_samples_split_values=10)
    cv_protocol = KFold(n_splits=5, shuffle=True, random_state=42)
    param_grid = {"margin": margin, "loss":["linear_hinge"], "max_depth":[1,2,4,6], "min_samples_split":min_samples_split}
    estimator  = MaxMarginIntervalTree()
    cv         = GridSearchCV(estimator, param_grid, cv=cv_protocol, n_jobs=-1)
    cv.fit(X, y)
    return cv

def mmit_predict(estimator,X):
    pred = estimator.predict(X)
    return pred

accuracy = {}


for d in find_datasets(path_dir):
    X = d.X
    y = d.y
    y = set_y(y,y_type='log')
    X_train,X_test,y_train,y_test = get_train_test_split(X,y,test_size=0.5,random_state=1)
    estimator = mmit_fit(X_train,y_train)
    pred = mmit_predict(estimator,X_test)
    accuracy[str(d.name)] = get_accuracy(pred,y_test[:,0],y_test[:,1])


res_path = "/Users/avinashbarnwal/Desktop/aftXgboostPaper/result/simulated/accuracy_mmit.json"

with open(res_path, "w") as f:
    json.dump(accuracy,f)


# %%
#file = open(str(i)+".tex", 'w')
#file.write( _latex_export(estimator))
#file.close()
"""
alphas, pruned_trees = min_cost_complexity_pruning(estimator)
print alphas
for pt in pruned_trees:
    print sorted(pt.tree_.rules)
	 
param_grid =  {"margin": [0.0, 2.0], "loss":["linear_hinge"], "max_depth":[np.infty], "min_samples_split":[0]}
search = GridSearchCV(estimator, param_grid)
search.fit(x,y)
print search.cv_results_
"""


