
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


def find_datasets(path_dir):
    for d in listdir(path_dir):
        if d in ['simulated.linear','simulated.abs','simulated.sin']:
            if exists(join(path_dir, d, "features.csv")) and                     exists(join(path_dir, d, "targets.csv")) and                     exists(join(path_dir, d, "folds.csv")):
                yield Dataset(abspath(join(path_dir, d)))


def get_train_test_split(X,y,test_size=0.5,random_state=1):
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=test_size, random_state=42)
    return X_train,X_test,y_train,y_test


def mmit_fit(X,y):
    estimator = MaxMarginIntervalTree(margin=1.0, max_depth=4, loss = "linear_hinge", min_samples_split = 0)
    clf       = estimator.fit(X, y)
    return estimator

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


