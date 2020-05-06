import numpy as np
import pandas as pd


def set_y(y,y_type='log'):
    y = y.copy()
    if y_type=='antilog':
        y[:,0] = list(map(lambda x: np.exp(x),y[:,0]))
        y[:,1] = list(map(lambda x: np.exp(x),y[:,1]))
        return y
    elif y_type=='log':
        return y


def get_accuracy(pred,y_lower,y_higher):
    res = list(map(lambda x : x[0]>=x[1] and x[0]<=x[2],zip(pred,y_lower,y_higher)))
    accuracy = sum(res)/len(res) 
    return(accuracy)
