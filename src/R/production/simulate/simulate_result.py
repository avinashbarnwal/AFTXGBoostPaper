import json
import pandas as pd
import numpy  as np
import seaborn as sns
from matplotlib import pyplot as plt
plt.autoscale()

path = "/Users/avinashbarnwal/Desktop/aftXgboostPaper/result/simulated/"

def get_accuracy_data(model_res,name_model):
    file_name = path+model_res
    with open(file_name,'r') as f:
        result = json.load(f)
    df_accuracy = pd.DataFrame(result,index=[0]).T.reset_index()
    n = df_accuracy.shape[0]
    df_accuracy['model'] = np.repeat(name_model,n)
    df_accuracy.columns= ['Data','accuracy','model']
    return df_accuracy


model_res  = "accuracy_xgb.json"
df_acc_xgb = get_accuracy_data(model_res,'xgboost')
model_res  = "accuracy_mmit.json"
df_acc_mmit = get_accuracy_data(model_res,'mmit')
model_res  = "accuracy_survreg.JSON"
df_acc_survreg = get_accuracy_data(model_res,'survival')
model_res  = "accuracy_intervalCV.JSON"
df_acc_intervalCV = get_accuracy_data(model_res,'intervalCV')


df_accuracy = pd.concat([df_acc_xgb,df_acc_mmit,df_acc_survreg,df_acc_intervalCV],axis=0)
g = sns.scatterplot(data=df_accuracy, x='accuracy', y='model', hue='Data')
plt.legend(loc='lower left')
plt.savefig(path+"output.png",bbox_inches='tight')
plt.close()