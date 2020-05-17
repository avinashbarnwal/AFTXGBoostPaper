import numpy as np
import pandas as pd
import json
import matplotlib.pyplot as plt
%matplotlib inline
from   matplotlib.ticker import StrMethodFormatter
from   plotnine import *


ylim = [0, 1]
result_folder   = '../../../../result/simulated/'
df_scatter_plts = pd.DataFrame()
for dataset_id, dataset in enumerate(['simulated.abs','simulated.sin','simulated.linear']):
    # ax = axs[idx%3]
    idx += 1
    for dist_id, dist in enumerate(['normal', 'logistic', 'extreme']):
        for fold in [1, 2, 3, 4, 5]:
            with open(result_folder+f'{dataset}/{dist}-fold{fold}.json', 'r') as f:
                obj = json.load(f)
            df_acc = pd.DataFrame([{'Fold':'Fold '+str(fold),'Objective':'Accuracy','Parameter':obj['final_accuracy'][1],'Model':'xgb-'+dist,'Data':dataset}])
            df_run = pd.DataFrame([{'Fold':'Fold '+str(fold),'Objective':'Run Time','Parameter':obj['timestamp'][-1],'Model':'xgb-'+dist,'Data':dataset}])
            df_scatter_plts = pd.concat([df_scatter_plts,df_acc,df_run],axis=0)
            
    with open(result_folder+f'{dataset}/accuracy_intervalCV.JSON', 'r') as f:
        obj = json.load(f)
        for fold in [1, 2, 3, 4, 5]:
            df_acc = pd.DataFrame([{'Fold':'Fold '+str(fold),'Objective':'Accuracy','Parameter':obj[str(fold)],'Model':'intervalCV','Data':dataset}])
            df_scatter_plts = pd.concat([df_scatter_plts,df_acc],axis=0)

    with open(result_folder+f'{dataset}/accuracy_mmit_cv.JSON', 'r') as f:
        obj = json.load(f)
        for fold in [1, 2, 3, 4, 5]:
            df_acc = pd.DataFrame([{'Fold':'Fold '+str(fold),'Objective':'Accuracy','Parameter':obj[str(fold)],'Model':'mmit','Data':dataset}])
            df_scatter_plts = pd.concat([df_scatter_plts,df_acc],axis=0)

    with open(result_folder+f'{dataset}/accuracy_survreg_tuned.JSON', 'r') as f:
        obj = json.load(f)
        for fold in [1, 2, 3, 4, 5]:
            df_acc = pd.DataFrame([{'Fold':'Fold '+str(fold),'Objective':'Accuracy','Parameter':obj[str(fold)],'Model':'survReg','Data':dataset}])
            df_scatter_plts = pd.concat([df_scatter_plts,df_acc],axis=0)

    with open(result_folder+f'{dataset}/runtime_intervalCV.JSON', 'r') as f:
        obj = json.load(f)
        for fold in [1, 2, 3, 4, 5]:
            df_run = pd.DataFrame([{'Fold':'Fold '+str(fold),'Objective':'Run Time','Parameter':obj[str(fold)],'Model':'intervalCV','Data':dataset}])
            df_scatter_plts = pd.concat([df_scatter_plts,df_run],axis=0)

    with open(result_folder+f'{dataset}/run_mmit_cv.JSON', 'r') as f:
        obj = json.load(f)
        for fold in [1, 2, 3, 4, 5]:
            df_run = pd.DataFrame([{'Fold':'Fold '+str(fold),'Objective':'Run Time','Parameter':obj[str(fold)],'Model':'mmit','Data':dataset}])
            df_scatter_plts = pd.concat([df_scatter_plts,df_run],axis=0)

    with open(result_folder+f'{dataset}/runtime_survreg_tuned.JSON', 'r') as f:
        obj = json.load(f)
        for fold in [1, 2, 3, 4, 5]:
            df_run = pd.DataFrame([{'Fold':'Fold '+str(fold),'Objective':'Run Time','Parameter':obj[str(fold)],'Model':'survReg','Data':dataset}])
            df_scatter_plts = pd.concat([df_scatter_plts,df_run],axis=0)


fig_name = result_folder+'simulated_compare_models.png'
p =  ggplot(df_scatter_plts, aes(x='Model',y='Parameter'))+ geom_point(stroke = 1,size=5,colour = "black", fill = "white")
p = p+facet_grid('Objective~Data',scales="free") + theme(axis_text_x=element_text(rotation=90, hjust=1))
p.save(fig_name,dpi=150)