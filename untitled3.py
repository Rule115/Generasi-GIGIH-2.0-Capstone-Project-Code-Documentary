# -*- coding: utf-8 -*-
"""Untitled3.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1tAToopNtIcd8odAx9dfjVoG6RRUOYUGH
"""

# Upload csv data to google colab
from google.colab import files
uploaded = files.upload()

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

import io
data = pd.read_csv(io.BytesIO(uploaded['heatmap.csv']), sep=";")

data['asi']



corr=data.corr()

plt.figure(figsize=(10,10))
heat_map = sns.heatmap(corr, annot=True)
plt.title( "Correlation between Variables" )
plt.show()