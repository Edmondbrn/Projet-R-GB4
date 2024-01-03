import pandas as pd 
import seaborn as sb
import matplotlib.pyplot as plt

df = pd.read_csv("LC-Adductomics.csv")
print(df)
sb.scatterplot(df["Albumin adduct of Nacetylcysteine"])
plt.show()