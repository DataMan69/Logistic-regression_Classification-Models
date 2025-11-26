import pandas as pd
import statsmodels.api as sm

file_path = r"D:\Indian Veterinary Research Institute M.Sc. Agrcultural Statistics\Thesis Work\Interviews\Collapsed_CSV - Copy.csv"
data = pd.read_csv(file_path)

X = data[[
    "Age (years)", "Farm Experience (Years)", "Experience in rearing of Goats (Years)",
    "No of Schooling Years", "Gender", "PO_Farming", "PO_Goat Farming",
    "SO_Farming", "SO_Goat Farming", "Dwelling Structure_Semi-Pucca",
    "Dwelling Structure_Pucca", "Household Size", "Land holding size (Ha)",
    "Herd_Size", "Mkt_Distance_KM", "AHS_Distance_KM"
]]
y = data["treatment status"]

X = sm.add_constant(X)
model = sm.Logit(y, X).fit(disp=0)
summary = model.summary2().tables[1]
summary['Significant'] = summary['P>|z|'] < 0.05
print(summary)
