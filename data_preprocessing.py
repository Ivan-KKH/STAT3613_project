# %%
import collections

# 1: NTE, 2: NTW 3:KL 4: HK Island
import numpy as np
import pandas as pd

def convertpref(i):
    if i == 1:
        return 5
    elif i == 2:
        return 4
    elif i == 3:
        return 3
    elif i == 4:
        return 2
    elif i == 5:
        return 1

data = pd.read_csv("raw_nov30.csv")

data = data.drop(data[data['Finished'] == '0'].index)
data_col_name = ['Gender',	'Year of Study',	'Monthly Expense',	'Living District',	'Transportation',	'Num_transfer',	'Time',	'Comfortability',	'Price',	'Walking Distance',	'Acceptable price',	'MTR',	'Bus',	'Shuttle Bus',	'Taxi',	'Tram']
data = data.drop([0, 1], axis = 0)
data = data.iloc[:,18:]
data = data.drop(['Q7_5_TEXT', 'Q7_5','Q5_6_TEXT'], axis= 1)
# drop non-binary gender/ prefer not to say
data = data[(data['Q1']  == '1') | (data['Q1'] == '2')]
data = data.reset_index()
data = data.drop(['index'], axis = 1)

CA_raw = data.loc[:, 'Q8_1_1': 'Q8_5_6']
mdsdata = data.loc[:, "Q11_1" : "Q11_5"]

# %%
data = data.drop(data.loc[:, 'Q8_1_1': 'Q8_5_6'], axis = 1)
data.columns= data_col_name


# group all responses for over $6000 monthly expense
# Group MTR and non-MTR
# Group more or equal to 3 transfers
data = data.replace({"Year of Study": {"1": "3", "2": "3"},  'Monthly Expense': {'4':'3', '5': '3', '6':'3'}, 'Transportation': {'3':'2', '4':'2', '5':'2', '6':'2'}, 'Num_transfer': {'4': '3', '5': '3'}})
data.loc[:, "Time": "Walking Distance"] = data.loc[:, "Transportation" : "Walking Distance"].astype(int).applymap(convertpref)
data = data.drop(data.loc[:, "MTR": "Tram"], axis = 1)

CA_raw = CA_raw.fillna(0)
CA_raw = CA_raw.astype(int)
count = CA_raw.sum()
CA = np.zeros((5, 6))

for i, val  in enumerate(count):
    CA[i//6, i%6] = int(val)
CA = pd.DataFrame(CA)
CA.columns = ['Fast', 'Con', 'Seat', 'Transfer', 'Price', 'Walk']
CA.index = ['MTR', 'Bus', 'Shttle', 'Taxi', 'Tram']


mdsdata.columns = ['MTR',	'Bus',	'Shuttle Bus',	'Taxi',	'Tram']
mdsdata = mdsdata.T
mdsdata = mdsdata.apply(pd.to_numeric)
mdsdata = mdsdata.applymap(convertpref)





# %%
data.to_csv('data.csv')
CA.to_csv('CA.csv')
mdsdata.to_csv("MDS.csv")



# %%
NT_E = data[data["Living District"] == '1']["Acceptable price"].astype("float")
NT_W = data[data["Living District"] == '2']["Acceptable price"].astype("float")
KL = data[data["Living District"] == '3']["Acceptable price"].astype("float")
HKI = data[data["Living District"] == '4']["Acceptable price"].astype("float")


# %%

# NTE
def get_cumulative(lst):
    lst = sorted(lst)
    lst_val = sorted(np.unique(lst))
    freq = collections.Counter(lst)
    Cumu = []
    for i in lst_val:
        if Cumu == []:
            Cumu.append(freq[i])
        else:
            Cumu.append(freq[i] + Cumu[-1]) 
    Cumu = len(lst) - np.array(Cumu) + 1
    return pd.DataFrame(list(zip(lst_val, Cumu)), columns = ["price", "sales"])    


NTE_sales = get_cumulative(NT_E)
NTE_sales.to_csv("NTE_sales.csv", index = None)

# NTW
NTW_sales = get_cumulative(NT_W)
NTW_sales.to_csv("NTW_sales.csv", index = None)

# KL
KL_sales = get_cumulative(KL)
KL_sales.to_csv("KL_sales.csv", index = None)

# HKI
HKI_sales = get_cumulative(HKI)
HKI_sales.to_csv("HKI_sales.csv", index = None)


# %%
print(np.median(NT_E))
print(np.median(NT_W))
print(np.median(KL))
print(np.median(HKI))



# %%
np.median(data[data["Shuttle Bus"] == "1"]["Acceptable price"].astype("float"))
# %%

# %%

# %%



data

# %%
