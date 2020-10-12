#!/usr/bin/env python
# coding: utf-8

# - __NOTE__ I'm lazy so I'm going to use list comphrensions to write some of the "boiler plate" code
#     - also _note_ some absue of the print() functon
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# - And just to keep things clean
# 
# ```
# 
# pandas.__version__
# 
# '1.0.3'
# 
# 
# ```

# In[1]:


import pandas as pd


# - Loading data

# _path from current location:_
# 
# ```
# "../Datasets/UCR/IMPD_UCR/"
# ```

# In[2]:


read_ucr_07_09 = [f"ucr_0{x} = pd.read_csv('../Datasets/UCR/IMPD_UCR/IMPD_UCR_200{x}_Data.csv')\n" for x in range(7,10)]
read_ucr_10_19 = [f"ucr_{x} = pd.read_csv('../Datasets/UCR/IMPD_UCR/IMPD_UCR_20{x}_Data.csv')\n" for x in range(10,19)]

read_ucrs_from_csv = read_ucr_07_09 + read_ucr_10_19


# In[3]:


read_ucrs_from_csv = "".join(read_ucrs_from_csv)


# In[4]:


print(read_ucrs_from_csv)


# In[5]:


ucr_07 = pd.read_csv('../Datasets/UCR/IMPD_UCR/IMPD_UCR_2007_Data.csv')
ucr_08 = pd.read_csv('../Datasets/UCR/IMPD_UCR/IMPD_UCR_2008_Data.csv')
ucr_09 = pd.read_csv('../Datasets/UCR/IMPD_UCR/IMPD_UCR_2009_Data.csv')
ucr_10 = pd.read_csv('../Datasets/UCR/IMPD_UCR/IMPD_UCR_2010_Data.csv')
ucr_11 = pd.read_csv('../Datasets/UCR/IMPD_UCR/IMPD_UCR_2011_Data.csv')
ucr_12 = pd.read_csv('../Datasets/UCR/IMPD_UCR/IMPD_UCR_2012_Data.csv')
ucr_13 = pd.read_csv('../Datasets/UCR/IMPD_UCR/IMPD_UCR_2013_Data.csv')
ucr_14 = pd.read_csv('../Datasets/UCR/IMPD_UCR/IMPD_UCR_2014_Data.csv')
ucr_15 = pd.read_csv('../Datasets/UCR/IMPD_UCR/IMPD_UCR_2015_Data.csv')
ucr_16 = pd.read_csv('../Datasets/UCR/IMPD_UCR/IMPD_UCR_2016_Data.csv')
ucr_17 = pd.read_csv('../Datasets/UCR/IMPD_UCR/IMPD_UCR_2017_Data.csv')
ucr_18 = pd.read_csv('../Datasets/UCR/IMPD_UCR/IMPD_UCR_2018_Data.csv')


# In[6]:


# not up to date, should not be used without explicityly declaring it's limited use case
ucr_19 = pd.read_csv('../Datasets/UCR/IMPD_UCR/IMPD_UCR_Current_Year.csv')


# - Exploring the data

# In[7]:


ucr_07.columns


# In[8]:


ucr_08.columns


# In[9]:


ucr_08.columns


# In[10]:


ucr_columns_07_09 = [f"print('ucr_0{x}: ', ucr_0{x}.columns)\n" for x in range(7,10)]
ucr_columns_10_19 = [f"print('ucr_{x}: ', ucr_{x}.columns)\n" for x in range(10,20)]
ucr_columns_07_09 = "".join(ucr_columns_07_09)
ucr_columns_10_19 = "".join(ucr_columns_10_19)
print(ucr_columns_07_09)
print(ucr_columns_10_19)


# In[11]:


print('ucr_07: ', ucr_07.columns)
print('ucr_08: ', ucr_08.columns)
print('ucr_09: ', ucr_09.columns)

print('ucr_10: ', ucr_10.columns)
print('ucr_11: ', ucr_11.columns)
print('ucr_12: ', ucr_12.columns)
print('ucr_13: ', ucr_13.columns)
print('ucr_14: ', ucr_14.columns)
print('ucr_15: ', ucr_15.columns)
print('ucr_16: ', ucr_16.columns)
print('ucr_17: ', ucr_17.columns)
print('ucr_18: ', ucr_18.columns)
print('ucr_19: ', ucr_19.columns)


# In[12]:


ucr_18


# In[13]:


# this extra column of ID seems pointless, let's remove it
ucr_16


# In[14]:


ucr_17


# In[15]:


ucr_16 = ucr_16.drop(['ID'], axis=1)

ucr_17 = ucr_17.drop(['ID'], axis=1)


# In[16]:


list_of_ucr = [f"ucr_0{x}" for x in range(8,10)] + [f"ucr_{z}" for z in range(10, 20)]


# In[17]:


list_of_ucr = ", ".join(list_of_ucr)
print(list_of_ucr)


# In[18]:


combined_ucr_07_19 = ucr_07.append([ucr_08, ucr_09, ucr_10, 
                              ucr_11, ucr_12, ucr_13, 
                              ucr_14, ucr_15, ucr_16, 
                              ucr_17, ucr_18, ucr_19], ignore_index=True)


# In[19]:


df = combined_ucr_07_19.copy()


# In[20]:


# kinda pointless at this stage
df.describe()


# In[21]:


df.isnull().sum()


# ### Dealing with date and time formatting

# - Altering the date, creating more variables for more options of seeing trends

# In[22]:


df['DATE_'] = pd.to_datetime(df.DATE_)


# In[23]:


type(df['DATE_'])


# In[24]:


df['YMD'] = df['DATE_'].dt.strftime('%Y-%m-%d')
df['YEAR'] = df['DATE_'].dt.year
df['MONTH'] = df['DATE_'].dt.month
df['DAY'] = df['DATE_'].dt.strftime('%d')
df['WEEKDAY'] = df['DATE_'].dt.strftime('%a')


# In[44]:


df.head()


# - Dealing with time

# In[65]:


df['TIME'].min(), df['TIME'].max()


# In[66]:


lowest_time = df['TIME'].min()

df.loc[df['TIME'] == lowest_time]


# In[68]:


max_time = df['TIME'].max()

df.loc[df['TIME'] == max_time]


# - Okay, this is the funnest I've had with naming functions/variables

# In[110]:


def cleaningTime(crimeTime):
    # just to ensure they're all strings
    crimeTime = str(crimeTime)
    # stripping random white space around 
    crimeTime = crimeTime.strip()
    if crimeTime > "23:59":
        return "00:00"
    elif crimeTime == ":":
        return "00:00"
    else:
        return crimeTime    


# - Testing if the function works

# In[111]:


# originally was ":"
cleaningTime(df['TIME'][25092])


# In[112]:


# originally was 99:99
cleaningTime(df['TIME'][316106])


# In[120]:


get_ipython().run_line_magic('timeit', "df['TIME'] = df['TIME'].apply(cleaningTime)")


# In[121]:


df['TIME'].min(), df['TIME'].max()


# - This sums it up for me, doesn't seem very valuable if the time is around "00:00", but we can still work with this

# In[124]:


df['TIME'].describe()


# - UCR and CRIME are somewhat correlated, but also don't fully overlap due to different record keeping each year. Also there are too many of each to quickly graph
#     - So making an abbreviated crime column based on the root of the crime and whether or not it was perpetuated _Doing this on the root, because every crime as a "root" and then branches off to more specific crimes under that "root"_

# In[27]:


df['UCR'].unique()


# In[28]:


df['CRIME'].unique()


# In[29]:


len(df['CRIME'].unique())


# In[30]:


len(df['UCR'].unique())


# - Also the UCR does not follow an overly obvious pattern grouping all crimes (ie roberry spans the 130s, 140s, 150s). So the abbriviation will be based on crime

# In[31]:


df[(df['UCR'] == 150)][0:5]


# In[32]:


df[(df['UCR'] == 145)][0:5]


# In[33]:


df[(df['UCR'] == 133)][0:5]


# - To make the abbrivation, I'll be splitting the observation/cell on the "-" that all crimes with branches have. But since not all crimes have any branches, we'll add a "-" to every obersvation (every observation, since it's faster than writing a list comprehension or if/else checking if the observation has a "-".

# In[34]:


df['CRIME'].str.contains(' - ATTEMPT', regex=False)


# In[35]:


df['ABBR_CRIME'] = df['CRIME'].astype(str) + " - "


# In[36]:


df['ABBR_CRIME'] = df['ABBR_CRIME'].str.replace(pat="\- ATTEMPT", 
                                                repl="ATTEMPT- ") # ensuring the "attempted" crimes are kept since they were not perpetuated
df['ABBR_CRIME'] = df['ABBR_CRIME'].str.split('-').str.get(0) # turning column into a list of strings, then returning only the first item in the list
df['ABBR_CRIME'] = df['ABBR_CRIME'].str.strip() # removing excess white space


# - Making a factor of the abbreviated crimes
#     - Which also doubles as a way to narrow down the "root" of the crimes, which will make it easier to graph

# In[37]:


factored_abbreviated_crime = df['ABBR_CRIME'].unique()
sorted(factored_abbreviated_crime)


# In[38]:


df['ABBR_CRIME'] = df['ABBR_CRIME'].replace(['LARC', 'AB'], 'LARCENY')


df['ABBR_CRIME'] = df['ABBR_CRIME'].replace(["BURG", "BURGLARY"],
                                                "BURGLARY")

df['ABBR_CRIME'] = df['ABBR_CRIME'].replace(["ATTEMPT STRONG ARMED ROBBER", "ATTEMPT ARMED ROBBERY", "ATTEMPT STRONG ARMED ROBBERY"], 
                                            "ROBBERY ATTEMPT")

df['ABBR_CRIME'] = df['ABBR_CRIME'].str.replace(pat="ATTEMPT VEHICLE THEFT", 
                                                repl="VEHICLE THEFT ATTEMPT")


df['ABBR_CRIME'] = df['ABBR_CRIME'].str.replace(pat="STOLEN VEHICLE", 
                                                repl="VEHICLE THEFT")

df['ABBR_CRIME'] = df['ABBR_CRIME'].str.replace(pat="AGGRAVATED ASSAULT", 
                                                repl="ASSAULT")

df['ABBR_CRIME'] = df['ABBR_CRIME'].str.replace(pat="MANSLAUGHTER BY NEG", 
                                                repl="MANSLAUGHTER BY NEGLEGENCE")


# In[39]:


factored_abbreviated_crime = df['ABBR_CRIME'].unique()
sorted(factored_abbreviated_crime)


# In[40]:


len(factored_abbreviated_crime)


# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:





# In[126]:


df


# In[140]:


def convertYtoLat(y):
    return (47 + (y / 6096 / 60))


# In[141]:


def convartXtoLon(x):
    import math
    return (-117.2 - (x / (6096 * math.cos(47.58)) / 60))


# In[142]:


y = df.copy()


# In[146]:


convartXtoLon(y['X_COORD'][1]) 


# In[145]:


convertYtoLat(y['Y_COORD'][1])


# In[ ]:


-116.70847180415454, 51.433614774715664


# In[149]:


import pyproj
import math

P = pyproj.Proj(proj='utm', zone=31, ellps='WGS84', preserve_units=True)
G = pyproj.Geod(ellps='WGS84')


# In[152]:


help(pyproj.Proj)


# In[151]:


def LatLon_To_XY(Lat,Lon):
    return P(Lat,Lon)    

def XY_To_LatLon(x,y):
    return P(x,y,inverse=True)    

def distance(Lat1, Lon1, Lat2, Lon2):
    return G.inv(Lon1, Lat1, Lon2, Lat2)[2]


# In[155]:


from pyproj import Proj
pnyc = Proj(
     proj='lcc',
     datum='NAD83',
     lat_1=39.768331,
#      lat_2=41.033333,
#      lat_0=40.166667,
     lon_0=-86.583502,
     x_0=984250.0,
     y_0=0.0)
x = [161402.77] # 161402.77 	1621638.94
y = [1621638.94]
lon, lat = pnyc(x, y, inverse=True)
lon, lat


# In[ ]:


39.7683331, -86.1583502

