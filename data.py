# hash comments out of entire line
'''
block comment with 3
qoutes (double or single)
comments out a block

Approximate analogies, not exactly
lists in R are like dictionaries in python in the sense that you refer to things inside of them by names
vectors in R are like lists in python because you refer to their contents by position
periods are the $ of R

Run the below in the R console first
library(reticulate)
py_require(c('numpy','matplotlib', 'scikit-learn', 'pandas'))
repl_python()

'''

'''Load library of tools'''
import numpy as np
import math
import pandas as pd
from pathlib import Path
import matplotlib.pyplot as plt

data_location = Path("/Users/beckette/Documents/UTHSCSA/GS2/TSCI5230/kaggle_dataset1/")
output_location = Path("palpatir_output")
reference_year =2025
trend_start_year = 2023
if not data_location.is_dir():
  raise FileNotFoundError("Set data location to correct path to csv files of interest")

#get list of filenames from datasets of interest
#glob function lists all the files that meet the wildcard syntax
[csv_file.name for csv_file in data_location.glob("*.csv")]

#create data dictionary of filenames
dat = {csv_file.name:pd.read_csv(csv_file) for csv_file in data_location.glob("*.csv")}
type(dat)
dat.keys() #like names(dat) in R, BUT keys are specific to dictionaries in python
test = dat["patients.csv"].assign(BIRTHDATE=pd.to_datetime(dat["patients.csv"]["BIRTHDATE"]))
dat["patients.csv"]["BIRTHDATE"].dtype
dat["patients.csv"].dtypes

#11.18.26 in class functions and arguments
# lists inherit different functions that render the lists di
# function is a standalone item that behaves like an R function
# method is bound to a specific object, and methods available determined by object type

#Python lists do not change with (+), original list stays intact
my_list = [1,2,3,4]
my_list + my_list
my_list_3
#use.extend([] or another defined list) to change the original
my_list_3.extend(["apple","blueberry"])
my_list_3

#add a value by indicating position then value
my_list_3.insert(0,1)
my_list_3
#remove a value by indicating the value, will remove the first value matching it
my_list_3.remove(1)
my_list_3

#find position of item in list
my_list_3.index("apple")
#count number of occurrences of value
my_list_3.count("apple")

#sort list, lists always sort numbers first and then characters
my_list_3.sort(key=str)
my_list_3
my_list_3.sort(key=str,reverse=True)
my_list_3

#list comprehension
#xx is an expression
[xx for xx in my_list]
[xx+1 for xx in my_list]
[xx*2 for xx in my_list]
[pow(xx,2) for xx in my_list]
[math.sqrt(xx) for xx in my_list]

["a" for xx in my_list]

#generate a list of lists
[[xx,xx+1] for xx in my_list]

#filtering lists
[xx for xx in my_list_3 if isinstance(xx,int)]
[xx for xx in my_list_3 if isinstance(xx,str)]
[xx for xx in my_list_3 if isinstance(xx,int) and xx>2]

#data dictionary creation
{"one":1,"two":2,"three":3,"four":4}
dict(one=1,two=2,three=3,four=4)
zip(my_list_2,my_list)
dict(zip(my_list_2,my_list))

my_dict = dict(zip(my_list_2,my_list))
my_dict["three"] #need error message
my_dict.get("three") #ignore fact no such thing exists if it does not in fact exist
my_dict.keys()

[xx for xx in my_dict.values()]
my_dict.items()

#Dictionary comprension
{xx:yy for xx,yy in my_dict.items()}
{xx:yy for xx,yy in my_dict.items() if xx in ["one","four"]}
{xx:yy for xx,yy in my_dict.items() if xx.startswith("t")}
{xx:yy for xx,yy in my_dict.items() if xx.startswith("t") and yy>2}
{xx.upper():yy for xx,yy in my_dict.items() if xx.startswith("t") and yy>2}

#swap around to get numbered dictionary for character values
{yy:xx for xx,yy in my_dict.items()}

#convert dictioinary to a list
[f"{xx} is {yy}" for xx,yy in my_dict.items() if xx in ["one","four"]] #curly acts as placeholder if inside of qoutes, inside curly brackets must be a variable that exists
#if outside of expression is delineated by curly brackets you will get a dictionary, so dictionary comprehension is curly, i.e. curly gives dictionary as output
#if outside is square brackets it will give back a list regardless what is in the squarebrackets, so list comprehension is square, i.e. brackets gives a list as output



#Homework from Nov 18: use the .assign() method to convert all date columns to actual dates using pd.to_datetime()








