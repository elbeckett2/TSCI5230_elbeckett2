# hash comments out of entire line
'''
block comment with 3
qoutes (double or single)
comments out a block

Approximate analogies, not exactly
lists in R are like dictionaries in python in the sense that you refer to things inside of them by names
vectors in R are like lists in python because you refer to their contents by position
periods are the $ of R

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
if 
