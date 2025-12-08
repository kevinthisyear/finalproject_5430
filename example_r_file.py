# Load packages
# library(tidyverse)
import pandas as pd
# import pandas and matplotlib

# vector
nums = [1, 2, 3, 4, 5]
# list/arrays in python

# data frame
df = pd.DataFrame({
    "name": ["Alice", "Bob", "Carla"],
    "age": [25, 30, 22]
})
# pandas DataFrame

# A simple function
def square(x):
    return(x**2)
# functions in both

# Apply function in a for-loop
for i in nums:
    print(square(i))
# python for loops

# A tidyverse pipeline
df_filtered = df
df_filtered = df_filtered[df_filtered["age"]> 23]
df_filtered = df_filtered[["name", "age"]]
# pandas equivalent pipeline

## ggplot
import matplotlib.pyplot as plt
plt.hist(df_filtered["age"], bins=30)
plt.show()
# needs to be translated into matplotlib
