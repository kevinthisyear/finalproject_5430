# Load packages
library(tidyverse)
# import pandas and matplotlib 

# vector
nums <- c(1, 2, 3, 4, 5)
# list/arrays in python

# data frame
df <- data.frame(
  name = c("Alice", "Bob", "Carla"),
  age = c(25, 30, 22)
)
# pandas DataFrame

# A simple function
square <- function(x) {
  return(x^2)
}
# functions in both

# Apply function in a for-loop
for (i in nums) {
  print(square(i))
}
# python for loops

# A tidyverse pipeline
df_filtered <- df %>%
  filter(age > 23) %>%
  select(name, age)
# pandas equivalent pipeline

## ggplot 
ggplot(df_filtered, aes(x = age)) +
  geom_histogram()
# needs to be translated into matplotlib
