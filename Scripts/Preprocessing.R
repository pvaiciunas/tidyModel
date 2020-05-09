# BAsed off of:
# 
# 
# 
# Includes own annotations and augmentations to the code.

library(tidyverse)
library(tidymodels)
library(nycflights13)    # for flight data
library(skimr)           # for variable summaries


# The parsnip package was used as a wrapper for models. Get everything on the same page
# and allows for consistent analysi of output and a standardization of input.
# 
# Speaking of input, data cleaning is a big part of analysis. To preprocess the 
# data in a standardized way, we will use the 'recipes' package of the tidymodels-verse.
# 
# Examples of what you can do:
#  converting qualitative predictors to indicator variables (also known as dummy variables),
#  transforming data to be on a different scale (e.g., taking the logarithm of a variable),
#  transforming whole groups of predictors together,
#  extracting key features from raw variables (e.g., getting the day of the week out of a date variable),


# Load the data -----------------------------------------------------------


set.seed(123)

flight_data <- 
  flights %>% 
  mutate(
    # convert the arrival delay to a factor
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    date = as.Date(time_hour)
  ) %>% 
  # Include the weather data
  inner_join(weather, by = c("origin", "time_hour")) %>% 
  # Only retain the specific columns we will use
  select(dep_time, flight, origin, dest, air_time, distance, 
         carrier, date, arr_delay, time_hour) %>% 
  # Exclude missing data
  na.omit() %>% 
  # For creating models, it is better to have qualitative columns
  # encoded as factors (instead of character strings)
  mutate_if(is.character, as.factor)



# Examine the Data --------------------------------------------------------

# you can see thatabout 16% of the flights are delayed
flight_data %>% 
  count(arr_delay) %>% 
  mutate(prop = n/sum(n))


# Now we'll take a quick look at the variables we will be working with.
# 
# Note that the arr_delay variable is a factor. For logistic regression
# its important that this variable is a factor.
glimpse(flight_data)

# There are going to be two varibles not used in the prediction model. But we'll
# want to hld on to them for post-model analysis and identfying any egregiously
# incorrect prediciotns. These are 'flight', a numeric value, and 'time_hour', a
# date-time value
#
# In addition, we'll want to do some work on the 'dest' and carrier' variables
skim(flight_data, dest, carrier)

# These are factors, and have 104 and 16 unique values respectively. Since this 
# will be something for a simple logistic regression, we'll need to convert to 
# dummy variables. BUT given that there are so many unqiue vlaues, we'll have to treat 
# them a little differently later.


# Data Splitting ----------------------------------------------------------

# We'll use the rsampel package to split the data into trainign and testing.
# 3/4s of the data will go into the training set.
set.seed(555)

data_split <- initial_split(flight_data, prop = 3/4)

# Create data frames for the two sets
train_data <- training(data_split)
test_data <- testing(data_split)



# Creating Recipe and Rolls -----------------------------------------------

# We cna use the concept of recipes to create a few new predictors and 
# do some preprocessing
flights_rec <- recipte(arr_delay ~ ., data = train_data)

# this is just like the formlua format used in things like lm()
# The '.' denotes 'all data', and the left hand side of the 
# formula denotes the outcome variable. You specificy the data
# set with the 'data' assignemnt

# Now we can add roles to this recipe. We can use the update_role() function to
# let recipes know that flight and time_hour are variables with a custom role
# that we called "ID" (a role can have any character value). Whereas our formula
# included all variables in the training set other than arr_delay as predictors,
# this tells the recipe to keep these two variables but not use them as either
# outcomes or predictors.
flights_rec <- recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID")

# This step of adding roles to a recipe is optional; the purpose of using it
# here is that those two variables can be retained in the data but not included
# in the model. This can be convenient when, after the model is fit, we want to
# investigate some poorly predicted value. These ID columns will be available
# and can be used to try to understand what went wrong.

summ
