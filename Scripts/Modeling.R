
# BAsed off of:
# https://www.tidymodels.org/start/models/
# 
# Includes own annotations and augmentations to the code.

library(tidyverse)
library(tidymodels)
library(readr)


# First example is based off of sea urching growth data.


# Load Data ---------------------------------------------------------------

urchins <-
  # Data were assembled for a tutorial 
  # at https://www.flutterbys.com.au/stats/tut/tut7.5a.html
  read_csv("https://tidymodels.org/start/models/urchins.csv") %>% 
  # Change the names to be a little more verbose
  setNames(c("food_regime", "initial_volume", "width")) %>% 
  # Factors are very helpful for modeling, so we convert one column
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))
#> Parsed with column specification:
#> cols(
#>   TREAT = col_character(),
#>   IV = col_double(),
#>   SUTW = col_double()
#> )


str(urchins)



# Examine the data --------------------------------------------------------

ggplot(urchins,
       aes(x = initial_volume, 
           y = width, 
           group = food_regime, 
           col = food_regime)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  scale_color_viridis_d(option = "plasma", end = .7)



# Model the data ----------------------------------------------------------

# There's a categorical and a continuous variable here, so a two-way analysis of
# variance semes like a good idea..
# 
# Since the slopes appear different for atleast two of the feeding regimes, we'll
# want to build a model that's good for two way interactions. i.e. :
#   width ~ initial_volume * food_regime
 
# Specifying the model
# tidymodels starts with specifying the functional form of the model using the
# parsnip package. Since there is a numeric outcome, and the model shold be
# linear with slopes and intercepts, the model type is 'linear regression'. This
# will be denoted as 'linear_reg()'
#
# Once the functional form is chosen, you can then choose the engine that will
# be performing the driving of the model. For linear regression, we'll be using
# the 'lm' package, but for things like random forest etc, we'll need to choose
# other engines like the 'rf' package or something.
# 
# The help pages for the funcitonal forms (i.e. linear_reg()) will list what
# engines are allowed
#
# First w'll create the model object
lm_mod <- linear_reg() %>% 
  set_engine("lm")

# What other engines can we use?
?linear_reg()

# Now you can fit the model
lm_fit <- lm_mod %>% 
  fit(width ~ initial_volume * food_regime, data = urchins)

# Look at the ouput
lm_fit

# This is messy, we can improve with the 'tidy()' function
tidy(lm_fit)


# Predicting values -------------------------------------------------------

# Parsnip has their own predict wrappers that standardize things across engines. When
# switching types of models, this makes things a lot easier
new_points <- expand.grid(initial_volume = 20,
                          food_regime = c("Initial", "Low", "High"))
new_points

mean_pred <- predict(lm_fit, new_data = new_points)
mean_pred

# When making predictions, the tidymodels convention is to always produce a 
# tibble of results with standardized column names. This makes it esay to combine
# original data and the predictions in a useable format
conf_int_pred <- predict(lm_fit, 
                         new_data = new_points,
                         type = "conf_int")
conf_int_pred

# now combine:
plot_data <- 
  new_points %>% 
  bind_cols(mean_pred) %>% 
  bind_cols(conf_int_pred)

# and plot
ggplot(plot_data, aes(x = food_regime)) +
  geom_point(aes(y = .pred)) +
  geom_errorbar(aes(ymin = .pred_lower,
                    ymax = .pred_upper),
                width = 0.2) +
  labs(y = "urchin size")



# Modeling with a new engine ----------------------------------------------


