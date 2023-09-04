# Libraries ----
# Loading the necessary libraries. I prefer to keep all libraries loaded up here, instead of
# spreading them out across the code.

library(tidyverse)    # Allows for easier data cleaning and tidying.
library(lubridate)    # Permits working with date-formatted variables.
library(modelsummary) # For presentable, good tables which are customizable.
library(kableExtra)   # Added customization for tables.
library(estimatr)     # To make models with clustered standard errors.
library(forecast)     # For ARIMA models to forecast.

# Data ----
# I have three separate .csv files for this particular report. The first order of business is
# to go through them, make adjustments, clean the data and if needed, merge them by their
# common columns to make future processes easier and simpler.

data1 <- read_csv("~/GitHub/retailsales/features.csv") %>% 
  # Convert from character to date format.
  mutate(., Date = as.Date(Date, "%d/%m/%Y"),
  # Convert from number to factor. We want to do this because the number here doesn't really
  # represent a number. It's a representation of a store "name," and therefore, it should
  # not be treated as a numerical value within the dataset.
         Store = as_factor(Store))


data2 <- read_csv("~/GitHub/retailsales/sales.csv") %>% 
  # Convert from character to date format.
  mutate(., Date = as.Date(Date, "%d/%m/%Y"), 
         Store = as_factor(Store),
  # This is due to the same reason as "Store." We do not want
  # to conceptualize "Dept" as a numerical value since it is
  # not meant to be one.
         Dept = as_factor(Dept))

data3 <- read_csv("~/GitHub/retailsales/stores.csv") %>% 
  mutate(., Store = as_factor(Store),
         # As far as I know, "character" types are functionally
         # similar to "factor" type. However, consistency in types
         # may spare me headaches later on.
         Type = as_factor(Type)) 

# What do we want to learn from this data? The response to this question will motivate how we
# proceed not only with the models, but also whether we want to merge any of the datasets we
# have at hand.

df0 <- list(data1, data2) %>%
  reduce(left_join, by = c("Store", "Date", "IsHoliday"))

df1 <- list(df0, data3) %>% 
  reduce(left_join, by = "Store") %>%
  drop_na(., Weekly_Sales)

# Once we have the data, it may be beneficial for us to understand the nature of the data
# we are dealing with. How many observations are there? What is the minimum number of sales?
# What is the average? Etc.

datasummary(((`Weekly Sales` = `Weekly_Sales`) + (`Fuel Price` = `Fuel_Price`) +
               CPI + Unemployment + Temperature) ~
              (Min + Max + Mean + Median + SD + (`Observations` = N)),
              data = df1,
            output = 'kableExtra')

# Modeling ----

# Which factors influence sales the most, and how? We can make some models to get at this
# question. This model will also permit us to forecast future sales.

lm1 <- lm_robust(Weekly_Sales ~ Temperature + CPI + Unemployment + Fuel_Price + IsHoliday + Date,
                 data = df1,
                 clusters = Store,
                 # Clustering by store is really, really, really, really important.
                 # This is because stores, in all likelihood, are independent of each other.
                 # In other words, a sale made in Store 1 probably has nothing to do with
                 # a sale made in Store 6. Therefore, we want to cluster the standard errors
                 # of our model by store to be able to see effects. If we do not do this,
                 # then—due to the large amount of observations—every coefficient will
                 # be statistically significant, when that isn't really the case.
                 se_type = "stata" # Necessary due to bug in `estimatr` package to avoid a crash.
                 )

modelsummary(lm1,
             output = 'kableExtra',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = c('Temperature' = 'Temperature',
                          'CPI' = 'Inflation index',
                          'Unemployment' = 'Unemployment',
                          'Fuel_Price' = 'Fuel Price',
                          'IsHolidayTRUE' = 'Holiday',
                          'Date' = 'Date',
                          '(Intercept)' = 'Constant'),
             gof_omit = 'AIC|BIC|RMSE'
             )

# Forecasting ----

# The model `lm1` above is not a good model, with an R-squared of merely 0.002. With real
# data, we would likely see far better goodness-of-fit numbers. Alternatively, the factors
# we are thinking of (e.g., inflation, fuel price, whether it's a holiday or not, etc.)
# are just poor predictors of weekly sale numbers.
# However, for the purpose of this exercise, I will assume that the model `lm1` does an 
# acceptable job at explaining the variation in our weekly sale numbers, and therefore can
# be used to forecast future sales.

# First, let's look at current sales over time.
# How do sales numbers fluctuate with time?

totalsales <- df1 %>% 
  filter(., Weekly_Sales > 0) %>% 
  group_by(., Date) %>% 
  reframe(., totalsales = sum(Weekly_Sales, na.rm = TRUE))

p1 <- totalsales %>% 
  ggplot(., aes(x = Date,
                y = totalsales)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "3 months", date_labels = "%m/%Y") +
  labs(x = "Date",
       y = "Total Sales ($)",
       title = "All sales over time") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 60, vjust = 0.6, hjust = 0.5))
p1

arima1 <- totalsales %>% 
  select(., -"Date") %>% 
  ts(., start = c(2010, 2), end = c(2012, 10), frequency = 12) %>% 
  auto.arima(., seasonal = TRUE) # This does default TRUE, but I wanted to specify it.

forecast1 <- forecast(arima1)

plot(forecast1)
