# Libraries ----
# Loading the necessary libraries. I prefer to keep all libraries loaded up here, instead of
# spreading them out across the code.

library(tidyverse)    # Allows for easier data cleaning and tidying.
library(lubridate)    # Permits working with date-formatted variables.
library(modelsummary) # For presentable, good tables which are customizable.
library(kableExtra)   # Added customization for tables.
library(estimatr)     # To make models with clustered standard errors.

# Data ----
# I have three separate .csv files for this particular report. The first order of business is
# to go through them, make adjustments, clean the data and if needed, merge them by their
# common columns to make future processes easier and simpler.

data1 <- read_csv("~/GitHub/retailsales/features.csv") %>% 
  mutate(., Date = as.Date(Date, "%d/%m/%Y"), # Convert from character to date format.
         Store = as_factor(Store)) # Convert from number to factor. We want to do this because
                                   # the number here doesn't really represent a number. It's a
                                   # representation of a store "name," and therefore, it should
                                   # not be treated as a numerical value within the dataset.

data2 <- read_csv("~/GitHub/retailsales/sales.csv") %>% 
  mutate(., Date = as.Date(Date, "%d/%m/%Y"), # Convert from character to date format.
         Store = as_factor(Store),
         Dept = as_factor(Dept)) # This is due to the same reason as "Store." We do not want
                                 # to conceptualize "Dept" as a numerical value since it is
                                 # not meant to be one.

data3 <- read_csv("~/GitHub/retailsales/stores.csv") %>% 
  mutate(., Store = as_factor(Store),
         Type = as_factor(Type)) # As far as I know, "character" types are functionally
                                 # similar to "factor" type. However, consistency in types
                                 # may spare me headaches later on.

# What do we want to learn from this data? The response to this question will motivate how we
# proceed not only with the models, but also whether we want to merge any of the datasets we
# have at hand.

df1 <- list(data1, data2) %>%
  reduce(left_join, by = c("Store", "Date", "IsHoliday")) %>% 
  drop_na(Weekly_Sales) # Remove columns with NA in weekly sales.

# Once we have the data, it may be beneficial for us to understand the nature of the data
# we are dealing with. How many observations are there? What is the minimum number of sales?
# What is the average? Etc.

datasummary(((`Weekly Sales` = `Weekly_Sales`) + (`Fuel Price` = `Fuel_Price`) +
               CPI + Unemployment + Temperature) ~
              (Min + Max + Mean + Median + SD + (`Observations` = N)),
              data = df1,
            output = 'kableExtra')

# How do sales numbers fluctuate with time?

df1 %>% 
  group_by(., Date) %>% 
  reframe(., totalsales = sum(Weekly_Sales, na.rm = TRUE)) %>% 
  ggplot(., aes(x = Date,
                y = totalsales)) +
  geom_line() +
  geom_smooth(method = 'lm', se = F) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(limits = c(min(df1$Date), max(df1$Date)),
               date_breaks = "3 months", date_labels = "%m/%Y") +
  labs(x = "Date",
       y = "Total Sales ($)",
       title = "All sales over time") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 60, vjust = 0.6, hjust = 0.5))
  

# Modeling ----

# At first, for instance, we may simply be curious about the relationship between temperatures
# and sales. Do people shop more on days which are cooler? Hotter? In order to do this, merging
# is necessary.

# First, a visualization of what the distribution of weekly sales looks like.
ggplot(data = df1,
       aes(x = Date,
           y = Temperature)) +
  geom_point()

lm1 <- lm(Weekly_Sales ~ Temperature,
          data = df1)

# This is a really simple model. We're simply looking at the relationship between weekly sales
# and temperature and nothing more.

modelsummary(lm1, stars = TRUE)


