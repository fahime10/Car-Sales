# Project:
# Predictive Inventory Management: Linking Demand Forecasting to 
# Optimal Stocking Strategies

install.packages("tidyverse")

library(tidyverse)
library(ggplot2)

data <- read.csv("./data/car_data.csv")

View(data)
str(data)
glimpse(data)

# Data cleaning

colSums(is.na(data))
# No nulls found

sum(duplicated(data))
# No duplicates found

data <- data %>%
  select(-Phone)
# Phone number is not useful for this project

data <- data %>%
  mutate(Date = mdy(Date))

# Exploratory Data Analysis
# Date
dim(data)

min(data$Date)
max(data$Date)
max(data$Date) - min(data$Date)
# The data covers 728 days, which is alright for forecasting

# SKUs
n_distinct(data$Company)
# 30 different companies

n_distinct(data$Model)
# 154 unique car models

data %>%
  count(Body.Style, sort = TRUE)

data %>%
  count(Transmission, sort = TRUE)

# Time Series
monthly_sales <- data %>%
  mutate(Month_Year = floor_date(Date, "month")) %>%
  group_by(Month_Year) %>%
  summarise(Total_Units_Sold = n())

ggplot(monthly_sales, aes(x = Month_Year, y = Total_Units_Sold)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  scale_x_date(
    date_breaks = "2 month",
    date_labels = "%b %Y"
  ) +
  labs(
    title = "Monthly Car Demand",
    x = "Timeline",
    y = "Units sold"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
    plot.margin = margin(t = 10, r = 50, b = 10, l = 10)
  )

# Best selling car body style
ggplot(data, aes(x = reorder(Body.Style, Body.Style, function(x)-length(x)))) +
  geom_bar(fill = "coral", color = "black") +
  labs(
    title = "Total Sales Volume by Vehicle Body Style",
    x = "Body Style",
    y = "Units sold"
  ) +
  theme_minimal()