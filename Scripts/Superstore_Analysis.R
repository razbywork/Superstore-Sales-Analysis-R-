#Loading the Data
library(tidyverse)

data <- read.csv("Sample - Superstore.csv")

glimpse(data)
summary(data)

#Data Cleanup-Convert dates
data <- data %>%
  mutate(
    Order.Date = as.Date(Order.Date, format="%m/%d/%Y"),
    Ship.Date = as.Date(Ship.Date, format="%m/%d/%Y")
  )

#Data Cleanup-Checking for missing values
colSums(is.na(data))

#EDA-Sales by category
data %>%
  group_by(Category) %>%
  summarise(
    total_sales = sum(Sales),
    total_profit = sum(Profit)
  )

#EDA-Sales by region
data %>%
  group_by(Region) %>%
  summarise(
    sales = sum(Sales),
    profit = sum(Profit)
  )

#Visualizations-Sales by Category
ggplot(data, aes(Category, Sales)) +
  geom_boxplot()+
  labs(
    x = "Category",    # X-axis title
    y = "Sales",    # Y-axis title
    title = "Sales by Category" # Main plot title
  )

#Visualizations-Profit by Region
data %>%
  group_by(Region) %>%
  summarise(profit = sum(Profit)) %>%
  ggplot(aes(Region, profit)) +
  geom_col()+
  labs(
    x = "Region",    # X-axis title
    y = "Profit",    # Y-axis title
    title = "Profit by Region" # Main plot title
  )

#Visualizations-Relationship between Discount and Profit
ggplot(data, aes(Discount, Profit)) +
  geom_point(alpha = 0.4)+
  labs(
    x = "Discount",    # X-axis title
    y = "Profit",    # Y-axis title
    title = "Relationship between Discount and Profit" # Main plot title
  )

#Additional Analysis-Most Profitable Products
data %>%
  group_by(Product.Name) %>%
  summarise(profit = sum(Profit)) %>%
  arrange(desc(profit)) %>%
  head(10)

#Additional Analysis-Losing Products
data %>%
  group_by(Product.Name) %>%
  summarise(profit = sum(Profit)) %>%
  arrange(profit) %>%
  head(10)

#Additional Analysis-Sales Over Time
data %>%
  mutate(year = lubridate::year(Order.Date)) %>%
  group_by(year) %>%
  summarise(sales = sum(Sales)) %>%
  ggplot(aes(year, sales)) +
  geom_line()+
  labs(
    x = "Year",    # X-axis title
    y = "Sales",    # Y-axis title
    title = "Sales Over Time" # Main plot title
  )

#Additional Analysis-Most Profitable Customers
data %>%
  group_by(Customer.Name) %>%
  summarise(profit = sum(Profit)) %>%
  arrange(desc(profit))

#Advanced Analysis-Profitability by Discount
data %>%
  group_by(Discount) %>%
  summarise(avg_profit = mean(Profit))