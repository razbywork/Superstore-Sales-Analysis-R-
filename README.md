# 📊 Superstore Sales Analysis (R + Power BI)

## 📌 Project Overview
This project analyzes a retail dataset containing sales transactions from a fictional Superstore in the United States.  
The goal of the analysis is to explore **sales performance, profitability drivers, and customer behavior** using **R for data analysis** and **Power BI for interactive dashboard visualization**.

The project includes:

- Data cleaning and preparation in R
- Exploratory Data Analysis (EDA)
- Statistical and visual insights
- Business-oriented conclusions
- Interactive Power BI dashboard

This project demonstrates an **end-to-end data analytics workflow**, from raw data exploration to a business-ready dashboard.

---

# 📂 Dataset

The dataset contains **9,994 sales transactions** and **21 variables**, including:

- Order information (Order Date, Ship Date, Shipping Mode)
- Customer information (Customer ID, Name, Segment)
- Geographic information (City, State, Region)
- Product information (Category, Sub-Category, Product Name)
- Sales metrics (Sales, Quantity, Discount, Profit)

---

## 🌐 Link to Dataset
https://www.kaggle.com/datasets/vivek468/superstore-dataset-final

---

# 🛠 Tools & Technologies

### Data Analysis
- R
- tidyverse
- ggplot2
- dplyr
- lubridate

### Dashboard & Visualization
- Power BI Desktop

---

# 🧹 Data Preparation

Before analysis, several preprocessing steps were performed:

- Converted `Order.Date` and `Ship.Date` into Date format
- Checked for missing values
- Verified dataset structure
- Created time-based aggregations (Year, Month)

✅ No missing values were found in the dataset.

---

# 📊 Exploratory Data Analysis

## Sales and Profit by Category

The dataset contains three main product categories:

- Furniture  
- Office Supplies  
- Technology  

### Key Findings

- **Technology generates the highest total sales and profit**
- **Office Supplies produces strong profits despite lower sales**
- **Furniture generates high sales but low profitability**

This suggests **lower profit margins in Furniture**.

![Sales by Category](Plots/Sales_by_Category.png)

---

# 🌎 Regional Profitability

Profitability varies significantly across regions.

### Findings

| Region | Profit Ranking |
|--------|----------------|
| West   | Highest |
| East   | Second |
| South  | Moderate |
| Central| Lowest |

![Profit by Region](Plots/Profit_by_Region.png)

The **West region leads in profitability**, indicating stronger market performance.

---

# 💸 Discount Impact on Profit

### Findings

- Small discounts (0–10%) remain profitable
- Discounts above ~30% generate losses
- Strong negative correlation between discount and profit

![Discount vs Profit](Plots/Relationship_between_Discount_and_Profit.png)

This indicates **over-discounting reduces profitability**.

---

# 📈 Sales Trends Over Time

### Findings

- Slight decrease between 2014–2015
- Strong growth from 2015–2017

![Sales Over Time](Plots/Sales_Over_Time.png)

---

# 🏆 Product Profitability Analysis

### Most Profitable Products
- Canon imageCLASS 2200 Advanced Copier
- Fellowes PB500 Binding Machine
- HP LaserJet 3310 Copier
- Canon PC1060 Personal Laser Copier

These products likely have **high margins and strong demand**.

---

### Least Profitable Products
- Cubify CubeX 3D Printer
- Lexmark MX611dhe Laser Printer
- Conference Tables
- Binding Systems

Possible reasons:
- Heavy discounting
- High cost structure
- Low demand

---

# 📊 Power BI Dashboard

An interactive dashboard was built to visualize key business metrics.

### Dashboard Features

- KPI Cards (Total Sales, Profit, Orders, Profit Margin)
- Sales Trend Over Time
- Sales by Category
- Sales by State (Map)
- Sales by Segment
- Sales by Sub-Category
- Discount vs Profit Scatter Analysis
- Profit Margin by Category
- Sales by Ship Mode
- Top Products by Sales

### Dashboard Preview

![Dashboard](Dashboard/Dashboard.png)

---

# 📌 Key Business Insights

1️⃣ Office Supplies products drive the highest profit margin  
2️⃣ Furniture has weak profitability despite high sales  
3️⃣ Large discounts strongly reduce profitability  
4️⃣ Sales show strong seasonality with peaks toward year-end (late 2014)
5️⃣ Standard shipping dominates total sales
6️⃣ Consumer segment generates most revenue  

---

# 👨‍💻 Author

**Raz Ben-Yehuda**  
B.Sc. Industrial Engineering & Management  

### Skills Demonstrated

- Data analysis with R  
- Exploratory data analysis  
- Data visualization  
- Power BI dashboard design  
- Business insight generation  
- Data storytelling
