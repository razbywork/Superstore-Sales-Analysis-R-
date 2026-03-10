# 📊 Superstore Sales Analysis (R)

## 📌 Project Overview
This project analyzes a retail dataset containing sales transactions from a fictional Superstore in the United States.  
The goal of the analysis is to explore **sales performance, profitability drivers, and customer behavior** using **R and the tidyverse ecosystem**.

The analysis includes:

- Data cleaning and preparation
- Exploratory Data Analysis (EDA)
- Data visualization
- Profitability analysis
- Customer and product insights

This project demonstrates practical **data analysis skills using R**, including **data manipulation, aggregation, and visualization**.

---

# 📂 Dataset

The dataset contains **9,994 sales transactions** and **21 variables**, including:

- Order information (Order Date, Ship Date, Shipping Mode)
- Customer information (Customer ID, Name, Segment)
- Geographic information (City, State, Region)
- Product information (Category, Sub-Category, Product Name)
- Sales metrics (Sales, Quantity, Discount, Profit)

---

# 🛠 Tools & Libraries

The project was built using:

- **R**
- **tidyverse**
- **ggplot2**
- **dplyr**
- **lubridate**

---

# 🧹 Data Preparation

Before analysis, several preprocessing steps were performed:

- Converted `Order.Date` and `Ship.Date` into Date format
- Checked for missing values
- Verified dataset structure

✅ No missing values were found in the dataset.

---

# 📊 Exploratory Data Analysis

## Sales and Profit by Category

The dataset contains three main product categories:

- Furniture  
- Office Supplies  
- Technology  

### Key Findings

- **Technology generates the highest total sales and profit.**
- **Office Supplies produces strong profits despite slightly lower sales.**
- **Furniture generates high sales but relatively low profit.**

This suggests **lower profit margins in the Furniture category**.

### Visualization

![Sales by Category](Plots/Sales_by_Category.png)

The boxplot shows:

- Technology has the **largest sales outliers**, indicating high-value purchases.
- Furniture and Office Supplies have **lower typical transaction sizes**.

---

# 🌎 Regional Profitability

Profitability varies significantly across regions.

### Findings

| Region | Profit Ranking |
|------|------|
| West | Highest |
| East | Second |
| South | Moderate |
| Central | Lowest |

### Visualization

![Profit by Region](Plots/Profit_by_Region.png)

The **West region leads in profitability**, suggesting stronger market performance or better pricing strategy.

---

# 💸 Discount Impact on Profit

A key business question is how discounts affect profitability.

### Findings

- Small discounts (0–10%) still produce positive profits.
- Discounts **above ~30% consistently generate losses**.
- Large discounts correlate strongly with **negative profit transactions**.

### Visualization

![Discount vs Profit](Plots/Discount_Profit_Relationship.png)

The scatter plot clearly shows that **higher discounts tend to produce negative profits**, indicating potential over-discounting.

---

# 📈 Sales Trends Over Time

### Findings

- Sales slightly decreased between **2014 and 2015**
- Sales increased significantly between **2015 and 2017**

This suggests strong **business growth in later years**.

### Visualization

![Sales Over Time](Plots/Sales_Over_Time.png)

---

# 🏆 Product Profitability Analysis

### Most Profitable Products

Top profitable products include:

- Canon imageCLASS 2200 Advanced Copier
- Fellowes PB500 Binding Machine
- HP LaserJet 3310 Copier
- Canon PC1060 Personal Laser Copier

These products likely have **high margins and strong demand**.

---

### Least Profitable Products

Products generating the largest losses include:

- Cubify CubeX 3D Printer
- Lexmark MX611dhe Laser Printer
- Conference Tables
- Binding Systems

Possible reasons:

- Excessive discounting
- High product costs
- Low demand

---

# 👥 Customer Profitability

The most profitable customers include:

- Tamara Chand
- Raymond Buch
- Sanjit Chand
- Hunter Lopez

Identifying high-value customers can support:

- Customer retention strategies
- Targeted marketing campaigns
- Loyalty programs

---

# 📌 Key Business Insights

The analysis reveals several important insights:

1️⃣ **Technology products drive the highest profits**

2️⃣ **Furniture has weaker profitability despite high sales**

3️⃣ **Large discounts strongly reduce profitability**

4️⃣ **The West region generates the highest total profit**

5️⃣ **Sales increased significantly after 2015**

These findings suggest opportunities to:

- Optimize discount strategies
- Focus on profitable product categories
- Target high-value customers

---

# 🚀 Future Improvements

Possible extensions for this project include:

- Profit prediction using regression models
- Customer segmentation
- Product recommendation analysis
- Interactive dashboards using **Shiny or Tableau**

---

# 👨‍💻 Author

**Raz Ben-Yehuda**  
B.Sc. Industrial Engineering & Management  

Skills demonstrated in this project:

- Data analysis with **R**
- Exploratory data analysis
- Data visualization
- Business insight generation
