# **CustomerInsights\_DS**

## **Project Overview**

A Data Science project aimed at analyzing customer transactional data to uncover insights. The project covers **data cleaning, visualization, K-Means clustering, and association rule mining** to understand spending patterns, customer segmentation, and relationships between purchased items.

---

## **Features**

* **Data Cleaning**: Handles missing values and prepares data for analysis.
* **Data Visualization**:

  * Total spending per city
  * Spending by payment type (Cash vs Credit)
  * Spending by age group
  * Distribution of total spending (boxplot)
* **K-Means Clustering**:

  * Clusters customers based on total spending and age
  * Visual scatter plots for cluster analysis
* **Association Rules Mining**:

  * Generates rules using Apriori algorithm
  * Configurable support and confidence
  * Visualization of rules as graphs and scatterplots

---

## **How to Run**

1. Clone the repository:

```bash
git clone https://github.com/RokiaAlaa/CustomerInsights_DS.git
```

2. Open the R project and launch the Shiny app:

```R
shiny::runApp()
```

3. Upload the dataset (CSV format) [attached with the source code] and interact with the app.

---

## **Dependencies**

* R packages: `shiny`, `dplyr`, `arules`, `arulesViz`, `svDialogs`
