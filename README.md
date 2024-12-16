# Statistical-Learning-in-Marketing-Mini-project-4-
Data Complexity Reduction | Marketing Mix Analysis | Time Series Analysis | Segmentation


## 📝 Objectives  
The assignment addresses four key managerial questions, using different analytical methods:

1. **Project 1**: Data Complexity Reduction  
2. **Project 2**: Marketing Mix Analysis  
3. **Project 3**: Marketing Dynamics  
4. **Project 4**: Customer Segmentation  

---

## 🗂 Project Structure  

### 1️⃣ Project 1: Data Complexity Reduction  
**Objective**: Simplify customer survey data by reducing the number of variables related to lifestyle and perceptions into a smaller set of factors.  

**Data**: `data for project 1& 2.csv`  
- Contains demographic data, lifestyle variables, and perceptions of organic foods from 250 consumers.  

**Methods**:  
- **Principal Component Analysis (PCA)**: Identify significant factors.  
- **Factor Loadings**: Understand variable contributions to factors.

**Outputs**:  
- Reduced survey data into key factors to simplify analysis.  

---

### 2️⃣ Project 2: Marketing Mix Analysis  
**Objective**: Identify the factors that drive **purchase intention (PI)** and **willingness-to-recommend (WTR)** and analyze their impact on different customer groups.  

**Data**: `data for project 1& 2.csv`  

**Methods**:  
- **Linear Regression**: Analyze relationships between survey factors and target variables.  
- **Interaction Analysis**: Test differences in high and low spending groups.  

**Outputs**:  
- Significant predictors for PI and WTR.  
- Insights into customer spending behavior.  

---

### 3️⃣ Project 3: Marketing Dynamics  
**Objective**: Analyze the impact of **advertising** and **pricing** on Ixmør's sales over time, while accounting for competitor actions.  

**Data**: `data for project 3.csv`  
- Contains weekly time-series data on sales, advertising, pricing, and competitor metrics over 207 weeks.  

**Methods**:  
- **Granger Causality Tests**: Identify lead-lag relationships.  
- **Vector Auto-Regression (VAR)**: Model dynamic interactions.  
- **Impulse Response Functions (IRF)**: Analyze long-term effects.  

**Outputs**:  
- Advertising has a **delayed effect** on sales.  
- Competitor pricing impacts sales immediately.  

---

### 4️⃣ Project 4: Customer Segmentation  
**Objective**: Segment the market to identify key customer groups for organic sandwich spreads.  

**Data**: `data for project 4.csv`  
- Contains data on 500 respondents' demographics and preferences for sandwich spreads.  

**Methods**:  
- **Hierarchical Clustering (Ward's Method)** and **K-means Clustering**  
- **ANOVA**: Compare cluster differences.  
- **PCA Visualization**: Validate segment separation.  

**Outputs**:  
Identified **3 key segments**:  
1. 🟢 **Sustainability-Oriented**: Eco-conscious older individuals.  
2. 🔵 **Foodies**: Younger women focused on taste.  
3. 🟡 **Quality Seekers**: Balanced focus on health and taste.

---
## 📈 Visual Highlights  

### Comparing Different Clustering Methods  
![Comparing Clustering](Image/Comparing%20different%20clustering...)

### IRF Dynamic Analyses on Sales  
![IRF Dynamic Analyses](Image/IRF%20dynamic%20analyses%20on%20Sales...)

### SEM Path for Project 1 Exploration  
![SEM Path](Image/SEM%20path%20for%20Project%201%20explora...)


---

## 📊 Data Files  

| File Name               | Description                                             |  
|-------------------------|---------------------------------------------------------|  
| `data for project 1& 2.csv` | Customer survey data for Projects 1 and 2.             |  
| `data for project 3.csv`   | Time-series data on sales, advertising, and pricing.    |  
| `data for project 4.csv`   | Demographic and preference data for customer segmentation. |  

---

## 💻 Code Files  

| File Name           | Description                             |  
|----------------------|-----------------------------------------|  
| `assignment1.R`      | Code for data complexity reduction     |  
| `assignment2.R`      | Code for marketing mix analysis        |  
| `assignment3.R`      | Code for marketing dynamics analysis   |  
| `assignment4.R`      | Code for customer segmentation         |  

---

## 🗃 Reports and Presentations  

| File Name                 | Description                                     |  
|---------------------------|-------------------------------------------------|  
| `assignment 1.pptx`       | Report for Project 1 (Team)                     |  
| `assignment 2.pptx`       | Report for Project 2 (Team)                     |  
| `s5065623_assignment 3.pptx` | Report for Project 3 (Individual)              |  
| `s5065623_assignment 4.pptx` | Report for Project 4 (Individual)              |  
| `SLIM 2425 Integrated Assignment.pdf` | Assignment guidelines and project descriptions. |  


---

## 🛠 Tools Used  

- **Language**: R  
- **Libraries**:  
  - `dplyr` for data manipulation  
  - `ggplot2` for visualization  
  - `cluster` and `factoextra` for clustering analysis  
  - `vars` for time-series modeling  

---

## 🎯 Conclusion  

This project provides actionable, data-driven recommendations to help **Ixmør**:  
- Simplify survey data for decision-making.  
- Identify key drivers of **WTR** and **PI**.  
- Understand the delayed effects of advertising on sales.  
- Target specific market segments with tailored strategies.  
