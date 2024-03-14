## Introduction

Employee attrition poses significant challenges for organizations, impacting performance, morale, and financial stability. This project aims to identify factors contributing to employee attrition within IBM using the IBM HR Analytics Employee Attrition & Performance dataset obtained from Kaggle. By understanding the drivers of attrition, organizations can implement strategies to retain talent, improve employee satisfaction, and enhance overall performance.

## Statement of Goals

The primary goal of this study is to:
- Identify factors influencing employee attrition at IBM and provide insights to help organizations reduce turnover rates and improve retention strategies.

## Data Description

The IBM HR Analytics Employee Attrition & Performance dataset comprises 35 columns and 1,470 rows, containing various employee attributes such as age, monthly income, job role, job satisfaction, and attrition status. Some key features investigated include:
- Age
- Monthly Income
- Job Satisfaction
- Job Level
- Department
- Marital Status
- Work Life Balance
- Attrition (Target Variable)

## Analysis Overview

### Research Questions
1. What factors contribute to employee attrition at IBM?
2. How does job satisfaction influence attrition rates?
3. Are there significant relationships between employee demographics and attrition?

### Key Visualizations and Findings
- **Pie Chart for Percentage of Employee Attrition:** Indicates an imbalanced dataset, highlighting the need for cautious analysis to prevent bias.
- **Mosaic Plot for Job Satisfaction and Attrition:** Shows that job satisfaction levels of "Low" and "Very High" have the most significant impact on attrition.
- **Line Graph (Income, Job Satisfaction, Attrition):** Demonstrates a correlation between lower monthly income and higher attrition rates.
- **Histogram for Job Satisfaction by Department:** Provides context for further analysis, showing similar average job satisfaction levels across departments.
- **Probability of Attrition Based on Monthly Income (Job Satisfaction=High):** Illustrates the influence of monthly income on attrition across different departments.
- **Mosaic Plot for Overtime, Marital Status, and Attrition:** Indicates higher attrition rates among employees who work overtime and are single.
- **Mosaic Plot for Work Life Balance vs. Attrition:** Shows that poor work-life balance is associated with higher attrition rates.
- **Model Evaluation:** Compares accuracy and ROC curves of different models, with Lasso Regression performing the best.
- **Lasso Regression Most Important Features:** Identifies top features influencing attrition, including job level, job role, overtime, and work-life balance.

## Future Work and Limitations

### Future Research Directions
- Longitudinal studies tracking employee attrition and job satisfaction over time.
- Qualitative research (interviews, focus groups) to gain deeper insights into reasons for attrition.
- Industry-specific analysis to understand sector-specific attrition trends.
- Evaluation of organizational interventions to promote job satisfaction and reduce attrition.
- Analysis of external factors (e.g., economy, industry trends) on attrition rates.

### Limitations
- Single dataset analysis may limit generalizability to other sectors or organizations.
- Exclusion of personal or family reasons for attrition may overlook important factors.
- Lack of investigation into external factors and organizational interventions.
- The project highlights the need for future research to address these limitations and provide a more comprehensive understanding of employee attrition.

## References

IBM HR Analytics Employee Attrition & Performance Dataset. Kaggle. [Link](https://www.kaggle.com/datasets/pavansubhasht/ibm-hr-analytics-attrition-dataset)


