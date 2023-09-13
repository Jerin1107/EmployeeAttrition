# 1 Introduction
# Dataset
# https://www.kaggle.com/datasets/rohitsahoo/employee?select=train.csv

# 1.1 Loading packages and Attributes of data

library(dplyr) # Data manipulation
library(tidyverse) # Data manipulation
#install.packages('Amelia')
library(Amelia) # Confirm missing value
library(ggplot2) # Visualization
#install.packages('fmsb')
library(fmsb) # radar chart
#install.packages('qtlcharts')
library(qtlcharts) # Make plot of correlation table
#install.packages('magrittr')
library(magrittr)
library(corrplot)
library(GGally)
#install.packages('rsample')
library(rsample)

library(tidyverse)
#install.packages('tidymodels')
library(tidymodels)
library(scales)
#install.packages('janitor')
library(janitor)
library(gridExtra)
library(glue)
#install.packages('ggcorrplot')
library(ggcorrplot)
#install.packages('vip')
library(vip)
#install.packages("glmnet")
library(glmnet)
#install.packages("ranger")
library(ranger)

library(cowplot)




attrition_data <- read.csv("/Users/jerineasothomas/Downloads/EDA_Project/train.csv")

head(attrition_data)

str(dat) # 1058 obs. of 35 variables

# 1.2 Drop useless features

attrition_data %>% group_by(Over18) %>% summarize(N = n())

attrition_data %>% group_by(StandardHours) %>% summarize(N = n())

attrition_data %>% group_by(EmployeeCount) %>% summarize(N = n())

attrition_data <- subset(attrition_data, select=-c(Over18, StandardHours, EmployeeCount))

# 2 Exploratory Data Analysis

#2.1 Summary of data and Missing values

colSums(is.na(dat))

missmap(dat)

# No missing values

# 2.2 Attrition Analysis

class(attrition_data$Attrition)

attrition_data$Attrition <- as.factor(attrition_data$Attrition)



attrition_data %>% group_by(Attrition) %>% summarize(N = n()) %>%
  ggplot(aes(Attrition, N, fill = Attrition)) +
  geom_col() +
  theme_bw() + 
  scale_fill_brewer(palette="Set1") + 
  geom_text(aes(label = N), size = 5, vjust = 1.2, color = "#FFFFFF") + 
  ggtitle("Count of Employee Attrition") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x = "Attrition", y = "Count")

library(ggrepel)

#----Used in Report
attrition_data %>% 
  group_by(Attrition = ifelse(Attrition == 0, "No", "Yes")) %>% 
  summarize(N = n()) %>%
  ggplot(aes(x = "", y = N, fill = Attrition)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text_repel(aes(label = paste0(round(N/sum(N)*100), "%")), size = 5, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette="Set3") +
  ggtitle("Percentage of Employee Attrition") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15))






attrition_data %>% group_by(Attrition) %>% summarize(N = n()) %>% mutate(percent = N*100/sum(N)) %>%
  ggplot(aes("", Attrition, fill = Attrition)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  theme_bw() + 
  scale_fill_brewer(palette="Set1") + 
  coord_polar("y", start = 0) +
  ggtitle("Percent of Employee Attrition") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  geom_text(aes(label = paste0(round(percent, 1), "%")), position = position_stack(vjust = 0.5), color = "white")

# 2.3 Age Distribution by Attrition

ggplot(attrition_data, aes(x=Age, fill=Attrition, color=Attrition)) +
  geom_density(position="identity", alpha=0.5) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set1") +
  ggtitle("Density plot of Age") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
  labs(x = "Age") 

# 2.4 Marital Status by Attrition

attrition_data %>% group_by(Attrition, MaritalStatus) %>% summarize(N = n()) %>% mutate(countT = sum(N)) %>%
  group_by(Attrition, MaritalStatus, add=TRUE) %>% mutate(per=paste0(round(100*N/countT,1),'%')) %>%
  ggplot(aes(x=Attrition, y=N, fill=MaritalStatus)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set2") +
  geom_text(aes(label = per), size = 4, vjust = 1.2, color = "#FFFFFF", position = position_dodge(0.9)) + 
  ggtitle("Marital Status by Attrition") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x = "Attrition", y = "Count")

# 2.5 Monthly Income by Attrition

attrition_data %>% group_by(Attrition) %>% summarize(Mean = mean(MonthlyIncome))

ggplot(attrition_data, aes(x=Attrition, y=MonthlyIncome, color=Attrition, fill=Attrition)) +
  geom_boxplot() + 
  theme_bw() + 
  scale_fill_brewer(palette="Set2") +
  scale_color_manual(values=c("#661304", "#040242")) +
  ggtitle("Monthly Income by Attrition") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x = "Attrition", y = "Monthly Income")

# 2.6 Job Satisfaction by Attrition

ggplot(attrition_data, aes(x=Attrition, y=JobSatisfaction, color=Attrition, fill=Attrition)) +
  geom_boxplot() + 
  theme_bw() + 
  scale_fill_brewer(palette="Set2") +
  scale_color_manual(values=c("#661304", "#040242")) +
  ggtitle("Job Satisfaction by Attrition") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x = "Attrition", y = "Job Satisfaction")

# 2.7 Department by Attrition

attrition_data %>% group_by(Attrition, Department) %>% summarize(N = n()) %>% mutate(countT = sum(N)) %>%
  group_by(Attrition, Department, add=TRUE) %>% mutate(per=paste0(round(100*N/countT,1),'%')) %>%
  ggplot(aes(x=Attrition, y=N, fill=Department)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set2") +
  geom_text(aes(label = per), size = 4, vjust = 1.2, color = "#FFFFFF", position = position_dodge(0.9)) + 
  ggtitle("Department by Attrition") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x = "Attrition", y = "Count")

# 2.8 Educational Level by Attrition

attrition_data$Educational_Level <-  ifelse(attrition_data$Education == 1, "Without College D.",
                                            ifelse(attrition_data$Education == 2 , "College D.",
                                                   ifelse(attrition_data$Education == 3, "Bachelors D.",
                                                          ifelse(attrition_data$Education == 4, "Masters D.", "Phd D."))))

attrition_data %>% group_by(Attrition, Educational_Level) %>% summarize(N = n()) %>% mutate(countT = sum(N)) %>%
  group_by(Attrition, Educational_Level, add=TRUE) %>% mutate(per=paste0(round(100*N/countT,1),'%')) %>%
  ggplot(aes(x=fct_reorder(Educational_Level,N), y=N, fill=Educational_Level)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~Attrition) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set2") +
  geom_text(aes(label = per), size = 3, vjust = 1.2, color = "#FFFFFF", position = position_dodge(0.9)) + 
  ggtitle("Educational Level by Attrition") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), axis.text.x = element_text(angle=18)) +
  labs(x = "Attrition", y = "Count")

# 2.9 Distance from Work by Attrition

ggplot(attrition_data, aes(x=DistanceFromHome, fill=Attrition, color=Attrition)) +
  geom_density(position="identity", alpha=0.5) + 
  theme_bw() + 
  scale_fill_brewer(palette="Set1") +
  ggtitle("Distance from Work by Attrition") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), legend.position="bottom") +
  labs(x = "Distance from Work") 


#----Used in Report
# 2.10 Job Satisfaction by Department

ggplot(attrition_data, aes(x=Department, y=JobSatisfaction, fill=Department)) +
  theme_bw() + 
  scale_fill_brewer(palette="Set2") +
  geom_bar(stat="summary", fun="mean") +
  ggtitle("Job Satisfaction by Department") +
  xlab("Department") +
  ylab("Mean Job Satisfaction") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15))

ggplot(attrition_data, aes(x=Department, y=JobSatisfaction, fill=Department)) +
  theme_bw() + 
  scale_fill_brewer(palette="Set2") +
  geom_bar(stat="summary", fun="mean") +
  ggtitle("Job Satisfaction by Department") +
  xlab("Department") +
  ylab("Mean Job Satisfaction") +
  labs(title = "Job Satisfaction by Department", x = "Department", y = "Mean Job Satisfaction", fill = "Department") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15), 
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10))

# 2.11 Job Satisfaction by Job Role
ggplot(attrition_data, aes(x=JobRole, y=JobSatisfaction, fill=JobRole)) +
  theme_bw() + 
  scale_fill_brewer(palette="Set3") +
  geom_bar(stat="summary", fun="mean") +
  ggtitle("Job Satisfaction by Job Role") +
  xlab("Job Role") +
  ylab("Mean Job Satisfaction") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15))

# 2.13

data_subset <- attrition_data[, c("Educational_Level", "JobLevel", "PerformanceRating", "JobSatisfaction")]

ggpairs(data_subset, 
        aes(color = PerformanceRating, alpha = 0.7),
        upper = list(continuous = "density"),
        lower = list(continuous = "smooth", method.args = list(method = "glm")),
        diag = list(continuous = "density"),
        title = "Pairwise scatterplots for employee variables")



# 2.14

#attrition_data2$Attrition <- as.integer(attrition_data$Attrition == "Yes")

correlation_matrix <- cor(attrition_data[, sapply(attrition_data, is.numeric)])


corrplot(correlation_matrix, method="color")

# 2.15

ggplot(attrition_data, aes(x = Department, y = JobSatisfaction, fill = Attrition)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  ggtitle("Job Satisfaction by Department and Attrition") +
  xlab("Department") +
  ylab("Job Satisfaction") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  facet_grid(. ~ Attrition)

# Model Building 

hrnew <- read.csv("/Users/jerineasothomas/Downloads/WA_Fn-UseC_-HR-Employee-Attrition.csv")


hrnew <-
  hrnew %>%
  mutate(across(c(Attrition,Over18,OverTime),
                ~ if_else(. == "Yes",1,0))) %>% 
  mutate(across(c(Attrition,Over18,OverTime),
                ~ as.factor(.))) %>% 
  mutate(Attrition = fct_relevel(Attrition,c("1","0"))) %>%
  # Binary categorical
  mutate(across(c(Department, EducationField,
                  JobRole, MaritalStatus),~ as.factor(.))) %>%
  # Nominal categorical
  mutate(across(c(EnvironmentSatisfaction, JobSatisfaction,
                  RelationshipSatisfaction,
                  WorkLifeBalance,BusinessTravel, Education ,
                  JobInvolvement,JobLevel, StockOptionLevel,
                  PerformanceRating),
                ~as.ordered(.))) %>%
  # Ordinal categorical
  mutate(BusinessTravel = factor(BusinessTravel, ordered = TRUE,
                                 levels = c("Non-Travel",
                                            "Travel_Rarely","Travel_Frequently"))) %>%
  # Reordering
  select(-EmployeeCount,-StandardHours,-Over18)
# Removing non pertinant variables


# Dividing features into vectors to faciltate plotting
numerical <- c("Age", "DistanceFromHome","HourlyRate",
               "DailyRate", "MonthlyRate","MonthlyIncome",
               "PercentSalaryHike","YearsAtCompany",
               "YearsInCurrentRole","YearsSinceLastPromotion",
               "YearsWithCurrManager","TotalWorkingYears",
               "NumCompaniesWorked","TrainingTimesLastYear") 

categorical <- c("Gender","OverTime","Department",
                 "EducationField", "JobRole", "MaritalStatus")

ordinal <- c("EnvironmentSatisfaction", "JobSatisfaction",
             "RelationshipSatisfaction","WorkLifeBalance",
             "JobInvolvement","PerformanceRating",
             "BusinessTravel", "Education","JobLevel",
             "StockOptionLevel")

# Creating a train/test split
set.seed(1234)
spl_new <- initial_split(data = hrnew, strata = Attrition, prop = 0.8)
train_new <- training(spl_new)
test_new <- testing(spl_new)


# Model Building

hr_recipe_new <- recipe(data = train_new,formula = Attrition ~ .) %>%
  update_role(EmployeeNumber, new_role = "ID") %>%
  step_normalize(any_of(c("Age","MonthlyIncome","TotalSatisfaction"))) %>%
  step_dummy(all_nominal_predictors(),one_hot = TRUE) %>%
  step_nzv(all_nominal_predictors()) %>%
  step_corr(all_predictors())

hr_recipe_new %>% prep() %>% juice() %>% glimpse()

## Logistic regression

## At first I will fit a logistic model using **all** the predictors

glm_spec <- 
  logistic_reg() %>%
  set_engine("glm", maxit = 2000) %>%
  set_mode("classification")

glm_model <- 
  workflow() %>% 
  add_recipe(hr_recipe_new) %>% 
  add_model(glm_spec) %>%
  fit(data = train_new)

glm_model %>% tidy() %>%
  arrange(estimate) %>%
  filter(p.value <= 0.05) %>%
  mutate(term = fct_reorder(term,-estimate),
         condition = if_else(estimate >=0,FALSE,TRUE)) %>%
  ggplot(aes(x = term, y = estimate,fill = condition )) +
  geom_col(width = 0.8,color = "black",alpha = 0.75) + 
  geom_errorbar(aes(ymin = estimate - std.error * 1.96,
                    ymax = estimate + std.error * 1.96),
                width = 0.5, alpha = 0.5) +
  theme(legend.position = "none", axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  coord_flip() +
  labs(title = "Statisticly significant features (p.value < 0.05)",
       x = element_blank(), y = element_blank())

glm_model %>% tidy() %>%
  arrange(estimate) %>%
  filter(p.value <= 0.05) %>%
  mutate(term = fct_reorder(term,-estimate),
         condition = if_else(estimate >=0, TRUE, FALSE)) %>%
  ggplot(aes(x = term, y = estimate,fill = condition )) +
  geom_col(width = 0.8,color = "black",alpha = 0.75) + 
  geom_errorbar(aes(ymin = estimate - std.error * 1.96,
                    ymax = estimate + std.error * 1.96),
                width = 0.5, alpha = 0.5) +
  theme(legend.position = "none", axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  coord_flip() +
  labs(title = "Statisticly significant features (p.value < 0.05)",
       x = element_blank(), y = element_blank())



# We fit the logistic regression on the training data.
# Now we should fit the model on the **testing data** in order to see how it preforms on unseen data.

glm_pred <-
  bind_cols(
    test_new["Attrition"],
    predict(glm_model,test_new),
    predict(glm_model,test_new,type = "prob"))


glm_pred %>% 
  conf_mat(Attrition, .pred_class) %>%
  autoplot(type = "heatmap")

glm_pred %>%
  select(Attrition, .pred_class, .pred_1) %>%
  knitr::kable()

glm_pred %>%
  ggplot(aes(x = .pred_1, y = ..density.., fill = Attrition)) +
  geom_histogram(binwidth = 0.05, alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
  labs(title = "Distribution of Predicted Probabilities for Attrition",
       x = "Predicted Probability of Attrition", y = "Density") +
  theme_minimal()


roc_curve(glm_pred, truth = Attrition, estimate = .pred_1) %>% autoplot()

roc_curve(glm_pred, truth = Attrition, estimate = .pred_1) %>% pROC::autoplot()



metric_df <-
  bind_rows(
    accuracy(glm_pred,Attrition,.pred_class),
    roc_auc(glm_pred,Attrition,.pred_1)) %>%
  mutate(model = "LR1")

metric_df


# df_p <- data.frame(Actual = test_new$Attrition, Predicted = glm_pred$.pred_1)
# 
# # Create a scatter plot with lines for actual and predicted values
# ggplot(df_p, aes(x = Actual, y = Predicted)) +
#   geom_point() +
#   geom_abline(color = "blue") +
#   geom_smooth(method = "lm", color = "red") +
#   labs(title = "Actual vs. Predicted Attrition",
#        x = "Actual Attrition",
#        y = "Predicted Attrition")
# 
# ggplot(glm_pred, aes(x = Attrition, y = .pred_1, fill = Attrition)) +
#   geom_boxplot(alpha = 0.7) +
#   scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
#   labs(title = "Predicted Probabilities for Attrition",
#        x = "Attrition", y = "Predicted Probability") +
#   theme_minimal() 

### LR with feature selection

### We know how a logistic regression model with all predictors preforms, we should compare its preformance to a logistic model with only **pertinant features** taken in consideration.


# hr_recipev2 <- 
#   recipe(data = train_new,formula = Attrition ~ Age + TotalWorkingYears + MonthlyIncome +
#            OverTime + MaritalStatus + JobRole + BusinessTravel + JobLevel +
#            StockOptionLevel + JobSatisfaction + YearsWithCurrManager) %>%
#   step_normalize(any_of(c("Age","MonthlyIncome","JobSatisfaction"))) %>%
#   step_dummy(all_nominal_predictors(),one_hot = TRUE) %>%
#   step_nzv(all_nominal_predictors()) %>%
#   step_corr(all_predictors())
# 
# 
# hr_recipev2 %>% prep() %>% juice() %>% glimpse()
# 
# glm_model2 <- 
#   workflow() %>% 
#   add_recipe(hr_recipev2) %>% 
#   add_model(glm_spec) %>%
#   fit(data =  train_new)
# 
# 
# glm_pred <-
#   bind_cols(
#     test["attrition"],
#     predict(glm_model2,test),
#     predict(glm_model2,test,type = "prob"))
# 
# glm_pred %>% 
#   conf_mat(attrition, .pred_class) %>%
#   autoplot(type = "heatmap")
# 
# 
# metric_df <-
#   bind_rows(bind_rows(
#     accuracy(glm_pred,attrition,.pred_class),
#     roc_auc(glm_pred,attrition,.pred_1)) %>%
#       mutate(model = "LR2"),
#     metric_df)
# 
# 
# metric_df %>% filter(model == "LR2")


### Penalized regression

# Creating folds for cross validation
train_fold <- train_new %>% vfold_cv(5,strata = Attrition)

# Declaring the model we will use
lasso_spec <- logistic_reg(penalty = tune()) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

lasso_model <- 
  workflow() %>%
  add_recipe(hr_recipe_new) %>%
  add_model(lasso_spec)

# Creating the specification for our tune grid
lambda_grid <- crossing(penalty = 10 ^ seq(-7,-0.5,0.1))

lasso_grid <- tune_grid(lasso_model
                        ,resamples = train_fold,
                        grid = lambda_grid)

highest_acc <- lasso_grid %>% 
  select_best("accuracy",maximise = TRUE)

lasso_grid %>% autoplot()

# Applying the tuning to our workflow
lasso_model <- finalize_workflow(lasso_model,
                                 highest_acc) %>% fit(data = train_new)

#----Used in Report
lasso_model %>%
  pull_workflow_fit() %>%
  vi(lambda = highest_acc$penalty) %>%
  mutate(Importance = abs(Importance),
         Variable = fct_reorder(Variable, Importance),
         Sign = fct_rev(Sign)) %>%
  top_n(15,wt = Importance) %>% 
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col(color = "black", width = 0.8, alpha = 0.75) +
  theme(legend.position = "none", axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(title = "Most important features",subtitle = "Red bars: more chance to churn", y = element_blank()) 



# The model is properly fit and it's time to use the model to predict attrition.


lasso_pred <-
  bind_cols(
    test_new["Attrition"],
    predict(lasso_model,test_new),
    predict(lasso_model,test_new,type = "prob"))




lasso_pred %>% 
  conf_mat(Attrition, .pred_class) %>%
  autoplot(type = "heatmap")

roc_curve(lasso_pred, truth = Attrition, estimate = .pred_1) %>% autoplot()


metric_df <-
  bind_rows(bind_rows(
    accuracy(lasso_pred,Attrition,.pred_class),
    roc_auc(lasso_pred,Attrition,.pred_1)) %>%
      mutate(model = "Lasso"),
    metric_df)

metric_df %>% filter(model == "Lasso")
metric_df

## Random forest

### Now let's build and tune a random forest model


rf_spec <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()) %>%
  set_mode("classification") %>% 
  set_engine(engine = "ranger")


rf_grid <-
  crossing(mtry = c(9:17),min_n = c(seq(1,25,5)),trees = c(500))


rf_model <- 
  workflow() %>%
  add_recipe(hr_recipe_new) %>%
  add_model(rf_spec)


rf_tune <- tune_grid(rf_model,
                     resamples = train_fold,
                     grid = rf_grid
)
highest_acc <- rf_tune %>% select_best("accuracy")

rf_tune %>% autoplot()
rf_tune %>% collect_metrics() %>% arrange(-mean)


rf_model <- finalize_workflow(rf_model,
                              highest_acc) %>% fit(data = train_new)

rf_model %>%
  pull_workflow_fit()

rf_pred <-
  bind_cols(
    test_new["Attrition"],
    predict(rf_model,test_new),
    predict(rf_model,test_new,type = "prob"))

rf_pred %>% 
  conf_mat(Attrition, .pred_class) %>%
  autoplot(type = "heatmap")

roc_curve(rf_pred, truth = Attrition, estimate = .pred_1) %>% autoplot()

metric_df <-
  bind_rows(bind_rows(
    accuracy(rf_pred,Attrition,.pred_class),
    roc_auc(rf_pred,Attrition,.pred_1)) %>%
      mutate(model = "RF"),
    metric_df)

metric_df %>% filter(model == "RF")

metric_df %>%
  ggplot(aes(x = model, y = .estimate,fill = model)) +
  geom_col(width = 0.8, alpha = 0.75,color = "black") +
  labs(title = "Model evaluation",y = NULL, x = NULL) + 
  facet_wrap(~ .metric) +
  scale_y_continuous(limits = c(0.5,1),oob = rescale_none) +
  geom_text(aes(label = round(.estimate,3)),vjust = 2)

library(RColorBrewer)


#----Used in Report
metric_df %>%
  ggplot(aes(x = model, y = .estimate, fill = model)) +
  geom_col(width = 0.8, alpha = 0.75, color = "black") +
  labs(title = "Model Evaluation", y = NULL, x = NULL) + 
  facet_wrap(~ .metric) +
  scale_y_continuous(limits = c(0.5, 1), oob = rescale_none) +
  geom_text(aes(label = round(.estimate, 3)), vjust = 2) +
  scale_fill_manual(values = brewer.pal(n = length(unique(metric_df$model)), name = "Dark2"))

metric_df %>%
  ggplot(aes(x = model, y = .estimate, fill = model)) +
  geom_col(width = 0.8, alpha = 0.75, color = "black") +
  labs(title = "Model Evaluation", y = NULL, x = NULL) + 
  facet_wrap(~ .metric) +
  scale_y_continuous(limits = c(0.5, 1), oob = rescale_none) +
  geom_text(aes(label = round(.estimate, 3)), vjust = 2) +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw()

metric_df %>%
  ggplot(aes(x = model, y = .estimate, fill = model)) +
  geom_col(width = 0.8, alpha = 0.75, color = "black") +
  labs(title = "Model Evaluation", y = NULL, x = NULL) +
  facet_wrap(~ .metric) +
  scale_y_continuous(limits = c(0.5, 1), oob = rescale_none) +
  geom_text(aes(label = round(.estimate, 3)), vjust = 2, size = 5) +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw()



library(ggplot2)
library(ggpubr)

roc1 <- roc_curve(glm_pred, truth = Attrition, estimate = .pred_1) %>% autoplot()
roc2 <- roc_curve(lasso_pred, truth = Attrition, estimate = .pred_1) %>% autoplot()
roc3 <- roc_curve(rf_pred, truth = Attrition, estimate = .pred_1) %>% autoplot()

ggarrange(roc1, roc2, roc3, ncol = 3, common.legend = TRUE, legend = "right")


library(pROC)

roc_data <- bind_rows(
  roc_curve(glm_pred, truth = Attrition, estimate = .pred_1) %>% 
    mutate(model = "Logistic Regression"),
  roc_curve(lasso_pred, truth = Attrition, estimate = .pred_1) %>% 
    mutate(model = "Lasso Regression"),
  roc_curve(rf_pred, truth = Attrition, estimate = .pred_1) %>% 
    mutate(model = "Random Forest")
)

ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#F8766D", "#00BFC4", "#C77CFF")) +
  labs(title = "Model Evalution with ROC",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal()

#----Used in Report
ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#F8766D", "#00BFC4", "#C77CFF")) +
  labs(title = "Model Evaluation with ROC",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5))



