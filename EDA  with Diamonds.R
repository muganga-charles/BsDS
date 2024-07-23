# Pre-processing ----
## Loading packages and libraries ----
library(tidyverse)
library(caret)
library(dplyr)
library(mice)
library(ggplot2)
library(corrplot)
library(randomForest)
library(xgboost)
## Loading the data ----
data <- read.csv('pricingOfDiamonds.csv')
# review a  few rows of the data
head(data)
View(data)
data$price
# view the structure of the data 
str(data)
# summary stats
summary(data)

## Handling missing values ----
## Approach one
missing_values <- sapply(numeric_data, function(x) sum(is.na(x)))
missing_values

## Approach two
missing_values_two <- data %>% summarise(numeric_missing = sum(is.na(.)), categorical_missing
                     = sum(is.na(as.character(.))))# nolint
missing_values_two

## Approach three
missing_values_three <- data %>% summarise_all(funs(sum(is.na(.))))
missing_values_three

## Remove missing values ----
## Drop missing values ----
new_data <- na.omit(data)

#numeric_data <- sapply(data, is.numeric)
par(mfrow = c(2,1))
hist(data$price, col = "blue", main = "Histogram for price")
hist(data$depth, col = "blue", main = "Histogram for depth")

hist(data$x, col = "blue", main = "Histogram for x")

## imputing for missing values
## Approach 1
impute_mean <-  function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)) # normalized cont data
impute_median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE)) # skwed cont data
impute_mode <- function(x){
  model_value <- as.numeric(names(table(sort(x), descreasing = TRUE)[1])) # cat data
  replace(x, is.na(x), model_value)
}
# calling the function
impute_mean(data$depth)
impute_median(data$price)

## Approach 2
data$price[is.na(data$price)] <- median(data$price, na.rm = TRUE)
data$depth[is.na(data$depth)] <- mean(data$depth, na.rm = TRUE)

## Approach 3
#imputed_data <- mice(data, m = 5, method = "pmm", maxit = 50,  seed = 500)
#data <- complete(imputed_data, 1)

#lines(density(data$price), col = "blue", lwd = 2)

# OUTLIERS ----
## Visualizing outliers ----
numeric_data <- data %>% select_if(is.numeric)
par(mfrow = c(ceiling(sqrt(ncol(numeric_data))),ceiling(sqrt(ncol(numeric_data)))))
for (i in 1:ncol(numeric_data)){
  boxplot(numeric_data[, i], main = colnames(numeric_data)[i])
}

data <- data[,!names(data) %in% c("ID")]
head(data)

## removing outliers
removing_outliers <- function(data, columns) {
  for (column in columns) {
    #iqr_feature <- IQR(data[[column]])
    quantiles <- quantile(data[[column]], probs = c(0.25, 0.75), na.rm = TRUE) # nolint
    iqr_feature <- quantiles[2] - quantiles[1]
    lower_boundary <- quantiles[1] - 1.5 * iqr_feature
    upper_boundary <- quantiles[2] + 1.5 * iqr_feature
    
    data <- data[which((data[[column]] >= lower_boundary) & (data[[column]] <= upper_boundary)), ] # nolint
  }
  
  return(data)
}

cleaned_data <- remove_outliers(data, c("carat", "depth", "price", "x", "y"))

## visualizing to outliers
boxplot(cleaned_data$price)
numeric_data <- cleaned_data %>% select_if(is.numeric)
par(mfrow = c(ceiling(sqrt(ncol(numeric_data))),ceiling(sqrt(ncol(numeric_data)))))
for (i in 1:ncol(numeric_data)){
  boxplot(numeric_data[, i], main = colnames(numeric_data)[i])
}
# Relationships ----
#cont vs cont - corr matrix, scatter plots
#cont vs cat  - boxplots, ANOVA test
#cat vs cat  - barplot, Chi square test
## means(cont)  Shaipro- will

#cont vs cat ----
## Target = Price (cont variable)
categorical_variables <- cleaned_data %>% select_if(is.character) # grouping our cat data
head(categorical_variables)

perform_anova <- function(data, cont_var, cat_vars){ 
  for (cat_var in cat_vars) {
  anova_result <- aov(as.formula(paste(cont_var, '~', cat_var)), data = data)
  summary_anova <- summary(anova_result)
  p_value <-  summary_anova[[1]]["Pr(>F)"][1, ]
  
  if(p_value <= 0.05){
    cat("Significant relationship found between ", cont_var, "and ", cat_var, "with a p value of :", p_value, "\n") # noilint
  }
  else{
    cat("No significant rekationship found between ", cont_var, "and ", cat_var,  "with a p value of :", p_value, "\n") #nolint
  }
  }
}

perform_anova(cleaned_data, "price", c("cut", "colour", "clarity", "P", "PC"))

# cont vs cont ----
## approach one
correlation_matrix <- cor(numeric_data)
correlation_matrix
## Approach two
pairs(cleaned_data[,c("price","depth", "x", "y", "carat")])

## Approach three
corrplot(correlation_matrix, method = "circle")

#cat vs cat  ----
perform_chisq <- function(data, cat_v1, cat_data){
  for (cat_var2 in cat_data){
    chisq_result <- chisq.test(table(cleaned_data[[cat_v1]],cleaned_data[[cat_var2]]))
    p_value <- chisq_result$p.value
    
    if (p_value <= 0.05){
      cat("Significant relationship found between", cat_v1, "and", cat_var2, "with pvalue: ", p_value, "\n")
    }
    else{
      cat("Significant relationship not found between", cat_v1, "and", cat_var2, "with pvalue: ", p_value, "\n")
    }
  }
}

perform_chisq(cleaned_data, "cut", c("colour", "clarity", "P","PC"))

ml_data <- cleaned_data[,!names(cleaned_data) %in% c("depth", "P", "PC")]
head(ml_data)
ml_data

# Model building ----