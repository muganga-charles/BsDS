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
missing_values <- sapply(data, function(x) sum(is.na(x)))
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

## OUTLIERS ----

numeric_data <- data %>% select_if(is.numeric)
par(mfrow = c(ceiling(sqrt(ncol(numeric_data))),ceiling(sqrt(ncol(numeric_data)))))
for (i in 1:ncol(numeric_data)){
  boxplot(numeric_data[, i], main = colnames(numeric_data)[i])
}

data <- data[,!names(data) %in% c("ID")]
head(data)

#boxplot_gg <- function(data, columns){
  #for (column in columns){
    p <- ggplot(data = data, mapping = aes(x = "", y = .data[[column]])) + 
        goem_boxplot()+
        labs(title = paste("Boxplot for ", column))
    print(p)
  }
#}
#boxplot_gg(data, c("carat", "depth", "price", "x", "y"))

# Relationships ----
## cat and cat ----


