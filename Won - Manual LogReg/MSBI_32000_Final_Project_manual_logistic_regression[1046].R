#Packages
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)

#Data
diabetes_data <- read_csv("~/Documents/UChicago_BMI/Intermediate_Applied_Data_Analysis_MSBI_32000_Summer_2022/Datasets_for_Project/Data_Sets_for_Project/Early_Stage_Diabetes/diabetes_data_upload.csv")
diabetes_data

#Data Wrangling
spec(diabetes_data)
class(diabetes_data)
##Gender
unique(diabetes_data$Gender)
diabetes_data$Gender <- ifelse(diabetes_data$Gender=="Male",1,0)
diabetes_data$Gender
##Polyuria
unique(diabetes_data$Polyuria)
diabetes_data$Polyuria <- ifelse(diabetes_data$Polyuria=="Yes",1,0)
diabetes_data$Polyuria
##Polydipsia
unique(diabetes_data$Polydipsia)
diabetes_data$Polydipsia <- ifelse(diabetes_data$Polydipsia=="Yes",1,0)
diabetes_data$Polydipsia
##'sudden weight loss'
unique(diabetes_data$`sudden weight loss`)
diabetes_data$`sudden weight loss` <- ifelse(diabetes_data$`sudden weight loss`=="Yes",1,0)
diabetes_data$`sudden weight loss`
##weakness
unique(diabetes_data$weakness)
diabetes_data$weakness <- ifelse(diabetes_data$weakness=="Yes",1,0)
diabetes_data$weakness
##Polyphagia
unique(diabetes_data$Polyphagia)
diabetes_data$Polyphagia <- ifelse(diabetes_data$Polyphagia=="Yes",1,0)
diabetes_data$Polyphagia
##'Genital thrush'
unique(diabetes_data$`Genital thrush`)
diabetes_data$`Genital thrush` <- ifelse(diabetes_data$`Genital thrush`=="Yes",1,0)
diabetes_data$`Genital thrush`
##'visual blurring'
unique(diabetes_data$`visual blurring`)
diabetes_data$`visual blurring` <- ifelse(diabetes_data$`visual blurring`=="Yes",1,0)
diabetes_data$`visual blurring`
##Itching
unique(diabetes_data$Itching)
diabetes_data$Itching <- ifelse(diabetes_data$Itching=="Yes",1,0)
diabetes_data$Itching
##Irritability
unique(diabetes_data$Irritability)
diabetes_data$Irritability <- ifelse(diabetes_data$Irritability=="Yes",1,0)
unique(diabetes_data$Irritability)
##'delayed healing'
unique(diabetes_data$`delayed healing`)
diabetes_data$`delayed healing` <- ifelse(diabetes_data$`delayed healing`=="Yes",1,0)
unique(diabetes_data$`delayed healing`)
##'partial paresis'
unique(diabetes_data$`partial paresis`)
diabetes_data$`partial paresis` <- ifelse(diabetes_data$`partial paresis`=="Yes",1,0)
unique(diabetes_data$`partial paresis`)
##'muscle stiffness'
unique(diabetes_data$`muscle stiffness`)
diabetes_data$`muscle stiffness` <- ifelse(diabetes_data$`muscle stiffness`=="Yes",1,0)
unique(diabetes_data$`muscle stiffness`)
##Alopecia
unique(diabetes_data$Alopecia)
diabetes_data$Alopecia <- ifelse(diabetes_data$Alopecia=="Yes",1,0)
unique(diabetes_data$Alopecia)
##Obesity
unique(diabetes_data$Obesity)
diabetes_data$Obesity <- ifelse(diabetes_data$Obesity=="Yes",1,0)
unique(diabetes_data$Obesity)
##class
unique(diabetes_data$class)
diabetes_data$class <- ifelse(diabetes_data$class=="Positive",1,0)
unique(diabetes_data$class)

view(diabetes_data)
summary(diabetes_data)

#Checking Sample for Imbalance
xtabs(~class + Gender, data = diabetes_data)
xtabs(~class + Polyuria, data = diabetes_data)
xtabs(~class + Polydipsia, data = diabetes_data)
xtabs(~class + diabetes_data$`sudden weight loss`, data = diabetes_data)
xtabs(~class + weakness, data = diabetes_data)
xtabs(~class + Polyphagia, data = diabetes_data)
xtabs(~class + diabetes_data$`Genital thrush`, data = diabetes_data)
xtabs(~class + diabetes_data$`visual blurring`, data = diabetes_data)
xtabs(~class + Itching, data = diabetes_data)
xtabs(~class + Irritability, data = diabetes_data)
xtabs(~class + diabetes_data$`delayed healing`, data = diabetes_data)
xtabs(~class + diabetes_data$`partial paresis`, data = diabetes_data)
xtabs(~class + diabetes_data$`muscle stiffness`, data = diabetes_data)
xtabs(~class + Alopecia, data = diabetes_data)
xtabs(~class + Obesity, data = diabetes_data)

#Logistic Regression Variable Selection -- Check for Correlations (p-value < 0.05)
cor.test(diabetes_data$Age, diabetes_data$class, method = "spearman")
cor.test(diabetes_data$Gender, diabetes_data$class, method = "spearman")
cor.test(diabetes_data$Polyuria, diabetes_data$class, method = "spearman")
cor.test(diabetes_data$Polydipsia, diabetes_data$class, method = "spearman")
cor.test(diabetes_data$`sudden weight loss`, diabetes_data$class, method = "spearman")
cor.test(diabetes_data$weakness, diabetes_data$class, method = "spearman")
cor.test(diabetes_data$Polyphagia, diabetes_data$class, method = "spearman")
cor.test(diabetes_data$`Genital thrush`, diabetes_data$class, method = "spearman")
cor.test(diabetes_data$`visual blurring`, diabetes_data$class, method = "spearman")
cor.test(diabetes_data$Itching, diabetes_data$class, method = "spearman") #diabetes_data$Itching <- NA
cor.test(diabetes_data$Irritability, diabetes_data$class, method = "spearman")
cor.test(diabetes_data$`delayed healing`, diabetes_data$class, method = "spearman") #diabetes_data$'delayed healing' <- NA
cor.test(diabetes_data$`partial paresis`, diabetes_data$class, method = "spearman")
cor.test(diabetes_data$`muscle stiffness`, diabetes_data$class, method = "spearman")
cor.test(diabetes_data$Alopecia, diabetes_data$class, method = "spearman")
cor.test(diabetes_data$Obesity, diabetes_data$class, method = "spearman") #diabetes_data$Obesity <- NA

#Logistic Regression
logistic_regression <- glm(diabetes_data$class ~ diabetes_data$Age + 
      diabetes_data$Gender + 
      diabetes_data$Polyuria + 
      diabetes_data$Polydipsia + 
      diabetes_data$`sudden weight loss` + 
      diabetes_data$weakness + 
      diabetes_data$Polyphagia + 
      diabetes_data$`Genital thrush` + 
      diabetes_data$`visual blurring` + 
      diabetes_data$Irritability + 
      diabetes_data$`partial paresis` + 
      diabetes_data$`muscle stiffness` + 
      diabetes_data$Alopecia, family = "binomial", data = diabetes_data)
summary(logistic_regression)

cor.test(x = diabetes_data$class, y = logistic_regression$fitted.values, method = "spearman")

#Graphing Logistic Regression https://stackoverflow.com/questions/17011121/ggplot2-logistic-regression-plot-probabilities-and-regression-line
ggplot(data = diabetes_data, aes(x = logistic_regression$fitted.values, y = diabetes_data$class)) + 
    geom_point() + 
    geom_smooth(data = diabetes_data, 
                aes(x = logistic_regression$fitted.values, y = diabetes_data$class), 
                method = "glm", 
                method.args = list(family = "binomial"), 
                se = TRUE,
                show.legend = TRUE,)

