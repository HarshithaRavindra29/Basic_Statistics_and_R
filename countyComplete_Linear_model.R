# -------------------
# Title : County complete EDA with regression analysis of poverty using "Firm" data
# Author: Harshitha Ravindra
# Week: Oct 18, 2018
# Analysis : Linear models
# -------------------
# for data table syntax
library(data.table)


setwd('path')

# fread reads a csv file as a data table
County_complete = fread('countyComplete.csv')

#------------poverty by explanatory variable ----------------------------------
#Hypothesis:
# The poverty, or the economic condition of a region can be decided by the employment 
#opportunity in that area and the budget allocated for that area. 
#The 2 variables in CountyComplete data that satisfies the above conditions are
#i.	Total number of firms (firms)
#ii.	Federal spending (fed_spending)
#----------------------------------------------------------------------------------


#Linear Regression for explaining the Poverty in the county complete data
Firm_model = lm(poverty ~ firms,County_complete)
summary(Firm_model)
Fed_model = lm(poverty ~ fed_spending,County_complete)
summary(Fed_model)

# lm() function removes the row if there is any missing values (NA's) in the explanatory variable
# Therefore, while calculating total variance of Response variable (Poverty in this case),
# Rows of Poverty where there is NA in the explanatory variable must be removed

explained_var_firm = 1 - (var(Firm_model$residuals)
                          /var(County_complete$poverty[!is.na(County_complete$firms)]))
explained_var_firm
#---------------------------------------
explained_var_fed = 1 - (var(Fed_model$residuals)
                         /var(County_complete$poverty[!is.na(County_complete$fed_spending)]))
explained_var_fed

#The explained_var value obtained matches with the "R2" value in the model summary
#In the group mean model, the explained variance for firm was: 0.0257
#In linear model ,it is: 0.0085

#In the group mean model, the explained variance for fed spending was: 0.0614
#In linear model ,it is: 0.0026


#Clearly, group mean model gave a better reult than Linear. Even though, I expected 
#Linear to work better as its more sophisticated

#However, Linear doesn't work well in this case because these variables may
#not satisfy the conditions for linear  model

#testing condition 1: Linearity:
plot(County_complete$firms,County_complete$poverty)

#The plot doesn't show linear relationship between variables, 
#This could be because of the outliers


#Outlier treatment
# quantile function finds the percentile value of the "probs" mentioned. 
# i.e to find 25th percentile or quater1 value, probs will be 0.25
Q1 = quantile(County_complete$firms,na.rm = T,probs = 0.25)
Q3 = quantile(County_complete$firms,na.rm = T,probs = 0.75)
#function IQR finds the interquartile range, which is nothing but Q3-Q1
IQR_ = IQR(County_complete$firms, na.rm = T)

# After calculating the quantiles, I am using the standand out lier treatment method
County_complete$outlier_adj = ifelse(County_complete$firms >  Q3 + 1.5*IQR_,Q3+1.5*IQR_,
                                     ifelse(County_complete$firms < Q1 - 1.5*IQR_,
                                            Q1 - 1.5*IQR_,County_complete$firms))
plot(County_complete$outlier_adj,County_complete$poverty)
#In spite of outlier treatment, the plot doesn't show good linear relationship because 
#the data is strongly skewed


#------------------------------------------------------------------------
#This model may not be the best solution for this problem mainly 
# because of the presence of Outliers
#However, these Outliers cannot be removed as it is definitely saying something
#that a simple linear model cannot discern
#By using some better models this issue can probably be solved