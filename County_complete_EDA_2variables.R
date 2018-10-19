# -------------------
# Title : Data Stats and Info assignment 1
# Author: Harshitha Ravindra
# Date: September 10, 2018
# Analysis : Exploratory data analysis of county complete data
# -------------------

# County complete dataset has 53 columns/attributes of 3143 counties. For the exploratory 
# data analysis, I chose Percent women population, percent of women owned firms and per 
# capita income to see the impact of female population and women owned firms on the 
# per capita income at the county and state level.
# For explanation of the variable names, I refered the metadata resource in the link -  
# https://www.rdocumentation.org/packages/openintro/versions/1.7.1/topics/countyComplete

#-------------------
# Libraries used
#1.	data.table - to use data table syntax
#2.	vioplot - this is used to generate violin plots which is a boxplot with a depiction of 
#   distribution of the data
#3.	usmap - to plot US map 
#4. ggplot2 - to use color fill(scale fil continous function) and use color grading to 
#   differentiate the magnitude between regions
#5. rmarkdown - This library converts the R code to a report - Rmarkdown format, which I 
#   will be using to convert my work into the report for the assignment

library(data.table)
library(usmap)
library(vioplot)
library(ggplot2)
library(rmarkdown)

# setting working directory to a folder where I have saved the input files and save the output
setwd('path')
#-------------------

# I am using data.table syntax in this analysis as it is concise, faster and memory efficient. 

#-------------------
County_complete = fread('countyComplete.csv')

# Summary will show the mean, median, Quartile values, range and the number of missing values. 
# I checked the summary and commented it as it took a lot of space in the report
#summary(County_complete)

# changing FIPS to fips to make plot_usmap function recognise the county data
setnames(County_complete,"FIPS","fips")


# Choosing State, name and FIPS for categorical variable
# Variables to analyse: Percent women population, percent_women_owned_firms 
# and per_capita_income

Analyse_data = County_complete[,.(state, fips, female, 
                                  women_owned_firms, per_capita_income)]

#----------------------------------
# I am defining a function to collate the distribution of the columns that are being analysed, 
# The EDA function calculates the mean, standard deviation andd median of the focus column and 
# returns a row of a datatable 

EDA = function(to_analyse){
  average_val = round(mean(to_analyse, na.rm = T))
  std_dev = round(sd(to_analyse, na.rm = T),2)
  median_val = round(median(to_analyse, na.rm = T))
  output = data.table(average_val, std_dev,median_val)
  return(output)
}

# As I am interested to tabulate the central tendency for 3 columns, I use sapply - 
# which will apply the function to each column specified and return a list table format
measure_dispersion = sapply(Analyse_data[,3:5, with = F],FUN = EDA)
measure_dispersion

#------------------------------------------
# Violin plots to visualize the distribution of varibles
# removing 'NA's as the plot doesn't accept NAs

x1= na.omit(Analyse_data$female)
x2 = na.omit(Analyse_data$women_owned_firms)
x3 = na.omit(Analyse_data$per_capita_income)
vioplot(x1,x2,names=c("female percentage", "women owned firm percentage"),
        col="gold")
# From the plot, it can be seen that the population of women is majorly 50%, However the 
# share of women owned firms are just around 26%
#---------------------
vioplot(x3, names = "per capita income", col = "blue")
# Most of the regions have very close per capita income, what is the percent of women and 
# women owned firms in the peaking regions with high per capita?
#-----------------------------
# Analysis to explore the relationship between number of women owned firms, 
# women population and per capita income of the region

# This function plots a scatter plot of % women and % women owned firms, then it is 
# overlapped by another plot of % women and per capita income, by which we can compare the 
# relationship between the % women firms and per capita income
# functions used - par to combine multiple graphs, plot - to plot, 
# axis - to add artificial y- axis to the 2nd plot,
# mtext - to artifically add y label to the 2nd plot, legend - to add labels to the points
# 2 axis plot reference: https://thepracticalr.wordpress.com/2016/08/30/2-y-axis-plotting/
plotter = function(a,b,c,d){
  par(mar = c(5, 4, 2, 4))
  plot(a, b, ylab = "percent women owned firms",
       main = d, xlab = "percent women",
       col = "blue", pch = 16)
  par(new = TRUE)
  plot(a,c, xaxt = "n",yaxt = "n",
       ylab = "", xlab = "", col = "red", pch = 8)
  axis(side = 4)
  
  mtext("Per capita income", side = 4, line = 3)
  legend("topleft", c("w_o firms", "per capita"),
         col = c("blue", "red"), pch = c(1, 1))
}

#-----------------------
# For 30% of the counties, women owned firms data is missing
# For this analysis, I am excluding the counties with no information 

Analyse_data_70 = Analyse_data[is.na(women_owned_firms)==F,]
plotter(Analyse_data_70$female,Analyse_data_70$women_owned_firms,
        Analyse_data_70$per_capita_income,
"Impact of women in per capita income by counties")

# From the output we can see that the number of women owned firms are highest in the 
# counties where the female population is high, does it mean that always some percent of 
# women take up entrepreneurial task? 
# In certain counties, the number is high despite of low population, is it because of the
# facilities or the environment friendly to women entrepreneurs?

# There is also a strong correlation between the per capita income and women owned firms. 
# Two Hypotheses for this observation can be, 
#1. Per capita income of the region is high because of the large number of women owned firms
#2. Due to good economic condition of the county, women enterpreuners are thriving

#----------------------------------------------------------
# Analysis to see if there is a similar hypotheses at state level 
Analyse_state = Analyse_data[,.(female = mean(female),
                          per_capita_income = mean(per_capita_income), 
                          women_owned_firms= mean(women_owned_firms, na.rm = T)),state]

plotter(Analyse_state$female,Analyse_state$women_owned_firms,
        Analyse_state$per_capita_income,
        "Impact of women in per capita income by state")

# The state level may not be accurate as it is averaging the avererages.
# However, even states show similar behaviour.Especially the state with the 
# highest percapita income,has the highest number of women owned firms as the 
# 2 points overlap on the right top corner 

#----------------------------
#
# State wise visualization of each variable using usmaps library - through this we 
# can look at the distribution of each metrics across the States

#--------------------------------


plot_usmap(data = Analyse_state, regions = "state", values = "female", lines = "blue") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Female population", label = scales::comma
  ) + theme(legend.position = "right")

# The female population is more or less the same and slightly higher on the east


#------------------
plot_usmap(data = Analyse_state, regions = "state", values = "per_capita_income", 
           lines = "blue") + 
  scale_fill_continuous(
    low = "white", high = "green", name = "PCI", label = scales::comma
  ) + theme(legend.position = "right")


#-----------------------
plot_usmap(data = Analyse_state, regions = "state", values = "women_owned_firms", 
           lines = "blue") + 
  scale_fill_continuous(
    low = "white", high = "gold", name = "Women owned firms", label = scales::comma
  ) + theme(legend.position = "right")

#-----------------------------------
# Conclusion
# Through this analysis, we can establish that the per capita income is somewhat 
# correlated to presence of women and women entreprenuers 
# Further regression analysis can be performed to verify the correlation and 
# test the hypotheses

