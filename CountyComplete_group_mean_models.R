# -------------------
# Title : Data Stats and Info assignment 3
# Author: Harshitha Ravindra
# Date: September 30, 2018
# Analysis : Group mean models
# -------------------
# for data table syntax
library(data.table)

# for keeping large numbers as intergers and turning off scientific notation 
options(scipen = 999)

#for group based model analysis
require(mosaic)

setwd('path')

# fread reads a csv file as a data table
County_complete = fread('countyComplete.csv')

#------------poverty by explanatory variable ----------------------------------
# The poverty, or the economic condition of a region can be decided by the employment opportunity in that area and the budget allocated for that area. The 2 variables in CountyComplete data that satisfies the above conditions are
#i.	Total number of firms (firms)
#ii.	Federal spending (fed_spending)

# Creating range/bins for explanatory variables as they are numeric
# Checking distribution to determine the number of bins
boxplot(County_complete$firms)
boxplot(County_complete$fed_spending)

#as both the variables have similar skewed distribution, and require similar treatment and model building, 
#I am constructing a function that would do the outlier treatment, create Bins and construct a group based model
Grp_model = function(expl_var){

# Due to outliers, the bins may be skewed, therfore outlier treatment
# quantile function finds the percentile value of the "probs" mentioned. i.e to find 25th percentile or quater1 value, probs will be 0.25
Q1 = quantile(County_complete[[expl_var]],na.rm = T,probs = 0.25)
Q3 = quantile(County_complete[[expl_var]],na.rm = T,probs = 0.75)
#function IQR finds the interquartile range, which is nothing but Q3-Q1
IQR_ = IQR(County_complete[[expl_var]], na.rm = T)

# After calculating the quantiles, I am using the standand out lier treatment method
County_complete$outlier_adj = ifelse(County_complete[[expl_var]] >  Q3 + 1.5*IQR_,Q3+ 1.5*IQR_,
                                          ifelse(County_complete[[expl_var]] < Q1 - 1.5*IQR_,Q1 - 1.5*IQR_,County_complete[[expl_var]]))

#boxplot to see if outlier treatment removed the skewedness
boxplot(County_complete$outlier_adj)
# Determining number of bins based on distribution, using density method.
# no special reason for using this, we can simply use 5/6 bins based on the distribution range
uniq_expl_var = sort(unique(County_complete$outlier_adj))
Int = round((max(uniq_expl_var) - min(uniq_expl_var))/sd(uniq_expl_var),0)+1

#cut function is used for binning, which creates intervals bassed on the supplied int value, dig.lab sets the number of digits thats displayed in the interval, if its a small number, then intervals are displayed in scienific notation, to avoid it i have set it to 7
County_complete$expl_var_bin = cut(County_complete$outlier_adj, breaks = Int, dig.lab = 7)
#check the binning
v = data.table(table(County_complete$expl_var_bin))
setnames(v,c("V1","N"),c("Bin", "num_obs"))

#Function for group mean at the level of expl variable
Fm = mean(County_complete$poverty ~ County_complete$expl_var_bin)
#Fm
Fmf <- function(x){return(Fm[[x]])}
Fmff <- function(v){sapply(v,Fmf)}

#directly giving the explanatory variable throws error as there can be NAs and the function accepts only character, 
#also giving unique value will save computation
expl_input = Fmff(as.character(unique(County_complete$expl_var_bin[!is.na(County_complete$expl_var_bin)])))
#saving the output of the function as a dataframe
expl_data = data.frame(expl_var_bin = names(expl_input), fitted_expl_var = value(expl_input))
names(expl_data) = c(paste0(expl_var,"_var_bin"),paste0("fitted_poverty_",expl_var))
setnames(County_complete,"expl_var_bin", paste0(expl_var,"_var_bin"))
County_complete$outlier_adj = NULL
#returning a list as the output table "expl_data" is at the level of uniqu group,
# to combine this with county complete, county complete also need to be returned as its manipulated in this 
#function with addition of bins
return(list(expl_data,County_complete))
}
#call the custom group mean function, a list is returned and the 1st object is the output, 2nd is county complete
firm_data = Grp_model("firms")[[1]]
County_complete = Grp_model("firms")[[2]]
fed_spending = Grp_model("fed_spending")[[1]]
County_complete = Grp_model("fed_spending")[[2]]
#adding the fitted column to the county complete dataset using the merge function
#adding argument all.x = T, to create a left join, else rows with NA in firm 
#maynot be considered even if the have values in fedspending in variance calculation below
County_complete = merge(County_complete,firm_data,"firms_var_bin", all.x = T)
County_complete = merge(County_complete,fed_spending,"fed_spending_var_bin", all.x = T)

#the results from above analysis verifies that number of firms and federal spending are 
# good explanatory variables for understanding poverty

#calculating residuals
County_complete$poverty_firm_resid = County_complete$poverty - County_complete$fitted_poverty_firms
County_complete$poverty_fed_resid = County_complete$poverty - County_complete$fitted_poverty_fed_spending

#checking for partitioning property 
County_complete_firm = County_complete[!is.na(County_complete$firms),]
a = var(County_complete_firm$fitted_poverty_firms)
b = var(County_complete_firm$poverty_firm_resid)
c = var(County_complete_firm$poverty)
c-a-b

County_complete_fed = County_complete[!is.na(County_complete$fed_spending),]
x= var(County_complete_fed$fitted_poverty_fed_spending)
y = var(County_complete_fed$poverty_fed_resid)
z = var(County_complete_fed$poverty)
z-x-y

#Both these models satisfy the partitioning properties as c-a-b and z-x-y are zero

#explained variance
fed_ex_var = 1-(b/c)
firm_ex_var = 1-(y/z)


