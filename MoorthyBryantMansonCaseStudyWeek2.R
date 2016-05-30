#Code adapted from Noghrehchi and Alice as cited in Resources
#Methods validated and adapted based on paper by van Buuren

#Import Libraries
library(mice)
library(lattice)
library(VIM)
#set the working dirrectory where the data is stored
setwd("~/documents/quantify/")

#Read the data from csv
data = read.csv("carmpg1.csv", header = TRUE)


summary(data)
#Check check percentage of missing data in features (columns) and samples (rows)
#ideally less than 5%
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,pMiss)
apply(data,1,pMiss)

#d.pattern() to get a better understanding of the pattern of missing data
md.pattern(data)
# Missingness pattern can also be visualised in VIM package
aggr_plot1 = aggr(data, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
#Note that, blue refers to observed data and red to the missing data.

# Delete cases with missing values (complete case analysis), and fit a linear model,
# lm function does these two steps together
missing.model = lm(MPG ~ WEIGHT + CYLINDERS + HP + ENG_TYPE + SIZE, data=data)
summary(missing.model)
#(20 observations deleted due to missingness)

## Fill-in missing values by MI method

# Function mice() in mice package is a Markov Chain Monte Carlo (MCMC) method that uses 
# correlation structure of the data and imputes missing values for each incomplete 
# variable m times by regression of incomplete variables on the other variables iteratively. 
imp = mice(data, m=5, printFlag=FALSE, maxit = 40, seed=2525)

# The output imp contains m=5 completed datasets. Each dataset can be analysed
# using function with(), and including an expression for the statistical analysis approach
# we want to apply for each imputed dataset as follows
fit.mi = with(data=imp, exp = lm(MPG ~ WEIGHT + CYLINDERS + HP + ENG_TYPE + SIZE))
summary(fit.mi)

# Next, we combine all the results of the 5 imputed datasets using the pool() function
combFit = pool(fit.mi) 

round(summary(combFit),2)

#running again with 50 chains to compare
imp50 = mice(data, m=50, printFlag=FALSE, maxit = 40, seed=2525)
fit.mi50 = with(data=imp, exp = lm(MPG ~ WEIGHT + CYLINDERS + HP + ENG_TYPE + SIZE))
combFit50 = pool(fit.mi) 
round(summary(combFit50), 4)
pool.r.squared(fit.mi50)
#This was a big boost  when we ran it will the regression also with var ACCEL
#Professor Mcgee did not want this var included per class wall and question by Moorthy



# We can inspect the distributions of the original and the imputed data:
## scatterplot (of weight and hp) for each imputed dataset
xyplot(imp, WEIGHT ~ HP | .imp, pch = 20, cex = 1.4)

# Blue represents the observed data and red shows the imputed data. These colours are consistent with what they represent from now on. 
# Here, we expect the red points (imputed data) have almost the same shape as blue points
# (observed data). Blue points are constant across imputed datasets, but red points differ
# from each other, which represents our uncertainty about the true values of missing data.

## To detect interesting differences between observed and imputed data
densityplot(imp)

# This plot compares the density of observed data with the ones of imputed data. We expect them
# to be similar (though not identical) under MAR assumption.

# Distributions of the variables as individual points can be visualised by
# stripplot. imputations should be close to the data; we do not expect to see see impossible values like for example negative values
stripplot(imp, pch = 20, cex = 1.2)

# MICE runs m parallel chains, each with a certain number of iterations, and imputes
# values from the final iteration. How many iterations does mice() use and how can we make sure that
# this number is enough?
# To monitor convergence we can use trace plots, which plot estimates against the number of iteration.
plot(imp)


#Visiting sequence: In what sequence does mice() impute incomplete variables?
vis=imp$vis; vis
# mice() imputes values according to the column sequence of variables in the dataset, from left
# to right.