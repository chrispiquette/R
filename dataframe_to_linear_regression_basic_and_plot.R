### Goal:
## 1. Reading in a data frame
## 2. running a linear regression 
## 3. and plotting it using plot()
##Before reading in .csv file, underscores must be added to column header names.

setwd("C:/Users/Christopher/Documents/R")

getwd()

##reads in the csv as a dataframe, setting dates as character format
Imps_Conv_DF <- read.csv(file="C:/Users/Christopher/Documents/R/Ad_Metrics_DF.csv", as.is=TRUE)

Imps_Conv_DF <- read.csv(file="C:/Users/Christopher/Documents/R/Ad_Metrics_DF.csv", colClasses = c("Date","numeric","integer","integer","integer"))

##transformation (change format)

Imps_Conv_DF 

head(Imps_Conv_DF, n=10)

str(Imps_Conv_DF)

##gets averages across columns
sapply(Imps_Conv_DF, mean, na.rm=TRUE)

max(Imps_Conv_DF$Impressions, na.rm = TRUE)
max(Imps_Conv_DF$Spend, na.rm = TRUE)
max(Imps_Conv_DF$Cost, na.rm = TRUE)
min(Imps_Conv_DF$Date)

##Imps_Conv_DF_total_conv <- Imps_Conv_DF$Adj._External_View_Conversions+Imps_Conv_DF$Adj._External_Click_Conversions##

plot(Conv ~ Impressions, data=Imps_Conv_DF)
## if that returns "Error in plot.new() : figure margins too large", expand your R Studio plot viewer on the right.

plot(Conv ~ Date, data=Imps_Conv_DF)
##throws this error: Error in eval(predvars, data, env) : object 'Date' not found
## so, let's explore what data type is set for our data frame "Imps_Conv_DF":

str(Imps_Conv_DF)
## our Date column in the data frame is set to "YYYY-MM-DD"

## this one doesn't work: newDate <- as.Date("Date") 

newDate_from_DF <- as.Date(Imps_Conv_DF$ï..Date, "%Y%m%d")

str(Imps_Conv_DF)

plot(Conv ~ Cost, data=Imps_Conv_DF)
## if that returns "Error in plot.new() : figure margins too large", expand your R Studio plot viewer on the right.

##performs linear regression on the two variables designated (within our data frame)
lm(Conv ~ Cost, data=Imps_Conv_DF)
lm(Conv ~ Impressions, data=Imps_Conv_DF)

##Call:
##  lm(formula = Conv ~ Cost, data = Imps_Conv_DF)

##Coefficients:
##  (Intercept)         Cost  
##       3.8533       0.0656 

## combine the above plot, lm(linear regression) and line coefficient results from above
plot(Conv ~ Cost, data=Imps_Conv_DF)
lm(Conv ~ Cost, data=Imps_Conv_DF)
abline(3.8533, 0.0656)

plot(Conv ~ Impressions, data=Imps_Conv_DF)
abline(3.85, 0.0000574)

abline(lm(Conv ~ Cost, data=Imps_Conv_DF))

hist(Imps_Conv_DF$Conv)
hist(Imps_Conv_DF$Impressions)
hist(Imps_Conv_DF$Cost)

str(Imps_Conv_DF)
ls(Imps_Conv_DF)

plot(Imps_Conv_DF)

#plot Conversions over time
plot(Imps_Conv_DF$Conv~newDate_from_DF,type="l",col="purple",axes=T)


plot(Conv ~ Spend, data=Imps_Conv_DF)



