## Goal: See how classic cars (vs. other vehicles), credit limit ranges and various global countries effect marketing revenue by          modeling their relationships through a neural network.

## Results Summary :
## Our neural network model is about 2.7 times more likely to accurately predict marketing revenue vs. a random guess (with a margin of error).

## Note: This MySQL + R code is largely based on developer David Colley's article here: https://www.mssqltips.com/sqlservertip/5876/predicting-customer-spend-with-a-neural-network-using-r-and-sql-server/

## 1. Load the correct data cleaning packages and libraries .
## 2. Establish Local MySQL Connection
## 3. Read in and prep local MySQL data from ClassicModels customer dataset (MySQL table model here: https://www.richardtwatson.com/dm6e/images/general/ClassicModels.png)
## 4. Run back-propagation neural network with training data.
## 5. Run the same neural network from step 4, with test data.


## 1.
setwd("C:/Users/Christopher/Documents/git/pique_dev_repo/R_Project")

install.packages("RMySQL")
install.packages("DBI")
install.packages("RJDBC")
install.packages("RODBC")
install.packages("RMariaDB")
install.packages("dplyr")

library(RMariaDB)
library(DBI)
library(RMySQL)
library(RJDBC)
library(RODBC)
library(dplyr)

## 2. Enter MySQL password here.
localuserpassword <- "ABC123"

## note: here, the dbname is actually the "schema" in MySQL terms
classicmodels <- dbConnect(RMariaDB::MariaDB(), user='chpiquette', password=localuserpassword, dbname='classicmodels', host='127.0.0.1')

dbListTables(classicmodels)

## 3.

## Update 4.16: changed SQL this to full dataset (removing 2500 record LIMIT)
importedData <- dbGetQuery(classicmodels, "
SELECT 
     CASE WHEN p.productLine IN ('Classic Cars','Vintage Cars') THEN 1 ELSE 0 END AS cars_or_not,
     CASE WHEN c.creditLimit > 109463 THEN 1 ELSE 0 END as creditLimit_mean,
     CASE WHEN c.country IN ('France') THEN 1 ELSE 0 END AS is_France,
     CASE WHEN c.country IN ('USA') THEN 1 ELSE 0 END AS is_USA,
     CASE WHEN c.country IN ('Australia') THEN 1 ELSE 0 END AS is_Australia,
     CASE WHEN c.country IN ('Norway') THEN 1 ELSE 0 END AS is_Norway,
     # CASE WHEN c.country IN ('Poland') THEN 1 ELSE 0 END AS is_Poland,
     CASE WHEN c.country IN ('Germany') THEN 1 ELSE 0 END AS is_Germany,
     CASE WHEN c.country IN ('Spain') THEN 1 ELSE 0 END AS is_Spain,
     CASE WHEN c.country IN ('Sweden') THEN 1 ELSE 0 END AS is_Sweden,
     CASE WHEN c.country IN ('Denmark') THEN 1 ELSE 0 END AS is_Denmark,
     CASE WHEN c.country IN ('Singapore') THEN 1 ELSE 0 END AS is_Singapore,
     # CASE WHEN c.country IN ('Portugal') THEN 1 ELSE 0 END AS is_Portugal,
     CASE WHEN c.country IN ('Japan') THEN 1 ELSE 0 END AS is_Japan,
     CASE WHEN c.country IN ('Finland') THEN 1 ELSE 0 END AS is_Finland,
     CASE WHEN c.country IN ('UK') THEN 1 ELSE 0 END AS is_UK,
     CASE WHEN c.country IN ('Ireland') THEN 1 ELSE 0 END AS is_Ireland,
     CASE WHEN c.country IN ('Canada') THEN 1 ELSE 0 END AS is_Canada,
     CASE WHEN c.country IN ('Hong Kong') THEN 1 ELSE 0 END AS is_Hong_Kong,
     CASE WHEN c.country IN ('Italy') THEN 1 ELSE 0 END AS is_Italy,
     CASE WHEN c.country IN ('Switzerland') THEN 1 ELSE 0 END AS is_Switzerland,
     # CASE WHEN c.country IN ('Netherlands') THEN 1 ELSE 0 END AS is_Netherlands,
     CASE WHEN c.country IN ('Belgium') THEN 1 ELSE 0 END AS is_Belgium,
     CASE WHEN c.country IN ('New Zealand') THEN 1 ELSE 0 END AS is_New_Zealand,
     # CASE WHEN c.country IN ('South Africa') THEN 1 ELSE 0 END AS is_South_Africa,
     CASE WHEN c.country IN ('Austria') THEN 1 ELSE 0 END AS is_Austria,
     CASE WHEN c.country IN ('Philippines') THEN 1 ELSE 0 END AS is_Philippines,
     # CASE WHEN c.country IN ('Russia') THEN 1 ELSE 0 END AS is_Russia,
     # CASE WHEN c.country IN ('Israel') THEN 1 ELSE 0 END AS is_Israel,
     (quantityOrdered*priceEach) AS revenue
     -- SUM(quantityOrdered*priceEach) AS revenue
     FROM orderDetails d
     LEFT JOIN (SELECT 
     productCode, 
     productLine,
     productScale
     FROM  products
     )   p ON d.productCode = p.productCode    
     LEFT JOIN (SELECT DISTINCT
     o.orderNumber,
     creditLimit,
     country,
     o.customerNumber
     FROM customers c
     INNER JOIN orders o ON c.customerNumber = o.customerNumber
     ) c ON d.orderNumber = c.orderNumber
     ORDER BY ( SELECT NULL )
     "
  )


## Data normalization section, setting all columns to range 0-1

# create an object that is the max value of each column in our data frame
largest <- max(importedData$revenue) ## dependent variable
largest_cars_or_not <- max(importedData$cars_or_not)
largest_creditLimit_mean <- max(importedData$creditLimit_mean)
largest_is_France <- max(importedData$is_France)
largest_is_USA <- max(importedData$is_USA)
largest_is_Australia <- max(importedData$is_Australia)
largest_is_Norway <- max(importedData$is_Norway)
largest_is_Germany <- max(importedData$is_Germany)
largest_is_Spain <- max(importedData$is_Spain)
largest_is_Sweden <- max(importedData$is_Sweden)
largest_is_Denmark <- max(importedData$is_Denmark)
largest_is_Singapore <- max(importedData$is_Singapore)
largest_is_Japan <- max(importedData$is_Japan)
largest_is_Finland <- max(importedData$is_Finland)
largest_is_UK <- max(importedData$is_UK)
largest_is_Ireland <- max(importedData$is_Ireland)
largest_is_Canada <- max(importedData$is_Canada)
largest_is_Hong_Kong <- max(importedData$is_Hong_Kong)
largest_is_Italy <- max(importedData$is_Italy)
largest_is_Switzerland <- max(importedData$is_Switzerland)
largest_is_Belgium <- max(importedData$is_Belgium)
largest_is_New_Zealand <- max(importedData$is_New_Zealand)
largest_is_Austria <- max(importedData$is_Austria)
largest_is_Philippines <- max(importedData$is_Philippines)


# coerce all values to our data frame by dividing each observation by the largest value objects we just created for each variable

importedData$revenue <- (importedData$revenue/largest) ## dependent variable
importedData$cars_or_not <- (importedData$cars_or_not/largest_cars_or_not)
importedData$creditLimit_mean <- (importedData$creditLimit_mean/largest_creditLimit_mean)
importedData$is_France <- (importedData$is_France/largest_is_France)
importedData$is_USA <- (importedData$is_USA/largest_is_USA)
importedData$is_Australia <- (importedData$is_Australia/largest_is_Australia)
importedData$is_Norway <- (importedData$is_Norway/largest_is_Norway)
importedData$is_Germany <- (importedData$is_Germany/largest_is_Germany)
importedData$is_Spain <- (importedData$is_Spain/largest_is_Spain)
importedData$is_Sweden <- (importedData$is_Sweden/largest_is_Sweden)
importedData$is_Denmark <- (importedData$is_Denmark/largest_is_Denmark)
importedData$is_Singapore <- (importedData$is_Singapore/largest_is_Singapore)
importedData$is_Japan <- (importedData$is_Japan/largest_is_Japan)
importedData$is_Finland <- (importedData$is_Finland/largest_is_Finland)
importedData$is_UK <- (importedData$is_UK/largest_is_UK)
importedData$is_Ireland <- (importedData$is_Ireland/largest_is_Ireland)
importedData$is_Canada <- (importedData$is_Canada/largest_is_Canada)
importedData$is_Hong_Kong <- (importedData$is_Hong_Kong/largest_is_Hong_Kong)
importedData$is_Italy <- (importedData$is_Italy/largest_is_Italy)
importedData$is_Switzerland <- (importedData$is_Switzerland/largest_is_Switzerland)
importedData$is_Belgium <- (importedData$is_Belgium/largest_is_Belgium)
importedData$is_New_Zealand <- (importedData$is_New_Zealand/largest_is_New_Zealand)
# importedData$is_SouthAfrica <- (importedData$is_South_Africa/largest_is_SouthAfrica)
importedData$is_Austria <- (importedData$is_Austria/largest_is_Austria)
importedData$is_Philippines <- (importedData$is_Philippines/largest_is_Philippines)

## Review
summary(importedData)
head(importedData)
str(importedData)
### 2996 observations (rows), 24 variables (columns)

## Let's split the imported, cleaned up and normalized data into a Training set (80% of the observations), and Test set (for verifying after training).

trainset <- importedData[1:2397, ]
testset <- importedData[2398:2996, ]

## 4.

## With our data prepped and broken into Training and Test sets, 
## let's move into to the neural network architecture setup

install.packages("neuralnet")
library(neuralnet)

## Now train the neural network based on 23 inputs, 1 output
softmax = custom <- function(x) {log(1+exp(x))}
nn <- neuralnet(revenue ~ # this is the output variable
                  cars_or_not +   # these are the independet variables
                  creditLimit_mean +
                  is_France +
                  is_USA +
                  is_Australia +
                  is_Norway +
                  is_Germany +
                  is_Spain +
                  is_Sweden +
                  is_Denmark +
                  is_Singapore +
                  is_Japan +
                  is_Finland +
                  is_UK +
                  is_Ireland +
                  is_Canada +
                  is_Hong_Kong +
                  is_Italy +
                  is_Switzerland +
                  is_Belgium +
                  is_New_Zealand +
                  is_Austria +
                  is_Philippines, 
                data=trainset,       # the name of the data frame 
                hidden=10,                # how many neurons do we want in the hidden layer
                err.fct="sse",           # error function: sum of squared errors
                act.fct=softmax,
                algorithm="rprop+",
                threshold=0.05,
                learningrate=0.1,
                linear.output=FALSE,               
                rep=50,                   # training repetitions for the NN
                lifesign="full"           # verbose output during training 
)

# plot the NN
plot(nn, rep="best", intercept=0)

nn$result.matrix
plot(nn)

plot(nn, rep = "best", x.entry = NULL, x.out = NULL,
     radius = 0.15, arrow.length = 0.2, intercept = TRUE,
     intercept.factor = 0.4, information = TRUE, information.pos = 0.1,
     col.entry.synapse = "black", col.entry = "black",
     col.hidden = "black", col.hidden.synapse = "black",
     col.out = "black", col.out.synapse = "black",
     col.intercept = "blue", fontsize = 12, dimension = 6,
     show.weights = TRUE, file = NULL)


# To analyze the training data, we isolate the independent variables into a new data frame 'indVars':
indVars <- subset(trainset, 
                  
                  select = c( "cars_or_not",	"creditLimit_mean",	"is_France",	"is_USA",	"is_Australia",	"is_Norway",	"is_Germany",	"is_Spain",	"is_Sweden",	"is_Denmark",	"is_Singapore",	"is_Japan",	"is_Finland",	"is_UK",	"is_Ireland",	"is_Canada",	"is_Hong_Kong",	"is_Italy",	"is_Switzerland",	"is_Belgium",	"is_New_Zealand",	"is_Austria",	"is_Philippines"))

head(indVars)
# Now we demonstrate the 'compute' function (in neuralnet), which computes the expected outputs.
nn.results <- compute(nn, indVars)
results <- data.frame(actual=trainset$revenue, prediction=nn.results$net.result)

# Now, we can calculate the differences by adding a new column into importedData with the values of the differences between importedData$TotalDue and importedData$PredictedTotalDue - a simple subtraction.

comparison <- cbind(trainset$revenue, results$prediction)
comparison <- data.frame(comparison)
comparison$X1 <- comparison$X1 * largest 
comparison$X2 <- comparison$X2 * largest 
comparison$delta <- comparison$X1 - comparison$X2 
colnames(comparison) <- c("actual","predicted","delta")

# Given our training data, how much are customers likely to spend?

comparison <- cbind(comparison$actual, comparison$predicted, comparison$delta, runif(nrow(comparison), min=min(comparison$actual), max=max(comparison$actual)))
comparison <- data.frame(comparison)
colnames(comparison) <- c("actual","predicted","delta","random")
comparison$randomDelta <- comparison$actual - comparison$random 
head(comparison)
comparison$delta <- abs(comparison$delta)
comparison$randomDelta <- abs(comparison$randomDelta)
sum(comparison$delta) 
sum(comparison$randomDelta)
mean(comparison$delta)
mean(comparison$randomDelta)

## Training Set Results

# Sum Prediction Delta (NN)  2,914,986
# Sum Random Delta           7,823,965

# Mean Prediction Delta (NN)  1,216
# Mean Random Delta           3,264

## I.e., our NN is about 2.7 times more likely to accurately predict
# marketing revenue vs. a random guess, per the training dataset.


## 5.
## Now let's test the trained NN with our testset we created previously.

# To analyze the training data, we isolate the independent variables into a new data frame 'indVars':
indVars_test <- subset(testset, 
                  
                  select = c( "cars_or_not",	"creditLimit_mean",	"is_France",	"is_USA",	"is_Australia",	"is_Norway",	"is_Germany",	"is_Spain",	"is_Sweden",	"is_Denmark",	"is_Singapore",	"is_Japan",	"is_Finland",	"is_UK",	"is_Ireland",	"is_Canada",	"is_Hong_Kong",	"is_Italy",	"is_Switzerland",	"is_Belgium",	"is_New_Zealand",	"is_Austria",	"is_Philippines"))

head(indVars_test)
str(indVars_test)
# Now we demonstrate the 'compute' function (in neuralnet), which computes the expected outputs.
nn.results_test <- compute(nn, indVars_test)
results_test <- data.frame(actual=testset$revenue, prediction=nn.results_test$net.result)

# Now, we can calculate the differences by adding a new column into importedData with the values of the differences between importedData$TotalDue and importedData$PredictedTotalDue - a simple subtraction.

comparison_test <- cbind(testset$revenue, results_test$prediction)
comparison_test <- data.frame(comparison_test)
comparison_test$X1 <- comparison_test$X1 * largest 
comparison_test$X2 <- comparison_test$X2 * largest 
comparison_test$delta <- comparison_test$X1 - comparison_test$X2 
colnames(comparison_test) <- c("actual","predicted","delta")

# Given our training data, how much are customers likely to spend?

comparison_test <- cbind(comparison_test$actual, comparison_test$predicted, comparison_test$delta, runif(nrow(comparison_test), min=min(comparison_test$actual), max=max(comparison_test$actual)))
comparison_test <- data.frame(comparison_test)
colnames(comparison_test) <- c("actual","predicted","delta","random")
comparison_test$randomDelta <- comparison_test$actual - comparison_test$random 
head(comparison_test)
comparison_test$delta <- abs(comparison_test$delta)
comparison_test$randomDelta <- abs(comparison_test$randomDelta)
sum(comparison_test$delta) 
sum(comparison_test$randomDelta)
mean(comparison_test$delta)
mean(comparison_test$randomDelta)


## Test Set Results

# Sum Prediction Delta (NN)    819,131
# Sum Random Delta           2,245,485

# Mean Prediction Delta (NN)     1,368
# Mean Random Delta              3,749

## I.e., our NN is about 2.7 times more likely to accurately predict
# marketing revenue vs. a random guess.