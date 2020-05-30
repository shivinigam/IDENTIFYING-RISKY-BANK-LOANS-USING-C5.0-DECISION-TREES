##### Step 1: Collecting the data #####
credit <- read.csv("credit_new.csv")
View(credit)

##### Step 2: Exploring and Preparing the data ######
## cleaning and standardizing text data ##
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)

## data preparation - creating training and test data sets ##
set.seed(123)
train_sample <- sample(1000, 900)
str(train_sample)
credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

##### Step 3: training a model on the data #####
install.packages("C50")
library("C50")
credit_model  <- C5.0(credit_train[-17], credit_train$default)
credit_model
summary(credit_model)


##### Step 4: evaluating model performance#####
credit_pred <- predict(credit_model, credit_test)
install.packages("gmodels")
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

##### Step 5: improving model performance #####
###BOOSTING THE ACCURRACY OF DECISION TREES###
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)
credit_boost10 
summary(credit_boost10)
credit_boost_pred10 <- predict(credit_boost10, credit_test) 
CrossTable(credit_test$default,credit_boost_pred10,
           prop.chisq = FALSE,prop.r = FALSE,prop.c = FALSE, 
           dnn= c ('actual default','predict default'))
#####MAKING MISTAKES MORE COSTLIER THAN OTHERS####
matrix_dimensions <- list(c("no", "yes"), c("no", "yes")) 
names(matrix_dimensions) <- c("predicted", "actual") 
matrix_dimensions $predicted
matrix_dimensions $actual
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2,    dimnames = matrix_dimensions) 
error_cost 
credit_cost <- C5.0(credit_train[-17], credit_train$default,costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred,prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,             
           dnn = c('actual default', 'predicted default'))
