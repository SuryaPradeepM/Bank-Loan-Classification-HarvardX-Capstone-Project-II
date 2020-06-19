########################################################################################### 
#                                                                                         #
# PH125.9x Capstone-Part(II) Choose Your Own Project                                      #
#                                                                                         #
# HarvardX DataScience Course,                                                            #
#                                                                                         #
# Bank Loan Classification Project                                                        #
#                                                                                         #
# R Script to build models and create predictions for the movie ratings test set.         #                                                                               #
###########################################################################################
# Github link for the project:                                                            #
# https://github.com/SuryaPradeepM/Bank-Loan-Classification-HarvardX-Capstone-Project-II  #
###########################################################################################


# Load necessary dependencies (Install if unavaliable)
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")



####----------------------------Introduction----------------------------####
### OBJECTIVE: Will a person take a personal_loan or not? 
##  Response variable is personal_loan
#

## Read Dataset ##

# Read data directly from my s3 bucket
raw_data <- read_csv("https://datasetbankofindia.s3.ap-south-1.amazonaws.com/Bank_of_India.csv")

# Read data from local file if you clone my repo
# raw_data <- read_csv("./dataset/Bank_of_India.csv")

# The variables in the dataset are described as follows:
# * id:    Customer ID
# * age:    Customer's age in completed years
# * experience:    # years of professional experience
# * income:    Annual income of the customer (in thousands)
# * zip:    Home Address ZIP code.
# * family: The family size of the customer
# * credit_card_spend:    Avg. spending on credit cards per month (in thousands)
# * education:     Education Level. 1: Undergrad; 2: Graduate; 3: Advanced/Professional
# * mortgage:    Value of house mortgage if any. (in thousands)
# * personal_loan:    Did this customer accept the personal loan offered in the last campaign?
# * securities_account:    Does the customer have securities account with the bank?
# * cd_account:    Does the customer have a certificate of deposit (CD) account with the bank?
# * online:    Does the customer use internet banking facilities?
# * credit_card:    Does the customer use a credit card issued by Bank?

# Sample a few data points from the start
head(raw_data)

# Check for missing values
colSums(is.na(raw_data))
# turns out: No missing value; If there were missing values do imputation or knn imputation

# Summary Statistics of the dataset
summary(raw_data)

# Structure of dataset
str(raw_data)

####---------------------Preliminary Data Cleaning---------------------####
### Change wrong data types, Check for missing values
##
#

# Removing unnecessary columns ID and zipcode
raw_data$id <- NULL
raw_data$zip <- NULL

# Do necessary type conversions | Categoridcal Data
raw_data$personal_loan <- as_factor(raw_data$personal_loan)
raw_data$education <- as_factor(raw_data$education)
raw_data$family <- as_factor(raw_data$family)
raw_data$securities_account <- as_factor(raw_data$securities_account)
raw_data$cd_account <- as_factor(raw_data$cd_account)
raw_data$online <- as_factor(raw_data$online)
raw_data$credit_card <- as_factor(raw_data$credit_card)

# Structure of the dataset
str(raw_data)


####---------------------Exploratory Data Analysis---------------------####
### Basic Information & Plots
## 
#


# Calculating odds of taking a personal loan based on following cateogrical variables:

# Calculating odds of taking a personal loan based on whether securities_account = 1
limited = raw_data[raw_data$securities_account == "1",] 
(likely_securities=sum(limited$personal_loan=="1")/sum(limited$personal_loan=="0"))
#If people opened securities account, it is 0.12 times more likely that people would borrow than not

# Calculating odds of taking a personal loan based on whether cd_account = 1
limited = raw_data[raw_data$cd_account == "1",] 
(likely_CD=sum(limited$personal_loan=="1")/sum(limited$personal_loan=="0"))
#If people opened CD account, it is 0.86 times more likely that people would borrow than not

# Calculating odds of taking a personal loan based on whether online = 1
limited = raw_data[raw_data$online == "1",] 
(likely_Online=sum(limited$personal_loan=="1")/sum(limited$personal_loan=="0"))
#If people engaged in Online banking, it is 0.108 times more likely that people would borrow than not

# Calculating odds of taking a personal loan based on whether credit_card = 1
limited = raw_data[raw_data$credit_card == "1",] 
(likely_CC=sum(limited$personal_loan=="1")/sum(limited$personal_loan=="0"))
#If people used bank credit crads, it is 0.107 times more likely that people would borrow than not


## Visuals


### Univariate plots


# Age Distribution bar plot
raw_data %>% 
  ggplot(aes(x = age)) +
  geom_histogram(stat = 'bin', binwidth = 1.8, color = '#595959', fill = '#1E90FF') +
  labs(title = "Age Distribution") +
  theme_fivethirtyeight()

# Experience distribution bar plot
raw_data %>% 
  ggplot(aes(x = experience)) +
  geom_histogram(stat = 'bin', binwidth = 1.8, color = '#595959', fill = '#1E90FF') +
  labs(title = "Experience Distribution") +
  theme_fivethirtyeight()

# Note that age and experience distributions look similar, They might be highly correlated

# Income Distribution bar plot
raw_data %>% 
  ggplot(aes(x = income)) +
  geom_bar(stat = 'bin', bins = 40, color = '#595959', fill = '#1E90FF') +
  labs(title = "Income Distribution") +
  theme_fivethirtyeight()

# Mortgage Distribution bar plot
raw_data %>% 
  ggplot(aes(x = mortgage)) +
  geom_bar(stat = 'bin', color = '#595959', fill = '#1E90FF') +
  labs(title = "Mortage Distribution") +
  theme_fivethirtyeight()

# Seems like most people have no mortage (i.e, mortage is 0)

# Mortage Distribution bar plot for people with mortgages (exclude 0)
raw_data %>% 
  filter(mortgage > 0) %>%
  ggplot(aes(x = mortgage)) +
  geom_bar(stat = 'bin', bins = 40, color = '#595959', fill = '#1E90FF') +
  labs(title = "Mortgage Distribution") +
  theme_fivethirtyeight()

# It is a right skewed distribution

# Credit Card Spending Distribution bar plot for people with mortgages
raw_data %>% 
  ggplot(aes(x = credit_card_spend)) +
  geom_bar(stat = 'bin', bins = 50, color = '#595959', fill = '#1E90FF') +
  labs(title = "Credit Card Spending Distribution") +
  theme_fivethirtyeight()

# It is a right skewed distribution


### Bivariate Plots of categorical variables vs. personal Loan

# Stacked bar plot to test for Credit Card vs. personal loan
raw_data %>% 
  ggplot(aes(x = credit_card, y = personal_loan, fill = personal_loan)) +
  geom_bar(stat = 'identity') +
  coord_flip() + 
  scale_fill_manual(values=c('#DE3533','#51A8C9')) +
  labs(title = "Personal Loan vs. CD Account") +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text()) +
  xlab('Credit Card')


# We can observe that people who do not have a credit card are more likey to get personal loan

# Bar plot to test for Credit Card Spending vs. personal loan
raw_data %>% 
  ggplot(aes(x = credit_card_spend, y = personal_loan, fill = personal_loan)) +
  geom_bar(stat = 'identity') +
  coord_flip() + 
  scale_fill_manual(values=c('#DE3533','#51A8C9')) +
  labs(title = "Personal Loan vs. Credit Card Spending") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab('Credit Card Spending')

# We can observe that people whose credit card spending is high do not generally borrow loans

# Bar plot to test for Income vs. personal loan
raw_data %>% 
  ggplot(aes(x = income, y = personal_loan, fill = personal_loan)) +
  geom_bar(stat = 'identity') +
  coord_flip() + 
  scale_fill_manual(values=c('#DE3533','#51A8C9')) +
  labs(title = "Personal Loan vs. Income") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab('Income')

# We can observe that people with higher incomes do not generally borrow loans

# Stacked bar plot to test for family vs. personal loan
raw_data %>% 
  ggplot(aes(x = family, y = personal_loan, fill = personal_loan)) +
  geom_bar(stat = 'identity') +
  coord_flip() + 
  scale_fill_manual(values=c('#DE3533','#51A8C9')) +
  labs(title = "Personal Loan vs. Family") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab('Family Members')

# Stacked bar plot to test for education vs. personal loan
raw_data %>% 
  ggplot(aes(x = education, y = personal_loan, fill = personal_loan)) +
  geom_bar(stat = 'identity') +
  coord_flip() + 
  scale_fill_manual(values=c('#DE3533','#51A8C9')) +
  labs(title = "Personal Loan vs. Education") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab('Education Level')

# Stacked bar plot to test for online vs. personal loan
raw_data %>% 
  ggplot(aes(x = online, y = personal_loan, fill = personal_loan)) +
  geom_bar(stat = 'identity') +
  coord_flip() + 
  scale_fill_manual(values=c('#DE3533','#51A8C9')) +
  labs(title = "Personal Loan vs. Online") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab('Online')

# Stacked bar plot to test for Securites Account vs. personal loan
raw_data %>% 
  ggplot(aes(x = securities_account, y = personal_loan, fill = personal_loan)) +
  geom_bar(stat = 'identity') +
  coord_flip() + 
  scale_fill_manual(values=c('#DE3533','#51A8C9')) +
  labs(title = "Personal Loan vs. Securities Account") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab('Securities Account')

# Stacked bar plot to test for CD Account vs. personal loan
raw_data %>% 
  ggplot(aes(x = cd_account, y = personal_loan, fill = personal_loan)) +
  geom_bar(stat = 'identity') +
  coord_flip() + 
  scale_fill_manual(values=c('#DE3533','#51A8C9')) +
  labs(title = "Personal Loan vs. CD Account") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  xlab('CD Account')



# Correlation plot for the numeric columns
numeric_vaiables <- c('age', 'experience', 'income', 'mortgage', 'credit_card_spend')

# Using ggcor from GGally package to produce correlation heatmap
ggcorr(raw_data[, numeric_vaiables], nbreaks = 7, 
       low = "#1E90FF", mid = "#AAAAAA", high = "#F21A00",
       label = TRUE, label_size = 7, label_color='black',
       legend.position = 'left') +
  theme_gray()


# We can observe that age and experience are highly correlated, Hence, we remove experience in the next section


####------------------------Data Preprocessing-------------------------####
### Remove Experince variable, split into train, validation, test sets 
##  & scale the numeric data
#

# Removing Experience variable as it is highly correlated with age and will mess with our models if left
raw_data$experience <- NULL

# Data Splitting - Training Data = 60%, Validation Data = 20% & Testing Data = 20%

# Setting seed for reproducibility of results
# Remove sample.kind = "Rounding" if R version < 3.5
set.seed(7, sample.kind = "Rounding")

# sample into three sets to create indices for train, validation, test sets
idx <- sample(seq(1, 3), size = nrow(raw_data), replace = TRUE, prob = c(.6, .2, .2))

# Split the data into three sets
raw_train <- raw_data[idx == 1,]
raw_val <- raw_data[idx == 2,]
raw_test <- raw_data[idx == 3,]

# Standardizing the numeric varaibles as we've seen that the ranges of numerical variables vary quite a bit
# Using preProcess from caret package
(norm.values <- preProcess(raw_train, method=c("center", "scale")))

# Apply the preProcess params to the dat using preProcess.predict
train <- predict(norm.values, raw_train)
val <- predict(norm.values, raw_val)
test <- predict(norm.values, raw_test)

# Check the new ranges
summary(train)



####---------------------------Model Building--------------------------####
### Model development stage
##  At this stage we only use Test and Validation sets to choose best model
#   We use Test set only to evaluate final champion and challenger models


# Helper function to draw the confusion matrix using ggplot
prettyConfusion <- function(results){
  # Convert the results from confusionMatrix toa  data frame
  table <- data.frame(results$table)
  
  # Calcualte Proportions and Predicted columns
  plotTable <- table %>%
    mutate(Predicted = ifelse(table$Prediction == table$Reference, "Correct", "Wrong")) %>%
    group_by(Reference) %>%
    mutate(Proportion = Freq/sum(Freq))
  
  # Fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups
  ggplot(plotTable, aes(x = Reference, y = Prediction, fill = Predicted, alpha = Proportion)) +
    geom_tile() +
    geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", size=10) +
    scale_fill_manual(values = c(Correct = "springgreen2", Wrong = "orangered2")) +
    theme_bw() +
    xlim(rev(levels(table$Reference))) +
    theme_map() +
    theme(legend.position = "none")
}


##--------Logistic Regression Classifier----------##

# train
lr <- train(personal_loan ~ ., data = train, method = "glm", family = binomial)

# predictions
pred_lr <- predict(lr, val)

# Accuracy of the model
(acc_lr <- mean(pred_lr == val$personal_loan))

# Generate Confusion Matrix of the model
results_lr <- confusionMatrix(pred_lr, val$personal_loan, positive="1")

# F1 Score
(f1_lr <- results_lr$byClass['F1'])

# Draw a pretty confusion matrix using the custom helper function
prettyConfusion(results_lr)

##------------Naive Bayes Classifier--------------##

# train
nb <- train(personal_loan ~ ., data = train, method = "nb")

# predictions
pred_nb <- predict(nb, val)

# Accuracy of the model
(acc_nb <- mean(pred_nb == val$personal_loan))

# Generate Confusion Matrix of the model
results_nb <- confusionMatrix(pred_nb, val$personal_loan, positive="1")

# F1 Score
(f1_nb <- results_nb$byClass['F1'])

# Draw a pretty confusion matrix using the custom helper function
prettyConfusion(results_nb)

##---------Linear Discriminant Analysis------------##

# train
ld <- train(personal_loan ~ ., data = train, method = "lda", family = binomial)

# predictions
pred_ld <- predict(ld, val)

# Accuracy of the model
(acc_ld <- mean(pred_ld == val$personal_loan))

# Generate Confusion Matrix of the model
results_ld <- confusionMatrix(pred_ld, val$personal_loan, positive="1")

# F1 Score
(f1_ld <- results_ld$byClass['F1'])

# Draw a pretty confusion matrix using the custom helper function
prettyConfusion(results_ld)

##--------Quadratic Discriminant Analysis----------##

# train
qd <- train(personal_loan ~ ., data = train, method = "qda", family = binomial)

# predictions
pred_qd <- predict(qd, val)

# Accuracy of the model
(acc_qd <- mean(pred_qd == val$personal_loan))

# Generate Confusion Matrix of the model
results_qd <- confusionMatrix(pred_qd, val$personal_loan, positive="1")

# F1 Score
(f1_qd <- results_qd$byClass['F1'])

# Draw a pretty confusion matrix using the custom helper function
prettyConfusion(results_qd)

##----------------------Loess--------------------##

# train
loess <- train(personal_loan ~ ., data = train, method = "gamLoess")

# predictions
pred_loess <- predict(loess, val)

# Accuracy of the model
(acc_loess <- mean(pred_loess == val$personal_loan))

# Generate Confusion Matrix of the model
results_loess <- confusionMatrix(pred_loess, val$personal_loan, positive="1")

# F1 Score
(f1_loess <- results_loess$byClass['F1'])

# Draw a pretty confusion matrix using the custom helper function
prettyConfusion(results_loess)

##----------Support Vector Machine----------------##

# train
svm <- train(personal_loan ~ ., data = train, method = "svmLinear")

# predictions
pred_svm <- predict(loess, val)

# Accuracy of the model
(acc_svm <- mean(pred_svm == val$personal_loan))

# Generate Confusion Matrix of the model
results_svm <- confusionMatrix(pred_svm, val$personal_loan, positive="1")

# F1 Score
(f1_svm <- results_svm$byClass['F1'])

# Draw a pretty confusion matrix using the custom helper function
prettyConfusion(results_svm)


##--------------K Nearest Neighbours--------------##

# Setting seed for reproducibility of results
set.seed(7, sample.kind = "Rounding")

# k values to test best k
k_values <- data.frame(k = seq(2, 12, 1))

# train
knn <- train(personal_loan ~ ., data = train, method = "knn", 
             tuneGrid = k_values)
# best k value
knn$bestTune

# predictions
pred_knn <- predict(knn, val)

# Accuracy of the model
(acc_knn <- mean(pred_knn == val$personal_loan))

# Generate Confusion Matrix of the model
results_knn <- confusionMatrix(pred_knn, val$personal_loan, positive="1")

# F1 Score
(f1_knn <- results_knn$byClass['F1'])

# Draw a pretty confusion matrix using the custom helper function
prettyConfusion(results_knn)

##----------------Random Forest------------------##

# Setting seed for reproducibility of results
set.seed(7, sample.kind = "Rounding")

# Values of mtry
mtryGrid <- data.frame(mtry = c(3,5,7,9))

# train
rf <-  train(personal_loan ~ ., data = train, method = "rf",
             tuneGrid = mtryGrid, importance = T)

# Plot the values of accuracy for values of mtry
ggplot(rf) + theme_minimal()

# best model param
rf$bestTune

# predictions
pred_rf <- predict(rf, val)

# Accuracy of the model
(acc_rf <- mean(pred_rf == val$personal_loan))

# Generate Confusion Matrix of the model
results_rf <- confusionMatrix(pred_rf, val$personal_loan, positive="1")

# F1 Score
(f1_rf <- results_rf$byClass['F1'])


# Draw a pretty confusion matrix using the custom helper function
prettyConfusion(results_rf)

#Variable Importance
varImp(rf)

# We can observe that income, family and creditcard are most important variables

##----------------Ensemble Model------------------##

# votes are added up as total predictions from 6models that predict 1
votes <- (pred_rf == 1) + (pred_svm == 1) + (pred_ld == 1) + (pred_lr == 1) + (pred_loess == 1)

# Ensemble prediction is 1 if atleast three of the five models predict 1
pred_ensemble <- ifelse(votes >= 3, 1, 0)

# Accuradcy of the model
(acc_ensemble <- mean(pred_ensemble == val$personal_loan))

# Generate Confusion Matrix of the model
results_ensemble <- confusionMatrix(factor(pred_ensemble), val$personal_loan, positive="1")

# F1 Score
(f1_ensemble <- results_ensemble$byClass['F1'])

# Draw a pretty confusion matrix using the custom helper function
prettyConfusion(results_ensemble)

##------------Validation Accuracies--------------##

#All models Validation Accuracies
models <- c("Logistic regression", "Naive Bayes Classifier",
            "LDA", "QDA", "Loess","K nearest neighbors",
            "Random forest", "Ensemble" , "Support Vector Machine")
# Accuracy list
accuracy <- c(acc_lr, acc_nb, acc_ld, acc_qd, acc_loess, acc_knn, acc_rf, acc_ensemble, acc_svm)

# Output the accuracies table
data.frame(Model = models, Validation_Accuracy = accuracy) %>%
  arrange(desc(Validation_Accuracy)) %>%
  knitr::kable()

# All models Validation F1 Scores

f1 <- c(f1_lr, f1_nb, f1_ld, f1_qd, f1_loess, f1_knn, f1_rf, f1_ensemble, f1_svm)

# Output the f1 table
data.frame(Model = models, Validation_F1 = f1) %>%
  arrange(desc(Validation_F1)) %>%
  knitr::kable()

####-------------------------------Testing-----------------------------####
### Evaluating the models which were trained and tuned using train data on the test data
##  Generate predictions on test set 
#   Evaluate Accuracies for the top 5 models with good validation accuracies


# Logistic Regression test set predictions
test_lr <- predict(lr, test)

# Confusion matrix for the test predictions of lr model
test_results_lr <- confusionMatrix(test_lr, test$personal_loan, positive="1")

# F1 Score
(test_f1_lr <- test_results_lr$byClass['F1'])

# LDA test set predictions
test_ld <- predict(ld, test)

# Confusion matrix for the test predictions of lda model
test_results_ld <- confusionMatrix(test_ld, test$personal_loan, positive="1")

# F1 Score
(test_f1_ld <- test_results_ld$byClass['F1'])

# Loess test set predictions
test_loess <- predict(loess, test)

# Confusion matrix for the test predictions of loess model
test_results_loess <- confusionMatrix(test_loess, test$personal_loan, positive="1")

# F1 Score
(test_f1_loess <- test_results_loess$byClass['F1'])

# SVM test set predictions
test_svm <- predict(svm, test)

# Confusion matrix for the test predictions of svm model
test_results_svm <- confusionMatrix(test_svm, test$personal_loan, positive="1")

# F1 Score
(test_f1_svm <- test_results_svm$byClass['F1'])

# Random Forest test set predictions
test_rf <- predict(rf, test)

# Confusion matrix for the test predictions of random forest model
test_results_rf <- confusionMatrix(test_rf, test$personal_loan, positive="1")

# F1 Score
(test_f1_rf <- test_results_rf$byClass['F1'])

# votes are added up as total predictions from 6models that predict 1
test_votes <- (test_rf == 1) + (test_loess == 1) + (test_svm == 1) + (test_lr == 1) + (test_ld == 1)

# Ensemble test set predictions
test_ensemble <- ifelse(test_votes >= 3, 1, 0)

# Confusion matrix for the test predictions of ensemble model
test_results_ensemble <- confusionMatrix(factor(test_ensemble), test$personal_loan, positive="1")

# F1 Score
(test_f1_ensemble <- test_results_ensemble$byClass['F1'])

# Selected models
test_models <- c("Random Forest", "Loess" , "Support Vector Machine",
                 "Logistic Regression", "Linear Discriminatn Analyis", "Ensemble")

# Calculate Accuracies for the test set
test_accuracy <- c(mean(test_rf == test$personal_loan),
                   mean(test_loess == test$personal_loan),
                   mean(test_svm == test$personal_loan),
                   mean(test_lr == test$personal_loan),
                   mean(test_ld == test$personal_loan),
                   mean(test_ensemble == test$personal_loan))

# Output the testing accuracies table
data.frame(Model = test_models, Testing_Accuracy = test_accuracy) %>%
  arrange(desc(Testing_Accuracy)) %>%
  knitr::kable()

# All models Validation F1 Scores
test_f1 <- c(test_f1_rf, test_f1_loess, test_f1_svm, 
             test_f1_lr, test_f1_ld, test_f1_ensemble)

# Output the f1 table
data.frame(Model = test_models, Testing_F1 = test_f1) %>%
  arrange(desc(Testing_F1)) %>%
  knitr::kable()
