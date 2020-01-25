---
# title: "Zindi-Sendy Kickoff Tutorial"
# author: "Taryn Morris"
# date: "23/8/2019"
# Link: https://meet.google.com/xkx-fhgs-ufa


#This is a basic machine learning pipeline we will follow:
  
# 0. Configuration: Setting up your workspace
# 1. Import data
# 2. Data cleaning and formatting
# 3. Exploratory data analysis
# 4. Feature engineering and selection
# 5. Building Models
# 5. Hyperparameter tuning (we wont do this (much) today)
# 6. Model evaluation
# 7. Model Interpretation (we wont do this today)
# 8. Documentation and presentation (we wont do this today)


# 0 CONFIGURATION -----------------------------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(ggthemes)
library(corrplot)
library(psych)
library(caret)
library(caretEnsemble)
library(doParallel)
library(magrittr)
library(caret)
library(skimr)
library(RANN)
library(tidyverse)
library(lubridate)
library(ggcorrplot)



# ## 1) IMPORT DATA ------------------------------------------- -------------------------------------------------- -------

# Ici, vous importez les données, vérifiez-les correctement et regardez rapidement les variables.
# Soyez clair sur le problème que vous répondez et les variables auxquelles il est lié.

# ## Importer des données
sendy_raw <- as_tibble(read.csv("/home/moussa/Documents/PROJET/IA/zindi competition/dataset/Train.csv"))

# Regardez-le rapidement de haut en bas et de gauche à droite pour vous assurer que les données ont été chargées.
View(sendy_raw)
# OR
head(sendy_raw)
  
# Un autre moyen pratique de visualiser vos données ...
glimpse(sendy_raw) 

# Look at the names of the variables
names(sendy_raw)

# read through names and make sure we understand what each variable is ... 
# From this list we identify that  our response/target variable is "Time.from.Pickup.to.Arrival"



# What are we trying to predict?
  # Given this dataset, predict the time it takes for delivery (ie from pickup to arrival)

# What type of problem is it? Supervised or Unsupervised Learning? Classification or Regression? Binary or Multiclass? 
# Univariate or Multivariate? Clustering?
   # This is a multivariate supervised machine learning problem in which we have to predict numeric outcomes,
   # thus we'll be using regression techniques.








# 2) DATA CLEANING AND FORMATTING -------------------------------------------------------------------------------------

# Is our data in the right format --> individual observations as rows and features as columns. 


# Do we want to change column names.
  #probably as they imported poorly but for now we will leave this and we will just work with what we have


# Check data types (numeric, integr, dbl, char, factor, date). Are data types ok? 
#What needs fixing? Fix it!
glimpse(sendy_raw)

# First let's fix the time columns
sendy_data <- sendy_raw %>% 
  mutate(Placement...Time = parse_time(as.character(Placement...Time), '%I:%M:%S %p'),
         Confirmation...Time = parse_time(as.character(Confirmation...Time), '%I:%M:%S %p'),
         Arrival.at.Pickup...Time = parse_time(as.character(Arrival.at.Pickup...Time), '%I:%M:%S %p'),
         Pickup...Time = parse_time(as.character(Pickup...Time), '%I:%M:%S %p'),
         Arrival.at.Destination...Time = parse_time(as.character(Arrival.at.Destination...Time), '%I:%M:%S %p'))

View(sendy_data)
summary(sendy_data)


#let's check how our target/response variable was calculated
sendy_data <- sendy_data %>% 
  mutate(delivery_check = Arrival.at.Destination...Time - Pickup...Time) 

View(sendy_data)

sendy_data <- sendy_data %>% select(-delivery_check)  #removes the column we added to check the delivery time.


# Now let's fix the days of the week and the days of the month to be factors rather integers 
sendy_data <- sendy_data %>% 
  mutate_if(is.integer,as.factor) %>% 
  mutate (Distance..KM. = as.integer(Distance..KM.), 
          Time.from.Pickup.to.Arrival = as.integer(Time.from.Pickup.to.Arrival)) # make these integers again

glimpse(sendy_data)

# That looks much better.
Rider <- as_tibble(read.csv("/home/moussa/Documents/PROJET/IA/zindi competition/dataset/Riders.csv"))

merged.data <- merge(sendy_data, Rider, by="Rider.Id")






# 3) EXPLORATORY DATA ANALYSIS ------------------------------------------------------------------------------------------

#The purpose of EDA process is to find anomalies, patterns, trends, or relationships. #EDA generally starts out with
#a high-level overview, and then narrows in to specific parts of the dataset once as we find interesting areas to examine.

#What is our Response Variable? Look at it

#Our response variable is "Time.from.Pickup.to.Arrival". Let's look at some useful statistics:
summary(merged.data$Time.from.Pickup.to.Arrival)

# We can see that delivery time ranges from 1 second to 7883 seconds
# Any thoughts pop into mind here ???


#So Let's look at the distribution in a bit more detail with a histogram 
ggplot(data = merged.data) +
geom_histogram(mapping = aes(x = Time.from.Pickup.to.Arrival), fill = "gray", col = "black") +
  xlab("Delivery time (seconds)")
  
#hmmm we can see there are a lot of lower values. Let's look at this in minutes so it makes a bit more intuitive sense


merged.data <- merged.data %>% mutate(delivery_time_minutes = Time.from.Pickup.to.Arrival/60)
ggplot(data = merged.data) +
  geom_histogram(mapping = aes(x = delivery_time_minutes), fill = "gray", col = "black") +
  xlab("Delivery time (minutes)")

#increase our bins
ggplot(data = merged.data) +
  geom_histogram(mapping = aes(x = delivery_time_minutes), bins= 50, fill = "gray", col = "black") +
  xlab("Delivery time (minutes)")

#Hmmm. WHAT! There are a lot of 0 or low values. #whats up with this? How can a delivery take 0 minutes???

#let's look at all the deliveries that took under say ... 5 minutes to see if they make sense?
deliveries_under_5 <- merged.data %>% filter(delivery_time_minutes <= 5) %>% 
  select(ends_with("Time"), Time.from.Pickup.to.Arrival, delivery_time_minutes) %>% arrange(delivery_time_minutes)

View(deliveries_under_5)

# some of the pick up times are right before the delivery times ... 

#let's look at all the deliveries that took under say  minutes
deliveries_under_10 <- merged.data %>% filter(delivery_time_minutes <= 10) %>% 
  select(ends_with("Time"), Time.from.Pickup.to.Arrival, delivery_time_minutes) %>% arrange(delivery_time_minutes)

ggplot(data = deliveries_under_10) +
  geom_histogram(mapping = aes(x = delivery_time_minutes ), bins=40,fill = "gray", col = "black") +
  xlab("Delivery time (minutes) of deliveries under 10 minutes")
  

# there are probably several ways you can try and see what were more likely legitimate delivery times but for now. 
#Let's just drop anything under 3 minutes.

merged.data <- merged.data %>% filter(Time.from.Pickup.to.Arrival > 180)

summary(merged.data$Time.from.Pickup.to.Arrival)


ggplot(data = merged.data) +
  geom_histogram(mapping = aes(x = Time.from.Pickup.to.Arrival ), fill = "gray", col = "black")
### MUCH BETTER



# Let's look at some other variables

# Here we extend the analyisis to our other variables. We want to pay attention to the minimums and maximums. 
# Also, mean and median difference is something to be concerned about. 
# We would like all our variables to follow the normal distribution as much as possible.


# One way we can look at hte summary statistics of all the variables is
summary(merged.data)
# quite a lot of variables to look at. Let's use a different method

some_summaries <- skim_to_wide(sendy_data)
print(some_summaries, n=35)


###  Let's take a closer look at the numerical values
some_summaries[26:33, c(1:5,12,13,19)]


# We can see that ...
# 1) Precipitation and temperature have some missing values.
# But otherwise the rest look okay - Let's fix this ... 


### MISSING VALUES

# Let's have a look and decide what to do with the missing values.
# Remember, ML algorithms do not know how to handle missing values.


# Precip: 
Precip <- sendy_data %>% select(Precipitation.in.millimeters) %>% drop_na %>% 
  mutate(Precipitation.in.millimeters = as.factor(Precipitation.in.millimeters)) %>% 
  group_by(Precipitation.in.millimeters) %>% tally()

Precip

# it seems there are no 0 precip values so perhaps the NAs are 0s. We will assume this and replace all NAs with 0s.
sendy_data <- sendy_data %>% mutate(Precipitation.in.millimeters= replace_na(Precipitation.in.millimeters, 0))



# How about Temperature?
# what do we do with the missing temperature data.

# The mean is 23.21, and sd is 3.62
# Let's look at the distribution
ggplot(data = merged.data) +
  geom_histogram(mapping = aes(x = Temperature ), fill = "gray", col = "black")

# So it probably wouldnt be terrible to impute the few missing values with the mean (in this case!)

merged.data <- merged.data %>% mutate(Temperature= replace_na(Temperature, mean(Temperature, na.rm=T)))

some_summaries <- skim_to_wide(merged.data)
print(some_summaries, n=22)

some_summaries[14:22, c(1:5,12,13,19)]

#BOOM! We have fixed the missing data issues



### OUTLIERS
# Let's have a quick check for ouliers in the other numerica data (doing this visually for now but you could use more reliable methods)
sendy_numeric <- select_if(merged.data, is.numeric) # select numeric variables
view(sendy_numeric)
ggplot(stack(sendy_numeric[,-8]), aes(x = ind, y = values)) +
  geom_boxplot()       

#There are some high values in Precip. Are these are likely true values?





#Let's look at all the variables that are factors ... 
some_summaries <- skim_to_wide(merged.data)
print(some_summaries, n=24)
# we can see... 
# 1) we probably have a lot of similarity in month days and days of week (duh!)
# 2) there is only one kind of vehicle type - should we keep this?

#Let's address these two issues

#Lets look if there is repitiiton in the months and days
names(merged.data)
sendy_month_days <- merged.data %>% select(ends_with("Day.of.Month"))
sendy_month_days

# we can see they are very similar - if not identical. Let's see how similar ... 
summary(sendy_month_days[1] == sendy_month_days[2])
summary(sendy_month_days[2] == sendy_month_days[3]) #all true
summary(sendy_month_days[2] == sendy_month_days[4]) #all true
summary(sendy_month_days[2] == sendy_month_days[5]) #all true
summary(sendy_month_days[2] == sendy_month_days[6])


#how about the days of the week

sendy_week_days <- sendy_data %>% select(ends_with("Mo...1."))
sendy_week_days
summary(sendy_week_days[1] == sendy_week_days[2])
summary(sendy_week_days[2] == sendy_week_days[3]) #all #true
summary(sendy_week_days[2] == sendy_week_days[4]) #all #true
summary(sendy_week_days[2] == sendy_week_days[5]) #all #true
summary(sendy_week_days[2] == sendy_week_days[6]) 

#same thing 

# we can see these columns are actually almost identical except in 9 instances. We should probably check the 9 
# instances to see what caused this but for now we assume these are mostly identical 
# and we can drop the extra columns

# we can also drop vehicle type because these are all the same

merged.data <- merged.data %>% 
  mutate(Order.Month.Day = Pickup...Day.of.Month, Order.Week.Day = Pickup...Weekday..Mo...1.) %>% #add in a single day of month and say of week column
  select(-ends_with("Day.of.Month")) %>% #drop all other day of month columns
  select(-ends_with("Mo...1.")) %>%  ##drop all other day of week columns
  select(-Vehicle.Type) %>% # drop vehicvle type since they are all the same
  select(Order.No, User.Id, Platform.Type, Personal.or.Business, Order.Month.Day, Order.Week.Day, everything()) #reorder variables

view(merged.data)



# NOW

### Let's look at relationship between the numerical varaibles and the delivery time

merged.data <- merged.data %>%
  select(-delivery_time_minutes)%>%
  select(-Age)%>%
  select(-No_of_Ratings)

names(merged.data)
# select numeric variables
sendy_numeric <- select_if(merged.data, is.numeric)

# calulate the correlations
r <- cor(sendy_numeric, use="complete.obs")
round(r,2)

ggcorrplot(r, type="lower", lab=TRUE)

#here we see that Distance.KM (unsurprisingly) is highly correlated to delivery time.


# With this, we consider the EDA phase over and we move on to feature selection and creating some models.
# At this stage you may realise you have some more EDA  or data cleaning to do AND
# there is also a secondary data set with information regarding the Riders - linked to their Rider.Id that you would need to consider including.

# But for this tutorial we are done. So let's get to it.



### 4) FEATURE ENGINEERING & SELECTION ------------------------------------------------------------------------------------------

# Select which variables want to use in models
# transform any variables that might need to be transformed (eg log transformed, square root transformed)
# One-hot encode the categorical variables



#SELECT VARIABLES TO USE
# Select the variables we want to use in our simple model
sendy_processed <- merged.data %>% select(Order.No, Order.Week.Day, Arrival.at.Pickup...Time, Distance..KM., 
                                          Pickup.Lat,Pickup.Long,Destination.Lat,Destination.Long,Temperature,Average_Rating,Time.from.Pickup.to.Arrival)

#Actually let's remove order number from model as it isnt useful information 
sendy_processed$Order.No <- NULL # remove Order.No from data 

#Faisons d'abord une vérification globale:
anyNA(sendy_processed)
#pourrions détecter cela pour chaque colonne:
sapply(sendy_processed, {function(x) any(is.na(x))})

#Vérifier les valeurs aberrantes et autres points de données incohérents. Boîtes à moustaches.
boxplot(sendy_processed[-9], col = "orange", main = "Features Boxplot")
#Nous constatons qu'il existe plusieurs valeurs aberrantes potentielles, mais j'estime que la fonction "âge" pourrait être la plus problématique. Regardons cela isolé:
attach(sendy_processed)
boxplot(Time.from.Pickup.to.Arrival, col = "red")

#Découvrons quelles sont ces valeurs et combien d’entre elles sont présentes.
age_outliers <- which(Time.from.Pickup.to.Arrival > 4000)
sendy_processed[age_outliers, "Time.from.Pickup.to.Arrival"]

### ONE-HOT ENCODING DUMMY VARIABLES
#Now, creating dummy variables using one-hot encoding (for e.g.  day of the week)
dmy <- dummyVars(" ~ .", data = sendy_processed,fullRank = F)
sendy_processed <- data.frame(predict(dmy, newdata = sendy_processed))
view(sendy_processed)
sendy_processed <- sendy_processed[,-7]
write_csv(sendy_processed,"/home/moussa/Documents/PROJET/IA/zindi competition/dataset/x_tarin.csv")

  str(sendy_processed) # Cool #thumbsup!

glimpse(sendy_processed)
sendy_processed=sendy_processed[,-7]
view(sendy_processed["Order.Week.Day.7"])
summary(sendy_processed["Order.Week.Day.7"])
#correlation
cor(sendy_processed)
corrplot(cor(sendy_processed), method = "square")
chart.Correlation(sendy_processed)
 
# 5) MODEL BUILDING --------------------------------------------------------------------------------------------------------

### PRE-PROCESSING

#We are going to use the Caret package (http://topepo.github.io/caret/index.html) but you can easily use just the basic packages for this
#In this session we are going to build some simple models. But you will go forth and dazzle us with your brilliance during the next 8 hours :)


### SPLIT THE DATA INTO TRAIN AND TEST SETS
#We will use caret's createDataPartition() function, which generates the partition indexes for us, 
#and we will use them to perform the splits:

set.seed(123)
 
part.index <- createDataPartition(sendy_processed$Time.from.Pickup.to.Arrival, p = 0.8, list = FALSE) # create random order to split the data


sendy_train <- sendy_processed[part.index, ] # create the training data set 
sendy_test <- sendy_processed[-part.index, ] # create the test/validation data set


view(sendy_train)

# Before we test some models - we may need to consider normalising or scaling the data ... 
#While algorithms such as Linear Regression and Random Forest do not require feature scaling, 
#others such as KNN and SVM require because they account for euclidean distance.

# But I'm going to leave that to you ... I can't do allllll the work for you
# (Caret has options of including this in your model build)




# BUILD THE MODELS - WOOOOOOOT!

# Caret has a lot of different models, check 'em out!
modelnames <- paste(names(getModelInfo()), collapse=',  ')
modelnames

# Let's  lookup some information on models a   modellookup (but remember google is king ... or queen :) )
modelLookup('lm')

# Hyperparameter selection
  # Caret was actually doing hyperpar ameter selection without us asking! (although we didnt need it for the lm)

# Cross Validation
  # CV is a validation technique where we retrain our model on different splits of our data to get an 'average performance' 
  # For  more information on cross validation: https://towardsdatascience.com/cross-validation-70289113a072

# To control validation techniques during training we can use the train control function


my_trControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
# this function stipulats:
#     - the method of training: Cross validation (cv) 
#     - Number of folds: 10
#     - Is our process is going to be chatty: TRUE


# MODEL 1 - SIMPLE LINEAR MODEL
simple_lm <- lm(Time.from.Pickup.to.Arrival ~ ., data=sendy_train)
view(simple_lm)
model_lm <- train(Time.from.Pickup.to.Arrival ~ ., data=sendy_train, method='lm', trControl = my_trControl)
model_lm$results
summary(model_lm)
  


### MODEL 2 - "rpart"
modelLookup('rpart') # Recursive partitioning decision tree
model_rpart <- train(Time.from.Pickup.to.Arrival ~ ., data=sendy_train, method='rpart', trControl = my_trControl)
model_rpart$results[1,]

# HINT - if you dont like the autotuning of hyperparameters we could try tuning these ourselves
#model_rpart <- train(Time.from.Pickup.to.Arrival ~ ., data=sendy_train, method='rpart', trControl = trControl,
 #                    tuneGrid = expand.grid(cp = seq(0.000, 0.02, 0.0025)))
 # Knock yourselves out - you have all day :)


### MODEL 3 - "svmRadial"
modelLookup('svmRadial') # Support Vector Machine (Radial Kernel)
# this model takes a few mins to run
model_svmRadial <- train(Time.from.Pickup.to.Arrival ~ ., data=sendy_train, method='svmRadial', trControl = my_trControl)
model_svmRadial$results[1,]


### MODEL 4 - "RF"
modelLookup('rf') # Random forest
# this model takes a few mins to run
#model_rf <- train(Time.from.Pickup.to.Arrival ~ ., data=sendy_train, method='rf', trControl = my_trControl)
#model_rf$results[1,]


### MODEL 5 - "XGBTree"
modelLookup('xgbTree') # 
# this model takes a few mins to run
model_xgbTree <- train(Time.from.Pickup.to.Arrival ~ ., data=sendy_train, method='xgbTree', trControl = my_trControl)
model_xgbTree$results[1,]

### COMPARING MULTIPLE MODELS

# Compare the models using resamples
 #model_comp <- resamples(list(LinearModel = model_lm, Rpart = model_rpart, SVM = model_svmRadial, Random_Forest = model_rf, XGBoost = model_xgbTree))
model_comp <- resamples(list(LinearModel = model_lm, Rpart = model_rpart, SVM = model_svmRadial, XGBoost = model_xgbTree))

 summary(model_comp)
 dotplot(model_comp)
 modelCor(model_comp)
 #Nous pouvons voir ici la grande importance des variables "distance" et "avarage raiting" dans la prédiction .
 plot(varImp(model_xgbTree))                         

# https://github.com/gabrielpierobon/ML-workflow-caret

 
#### 6) MODEL VALIDATION -----------------------------------------------------------------------------------

 #Predict based on our models
 pred_lm <- predict.train(model_lm, sendy_test[,-length(sendy_test)])
 pred_rpart <- predict.train(model_rpart, sendy_test[,-length(sendy_test)])
 pred_svmRadial <- predict.train(model_svmRadial, sendy_test[,-length(sendy_test)])
 pred_xgbT <- predict.train(model_xgbTree, sendy_test[,-length(sendy_test)])

 length(sendy_test)
 sendy_test[,length(sendy_test)]
 view(sendy_test[,length(sendy_test)])
 
 # Check the error metric to see which model performed the best RMSE
 pred_RMSE <- data.frame(LM = RMSE(pred_lm, sendy_test[,length(sendy_test)]),
                         RPART = RMSE(pred_rpart, sendy_test[,length(sendy_test)]),
                         SVM = RMSE(pred_svmRadial, sendy_test[,length(sendy_test)]),
                         XGBT = RMSE(pred_xgbT, sendy_test[,length(sendy_test)]))
 print(pred_RMSE)
 
 dotplot(pred_RMSE, metric = "RMSE")
 #la corrélation entre les prédictions utilisant un réseau de neurones à 5 couches
 y_test=sendy_test[,length(sendy_test)]
 pred_cor <- data.frame(LM = cor(pred_lm, y_test),
                        RPART = cor(pred_rpart, y_test),
                        SVM = cor(pred_svmRadial, y_test),
                        XGBT = cor(ç=pred_xgbT, y_test))
 pred_cor
 plot(pred_cor)
 
 
 #fichier test 
 test <- as_tibble(read.csv("/home/moussa/Documents/PROJET/IA/zindi competition/dataset/Test.csv"))
 test
 test <- test %>% 
   mutate(Placement...Time = parse_time(as.character(Placement...Time), '%I:%M:%S %p'),
          Confirmation...Time = parse_time(as.character(Confirmation...Time), '%I:%M:%S %p'),
          Arrival.at.Pickup...Time = parse_time(as.character(Arrival.at.Pickup...Time), '%I:%M:%S %p'),
          Pickup...Time = parse_time(as.character(Pickup...Time), '%I:%M:%S %p'))
merge_test <- merge(test, Rider, by="Rider.Id")
attach(merge_test)
merge_test <- merge_test %>% 
  mutate(Order.Month.Day = Pickup...Day.of.Month, Order.Week.Day = Pickup...Weekday..Mo...1.) %>% #add in a single day of month and say of week column
  select(-ends_with("Day.of.Month")) %>% #drop all other day of month columns
  select(-ends_with("Mo...1.")) %>%  ##drop all other day of week columns
  select(-Vehicle.Type) %>% # drop vehicvle type since they are all the same
  select(Order.No, User.Id, Platform.Type, Personal.or.Business, Order.Month.Day, Order.Week.Day, everything()) #reorder variables


view(merge_test)
merge_test <- merge_test %>% mutate(Temperature= replace_na(Temperature, mean(Temperature, na.rm=T)))
sendy_test <- merge_test %>% select(Order.No,Order.Week.Day, Arrival.at.Pickup...Time, Distance..KM., 
                                    Pickup.Lat,Pickup.Long,Destination.Lat,Destination.Long,Temperature,Average_Rating)
sendy_test$Order.No <- NULL # remove Order.No from data
sendy_test$Order.Week.Day <- as.factor(sendy_test$Order.Week.Day)
dmy <- dummyVars(" ~ .", data = sendy_test)
sendy_test <- data.frame(predict(dmy, newdata = sendy_test))
str(sendy_test)
view(sendy_test)
sendy_test <- sendy_test[,-7]
write_csv(sendy_test,"/home/moussa/Documents/PROJET/IA/zindi competition/dataset/x_test.csv")


 ##faire la prediction
 #submission <- as_tibble(read.csv("/home/moussa/Documents/PROJET/IA/zindi competition/dataset/SampleSubmission.csv"))
 #submission
 #validation=
 #predictions <- predict(model_xgbTree, validation)
 #ssconfusionMatrix(predictions, validation$Species)
 # choose the best model - and then use this to predict on the provided test data. and submit your answer on zindi.africa :)
 
 
 # https://github.com/MwendaMugendi/zindi-presentation/ - here is a similar example in python notebook
 # https://github.com/gabrielpierobon/ML-workflow-caret - here is a similar (more thorough) example in python notebook
 
 
