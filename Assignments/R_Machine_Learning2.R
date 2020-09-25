library(tidyverse)
library(reshape2)

housing = read.csv(file = 'housing.csv')

head(housing)

# Summary stats
summary(housing)

par(mfrow=c(2,5))

colnames(housing)

ggplot(data = melt(housing), mapping = aes(x = value)) + 
  geom_histogram(bins = 30) + facet_wrap(~variable, scales='free_x')

# Clean the Data
housing$total_bedrooms[is.na(housing$total_bedrooms)] = median(housing$total_bedrooms , na.rm = TRUE)

# fix the total columns by making them means
housing$mean_bedrooms = housing$total_bedrooms/housing$households
housing$mean_rooms = housing$total_rooms/housing$households

drops = c('total_bedrooms', 'total_rooms')

housing = housing[ , !(names(housing) %in% drops)]

head(housing)

# Turning categoricals into booleans
categories = unique(housing$ocean_proximity)
#split the categories off
cat_housing = data.frame(ocean_proximity = housing$ocean_proximity)

for(cat in categories){
  cat_housing[,cat] = rep(0, times= nrow(cat_housing))
}
head(cat_housing)

for(i in 1:length(cat_housing$ocean_proximity)){
  cat = as.character(cat_housing$ocean_proximity[i])
  cat_housing[,cat][i] = 1
}

head(cat_housing)

cat_columns = names(cat_housing)
keep_columns = cat_columns[cat_columns != 'ocean_proximity']
cat_housing = select(cat_housing, one_of(keep_columns))

tail(cat_housing)

# Scale the numbers
colnames(housing)

drops = c('ocean_proximity', 'median_house_value')
housing_num = housing[ , !(names(housing) %in% drops)]

head(housing_num)

scaled_housing_num = scale(housing_num)

head(scaled_housing_num)

# Merge the altered numerical and categorical dataframes
cleaned_housing = cbind(cat_housing, scaled_housing_num, median_house_value=housing$median_house_value)
head(cleaned_housing)

# Create a test data set
set.seed(1738)

sample = sample.int(n = nrow(cleaned_housing), size = floor(.8*nrow(cleaned_housing)), replace = F)
train = cleaned_housing[sample, ] # just the samples
test = cleaned_housing[-sample, ] # everything but the samples

head(train)

nrow(train) + nrow(test) == nrow(cleaned_housing)

# Test some predictive models
library('boot')

?cv.glm # note the K option for K fold cross validation

glm_house = glm(median_house_value~median_income+mean_rooms+population, data=cleaned_housing)
k_fold_cv_error = cv.glm(cleaned_housing, glm_house, K=5)

k_fold_cv_error$delta

glm_cv_rmse = sqrt(k_fold_cv_error$delta)[1]
glm_cv_rmse # off by about $83,000

names(glm_house) #what parts of the model are callable?

glm_house$coefficients

# Random Forest Model
library('randomForest')
?randomForest
names(train)

set.seed(1738)

train_y = train[, 'median_house_value']
train_x = train[, names(train) !='median_house_value']

head(train_y)
head(train_x)

# random forest models with different parameters
rf_model = randomForest(train_x, y = train_y, ntree = 500, importance = TRUE)
rf_model1 = randomForest(train_x, y = train_y, ntree = 500, nodesize = 2, importance = TRUE)
rf_model2 = randomForest(train_x, y = train_y, ntree = 1000, nodesize = 2, importance = TRUE)
rf_model3 = randomForest(train_x, y = train_y, ntree = 1000, importance = TRUE)
rf_model4 = randomForest(train_x, y = train_y, ntree = 1500, importance = TRUE)
rf_model5 = randomForest(train_x, y = train_y, ntree = 1500, nodesize = 2, importance = TRUE)

names(rf_model)

rf_model$importance


# Out of bag error estimate (oob)
oob_prediction = predict(rf_model) # Leaving out a data source forces OOB prediction

train_mse = mean(as.numeric((oob_prediction - train_y)^2))
oob_rmse = sqrt(train_mse)
oob_rmse

# How well does the model predict on test data?
test_y = test[, 'median_house_value']
test_x = test[, names(test) !='median_house_value']

# Test the first model at ntrees = 500, nodesize = 5
y_pred = predict(rf_model, test_x)
test_mse = mean(((y_pred - test_y)^2))
test_rmse = sqrt(test_mse)
test_rmse

# Test the first model at ntrees = 500, nodesize = 2
y_pred1 = predict(rf_model, test_x)
test_mse1 = mean(((y_pred1 - test_y)^2))
test_rmse1 = sqrt(test_mse1)
test_rmse1

# Test the second model at ntrees = 1000, nodesize = 2
y_pred2 = predict(rf_model2, test_x)
test_mse2 = mean(((y_pred2 - test_y)^2))
test_rmse2 = sqrt(test_mse2)
test_rmse2

# Test the second model at ntrees = 1000, nodesize = 5
y_pred3 = predict(rf_model3, test_x)
test_mse3 = mean(((y_pred3 - test_y)^2))
test_rmse3 = sqrt(test_mse3)
test_rmse3

# Test the second model at ntrees = 1500, nodesize = 5
y_pred4 = predict(rf_model4, test_x)
test_mse4 = mean(((y_pred4 - test_y)^2))
test_rmse4 = sqrt(test_mse4)
test_rmse4

# Test the second model at ntrees = 1500, nodesize = 2
y_pred5 = predict(rf_model5, test_x)
test_mse5 = mean(((y_pred5 - test_y)^2))
test_rmse5 = sqrt(test_mse5)
test_rmse5

# Visualizations

randomForest::varImpPlot(rf_model)
randomForest::varImpPlot(rf_model1)
randomForest::varImpPlot(rf_model2)
randomForest::varImpPlot(rf_model3)
randomForest::varImpPlot(rf_model4)
randomForest::varImpPlot(rf_model5)



