# Predicting with Machine Learning

data(iris)

# Set random seed
set.seed(42)

# Randomly sample 100 of 150 row indexes
indexes <- sample(
  x = 1:150,
  size = 100)

# Inspect random indexes
print(indexes)

# Create a training set from indexes
train <- iris[indexes, ]

# Create a test set from remaining indexes
test <- iris[-indexes, ]

# Train a decision tree model
library(tree)

# Train the decision tree model
model <- tree(
  formula = Species ~ Petal.Length + Petal.Width,
  data = train)

# Inspect the model
summary(model)

#Visualize the decision tree model
plot(model)
text(model)

# Load color brewer library
library(RColorBrewer)

# Create a color palette
palette <- brewer.pal(3, "Set2")

# Create a scatterplot colored by species
plot(
  x = iris$Petal.Length,
  y = iris$Petal.Width,
  pch = 19,
  col = palette[as.numeric(iris$Species)],
  main = "Iris Petal Length vs. Width",
  xlab = "Petal Length (cm)",
  ylab = "Petal Width (cm)")

# Plot the decision boundaries
partition.tree(
  tree = model,
  label = "Species",
  add = TRUE)

# Predict with the model
predictions <- predict(
  object = model,
  newdata = test,
  type = "class")

# Create a confusion matrix
table(
  x = predictions,
  y = test$Species)

# Load the caret package
library(lattice)
library(ggplot2)
library(caret)

# Evaluate the prediction results
confusionMatrix(
  data = predictions,
  reference = test$Species)

# Save the tree model
save(model, file = "Tree.RData")

