library(e1071)
install.package(caTools)
library(caTools)
library(caret)

data<-read.csv("C:/Users/smrc/StudentData1.csv")
summary(data)
split<-sample.split(data, SplitRatio=0.5)
train_cl<-subset(data, split=="TRUE")
test_cl<-subset(data, split=="FALSE")
str(train_cl)
str(test_cl)

# feature scalling
train_scale<-scale(train_cl[, 1:4])
test_scale<-scale(test_cl[, 1:4])
train_scale

# fitting naive bayes model to training dataset
set.seed(120)
classifier_cl<-naiveBayes(Admission~ ., data=train_cl)
classifier_cl


y_pred<-predict(classifier_cl, newdata = test_cl) #predicting on test data

cm<-table(test_cl$Admission, y_pred) # confusion matrix
cm
confusionMatrix(cm)


library(randomForest)
library(ggplot2)
library(MASS)

data("Boston")

set.seed(123)
train_indices<-sample(1:nrow(Boston), 0.7*nrow(Boston))
train_data<-Boston[train_indices,]
test_data<-Boston[-train_indices,]

rf_model<-randomForest(medv ~., data=train_data)
predictions<-predict(rf_model, test_data)
ggplot(data.frame(predicted=predictions, Actual=test_data$medv), aes(x=Actual, y=predicted))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color= "blue", linetype="solid")+
  labs(title = "Random Forest Prediction on Boston Housing Dataset", x="Actual Values", y="Predicted Values")
accuracy<-sqrt(mean(predictions -test_data$medv)^2)
cat("Root mean squared error:", accuracy, "\n")

library(randomForest)
library(caret)

set.seed(42)
trainIndex<-createDataPartition(iris$Species, p=0.8, list=FALSE)
trainData<-iris[trainIndex,]
testData<-iris[-trainIndex,]
corMatrix<-cor(trainData[, -5])
highFeatures<-findCorrelation(corMatrix, cutoff=.7)

reducedtrainData<-trainData[, -highFeatures]
trainData
corMatrix
highFeatures

reducedtrainData

model1<-randomForest(Species~., data=reducedtrainData)
print(model1)

reducedTestdata<-testData[, -highFeatures]
pred1<-predict(model1, reducedTestdata)
confusionMatrix(pred1, testData$Species)



#Feature extraction
control2<-rfeControl(functions = rfFuncs, method="cv", number=10)
results2<-rfe(trainData[,1:4], trainData[,5], sizes=c(1:4), rfeControl = control2)
print(results2)

#random forest
model2 <- randomForest(Species ~ ., data = trainData)

model2<-randomForest(Species~.,data=trainData[,c(results2$optvariables, "Species")])
model2 <- randomForest(Species ~ ., data = trainData[, c(results2$optvariables, "Species")])


pred2<-predict(model2, testData[,c(results2$optvariables, "Species")])

confusionMatrix(pred2, testData$Species)



data(mtcars) # Loading Data
mtcars
# Need to scale / Normalize as PCA depends on distance measure
my_pca <- prcomp(mtcars, scale = TRUE, center = TRUE, retx = T)
names(my_pca)

summary(my_pca) # Summary
my_pca
my_pca$rotation # View the principal component loading
dim(my_pca$x) # See the principal components
my_pca$x

# Plotting the resultant principal components
# The parameter scale=0 ensures that arrows are scaled to represent the loadings
biplot(my_pca, main = "Biplot", scale = 0)

my_pca$sdev # Compute standard deviation
my_pca.var <- my_pca$sdev ^ 2 # Compute variance
propve <- my_pca.var / sum(my_pca.var) # Proportion of variance for scree plot

# Plot variance explained for each principal component
plot(propve, xlab = "principal component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b", main = "Scree Plot")

# Plot the cumulative proportion of variance explained
plot(cumsum(propve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

# Find Top n principal component which will at least 
# cover 90% variance of dimension
which(cumsum(propve) >= 0.9)[1]

# Predict mpg using first 4 new Principal Components
# Add a training set with principal components
train.data <- data.frame(disp = mtcars$disp, my_pca$x[, 1:4])

# Running a Decision tree algporithm
library(rpart)
library(rpart.plot)
rpart.model <- rpart(disp ~ ., data = trainData, method = "anova")
rpart.plot(rpart.model)

# LINEAR DISCREMINANT ANALYSIS 

library(MASS) 
library(tidyverse) 
library(caret) 
theme_set(theme_classic()) 
data("iris") # Load the data 

# Split the data into training (80%) and test set (20%) 
set.seed(123) 
training.individuals <- iris$Species %>% 
  createDataPartition(p = 0.8, list = FALSE) 
train.data <- iris[training.individuals, ] 
test.data <- iris[-training.individuals, ] 

# Estimate preprocessing parameters 
preproc.parameter <- train.data %>% preProcess(method = c("center", "scale")) 

# Transform the data using the estimated parameters 
train.transform <- preproc.parameter %>% predict(train.data) 
test.transform <- preproc.parameter %>% predict(test.data) 

model <- lda(Species~., data = train.transform) # Fit the model
predictions <- model %>% predict(test.transform) # Make predictions 
mean(predictions$class==test.transform$Species) # Model accuracy 
model <- lda(Species~., data = train.transform) 
model 


