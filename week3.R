getwd()

setwd("C:/Users/smrc/Documents/Rstudio")
ABC<-read.csv("iris_dirty.csv")
ABC
library(readr)
class(ABC)

Mydata1<-read.table(file="student_performance.txt", sep="\t")
#Second option to import
Mydata2<-read.delim("student_performance.txt")
Mydata1
Mydata2
file.choose(new=FALSE)
getlink<-"https://calmcode.io/static/data/fish.csv"
Mydata<-read.table(file=getlink, header=TRUE, sep=",")
Mydata


library(ggplot2)
data(diamonds, package="ggplot2")
data
head(diamonds)
tail(diamonds)

print(summary(Mydata1))
print(summary(ABC))
library(DataExplorer)
create_report(ABC)
create_report(iris)


md<-read.csv("Business Com questionnaire.csv")
md
print(summary(md))
library(DataExplorer)
create_report(md)

#checking for duplication 
sum(duplicated(ABC))
dim(ABC)

#removing duplicates
iris_new<-unique(ABC)

iris_new
rownames(iris_new)<-1:nrow(iris_new)
sum(duplicated(iris_new))
dim(iris_new)

#Encoding categorical variables

iris_new$Species<-tolower(iris_new$Species)
iris_new$Species<-factor(iris_new$Species)

levels(iris_new$Species)[as.integer(iris_new$Species)]
levels(iris_new$Species)[1]<-"setosa"
levels(iris_new$Species)[2]<-"versicolor"
as.numeric(iris_new$Species)
iris_new

create_report(iris_new)
install.packages("psych")


##Handling missing values

#replacing using mean

# Replacing missing values in Sepal.Length with the mean of Sepal.Length
iris_new$Sepal.Length[is.na(iris_new$Sepal.Length)] <- mean(iris_new$Sepal.Length, na.rm = TRUE)

# Replacing missing values in Sepal.Width with the mean of Sepal.Width
iris_new$Sepal.Width[is.na(iris_new$Sepal.Width)] <- mean(iris_new$Sepal.Width, na.rm = TRUE)

# Checking the number of missing values in the dataset
sum(is.na(iris_new))

iris_new
 #detecting the outliers

iris_new<-subset(iris_new, select = -c(Sepal.width))
boxplot(iris_new)

iris_new

sum(is.na(iris_new))
