# Vector exercise
age<-c(40,23,24)
age
dim(age)
color<-c("blue", "red", "green")
color
exercise<-c(TRUE, TRUE, FALSE, NA)
exercise
cats<-c(1, "one", TRUE)
cats

a<-c(80, TRUE)
a

sequence<-1:10
sequence

sequence<-seq(1, 10, 2)
sequence

sequence<-seq(1, 8, 2)
sequence

female<-rep("Female", 5)
female

data<-c(12,10,15,18,9,21)
data[3]

data<-c(12,10,15,18,9,21)
data[3:5]
data[4:6]
data[-2]
data[-5]
data[-(4:6)]
data[-c(2,5)]

data[-c(1,3,6)]

data >10
data < 10
data == 10

data>10|data<18
data>10&data<18

data[data>15]
data[data!=10]

sum(data<15)
sum(data<18)
which(data>15)
data[which(data>15)]

min(data)
which(data==min(data))
which.max(data)
max(data)

sort(data)
sort(data, decreasing=TRUE)

x<-c(2,5,7,11,14)
y<-c(1,4,5,7, 6)

x+2
3*x-1
x+y

#Data frame
Height<-c(156, 160, 154, 148, 170, 178, 188, 163, 165, 157)
Weight<-c(48, 56, 45, 50, 68, 75, 81, 65, 70, 49)
mydata<-data.frame(Height,Weight)
mydata
dim(mydata)

Namesresp<-c("Susan", "Mary" ,"Lily" ,"Hillary ","Mike", "John", "William", 
             "Cathy", "Jeff", "Julie")

Variables<-c("Height", "Weight")
dimnames(mydata)<-list(Namesresp, Variables)
mydata

Gender<-c("Female", "Female", "Female", "Female", "Male", "Male", "Male", 
          "Female", "Male", "Female")
Age<-c(30, 36, 26, 47, 31, 49, 28, 50, 42, 47)

mydata<-data.frame(mydata, Gender)
mydata<-data.frame(Namesresp, Height, Weight, Gender, Age)

mydata

mydata[,1]
mydata$Height

mydata[,2:3]
mydata[,c(1,3)]

mydata[4,]
mydata[3:6,]
mydata[c(2,5,7),]

mydata[-1,-5]
mydata[,-(3:4)]

mydata<-mydata[with(mydata, order(Namesresp)), ]
mydata

colnames(mydata)[which(names(mydata) == "Gender")] = "Sex"
mydata


#Matrices

x<-c(20, 30, 27, 25, 28, 21, 23, 24, 29, 21, 33, 19, 22, 24, 22, 20)

m<-matrix(x,nrow=4,ncol = 4)
m
#transpose
m<-matrix(x,nrow=4,ncol = 4,byrow = T)
m

group<-c("A","B","C","D")
obs<-c("sample 1", "sample 2", "sample 3", "sample 4")

dimnames(m)<-list(obs,group)
m

m[2,]
m[2:3,]

sample_5<-c(3,7,14,18)
rbind(m,sample_5)

E<-c(13,17,20,21)
cbind(m,E)

z<-matrix(1,2,3)
z

#list

a<-c(T,F,F,T,T)
b<-matrix(c(1,3,5,8),ncol=2)
c<-data.frame(id=101:103,age=c(40,38,33))
abc<-list(a,b,c)
abc 

abc[1]

#Array

a <- c(5,9,3)
b <- c(10,11,12,13,14,15)
ab <- array(c(a,b),dim = c(3,3,2))
ab

column.names <- c("COL1","COL2","COL3")
row.names <- c("ROW1","ROW2","ROW3")
matrix.names <- c("Matrix1","Matrix2")
ab <- array(c(a,b),dim = c(3,3,2),dimnames = list(row.names,column.names, matrix.names))
ab

ab[1,3,1]
ab[,,2]
