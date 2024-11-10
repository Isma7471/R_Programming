friend.data<-data.frame(
  friend_Id = c(1:5),
  friend_name = c("Eva", "Diana", "David","Thomas", "Adam"),
  stringsAsFactors = FALSE
)
print(friend.data)


print(str(friend.data))

print(summary(friend.data))

result<-data.frame(friend.data$friend_name)
print(result)
print(result[1])
result[1,]


#
result[1,1]

friend.data[1,]
friend.data[1,1]
friend.data[1,2]
friend.data[,2]


#Expanding data frame

friend.data$location<-c("Norwich", "London", "Birmingham", "Sheffield", "Nottingham")

resultant<-friend.data
print(resultant)



#creating a Factor in R

x<-c("female","female", "male", 'male')
gender<-factor(x)
print(gender)

gender<-factor(c('female', 'female', 'male', 'male'),
              levels=c('female', 'male'));

gender


#Checking for a factor in R

gender<-factor(c('female', 'female', 'male', 'male'));
print(is.factor(gender))

class(gender)
class(resultant)


#accessing an elements og a factor in R

gender[3]
gender[5]


#accessing more than one element

gender[c(2:4)]
gender[c(2,4)]

gender[-3]

gender[c(1,2,3)]
gender[c(1:3)]

#Mdification

gender[2]<-"male"
gender


#Factors in dataframe

age<-c(42, 48, 49, 40, 52, 58, 53)
salary<-c(10320, 10620, 15020, 8606, 12390, 14070, 10220)
gender<-factor(c('female', 'male', 'male','female','male','female','male'))
employee<-data.frame(age,salary,gender)
print(employee)
print(is.factor(employee$gender))



#Numeric vectors

vector_1<-c(4,5,6,7)
#display types of vector

typeof(vector_1)

vector_2<-c(1L,3L,2L, 6L)

typeof(vector_2)

#Character vectors

vector_3<-c('world', '8', "Hello")

typeof(vector_3)


#Logical Vectors

vector_4<-c(FALSE, FALSE, TRUE, NA)
typeof(vector_4)
levels(vector_4)
dim(vector_4)
length(vector_4)
NROW(vector_4);NCOL(vector_4)

#creating vectors

x<-c(61, 7, 21, 77,89, 5)
cat('Using c function', x, "\n")
y<-seq(1, 10, length.out =5)
cat('Using seq() function', y, "\n")
z<-2:7
cat('Using colon', z)


#Accessing vector element

x<-c(61, 7, 21, 77,89, 5)
cat('Using c function', x[2], "\n")
y<-seq(1, 10, length.out =5)
cat('Using seq() function', y[c(4,1)], "\n")
z<-2:7
cat('Using colon', z[z>4])


#crearing a vector

x<-c(2,7, 6,7,5,)

x[3]<-1
x[2]<-3

cat('subscribt operator', x, "\n")

#modify using different logic
x[x>5]<-0

cat("using indexing", x, '\n')

#sorting elements in a vector
x<-c(2,7, 6,7,5, 8)
A<-sort(x)
cat('Ascending order', A, '\n')
B<-sort(x, decreasing = TRUE)
cat("DESCENDING ORDER", B)
