#remove all the global variables
rm(list=ls())

#Declare a variable to read and store stroke data set
stroke <- read.csv("C:/Users/dharan/Downloads/stroke.csv")

# View the stored data set
View(stroke)

#head and tail functions
head(stroke)
tail(stroke)

#to check dimension of the data set
dim(stroke)

#creating the  data frame including only required data columns
dataframe <- data.frame(stroke)
View(dataframe)

#str function
str(stroke)
  
#create categorical table for variable hypertension
table(stroke$hypertension)

#Cross verifying  total no of observations in the column of hypertension
length(stroke$hypertension)

#calculate proportion
table(stroke$hypertension)/5110

#Using mean() function to calculate avg age of the patients
mean(stroke$age)

#Using median() function to calculate median age of the patients
median(stroke$age)

#Using var() function to calculate variance of the avg glucose level of the patients
var(stroke$avg_glucose_level)

#Using standard deviation of bmi 
sd(stroke$bmi)

#Using min function to find min value of avg glucose level
min(stroke$avg_glucose_level)

#To find index of the minimum observation for a numeric variable avg glucose level
which.min(stroke$avg_glucose_level)

#To calculate maximum observation for a numeric variable avg_glucose_level we use max()
max(stroke$avg_glucose_level)

#To find index of the maximum observation for a numeric variable avg_glucose_level
which.max(stroke$avg_glucose_level)

#To calculate sum of all observation for a numeric variable bmi
sum(stroke$bmi)

#Calculate mean for a numeric variable age with the help of sum()
sum(stroke$age)/5110

#To calculate range it will display min and max value for a numeric variable bmi
range(stroke$bmi)


#To calculate percentile value with the help of quantile() for a numeric variable age
quantile(stroke$age)

#To calculate specific percentile value for a numeric variable age
quantile(stroke$age,probs = 0.7)

#To calculate random multiple percentiles for a numeric variable age
quantile(stroke$age,probs = c(0.2,0.4,0.6,0.8))

#To calculate correlation between numeric variables bmi and hypertension
cor(stroke$bmi,stroke$hypertension)

#To calculate covariance between numeric variables bmi and hypertension
cov(stroke$bmi,stroke$hypertension)

#To calculate variance between numeric variables bmi and hypertension
var(stroke$bmi,stroke$hypertension)

#To calculate summary for numeric variable age
summary(stroke$age)

#To calculate summary for categorical variable hypertension
summary(stroke$hypertension)

#summary 
summary(stroke)

#To check if there are any null values present in the dataframe
anyNA(dataframe)

# Installing Packages
install.packages("e1071")
install.packages("caTools")
install.packages("caret")

# Loading package
library(e1071)
library(caTools)
library(caret)

# Splitting data into train
# and test data
split <- sample.split(stroke, SplitRatio = 0.7)
train_cl <- subset(stroke, split == "TRUE")
test_cl <- subset(stroke, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 3:5])
test_scale <- scale(test_cl[, 3:5])

# Fitting Naive Bayes Model to training dataset
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(gender ~ ., data = train_cl)
classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)
y_pred

# Confusion Matrix
cm <- table(test_cl$gender, y_pred)
cm

install.packages('caTools')
library(caTools)

# Taking columns 3-5
dataset = stroke[3:5]
View(dataset)

# Encoding the target feature as factor
dataset$heart_disease = factor(dataset$heart_disease, levels = c(0, 1))
View(dataset)

# Splitting the dataset into the Training set and Test set
set.seed(123)
split1 = sample.split(stroke$age, SplitRatio = 0.7)
training_set = subset(stroke, split1 == TRUE)
test_set = subset(stroke, split1 == FALSE)

# Feature Scaling
training_set[3:5] = scale(training_set[3:5])
test_set[3:5] = scale(test_set[3:5])

View(training_set)
View(test_set)

# Fitting SVM to the Training set
install.packages('e1071')
library(e1071)

classifier = svm(formula = stroke ~ .,
                 data = test_set,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting on test data'
y_pred1 <- predict(classifier, newdata1 = test_set)
y_pred1

# Confusion Matrix
cm2 <- table(test_set$stroke, y_pred1)
cm2

#histogram
hist(stroke$age,
     main="No of stroke cases classified on the basis of age",
     xlab="age",
     xlim = c(0,100),
     col = c(0,2,4,6,8,10,12,14,16,18,20,24,26),
     las=1)

#Boxplot Multiple Box plots, each representing Stroke prediction Parameter
boxplot(stroke[, 9:10],
        col=c("Red","Blue"),
        notch = TRUE,
        main ='Box Plots for Stroke Prediction')

#SCatterPlot
plot(stroke$age,stroke$bmi,
     main = "Relationship between age and bmi",
     xlab = "age",
     ylab = "bmi",
     xlim=c(0,75),
     las=1,
     col=1:10,
     pch =8)

#Piechart with proportion
count<-table(stroke$smoking_status)
percent<-round(count/sum(count)*100)
genderlabel<-paste(names(count),"\n",percent,"%")
pie(count,
    labels = genderlabel,
    main = "No of cases classified on the basis of whether he or she smokes or not",
    col=c("darkolivegreen1","blue","red","violet"),
    border="brown1",
    clockwise = TRUE)