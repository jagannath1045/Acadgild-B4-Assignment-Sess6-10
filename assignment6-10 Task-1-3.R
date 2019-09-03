#Task 1: 

1. Import the Titanic Dataset from the link => Titanic Data Set. 

Perform the following: 
  
  a.	Is there any difference in fares by a different class of tickets? 
  
  Note - Show a boxplot displaying the distribution of fares by class 

b.	Is there any association with Passenger class and gender? 
  
  Note - Show a stacked bar chart 

Task 2: 
  
  1. Create a box and whisker plot by class using mtcars dataset. 


Task 3: 
  
  
  1.  A recent national study showed that approximately 44.7% of college students have used Wikipedia as a source in at least one of their term papers. Let X equal the number of students in a random sample of size n = 31 who have used Wikipedia as a source. 

Perform the below functions 

a.	Find the probability that X is equal to 17 

b.	Find the probability that X is at most 13 

c.	Find the probability that X is bigger than 11. 

d.	Find the probability that X is at least 15. 

e.	Find the probability that X is between 16 and 19, inclusive 

Task 4: 
  
  1.	If Z is norm (mean = 0, sd = 1) 

Find P(Z > 2.64) 
Find P(|Z| > 1.39) 

2.	Suppose p = the proportion of students who are admitted to the graduate school of the University of California at Berkeley, and suppose that a public relation officer boasts that UCB has historically had a 40% acceptance rate for its graduate school. Consider the data stored in the table UCBAdmissions from 1973. Assuming these observations constituted a simple random sample, are they consistent with the officer?..s claim, or do they provide evidence that the acceptance rate was significantly less than 40%? Use an α = 0.01 significance level. 
3.	How do you test the proportions and compare against hypothetical props? 
  
  Test Hypothesis: the proportion of automatic cars is 40%. 
##

##Solution- Task 1
library("readr")
library(readxl)
TitanicData <- read_xls("D:/DocumentsR/R Scripts & Data- acadgild sessions/data files R sessions/titanic3.xls")

View(TitanicData)
str(TitanicData)

colnames(TitanicData) <- c("Pclass","Survived","Name","Sex","Age","SibSp","Parch","Ticket","Fare",
                           "Cabin","Embarked","Boat","Body","destination")

Titanic <- TitanicData %>% mutate(Pclass = as.factor(Pclass))  # Passennger class as factor
str(Titanic)
View(Titanic)

boxplot(Fare~Pclass, data = Titanic, col = topo.colors(3),
        xlab = "Class of Ticket", ylab = "Fares", main = "Fares by different Class of Tickets")


Titanic<-TitanicData


A<- table(Titanic$Sex, Titanic$Pclass)
A
str(A)
head(A)

bp <- barplot(A, col= rainbow(length(A)), legend = rownames(A),
              main = "Passenger class and gender",
              xlab = "Class of Ticket", ylab = "No. of Passangers by Gender")

##Solution Task2

library(readr)
library(ggplot2)
library(dplyr)
mtcars 
View(mtcars)
str(mtcars)
mtcars1 <- mutate(mtcars, 
                  cyl = as.factor(cyl),
                  disp = as.factor(disp),
                  vs = as.factor(vs),
                  am = as.factor(am),
                  gear = as.factor(gear),
                  carb = as.factor(carb),
                  mpg = mpg, hp = hp, drat = drat, qsec=qsec)
str(mtcars1)

boxplot(mpg~carb, data = mtcars1, col = c("Red","Green","Blue","Pink","yellow","orange"),main="Boxplot showing distribution of mpg for each carb")

## Solution Task 3

# a. Find the probability that X is equal to 17 
dbinom(17, 31, 0.447) 

# b. Find the probability that X is at most 13 
pbinom(13, 31, 0.447) 

# c. Find the probability that X is bigger than 11. 
pbinom(11, 31, 0.447, lower.tail = F) 

# d. Find the probability that X is at least 15. 
pbinom(14, 31, 0.447, lower.tail = F) 

# e. Find the probability that X is between 16 and 19, inclusive 
sum(dbinom(16:19, 31, 0.447)) 
diff(pbinom(c(19,15), 31, 0.447, lower.tail = FALSE)) 


  
# a. Find the probability that X is equal to 17 > 
dbinom(17, 31, 0.447)  
  
# b. Find the probability that X is at most 13 > 
pbinom(13, 31, 0.447) 
# c. Find the probability that X is bigger than 11. 
pbinom(11, 31, 0.447, lower.tail = F)
  


# d. Find the probability that X is at least 15.  
pbinom(14, 31, 0.447, lower.tail = F) 
  
# e. Find the probability that X is between 16 and 19, inclusive 
  sum(dbinom(16:19, 31, 0.447)) 
  diff(pbinom(c(19,15), 31, 0.447, lower.tail = FALSE)) 
  
  
