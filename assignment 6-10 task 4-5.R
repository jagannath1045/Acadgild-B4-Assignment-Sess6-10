# 1. If Z is norm (mean = 0, sd = 1)
#  Find P(Z > 2.64)

pnorm(2.64, mean = 0, sd = 1, lower.tail = FALSE)

#------------------------

#  Find P(|Z| > 1.39)
#  = 1 - P(-1.39 < X < 1.39)
1 - (pnorm(1.39, mean = 0, sd=1) - pnorm(-1.39, mean = 0, sd=1))



# Suppose p = the proportion of students who are admitted to the graduate school 
# of the University of California at Berkeley, and suppose that a public relation 
# officer boasts that UCB has historically had a 40% acceptance rate for its graduate
# school. Consider the data stored in the table UCBAdmissions from 1973. Assuming 
# these observations constituted a simple random sample, are they consistent with 
# the officer's claim, or do they provide evidence that the acceptance rate was 
# significantly less than 40%? Use an alpha = 0.01 significance level.



View(UCBAdmissions)
class(UCBAdmissions)

# Our null hypothesis, H0 is p= 0.40
# Alternative Hypothesis , Ha is p < 0.4

-qnorm(0.99)   # to find z alpha

A <- as.data.frame(UCBAdmissions)
head(A)

xtabs(Freq ~ Admit, data = A)

# calculate the value of the test statistic.
phat <- 1755/(1755 + 2771)
(phat - 0.4)/sqrt(0.4 * 0.6/(1755 + 2771))


prop.test(1755, 1755 + 2771, p = 0.4, alternative = "less",
          conf.level = 0.99, correct = FALSE)


library(IPSUR)
library(HH)
library(ggplot2)
temp <- prop.test(1755, 1755 + 2771, p = 0.4, alternative = "less",conf.level = 0.99, correct = FALSE)
plot(temp, "Hypoth")

## task 5
#a


library(readxl)
AirQualityUCI <- read_excel("C:/Users/Jagannath/Downloads/AirQualityUCI.xlsx")
View(AirQualityUCI)
dim(AirQualityUCI)
str(AirQualityUCI)

#b
library(psych)
describe(AirQualityUCI)

#c
col1<- mapply(anyNA,AirQualityUCI)
col1
summary(AirQualityUCI)
is.na(AirQualityUCI)

#or

AirQualityUCI[AirQualityUCI == -200] <- NA
View(AirQualityUCI)
library(VIM)
aggr(AirQualityUCI, col=c('pink','yellow'),
     numbers=TRUE, sortVars=TRUE,
     labels=names(AirQualityUCI), cex.axis=.7,
     gap=3, ylab=c("Missing data","Pattern"))    # graphical presentation of NAs

sapply(AirQualityUCI, function(x) sum(is.na(x)))     # count of NAs

# Variable NMHC(GT) is having 90% of missing values.
# Hence, NMHC(GT) is not considered and omitted from the data frame

AirQualityUCI$`NMHC(GT)` <- NULL

#d

names(AirQualityUCI)
AirQualityUCI$Date1 <- as.numeric(as.Date(AirQualityUCI$Date))
install.packages("mice")
library(mice)
imputed <- mice(AirQualityUCI[,-c(1,2,4)], m=5, maxit = 5, method = 'cart', seed = 100) # impute missing values
summary(imputed)
complete <- complete(imputed) # replaces the NAs with imputed values
str(complete)
sapply(complete, function(x) sum(is.na(x)))  # check missing values

#e

summary(AirQualityUCI)
plot(AirQualityUCI$`NOx(GT)`~AirQualityUCI$`PT08.S2(NMHC)`)
plot(AirQualityUCI$`PT08.S1(CO)`~AirQualityUCI$`PT08.S3(NOx)`)
plot(AirQualityUCI$`NO2(GT)`~AirQualityUCI$`PT08.S4(NO2)`)
plot(AirQualityUCI$`PT08.S5(O3)`~AirQualityUCI$T)
#or

pairs(AirQualityUCI)    # graph
#-----------------------------------------------------------------------------
final <- complete
final$Date <- AirQualityUCI$Date
final$Time <- AirQualityUCI$Time
library(stringr)
AirQualityUCI$Time1 <- sub(".+? ", "", AirQualityUCI$Time)
AirQualityUCI$datetime <- as.POSIXct(paste(AirQualityUCI$Date, AirQualityUCI$Time1), format="%Y-%m-%d %H:%M:%S")
View(AirQualityUCI)
str(AirQualityUCI)

# f. Test relevant hypothesis for valid relations

t.test(AirQualityUCI$`CO(GT)`, AirQualityUCI$`PT08.S1(CO)`, paired = T)
t.test(AirQualityUCI$`C6H6(GT)`, AirQualityUCI$`PT08.S2(NMHC)`, paired = T)
t.test(AirQualityUCI$`NOx(GT)`, AirQualityUCI$`PT08.S3(NOx)`, paired = T)

mod <- lm(AirQualityUCI$`CO(GT)`~AirQualityUCI$Date1)
summary(mod)

mod <- lm(AirQualityUCI$`CO(GT)`~AirQualityUCI$T)
summary(mod)

mod <- lm(AirQualityUCI$`CO(GT)`~AirQualityUCI$RH)
summary(mod)

#g Create cross tabulations with derived variables

mydata<-AirQualityUCI
View(mydata) # 2-Way Frequency Table
attach(mydata)
#mytable <- table(A,B) # A will be rows, B will be columns
#mytable # print table
margin.table(mytable, 1) # A frequencies (summed over B)
prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages


range(AirQualityUCI$RH)

final <- within(AirQualityUCI,
                {
                  RHcat <- NA
                  RHcat[RH<20] <- "Very Low"
                  RHcat[RH>=20 & RH<=40] <- "Low"
                  RHcat[RH>40 & RH<=60] <- "Medium"
                  RHcat[RH>60 & RH<=80] <- "High"
                  RHcat[RH>80] <- "Very High"
                })

mytable <- xtabs(`CO(GT)` ~ +RHcat, data = final)
ftable(mytable)  # print table 
summary(mytable) # chi-square test of indepedence

mytable <- xtabs(`C6H6(GT)` ~  +RHcat, data = final)
ftable(mytable)  # print table 
summary(mytable) # chi-square test of indepedence

mytable <- xtabs(`NOx(GT)` ~  +RHcat, data = final)
ftable(mytable)  # print table 
summary(mytable) # chi-square test of indepedence

with(final, tapply(`NO2(GT)`, list(RHcat=RHcat), sd)) # using with()
with(final, tapply(`NO2(GT)`, list(RHcat=RHcat), mean))

#h. Check for trends and patterns in time series.

#plot time series
tsAirqualityUCI <- EuStockMarkets[, 1] # ts data
decomposedRes <- decompose(tsAirqualityUCI, type="mult") # use type = "additive" for additive components
plot (decomposedRes) # see plot below
stlRes <- stl(tsAirqualityUCI, s.window = "periodic")
plot(AirQualityUCI$T, type = "l")
#or
install.packages('xts')
library(xts)

timeseries <- xts(final$`CO(GT)`, final$datetime)
plot(timeseries)
summary(timeseries)

ts (AirQualityUCI, frequency = 4, start = c(1959, 2))# frequency 4 =>Quarterly Data
ts (1:10, frequency = 12, start = 1990) # freq 12 => Monthly data.
ts (AirQualityUCI, start=c(2009), end=c(2014), frequency=1) # Yearly Data
ts (1:1000, frequency = 365, start = 1990) # freq 365 => daily data.

# i. Find out the most polluted time of the day and the name of the chemical compound
names(AirQualityUCI)
library(dplyr)

polluted <- AirQualityUCI%>%group_by(Time)%>%
  select(Time, `CO(GT)`, `C6H6(GT)`, `NO2(GT)`, `NOx(GT)` )%>%
  summarise(CO = mean(`CO(GT)`), C6H6 = mean(`C6H6(GT)`), NO2 = mean(`NO2(GT)`), NOX =mean(`NOx(GT)`))%>%
  
  polluted[c(which.max(polluted$CO),which.max(polluted$C6H6),which.max(polluted$NO2),which.max(polluted$NOX)),]

# 19:00:00 is the most polluted time of the day with CO, C6H6, NO2 & NOx

