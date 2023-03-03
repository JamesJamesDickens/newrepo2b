library(tidyverse)
# Multiple Linear Regression / Using Dummy Variables

#  We now consider producing Multiple Linear Regression Models 
# that have categorical variables.In order to do so, we create 
# what are called dummy or indicator variables.

# Example  

# The following table displays information regarding variables 
# that are related to the price of a home.  High School Status 
# refers to a ranking of the high school near the home 
# ( E is "exemplary",
# NE is not "exemplary") Note that the variable High School Status
# is categorical having two levels. the other explanatory variable
# sqft is quantitative and the response variable price is quantitative.


#  High School Status     sqft          price

#         NE              1872           145
#         NE              1954           69.9
#          E              4104           315
#         NE              1524           144.9
#         NE              1297           134.9
#          E              3278           369
#         NE              1192           95
#          E              2252           228.9
#         NE              1620           149
#          E              2466           295
#          E              3188           388.5
#         NE              1061           75
#         NE              1195           130
#          E              1552           174
#          E              2901           334.9 

# We now structure and prepare the data for R input 

price<- c(145,69.9,315,144.9,134.9,369,95,228.9,149,295,388.5,75,130,
          174,334.9) 
price

sqft<-c(1872,1954,4104,1524,1297,3278,1192,2252,1620,2466,3188,1061,
        1195,1552,2901)
sqft

HighSchoolStatus<- c("NE","NE","E","NE","NE","E","NE","E","NE","E",
                    "E","NE","NE","E","E")
HighSchoolStatus

# Lets create a data frame  (data table)

data.frame(sqft, HighSchoolStatus, price)-> dff
dff

# Use the factor command to view the levels (or factors) of
# the categorical variable 

factor(dff$HighSchoolStatus)

# Typically, the levels of a categorical variable are assigned values
# of 0 or 1. There are two levels.  R will assign E to 1  and NE to 0.
# The standard number assignment for levels of a categorical variable
# are processed as follows:

#   E -> 1;            NE -> 1;
#   0 otherwise         0 otherwise
# Note:You could code using the numerical assignments by hand, but R
# will do it automatically.

# We will now produce the model

lm(price ~ sqft + HighSchoolStatus , dff)->reg
reg

summary(reg)  

# The Multiple Linear Regression Model is

# E(price) = 125.72281 + 0.06207sqft - 98.64787HighSchoolStatusNE 

# Remember HighSchoolStatusNE is a categorical variable that can 
# take on a value of 0 or 1

# Use R to determine which level gets the 1 and which level gets the
# 0.

contrasts(as.factor(dff$HighSchoolStatus))

# Find the average price of a house if the HighSchool Status is 
# exemplary and the square footage is 2500

# E(price) = 125.72281 + 0.06207(2500) - 98.64787(0)
# E(price) = 280.898

# Find the average price of a house if the HighSchool Status is 
# not exemplary and the square footage is 2500

# E(price) = 125.72281 + 0.06207(2500) - 98.64787(1)
# E(price) = 182.2499



#Example 3

# We will now call the data set "Salaries" to illustrate multiple 
# regression involving a categorical variable

# Install the following package
 install.packages("car")

# Now call the library
 library(car)

# Now call the data set
Salaries

library(tidyverse)

as_tibble(Salaries)

# Lets produce a Linear Regression Model that predicts salary
# based on the independent variables sex and yrs,service 
# (years of service)

#First lets acknowledge that R will create the dummy variables 
# for the variable sex

contrasts(as.factor(Salaries$sex))

# Compute the model
model1 <- lm(salary ~ yrs.service + sex, data = Salaries)
model1

summary(model1)
# The Linear Multiple Regression model is ;
#  y = 92356.9 + 747.6yrs.service + 9071.8sexMale

# If sex is Male and years of service is 7, what is the predicted
# salary ?
  # Answer;   y = 92356.9 + 747.6(7) + 9071.8(1)   
  #             =  106661.9    $106,661.9


# If sex is Female and years of service is 7, what is the predicted
# salary ?
  
  # Answer;   y = 92356.9 + 747.6(7) + 9071.8(0)   
  #             =  97590.1     $97,590.1

  

# Lets add another categorical variable to our model;  rank.
# Note that the categorical variable rank, has three levels:
# AsstProf, AssocProf , and Prof

# We can also use R to find the Levels
factor(Salaries$rank)


# Lets acknowledge that R will create the dummy variables
contrasts(as.factor(Salaries$rank))

# Lets create the new model.

model2 <- lm(salary ~ yrs.service + sex + rank,  data = Salaries)
model2

contrasts(as.factor(Salaries$rank))

# For Which Model is the coefficient for the variable yrs.service 
# not significant?  Model 1 or Model 2. Also, are both models
# significant?  Investigate by calling the summaries for both models

summary(model1)

summary(model2)


# Yes both models are significant.
# For model 2 the coefficient for yrs of service is not significant
# for the p value is .13694

# Let us look at the regression equation for model 2

# E(Salaries) = 76612.8 -171.8yrs.service + 5468.7sexMale 
# + 14702.9rankAssocProf + 48980.2rankProf

# Let's predict a salary for a male employee who is an Assistant
# Professor and who has 22 years of service.

# E(Salaries) = 76612.8 -171.8(22) + 5468.7(1) 
# + 14702.9(0) + 48980.2(0)  =  728333.2 

#  $ 72,8333.20 



# Another Example:
# Now we use R coding to produce the summary table found on page 277
# in your book.  The data is on page 274. The required reading starts
# at the bottom of page 273 to page 277.  The reading involves an 
# observational study on bats and birds.   The task is to produce a
# Linear Multiple Regression Equation that uses independent variables
# Species and Mass to predict Flight Energy for bats and birds


# The table on page 274 features three levels for the categorical
# variable Species; they are :  Echolocating bats -> elb,  
# Non-Echolocating birds ->  nelbi, and Non-Echolocating bats -> nelba

# The R coding that I developed follows;

Spec<- c("elba","elba","elba","elba","nelbi","nelbi","nelbi","nelbi",
           "nelbi","nelbi", "nelbi","nelbi","nelbi", "nelbi","nelbi",
           "nelbi", "nelba", "nelba","nelba","nelba")
Spec

Mass<-c(779,628,258,315,24.3,35,72.8,120,213,275,370,384,442,412,
          330,480,93,8,6.7,7.7)
Mass

qqnorm(log(Mass))

FEE<-c(43.7,34.8,23.3,22.4,2.46,3.93,9.15,13.8,14.6,22.8,26.2,25.9,
         29.5,43.7,34,27.8,8.83,1.35,1.12,1.02)
FEE

# Lets produce a data table

data.frame(Spec, log(Mass),log(FEE))-> BB
BB


factor(BB$Spec)

# Now create the linear multiple regression model

lm(log(FEE) ~ Spec + log(Mass) , BB) -> lmodel

lmodel
summary(lmodel)


# Dummy variable construction    (number of levels - 1)

# Specnelba = 1 , 0 otherwise
# Specnelbi = 1 , 0 otherwise
# The reference level is therefore is elba  
# (If a prediction is desired for elba, set the other two
# dummy variables equal to 0)

# Again, we let R create the Dummy Variable Matrix

contrasts(as.factor(BB$Spec))

# E(logFEE) = 0.81496log(Mass) + 0.07866Specnelba + 
# 0.10226Specnelbi -1.57636 

# Use your model to predict the log(FEE) for species nelba and
# if log(Mass) = 5.829 

# E(logFEE) = 0.81496(5.829) + 0.07866(1) + 0.10226(0) -1.57636 
# E(logFEE) = 3.25270



# CLASSWORK
# Data
Gender<-c("Female","Female","Female","Female","Female","Female","Female","Female","Female","Female",
          "Male","Male","Male","Male","Male","Male","Male","Male","Male","Male")
Gender

MRICount<-c(816932,951545,991305,833868,856472,852244,790619,866662,857782,948066,949395,1001121,
            1038437,965353,955466,1079549,924059,955003,935494,949589)
MRICount

IQ <- c(133,137,138,132,140,132,135,130,133,133,140,140,139,133,133,141,135,139,141,144)
IQ

#1 Use and show R code to Create a data frame for the collection of 
# vectors above.
#2 Use and show R code to produce a Multiple Linear Regression Model.
#3 Use and show R code to produce summary indicators for your model.
# What is the p value for your model?  What is the Multiple 
# Rsquared value for your model?
#4 Use your regression model to predict IQ for a Female who has an 
# MRICount of 855000
#  (Show all of your work.)

#5  Do you think that this is a good model.  Justify your answer.





q()
y

