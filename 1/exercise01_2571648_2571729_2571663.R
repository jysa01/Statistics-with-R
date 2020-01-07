### Stats with R Exercise sheet 1 

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, October 28. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

## Please write below your (and your teammates) name, matriculation number. 
## Name: Sara Khan, Jyothsna Shashikumar Sastry, Hasan Md Tusfiqur Alam
## Matriculation number: 2571648, 2571729, 2571663

## Change the name of the file by adding your matriculation numbers
## (exercise01_firstID_secondID_thirdID.R)



## Many of the things on this exercise sheet have not been discussed in class. 
## The answers will therefore not be on  the slides. You are expected to find 
## the answers using the help function in R, in the textbooks and online. If you 
## get stuck on these exercises, remember: Google is your friend.
## If you have any questions, you can ask these during the tutorial, or use the 
## moodle discussion board for the course.

###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.

getwd()

## b) Get help with this function.

help("getwd")

## c) Change your working directory to another directory.

setwd("E:/MSc/WS2018/Statistics with R/Exercise")


###############
### Exercise 2: Participants' age & boxplots
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.

install.packages('languageR')
library(languageR)


## b) Specifically, we will deal with the dataset 'dutchSpeakersDistMeta'. 
##    This dataset should be available to you once you've loaded languageR.
##    The dataset contains information on the speakers included in the Spoken 
##    Dutch Corpus. Inspect 'dutchSpeakersDistMeta'. Look at the head, tail, 
##    and summary. What do head and tail show you?

head(dutchSpeakersDistMeta, n = 10)
# head function shows the first n entries of a dataframe, vector or table etc.

tail(dutchSpeakersDistMeta, n = 10)
# tail function shows the last n entries of a dataframe, vector or table etc.

summary(dutchSpeakersDistMeta)

## c) Each line in this file provides information on a single speaker. How many 
##    speakers are included in this dataset? In other words, use a function to 
##    retrieve the number of rows for this dataset.

nrow(dutchSpeakersDistMeta)

# total number of rows in dataset is: 165

## d) Let's say we're interested in the age of the speakers included in the 
##    corpus, to see whether males and females are distributed equally. 
##    Create a boxplot for Sex and AgeYear.

boxplot(AgeYear~Sex, data=dutchSpeakersDistMeta, col=(c("gold", "darkgreen")), xlab="Sex", ylab="Age Year", main="Boxplot for Sex vs AgeYear")

## e) Does it seem as if either of the two groups has more variability in age?

## Answer: Yes, the females have  a wider span than the males


## f) Do you see any outliers in either of the two groups?

## Answer: Yes, in male group we see two outliers in AgeYear. They are around year 1930


## g) Now calculate the mean and standard deviation of the AgeYear per group. 
##    Do this by creating a subset for each group.
##    Do the groups seem to differ much in age?
install.packages("magrittr")
library(magrittr)
install.packages("dplyr")
library(dplyr)
# necessary pacakages are installed and loaded

dutchSpeakersDistMeta %>% group_by(Sex) %>% summarise(mean = mean(AgeYear, na.rm = TRUE), sd = sd(AgeYear, na.rm = TRUE))

#Result:
# Sex     mean    sd
# <fct>  <dbl> <dbl>
# 1 female 1967.  15.9
# 2 male   1967.  14.7

# For both of the groups, the mean are same; 1967. the standard devaition for female group is slightly higer than male.
# Overall, there doesn't seem to be much differences in age.


## h) What do the whiskers of a boxplot mean?

## the whiskers represents the upper and lower bounds(values) of the dataset. All the data points have the values in between
## that range. 
## Incase of outliers, the box and whiskers chart may not show the minimum or maximum value.
## Instead, the ends of the whiskers represent one and a half times the interquartile range (1.5*IQR).
## Boxplot is a good visualization tool to observe the outliers.


###############
### Exercise 3: Children's stories & dataframes
###############
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 25 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18 15 22 19 18 17 18 20 17 12 16 16 17 21 25 18 20 21 20 20 15 18 17 19 20 


## a) What measurement scale is this data? Is it discrete or continuous? Explain
##    in one sentence why? (remember, comment out written answers)

## Answer: Ratio scale as there can be a natural 0 in the observation.
##  Its discrete. Because, the no. times a child used 'then' is always a whole number. there cannot be any possible 
##  value in the middle. e.g: 2.5 between 2 and 3.


## b) In the next questions (c-e), you will create a dataframe of this data, 
##    which will also include participant IDs.
##    Why is a dataframe better suited to store this data than a matrix?

## Answer: # Dataframe gives us the flexibility to store data belonging to different types together, eg: numeric, strings, factors etc

## c) First create a vector with participant IDs. Your vector should be named 
##    'pps', and your participants should be labeled from 1 to 25

pps <- c(1:25)

## d) Next, create a vector containing all the observations. Name this vector 'obs'.

obs <- c(18, 15, 22, 19, 18, 17, 18, 20, 17, 12, 16, 16, 17, 21, 25, 18, 20, 21, 20, 20, 15, 18, 17, 19, 20)


## e) Create a dataframe for this data. Assign this to 'stories'. 
stories <- data.frame(pps, obs)


## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class is the variable 'pps'?

summary(stories)
class(stories$pps)
## 'pps' has 'integer' class

## g) Change the class of 'pps' to factor. Why is factor a better class for this
##    variable?

stories$pps <- as.factor(stories$pps)
class(stories$pps)

## Factors are categorical variable that are useful to map data with levels. since in pps variable, each of the value
## represent the Id of the participants, factor is a better class for 'pps' variable.

## h) Plot a histogram (using hist()) for these data. Set the number of breaks 
##    to 8.

hist(stories$obs, breaks = 8, col="lightblue")

## i) Create a kernel density plot using density().

d <- density(stories$obs)
plot(d, main = "Kernel Density for column observation")
polygon(d, col="lightgreen", border="blue")

## j) What is the difference between a histogram and a kernel density plot?

# The histogram suggests how the values are distributed based on the number of bins indicated.
# Kernal density plot indicates the shape of the distribution in addition to indicating where the observations are more dense


## k) Overlay the histogram with the kernel density plot 
##    (hint: the area under the curve should be equal for overlaying the graphs 
##    correctly.)

hist(stories$obs, prob=TRUE, col = "lightblue")
lines(d, col = "red")

###############
### Exercise 4: Normal distributions
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the x-values to plot the range for
##    (will become the x-axis in the plot).
##    Get R to generate the range from -5 to 5, by 0.1. Assign this to the 
##    variable x.
x <- seq(-5, 5, 0.1)

## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution.

help(dnorm)
y <- dnorm(x, mean = 0, sd = 1)


## c) Now use plot() to plot the normal distribution for z values of "x". 

plot(x,y)


## d) The plot now has a relatively short y-range, and it contains circles 
##    instead of a line. 
##    Using plot(), specify the y axis to range from 0 to 0.8, and plot a line 
##    instead of the circles.

plot(x, y, ylim = c(0, 0.8), type = "line")

## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the median of x using
##    the argument 'v'.
##    In order to get a dashed line, set the argument 'lty' to 2.

help("abline")
abline(v=median(x), type = "1", lty=2)

## f) Take a look at the beaver1 dataset. (You can see it by typing "beaver1".) 
##    Then select only the temperature part and store it in a variable "b1temp".

b1temp <- beaver1$temp

## g) Calculate the mean and standard deviation of this dataset and plot a normal
##    distribution with these parameters.
m <- mean(b1temp)
std <- sd(b1temp)

d1 = dnorm(b1temp, mean = m, sd = std)
plot(b1temp, d1)

## h) We observe two tempareatures (36.91 and 38.13). What's the likelihood that
##    these temepratures (or more extreme ones) respectively come 
## from the normal distribution from g)?

?pnorm
pnorm(36.91, mean = m, sd = std)
# 0.5976096

pnorm(38.13, mean = m, sd = std)
# 1


## i) Use the random sampling function in R to generate 20 random samples from
##    the normal distribution from g), and draw a histrogram based on this sample.
##    Repeat 5 times. What do you observe?

s1<-sample(d1,20)
s2<-sample(d1,20)
s3<-sample(d1,20)
s4<-sample(d1,20)
s5<-sample(d1,20)
hist(s1)
hist(s2)
hist(s3)
hist(s4)
hist(s5)
     
#The histogram differs for every sample randomly selected

