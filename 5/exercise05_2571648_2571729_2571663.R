### Stats with R Exercise sheet 5

##########################
#Week6: t-test and friends
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 25. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Sara Khan, Jyothsna Shashikumar Sastry, Hasan Md Tusfiqur Alam
## Matriculation number: 2571648, 2571729, 2571663

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################

###############
### Cleaning Data
###############

library(lsr)
library(tidyr)
library(effsize)


# set your wd and load the data frame digsym_clean.csv
#getwd()
#setwd("D:/SaarlandUniversity_Masters/3_WS18/StatisticsMitR/assignments/MySolutions/5")

dat<-read.csv("digsym_clean.csv")
str(dat)

# get rid of the column "X"
dat<-dat[-2]
str(dat)

# Say you're interested in whether people respond with different accuracy to 
# right vs wrong picture-symbol combinations.
# In other words, you want to compare the average accuracy for the digsym-right 
# and digsym-wrong condition.
# Like the conscientious researcher you are, you want to take a look at the data 
# before you get into the stats.
# Therefore, you will need to create a barplot of the mean accuracy data 
# (split out by condition) using ggplot and the summarySE function (given below).
# Let's do it step by step.

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
  # data: an input dataframe
  # measurevar: a column name of <data> (as string), on which we would like to calculate standard 
  #             deviation (SD), standard error (SE) and confidence interval (CI).
  # groupvars: categorical columns of <data> (as vector of strings ) which we would like to use
  #            to make all possible combinations for which we calculate SD, SE, CI based 
  #            on <measurevar>.
  # na.rm: should we remove NA
  # conf.interval: confidence interval
  library(doBy)
  
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
  names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
  
  # Calculate standard error of the mean
  datac$se <- datac$sd / sqrt(datac$N)  
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# apply the function summarySE on the accuracy data grouping by right/wrong condition
# (use the provided documentation inside the function for the arguments description)
tab<-summarySE(dat,measurevar="accuracy",groupvars="condition",na.rm=FALSE, conf.interval=.95)
tab

# Create the barplot (use ggplot2 for this and all tasks below) with error bars 
# (which the function summarySE readily provided).
# Gauging from the plot, does it look like there's a huge difference in accuracy 
# for responses to the right and wrong condition?
install.packages("ggplot2")
library(ggplot2)
#?ggplot
#?geom_errorbar

ggplot(tab, aes(x=condition, y=accuracy, fill=condition)) +
  geom_bar(stat = "identity") +  
  geom_errorbar(aes(x=condition, ymin=accuracy-tab$se, ymax=accuracy+tab$se), width=.2, position=position_dodge(.9))


# Let's go back to our data frame "data", which is still loaded in your console
# Now that you've taken a look at the data, you want to get into the stats.
# You want to compute a t-test for the average accuracy data in the right and 
# wrong condition.
# Why can't you compute a t-test on the data as they are now? 
# Hint: which assumption is violated?
# The assumption that standard deviation is the same in both groups is violated here


# we need to reshape( - cast) the data to only one observation (average accuracy)
# per subject and right/wrong condition 
# Collapse the data, using 
# cast(data, var1 + var2 + var3 ... ~, function, value = var4, na.rm = T)
install.packages("reshape")
library(reshape)
reshaped<-cast(dat, Subject + condition ~ ., fun.aggregate = mean, value = "accuracy", na.rm = TRUE)
colnames(reshaped)[colnames(reshaped)=="(all)"] <- "average"

# Create a histogram of the accuracy data depending on the right and wrong 
# condition and display them side by side


ggplot(data=reshaped, aes(x=average, fill=condition)) + 
  geom_histogram(position="dodge") + 
  labs(title="Histogram for Average")

# Display the same data in a density plot 

ggplot(data=reshaped, aes(x=average, color=condition)) + 
  geom_density(position="dodge") + 
  labs(title="Histogram for Average")

# Based on the histograms and the density plots - are these data normally 
# distibuted?
#The plots suggest that the data are normally distributed (though not perfect) with a negative skew

# Create a boxplot of the accuracy data
ggplot(data=reshaped, aes(x=condition, y=average)) + 
  geom_boxplot(position="dodge") + 
  labs(title="Histogram for Average")

# Compute the t-test to compare the mean accuracy between wrong and right picture
# combinations.
# Do you need a paired t-test or independent sample t-test? why?
# We need paired t -tests as the data is same for the subjects 
t.test(reshaped$average ~ reshaped$condition, paired = TRUE)


# What does the output tell you? What conclusions do you draw?
#t = 3.7691, df = 36, p-value = 0.000588
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.01303888 0.04341757
#sample estimates:
#  mean of the differences 
#0.02822823
#the p value is < 0.05 and hence , we reject the null hypothesis. There is some corelation/dependence between average accuracy and conditions

# Compute the effect size using CohensD 
library(lsr)
cohensD(reshaped$average ~ reshaped$condition,method="paired")

# How big it is? How do you interpret this result?
#The value of d = 0.6196291, the effect is medium and  the mean=0.6 times the standard deviation

# In addition to the long-format data we've just been working on, you may also 
# encounter data sets in a wide format 
# (this is the format we have been using in class examples.)
# Let's do a transformation of our data set to see how it would like in a wide 
# format.
# Use "spread" in tidyr.

library(tidyr)
widedat<-spread(reshaped,key=condition,value=average)

# Compute the t test again on the wide format data - note that for wide-format 
# data you need to use a different annotation for the t-test.

t.test(widedat$right,widedat$wrong, paired = TRUE)
#t = 3.7691, df = 36, p-value = 0.000588
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.01303888 0.04341757
#sample estimates:
#  mean of the differences 
#0.02822823 

# Compare the t-test results from the wide-format and the long-format data.
#The wide and the long format of our data yield the same results

# Compute CohensD on the wide format data.
cohensD(widedat$right,widedat$wrong,method="paired")


# Let's try the t-test again, but for a different question:
# Suppose you are interested in whether reaction times in the digit symbol 
# task differ depending on gender.
# In other words, you want to test whether or not men perform significantly 
# faster on average than women, or vice versa.
# Collapse the data again, using 
# cast(data, var1 + var2 + var3 ... ~ ., function, value = var4, na.rm = T)
reshaped_RT <- cast(dat, StimulDS1.CRESP + Gender ~ ., fun.aggregate = mean, value = "correct_RT", na.rm = TRUE)
colnames(reshaped_RT)[colnames(reshaped_RT)=="(all)"] <- "average"

# Take a look at the resulting data frame using head()
head(reshaped_RT)


# Compute the t-test to compare the accuracy means of female and male 
# participants.
# Which t-test do you need and why? How do you interpret the result?
# We choose independent t tests as the response time is being compared between two different sets of people (classified based on gender)
t.test(reshaped_RT$average ~ reshaped_RT$Gender, paired = FALSE)
# p-value = 0.209 , this value is greater than 0.05
# Null hypothesis cannot be rejected as there is no dependency/correlation between Reaction times for the different subsets (classified based on gender)
