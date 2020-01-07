###############
### Cleaning Data
###############

# Please do the "Cleaning Data with R" exercise that was assigned in dataCamp.
# We recommend that you take notes during the DataCamp tutorial, so you're able to use the commands 
# you learned there in the exercise below.
# This week, the exercise will be about getting data into a good enough shape to start analysing. 
# Next week, there will be a tutorial on running t tests for this data.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete.

## Group Members info
## Name: Sara Khan, Jyothsna Shashikumar Sastry, Hasan Md Tusfiqur Alam
## Matriculation number: 2571648, 2571729, 2571663


# 1. Download the data file "digsym.csv" from the moodle and save it in your working directory. 

# 2. Read in the data into a variable called "dat".
dat<- read.csv("digsym.csv")

# 3. Load the libraries languageR, stringr, dplyr and tidyr.
install.packages("dplyr")
install.packages("stringr")
install.packages("tidyr")
library(languageR)
library(stringr)
library(dplyr)
library(tidyr)

# 4. How many rows, how many columns does that data have?
nrow(dat)
# [1] 3700

ncol(dat)
# [1] 11

# 5. Take a look at the structure of the data frame using "glimpse"

glimpse(dat)

# 6. View the first 20 rows, view the last 20 rows

head(dat,n=20)
tail(dat,n=20)

# 7. Is there any missing data in any of the columns?

any(is.na(dat))

#Yes

# 8. Get rid of the row number column

dat <- dat[,-1]

# 9. Put the Sub_Age column second
    
dat <- dat[,c("ExperimentName", "Sub_Age", "Group", "Gender", "List", "SubTrial", "StimulDS1.CRESP", "StimulDS1.RESP",  "StimulDS1.RT", "File")]


# 10. Replace the values of the "ExperimentName" column with something shorter, more legible

dat$ExperimentName <- as.factor(str_replace(dat$ExperimentName, "Digit Symbol - Kopie", "DS-K"))

# 11. Keep only experimental trials (encoded as "Trial:2" in List), get rid of practice trials 
# (encoded as "Trial:1"). When you do this, assign the subset of the data to a variable "data2", 
# then assign data2 to dat and finally remove data2.

data2 <- dat[which(dat$List == "Trial:2"),]
dat <- data2
remove(data2)

# 12. Separate Sub_Age column to two columns, "Subject" and "Age", using the function "separate"

dat <- separate(dat,col=Sub_Age,into = c("Subject","Age"), sep="_")


# 13. Make subject a factor

dat$Subject <- as.factor(dat$Subject)
  

# 14. Extract experimental condition ("right" vs. "wrong") from the "File" column:
# i.e. we want to get rid of digit underscore before and the digit after the "right" and "wrong".
  # the values of File column depends on the values of StimulDS1.CRESP.
  #If the value of StimulDS1.CRESP is m, then File column has right value
  #and if StimulDS1.CRESP has z, then the File column has wrong value.

rvw <- dat$File
rvw <- str_replace(rvw, "^[0-9]_", "")
rvw <- str_replace(rvw, "[0-9]$","")
rvw

# we didn't alter the values stored in dat$File variable for question 15


# 15. Using str_pad to make values in the File column 8 chars long, by putting 0 on the end  (i.e., 
# same number of characters, such that "1_right" should be replaced by "1_right0" etc)

dat$File <-  str_pad(dat$File, width=8,side="right", pad="0")
dat$File


# 16. Remove the column "List"

dat <- select(dat, -List)


# 17. Change the data type of "Age" to integer

dat$Age <- as.integer(dat$Age)

    
# 18. Missing values, outliers:
# do we have any NAs in the data, and if so, how many and where are they?

sum(is.na(dat))

#No missing values

# 19. Create an "accuracy" column using if-statement
# if actual response (StimulDS1.RESP) is the same as the correct response (StimulDS1.CRESP), put 
# in value 1, otherwise put 0


accuracy <- rep(1, length(dat$StimulDS1.RESP))

for (i in 1:length(dat$StimulDS1.RESP)) {
  if (dat$StimulDS1.RESP[i] != dat$StimulDS1.CRESP[i]) {
    accuracy[i] <- 0
  }
  
}

dat <- cbind(dat, accuracy)


# 20. How many wrong answers do we have in total?

wrong_ans <- sum(dat$accuracy == 0)

wrong_ans
# 185


# 21. Whats the percentage of wrong responses?

percentage_acc <- wrong_ans / length(dat$accuracy)

percentage_acc
# 0.05555556 


# 22. Create a subset "correctResponses" that only contains those data points where subjects responded correctly. 

correctResponses <- subset(dat, accuracy == 1)


# 23. Create boxplot of StimulDS1.RT - any outliers?

boxplot(correctResponses$StimulDS1.RT)


# 24. Create histogram of StimulDS1.RT with bins set to 50

hist(correctResponses$StimulDS1.RT, breaks = 50)


# 25. Describe the two plots - any tails? any suspiciously large values?

# Yes, the there are few outliers in StimulDS1.RT. One data point value is too large that we can observe from
# the both of the plots. 

# 26. View summary of correct_RT

summary(correctResponses$StimulDS1.RT)

# Max value 13852, which is the suspicious outlier


# 27. There is a single very far outlier. Remove it and save the result in a new dataframe named "cleaned".

outlier_row_num <- which(correctResponses$StimulDS1.RT == 13852)

cleaned <- correctResponses[-c(outlier_row_num), ]
summary(cleaned)


## EXTRA Exercises:
##You can stop here for your submission of this week's assignment,
##but you are encouraged to try it now. 
##All these exercises will be discussed and solved in the tutorial!

# 28. Dealing with the tail of the distribution: outlier removal
# Now, remove all correct_RT which are more than 2.5. SD away from the grand mean


# 29. Create new "correct_RT_2.5sd" column in data which prints NA if an RT value is below/above the cutoff


# 30. Take a look at the outlier observations
# any subjects who performed especially poorly?


# 31. How many RT outliers in total?


# 32. Plot a histogram and boxplot of the correct_RT_2.5sd columns again - nice and clean eh?


# 33. Next, we'd like to take a look at the avrg accuracy per subject
# Using the "cast" function from the library "reshape", create a new data.frame which shows the 
# average accuracy per subject. Rename column which lists the average accuracy as "avrg_accuracy".


# 34. Sort in ascending order or plot the average accuracies per subject.


# 35. Would you exclude any subjects, based on their avrg_accuracy performance?


# 36. Congrats! Your data are now ready for analysis. Please save the data frame you created into a new 
# file "digsym_clean.csv".