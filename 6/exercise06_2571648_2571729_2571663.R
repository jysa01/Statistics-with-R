### Stats with R Exercise sheet 6

##########################
#Week 7: Correlation and Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 2. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Sara Khan, Jyothsana Shashikumar Sastry, Hasan Md. Tusfiqur Alam
## Matriculation number: 2571648,2571729,2571663

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################


library(reshape)
library(languageR)
library(ggplot2)

#######################
### PART 1: Correlation
#######################

########
### Please, use ggplot to make plots in all exercises below!
########

# Get some data - access the ratings data set in languageR and name it "data".
# Subjective frequency ratings and their length averaged over subjects, for 81 concrete English nouns.
data<-ratings

# Take a look at the data frame.
head(data)

# Let's say you're interested in whether there is a linear relationship between the word frequency of 
# the 81 nouns and their length.
# Take look at the relationship between the frequency and word length data by means a of a scatterplot 
# (from ggplot library).
library(ggplot2)
ggplot(data, aes(x=Length, y=Frequency)) + geom_point()

# Judging from the graphs, do you think that word frequency and word length are in any way correlated 
# with one another?
#Ans Looking at the graph, it doesn't seem that both the parameters are correlated.


# Compute the Pearson correlation coefficient between the two variables by means of cor().
# Tell R to only include complete pairs of observations.
# As a reminder: Pearson coefficient denotes the covariance of the two variable divided by the product 
# of their variance. It is scaled between 1 (for a perfect positive correlation) to -1 (for a perfect 
# negative correlation).
cor(data$Length, data$Frequency,  method = "pearson",use="complete.obs")

# Does the correlation coefficient suggest a small, medium or large effect?
# What about the direction of the effect?

#Ans
#-0.4281462
#It suggests a medium effect. The direction of effect is negative.

# Note that we have a large number of tied ranks in word length data (since there are multiple words 
# with the length of, e.g. 5).
# Thus, we might draw more accurate conclusions by setting the method to Kendall's tau instead of 
# Pearson (which is the default).
# How do you interpret the difference between these 2 correlation coefficients?
cor(data$Length, data$Frequency,  method = "kendall",use="complete.obs")
#Output-  -0.316297
#Pearson coefficient measures the degree of relationship b/w two variables whereas Kendall's method measures the strength of dependence
# What about significance? Use the more user-friendly cor.test!
#Ans The correlation coefficient value has been changed using kendall's method.
cor.test(data$Length, data$Frequency,  method = "kendall")
#Output
#data:  data$Length and data$Frequency
#z = -3.9186, p-value = 8.907e-05

# Take a look at the output and describe what's in there.
# What do you conclude?
#Ans p-value is <0.05, thus we cannot reject the null hypothesis i.e. there is some correlation b/w the two variables

# Finally, we can also calculate Spearman's rank correlation for the same data.
cor(data$Length, data$Frequency,  method = "spearman",use="complete.obs")
#Output- -0.4311981


###################################################################################################


#######################
### PART 2: Regression
#######################

# Fit a linear regression model to the data frame for the variables frequency (outcome variable) 
# and Length (predictor variable).
# General form: "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
lrm<-lm(Frequency ~ Length, data = data)

?lm
# How do you interpret the output? Is the relationship between the two variables positive or negative?
# Plot the data points and the regression line.
ggplot(data, aes(x = Length, y = Frequency))+ geom_point()+
  geom_abline(intercept = 6.5015, slope=-0.2943)


# Run the plotting command again and have R display the actual words that belong to each point. 
# (Don't worry about readability of overlapping words.)
ggplot(data, aes(x = Length, y = Frequency, label=rownames(data) ))+
  geom_abline(intercept = 6.5015, slope=-0.2943)+
  geom_text()

###################################################################################################


# Try this again for another example:
# Let's go back to our digsym data set.
# Set your wd and load the data frame digsym_clean.csv
# You can download this data frame from material of week 6: T-tests
dat<-read.csv("digsym_clean.csv")
str(dat)

# Suppose you want to predict reaction times in the digit symbol task by people's age.
# Fit a linear regression model to the data frame for the variables correct_RT_2.5sd (outcome variable) 
# and Age (predictor variable).
# General form: "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
lrm_dat<-lm(correct_RT_2.5sd ~ Age, data = dat)

# Let's cast the data to compute an RT mean (use correct_RT_2.5sd) for each subject, so that we have only one Age 
# observation by Subject.
# In case you're wondering why we still have to do this - like the t-test, linear regression assumes 
# independence of observations.
# In other words, one row should correspond to one subject or item only.
reshaped_dat<-cast(dat, Subject + Age ~., fun.aggregate = mean, value = "correct_RT_2.5sd", na.rm = TRUE)
colnames(reshaped_dat)[colnames(reshaped_dat)=="(all)"] <- "RTmean"
head(reshaped_dat)
# Fit the regression model.
lrm_reshaped_dat<-lm(RTmean ~ Age, data = reshaped_dat)

# Let's go over the output - what's in there?
# How do you interpret the output?
lrm_reshaped_dat
#Coefficients:
#(Intercept)          Age  
#     637.93        21.22 
# The positive slope indicates a positive relation between the input and the output terms with a bias term equal to the intercept

# Again plot the data points and the regression line. 
ggplot(reshaped_dat, aes(x = Age, y = RTmean))+ 
  geom_point() +
  geom_abline(intercept = 637.93, slope=21.22)


# Plot a histogram and qq-plot of the residuals. Does their distribution look like the normal distribution?
#pred_y<-predict.lm(lrm_reshaped_dat,reshaped_dat,se.fit = TRUE)
#residual_vector<- reshaped_dat$RTmean-pred_y$fit
error<- residuals(lrm_reshaped_dat)
ggplot(data=reshaped_dat, aes(x=residuals(lrm_reshaped_dat))) + 
  geom_histogram() 

ggplot(data=reshaped_dat, aes(sample=residuals(lrm_reshaped_dat))) + 
  stat_qq() 

#The plot of distribution of residuals with a histogram resembles a normal distribution 
#The qqplot for the same is not a normally distributed 

# Plot Cooks distance which estimates the residuals (i.e. distance between actual values and the 
# regression line) for individual data points in the model.
cooksd<-cooks.distance(lrm_reshaped_dat)
ggplot(reshaped_dat, aes(x=Age, y=cooksd )) + geom_point() 


# It actually looks like we have 1 influential observation in there that has potential to distort 
# (and pull up) our regression line.
# The last observation (row 37) in cast yielded a Cooks D is very high (greater than 0.6).
# In other words, the of the entire regression function would change by more than 0.6 when this 
# particular case would be deleted.
 
# What is the problem with observation 37?
# Run the plotting command again and have R display the subjects that belong to each point.
# observation 37 corresponding to the data recorded for subject 40 seems to be an outlier 
ggplot(reshaped_dat, aes(x=Age, y=cooksd, label=Subject)) + geom_point() + geom_text()



# Make a subset of "cast" by excluding this subject and name it cast2.
cast2<- subset(reshaped_dat, Subject != 40)


# Fit the model again, using cast2, and take a good look at the output.

lrm_cast2<-lm(RTmean ~ Age, data = cast2)
lrm_cast2

#(Intercept)    Age  
# 862.05        11.98 

# What's different about the output?
# How does that change your interpretation of whether age is predictive of RTs?
# The model is no longer affected by the outlier now as our data excludes outliers, we also notice that the slope of the new line
# is lesser than that for the model previously computed 

# Plot the regression line again - notice the difference in slope in comparison to our earlier model fit?

ggplot(cast2, aes(x = Age, y = RTmean))+ 
  geom_point() +
  geom_abline(intercept = 862.05, slope=11.98)

# Display the two plots side by side to better see what's going on.

install.packages("gridExtra")
library(gridExtra)

plot1 <- ggplot(reshaped_dat, aes(x = Age, y = RTmean))+ 
  geom_point() +
  geom_abline(intercept = 637.93, slope=21.22) +ggtitle("With Outlier")

plot2 <- ggplot(cast2, aes(x = Age, y = RTmean))+ 
  geom_point() +
  geom_abline(intercept = 862.05, slope=11.98) + ggtitle("Without outlier")

grid.arrange(plot1, plot2, ncol=2)

# Compute the proportion of variance in RT that can be accounted for by Age.
# In other words: Compute R Squared.
# Refer to Navarro (Chapter on regression) if you have trouble doing this.
summary(lrm_cast2)$r.squared

# How do you interpret this number?
# 0.03493231
# A small value for Residual Sum of Squares implies that the data points are closer to the regression line. 
# The error value obtained after leaving out the outlier is considerably small indicating that the coefficients obtained result in  best fit 

