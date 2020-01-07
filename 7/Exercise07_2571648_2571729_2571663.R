### Stats with R Exercise sheet 7

##########################
#Week 8: ANOVA
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 9. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

## Please write below your (and your teammates) name, matriculation number. 
## Name:Sara Khan, Jyothsna Sastry , Hasan Md. Tusfiqur Alam
## Matriculation number: 2571648, 2571729, 2571663

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################



#######################
### PART 1: Preparation
#######################

library(boot)
library(ggplot2)
library(reshape)

# This time we will be working with the "amis" data frame (package 'boot') that has 
# 8437 rows and 4 columns.

# In a study into the effect that warning signs have on speeding patterns, Cambridgeshire
# County Council considered 14 pairs of locations. The locations were paired to account 
# for factors such as traffic, volume and type of road. One site in each pair had a sign 
# erected warning of the dangers of speeding and asking drivers to slow down. No action 
# was taken at the second site. Three sets of measurements were taken at each site. 
# Each set of measurements was nominally of the speeds of 100 cars but not all sites 
# have exactly 100 measurements. These speed measurements were taken before the erection 
# of the sign, shortly after the erection of the sign, and again after the sign had been 
# in place for some time.

# 1. For the further reference please use ?amis. It may take some time to understand the dataset. 
?amis

# 2. Load the dataset and briefly inspect it. 
# Feel free to make some plots, calculate some statistics in order to understand the data.
?amis
head(amis)
str(amis)
summary(amis)
ggplot(amis, aes(x=period,y=speed, col=as.factor(warning))) + geom_point()
ggplot(amis, aes(x=as.factor(period),y=speed)) + geom_boxplot()
ggplot(amis, aes(x=as.factor(pair),y=speed)) + geom_point()

# 3. All our columns have numeric type. Convert the categorial columns to factors.
amis$period<-as.factor(amis$period)
amis$warning<-as.factor(amis$warning)
amis$pair<-as.factor(amis$pair)

# 4. Build a boxplot for the distribution of `speed` for each of `period` values 
# (before, immediately after and after some time). Build 2 plots side by side
# depending on the `warning` variable.
# (for all plots here and below please use ggplot)
install.packages("gridExtra")
library(gridExtra)
boxplot1_data<-subset(amis, warning==1)
boxplot2_data<-subset(amis, warning==2)
summary(boxplot1_data)
summary(boxplot2_data)
boxplot1<-ggplot(boxplot1_data, aes(x=factor(period),y=speed)) + geom_boxplot()
boxplot2<-ggplot(boxplot2_data, aes(x=factor(period),y=speed)) + geom_boxplot()
grid.arrange(boxplot1, boxplot2, ncol=2)

# 5. What can you conclude according this plot? What can you say about people behaviour in
# different periods: before, immediately after and after some time?
# Boxplot1
# Immediately after the warning sign is placed, there is a slight reduction in the vehicle speed. 
# This however increases when recorded after some period of time (at period 3)
# Boxplot2
# There exists not much difference between the vehicle speeds at the 3 periods in the absence of warning signs

# 6. What are your ideas about why the data with warning==2 (which correspond to the
# measurements in different times on sites where no sign was erected) was collected?
# The data recorded when there are no warning signs placed helps to understand the natural behavior and compare it against 
# that of the driver when the warnings are placed

#######################
### PART 2: 1-way ANOVA
#######################
install.packages("car")
library(car)

#1. First let's create a new data frame which will be used for all PART 2.
# For 1-way ANOVA we will be working with the subset of `amis` where the 
# warning sign was erected, which corresponds to warning==1, therefore first
# subset your data to filter out warning==2 and then apply cast() to average
# speed over each "pair" and "period. Assign this new data frame to the variable casted_data.
amis_data<-subset(amis, warning==1)
#summary(amis_data)
casted_data<-cast(amis_data, period + pair ~., fun.aggregate = 'mean', value = 'speed', na.rm = TRUE)
colnames(casted_data)[colnames(casted_data)=="(all)"] <- "AverageSpeed"
summary(casted_data)

# 2. Build a boxplot of the average speed depending on period
ggplot(data=casted_data, aes(x=factor(period), y=AverageSpeed)) + 
  geom_boxplot() + 
  labs(title="Boxplot of the average speed depending on period")

# 3. Is there a difference between the periods?
# Yes there exists a difference between the Average speed plot for periods
# Average speeds recorded for period 3 is the highest, followed by that for Period 1 and then 2. 

# 4. Now, let's check each ANOVA assumptions and whether they are violated or not and why.

# a) Independence assumption
# (you need to figure out the best way to do it and give a detailed justified answer)
  permuted_data<-casted_data
  head(casted_data)
  permuted_data<- permuted_data[sample(1:nrow(permuted_data)), ]
  head(permuted_data)
  summary(aov(casted_data$AverageSpeed~casted_data$period))
  summary(aov(permuted_data$AverageSpeed~permuted_data$period))
  # The variance remains same for permutations of the data. This upholds independence assumption
  
# b) Normality of residuals
# (you need to figure out the best way to do it and give a detailed justified answer)
  res = residuals(lm(casted_data$AverageSpeed~casted_data$period))
  model=aov(casted_data$AverageSpeed~casted_data$period) 
  res=model$residuals
  shapiro.test(res)
  # Assume normality of residuals
  # The obtained p value is large indicating that the assumped hypothesis cannot be rejected


# c) Homogeneity of variance of residuals
# (you need to figure out the best way to do it and give a detailed justified answer)
  period1<-as.factor(casted_data$period)
  leveneTest(casted_data$AverageSpeed~period1)
  # Large value of p indicates Homogeneity of variance of residuals

# 5.Now we are ready to perform 1-way ANOVA: please use the function aov() on the speed
# depending on the period,report p-value and interpret the result in details
?aov
anova1<-aov(casted_data$AverageSpeed ~ casted_data$period)
summary(anova1)
# p>0.05 (p=0.382)
# Thus, we fail to reject the null hypothesis. There is no significant relationship between The Average speed and period
# The F value is 0.986, The ratio of Variance between groups to the variance within groups is small

# 6. Please do a pairwise t-test with pairwise.t.test()
pairwise.t.test(casted_data$AverageSpeed,casted_data$period)
?pairwise.t.test
# 7. Report pair-wise p-values and interpret the result in details
#   1    2   
#2 0.81 -   
#3 0.81 0.51
#Since all values are greater than 0.05, none of the interactions between the 3 period groups are statistically significant

# 8. Try to use no adjustment for pairwise testing and then Bonferroni correction.
# Does the result change?

pairwise.t.test(x=casted_data$AverageSpeed, g=casted_data$period, p.adjust.method = "none")
#   1    2   
#2 0.59 -   
#3 0.40 0.17

pairwise.t.test(x=casted_data$AverageSpeed, g=casted_data$period, p.adjust.method = "bonferroni")
#   1    2   
#2 1.00 -   
#3 1.00 0.51
# Yes, there is a difference in the results. There is an increase in the p values when bonferroni is used in comparison to no adjustment 

#######################
### PART 3: 2-way ANOVA
#######################
# 1. Now we want to analyze the influence of 2 categorial variables (period and warning) on the speed.
# So let's turn back to our initial dataset amis (not its subset with warning==1)
# First, we need to again average the speed over each `pair`, `warning` and `period
# Cast your data again and assign the resuts to casted_data2
casted_data2<-cast(amis, pair+period+warning ~., fun.aggregate = mean, value = "speed", na.rm = TRUE)
colnames(casted_data2)[colnames(casted_data2)=="(all)"] <- "AverageSpeed"
summary(casted_data2)

# 2. Calculate the mean for each of 6 pairs of `period` and `warning`
tab<-aggregate(AverageSpeed ~ period + warning, casted_data2, mean)
tab
# 3. Do you think there is a significant difference in some of the groups?
var(tab[,3])
# 2.367826
# The average speeds have a variance of 2.367826. 

# 4. Now apply 2-way ANOVA: please use the function aov() on the speed depending on the period and warning
# report p-value and interpret the result in details
anova2<-summary(aov(casted_data2$AverageSpeed~casted_data2$period*casted_data2$warning,casted_data2))
anova2
# The p values between speed and warning = 0.00488 with F value = 8.396. The ratio of Variance between groups to the variance within groups is sufficiently large and the p value impleis statistical significance in their relationship
# The p values between speed and period = 0.33507 imply that the interactions between these two entities are statistically insignificant though the F value > 1
# The p values between perid and warning =  0.69975 imply that the interactions between these two entities are statistically insignificant

# 5. What do you conclude about the behaviour of drivers based on the 2-way ANOVA?
# There is no significant interaction between Period~Speed, Period and warning
# The speed however, is affected by the presence/absence of warnings (statistically significant)


