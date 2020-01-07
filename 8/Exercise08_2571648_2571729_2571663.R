### Stats with R Exercise sheet 8

##########################
#Week9: Checking Assumptions underlying ANOVA and linear regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 16. Write the code below the questions. 
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

###############################################################################
###############################################################################

########
### Exercise 1
########

########
### Please, use ggplot to make plots in all exercises below!
########
library(ggplot2)

# a) Read in the data kidiq.txt (available in the Moodle) and take a look
#    at the data summary. It contains information about the mum's iq and 
#    their child's iq. 
#    mom_hs indicates whether the mother has a high school degree
#    1= high school education, 0= no high school degree.
getwd()
#setwd("D:/SaarlandUniversity_Masters/3_WS18/StatisticsMitR/assignments/MySolutions/8")

kidiq<-read.table(file="kidiq.txt")
summary(kidiq)



# b) Plot kid_score against mom_iq in a scatter plot, and add a regression line 
#    (kid_score should be the response variable and mom_iq the predictor 
#    variable) to the plot. 
#    Name the plot and the axis in sensible ways.
ggplot(kidiq, aes(x=mom_iq,y=kid_score))+
  geom_point()+
  xlab("mom's iq")+
  ylab("kid's score")+
  geom_smooth(method='lm')+
  ggtitle("Plot of the kid's score versus mom's iq")


# c) Calculate a simple regression model for kid_score with mom_hs as a 
#    predictor and interpret the results.

linmod1 <- lm(data = kidiq, kid_score ~ mom_iq)
summary(linmod1)
#As p value is less than 0.05, there exists a statistical significance in the relationship between kid score and mom_hs.


# d) Next, fit a regression model with two predictors: mom_hs and mom_iq. 
#    Interpret the model and compare to the previous model.
linmod2 <- lm(data = kidiq, kid_score ~ mom_iq+mom_hs)
summary(linmod2)
# The result shows that there is a relationship between kid's iq and 
# both the mom_iq and mom_hs as p values with regard to both mom_iq and mom_hs are <0.05 
# However, this linar model does not give any information about the interaction between parameters 

# e) Now plot a model where both predictors are shown. Do this by plotting 
#    data points for mothers with high school degree==1 in one color and those 
#    without one in another color. Then also fit two separate regression lines such 
#    that these lines reflect the model results.
#	   HINT: One solution in ggplot is to calculate fitted values of the regression model 
#    and then plot them along with the original data points:
#    pred = data.frame(mom_iq=kidiq$mom_iq, mom_hs=kidiq$mom_hs, kid_score_pred=fitted(your_model))
pred = data.frame(mom_iq=kidiq$mom_iq, mom_hs=kidiq$mom_hs, kid_score=kidiq$kid_score, kid_score_pred=fitted(linmod2))
summary(pred)

ggplot(data = pred, aes(x = kid_score, y = mom_iq, col = factor(mom_hs))) +
  geom_point() +
  geom_line(aes(x = kid_score_pred, y = mom_iq))


# f) Next, we will proceed to a model including an interaction between mom_hs
#    and mom_iq. Interpret your results.
    model2=lm(kid_score~mom_iq*mom_hs , data=kidiq)
    summary(model2)
    #The overall result remains the same as the model without interactions in d part. In all the cases, the p value is less than 0.05

# g) Next, let's plot the results of this model.
    pred2=data.frame(mom_iq=kidiq$mom_iq, mom_hs=kidiq$mom_hs, kid_score_pred=fitted(model2))
    ggplot(pred2, aes(x=mom_iq,y=kid_score_pred, col=factor(mom_hs)) )+
      geom_point()

    


# h) Next, let's explore the "predict.lm" function. Please first generate
#    a new dataframe with one datapoint (a mother with high school degree
#    and iq of 100). Then, use the predict function to predict the child's iq. 
#    Please specify the predict function to also give you the 0.95 confidence 
#    interval.
    
    newFrame <- data.frame(mom_iq=100, mom_hs=1)
    predict.lm(model2, newFrame, level=0.95)

# i) Meaning of confidence intervals for regression line.
#    Let's go back to the exercise b) and plot again the data points with the 
#    regression line. By default, there should also be displayed the borders of 
#    the confidence interval. What is the meaning of the confidence interval?
      ggplot(kidiq, aes(x=mom_iq,y=kid_score))+
      geom_point()+
      xlab("mom's iq")+
      ylab("kid's score")+
      geom_smooth(method='lm')+
      ggtitle("Plot of the kid's score versus mom's iq")
    #It means if we replicate the same dataset multiple times with different random samples
    #and compute CI for each sample, then 95% of the confidence intervals would contain the true
    #slope of the regression line.
  

# j) Finally, do model checking on your model with the interaction, i.e. inspect 
#    the standard model plots provided by R, and interpret what you see.
      par(mfcol=c(2,3))
      plot(lm(kid_score~mom_iq*mom_hs, data=kidiq), which=seq(1,6))
      
# Plot: Residuals vs Fitted: we can observe there are almost equally spreaded residuals around the horizontal line
      # without any distinct patterns. Its a good indication that the data doesn't have a non-linear relationship
      
# plot: Scale-Location: This graph shows whether the residuals are spreaded equally along the ranges predictiors. 
      # we can obeserve the data points are spreaded equally on the both sides of horizontal line. It indicates the 
      # the assumption has equal variance (homoscedasticity)
      
# plot: Residuals vs Leverage: This plot helps to observe whether there any influential cases or influential outliers
      # for the model assumption. In our case, we can see there is not influential cases as we can see all the data 
      # points are well within less cook's distance except for the last one, maybe.
      
# plot: Normal Q_Q: This plot indicates whether the residuals are normally distributed or not. In our case we can observe
      # that they are fitted alomost in a straight line, which indicates they are noramlly distributed.
      
# plot: Cook's distance: This plot indcates whether there is any influential data points. observation 266 doesn't seem
      # substentially different from couple of other observations. observation 213 has a slightly higher value than the
      # rests but to us it doesn't seem to be an outlier. Observation 111 has much higher value. So, it is considered to 
      # be influetial and we can have a closer look on that particualar observation
kidiq[111,] 
# row_id kid_score   mom_hs   mom_iq      mom_work mom_age
#111        49        0       112.0189        1      28

# Compare to mom_iq, the kid_score is pretty low, which is not a normal trend of the given dataset.



# plot: Cook's Dist vs Leverage:
# This plot also is to find whether there are any highly leveraged observation which also have high influence in the
# model. As we can see observations 111, 212 are highly leveraged, but the doesn't have that much influence in the 
# fitted model
      
      
      

