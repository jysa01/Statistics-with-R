### Stats with R Exercise sheet 4

##########################
#Week5: Tests for Categorial Data
##########################

## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 18. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name:Sara Khan, Jyothsna Sastry , Hasan Md. Tusfiqur Alam
## Matriculation number: 2571648, 2571729, 2571663

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

#################################################################################
#################################################################################

##########
##Exercise 1. Binomial distribution
##########
## Suppose there are 12 multiple choice questions in a quiz. 
## Each question has 5 possible answers, and only one of them is correct. 

## a) Please calculate the probability of getting exactly 4 answers right 
##    if you answer by chance. Calculate this using the dbinom() function.
      dbinom(4,size=12,0.2)

## b) Next please calculate the probability of answering 4 or less questions 
##    correctly by chance. 
  dbinom(0, size=12, prob=0.2) + 
  + dbinom(1, size=12, prob=0.2) + 
  + dbinom(2, size=12, prob=0.2) + 
  + dbinom(3, size=12, prob=0.2) + 
  + dbinom(4, size=12, prob=0.2) 

##########
##Exercise 2. Chi-square test
##########
## a) Consider the dataset dutchSpeakersDistMeta from our first tutorial again. 
##    Load the package (languageR) and look at the summary of the variables, 
##    as well as their classes. Which variables are factors?
      library(languageR)
      summary(dutchSpeakersDistMeta)
      str(dutchSpeakersDistMeta)
      #Speaker, Sex,AgeGroup,ConversationType and EduLevel are factors

## b) We want to find out whether there is a difference between males and females 
##    with respect to the age groups they are in.
##	  First use the function 'table()' to get the counts and create 
##    a contingency table of AgeGroup by Sex.
      table(dutchSpeakersDistMeta$AgeGroup)
      table(dutchSpeakersDistMeta$Sex)
      Age<-table(dutchSpeakersDistMeta$AgeGroup,dutchSpeakersDistMeta$Sex)
##    Visualize your data with a single bar plot (use ggplot) that represents the counts with respect to each age group 
##	  and each sex.
      install.packages("ggplot2")
      library(ggplot2)
      library(plyr)
      
      ggplot(dutchSpeakersDistMeta, aes(x=factor(AgeGroup)))+
        geom_bar(stat="count", width=0.7, position = "dodge", aes(fill = Sex))
        
## c) Inspect the table 'age' you created. Does it look like there could be a significant 
##    difference between the sexes?
      Age
      #Yes, there is significant difference as there are total 90 females as copared to 73 males
## d) We are going to calculate whether there's a difference between males and females 
##    regarding their age group
##    using the function chisq.test. Look at the help of this function. Then use the 
##    function to calculate whether there's a difference in our table 'age'. 
##    Is there a significant difference in age group?
      help(chisq.test)
      chisq.test(Age)
      #OUtput
      #data:  Age
      #X-squared = 3.2785, df = 4, p-value = 0.5124
      #As value of p>0.5, we cannot reject the null hypothesis. The null hypothesis is there is no significance difference in age groups
    
## e) What are the degrees of freedom for our data? How are they derived?
    #Degrees of freedom= (#rows-1)*(#colums-1)
    #In this case, (5-1)*(2-1)=4

##########
##Exercise 3. Binomial versus chi-square
##########
##    In this exercise, we'll do significance tests for a paper on therapeutic touch 
##    (google it if you want to know what that is...) that was published in the Journal 
##    of the American Medical Association (Rosa et al., 1996).
##    The experimenters investigated whether therapeutic touch is real by using the 
##    following method:
##    21 practitioners of therapeutic touch were blindfolded. The experimenter 
##    placed her hand over one of their hands. If therapeutic touch is a real 
##    phenomenon, the principles behind it suggest that the participant should 
##    be able to identify which of their hands is below the experimenter's hand. 
##    There were a total of 280 trials, of which the therapeutic touch therapists 
##    correctly indicated when a hand was placed over one of their hands 123 times.

## a) What is the null hypothesis, i.e. how often would we expect the participants to 
##    be correct by chance (in raw number and in percentage)?
      
      #Ans
      #Null hypothesis- The therapeutic touch is not real or doesn't works
      #The participants should be correct by chance in % is 50% and in raw number is 0.5
## b) Using a chisquare test, what do you conclude about whether therapeutic touch 
##    works? 
      ??chisq.test
      df<-as.data.frame(rbind(123,157))
      df
      chisq.test(df) 
      #Considering one tail test
      #As p value is less than 0.05, we reject the null hypothesis and thus therapeutic touch doesn't works
## c) Now calculate significance using the binomial test as we used it in exercise 1.
      
      #Ans
      binom.test(123,280,0.5)
      #We have used one tail test
## d) The results from these two tests are slightly different. Which test do you think 
##    is better for our data, and why?
      #Ans
      #Binomial tests are better. Because binomial test is an exact test but chi square is just an asymptotic test

##########
##Exercise 4.
##########
## Describe a situation where you would choose McNemar's test over the ChiSquare test. 
## What would be the problem of using the normal ChiSquare test in a case where 
## McNemar's test would be more appropriate?

#Ans
#The McNemar is not testing for independence(in Chi square), but consistency in responses across two variables.
#In the McNemar test, we can compare counts directly, because the comparison is not based on row totals. 
#Thus, when we have more observations for a single goal, McNemar's test is used.  