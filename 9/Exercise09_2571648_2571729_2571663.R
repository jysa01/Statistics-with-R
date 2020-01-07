### Stats with R Exercise sheet 9

##########################
#Week 10: Linear Mixed Effects Models
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 30. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Sara Khan, Jyothsna Shasikumar Sastry, Hasan Md. Tusfiqur Alam 
## Matriculation number:2571648, 2571729, 2571663

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################

library(lme4)
library(lattice)
library(Matrix)

# Read in the data file from gender.Rdata, sem.Rdata or relclause.Rdata.
# You can choose the one you'd like best based on the description of the items below, 
# and explore the analysis for that dataset. Afterwards, you can adapt your analysis 
# for the other datasets (that should be considerably less work.)

# The files contain data from an experiment where people were reading sentences, 
# and pressed the space bar to see the next word. The duration for which a word was 
# viewed before pressing the space bar again is the reading time of the word, and is 
# stored in the file as "WORD_TIME". The experiment had 24 items (given as "ITEM_ID") 
# and 24 subjects (given as "PARTICIPANT"). The order in which the different sentences 
# were presented in the experiment is given in the variable "itemOrder". 

# For each of the files, the sentences that were shown had a different property. 

# Sentences in the sem.Rdata experiment had a semantic violation, i.e. a word that 
# didn't fit in with the previous words in terms of its meaning. The experiment 
# contained two versions of each item, which were identical to one another except 
# for the one sentence containing a semantic violation, while the other one was 
# semantically correct. These conditions are named "SG" for "semantically good" 
# and "SB" for "semantically bad".

# Semantic materials (the experiment is in German, English translation given 
# for those who don't speak German')

# Christina schießt / raucht eine Zigarette nach der Arbeit. 
# "Christina is shooting / smoking a cigarette after work."

# The crticial word here is "Zigarette", as this would be very surprising in the 
# context of the verb "schießt", but not in the context of the verb "smoke". 
# Reading times are comparable because the critical word "Zigarette" is identical 
# in both conditions.

# Syntactic items:
# Simone hatte eine(n) schreckliche(n) Traum und keine Lust zum Weiterschlafen. 
# "Simone had a[masc/fem] horrible[masc/fem] dreammasc and didn't feel like sleeping 
# any longer."

# Here, there are again two conditions, one using correct grammatical gender on 
# "einen schrecklichen" vs. the other one using incorrect grammatical gender 
# "eine schreckliche". The critical word is "Traum" (it's either consisten or 
# inconsistent with the marking on the determiner and adjective)

# Relative clause items:
# Die Nachbarin, [die_sg nom/acc einige_pl nom/acc der Mieter auf Schadensersatz  
# verklagt hat_sg/ haben_pl]RC, traf sich gestern mit Angelika. 
# "The neighbor, [whom some of the tenants sued for damages / who sued some of  the
# tenants for damages]RC, met Angelika yesterday."

# When reading such a sentence, people will usually interpret the relative pronoun 
# die as the subject of the relative clause and the following noun phrase 
# "einige der Mieter" as the object. This interpretation is compatible with 
# the embedded singular-marked (sg) verb hat at the end of the relative clause. 
# Encountering the verb haben, which has plural marking (pl), leads to processing 
# difficulty: in order to make sense of the relative clause, readers need to 
# reinterpret the relative pronoun die as the object of the relative clause 
# and the following noun phrase "einige der Mieter" as its subject. 
# (Note that the sentences are all grammatical, as the relative pronoun and 
# following NPs are chosen such that they are ambiguous between nominative (nom)
# and accusative (acc) case marking.)

# The number of the word in a sentence is given in column "SEMWDINDEX". 
# 0 designates the word where the semantic violation happens (in the SB condition; 
# in the SG condition, it's the corresponding word). We call this word the 
# "critical word" or "critical region". -1 is the word before that, -2 is 
# two words before that word, and 2 is two words after that critical word. 
# "EXPWORD" shows the words. We expect longer reading times for the violation 
# at word 0 or the word after that (word 1) (if people press the button quickly 
# before thinking properly).

#######################################################################################
#######################################################################################

# a) Take a look at the data.
getwd()
#setwd("D:/SaarlandUniversity_Masters/3_WS18/StatisticsMitR/assignments/MySolutions/9")
##NOTE:  You can change it to gender.Rdata if u have saved like that##
gD <- "gender.Rdata.txt"
genderData<-read.table(gD, header=TRUE, sep=" ")
rD <- "relclause.Rdata.txt"
relclauseData <- read.table(rD, header=TRUE, sep=" ")
sD <- "sem.Rdata.txt"
semData <- read.table(sD, header=TRUE, sep=" ")
str(genderData)
summary(genderData)
##Output str(genderData)
#'data.frame':	5782 obs. of  7 variables:
#  $ PARTICIPANT: Factor w/ 24 levels "p0125fr","p0227fr",..: 5 5 5 5 5 5 5 5 5 5 ...
#$ ITEM_ID    : Factor w/ 24 levels "G 1","G 10","G 11",..: 6 6 6 6 6 6 6 6 6 6 ...
#$ ITEM_TYPE  : Factor w/ 2 levels "GB","GG": 1 1 1 1 1 1 1 1 1 1 ...
#$ EXPWORD    : Factor w/ 181 levels "�"pfel","über",..: 23 168 167 34 38 46 36 34 75 2 ...
#$ WORD_TIME  : int  1902 952 1299 622 581 1165 1308 545 530 818 ...
#$ itemOrder  : int  3 3 3 3 3 3 3 3 3 3 ...
#$ RELWDINDEX : int  5 1 0 3 -3 -4 -6 -2 4 2 ...

str(relclauseData)
summary(relclauseData)
str(semData) 
summary(semData)

# b) Plot it (use ggplot for this task and all the tasks below).
#    (You can provide any plots we have seen so far to interpret the data.
#    For example, you can study the difference between the subjects (participants) 
#    in terms of responce time or the difference between items (sentences) in 
#    terms of response time).

library(ggplot2)
library(reshape)
ggplot(data=genderData, aes(WORD_TIME))+
  geom_histogram()
# WORD_TIME data is positively skewed. There may be 2 outliers since they are very far away 
# and have very low count values.


ggplot(data=genderData, aes(x=WORD_TIME, fill=PARTICIPANT, col=PARTICIPANT)) + 
  geom_density(alpha=0.1)
# There may be significant difference between word time distributions of some participants as 
# seen in the density plot. The distribution peaks of the participants is very scattered

ggplot(data=genderData, aes(x=WORD_TIME, fill=ITEM_ID, col=ITEM_ID)) + 
  geom_density(alpha=0.1)
# The distributions are positively skewed. But we can comment that item ids do not make
# make much difference on the response time, that is, all items may belong to a single 
# distribution.

ggplot(data=genderData, aes(x=WORD_TIME, fill=ITEM_TYPE, col=ITEM_TYPE)) + 
  geom_density(alpha=0.2)
# same observation can be made for item type as well. Distribution is positively skewed. The distribution
# for GG item type has a longer tail which may be due to outliers. Lets check the boxplot.

ggplot(data=genderData, aes(y=WORD_TIME, x=ITEM_TYPE, col=ITEM_TYPE)) + 
  geom_boxplot(alpha=0.2, show.legend=F)
# Looks like the participants take same time for responding to corrcect and wrong gender 
# in sentences (mean and width of boxplot is same). The extreme points are more in 
# correct gender (GG) type as concluded by darker colored points between 2000 - 4000 word time.
# The points above 5000 word time may be outliers (We can check this by plotting word time 
# of other participants for this word and later by checking the Cook's distance.)

subset(genderData, WORD_TIME > 5000)
# Find the words which triggered the longest response time
#              PARTICIPANT ITEM_ID ITEM_TYPE EXPWORD WORD_TIME itemOrder RELWDINDEX
#       54241      p0620mr    G 10        GG  Titel.      6269        46          4
#      112126      p2220fr     G 6        GG    See.      5036        13          4

# Subset the data for only these two words
d1<-subset(genderData, EXPWORD %in% c('Titel.', 'See.'))
str(d1)
d1$EXPWORD <- factor(d1$EXPWORD)
ggplot(data=d1, aes(x=EXPWORD, y=WORD_TIME, col=ITEM_TYPE))+
  geom_boxplot(alpha=0.2)
# word time of participant 'p0620mr' seems to be unusually very long. The mean and 3rd quartile 
# for that word is less than 2000 ms. 
# For the 2nd participant, we cannot say surely whether
# word time of 5036 is too long, since, from the boxplot it can be seen that while 50% of the 
# word time below the mean is closely spaced, the word times above the mean have a larger spread.

#lets see if these words occur frequently in the sentences.
ggplot(data=genderData, aes(x=as.numeric(EXPWORD), y=WORD_TIME, col=ITEM_TYPE))+
  geom_point(alpha=0.2)
# Occurrence of some words in sentences is more often than the rest. These may be the 
# function words. The words which are less frequent are content words. It is for these low
# frequency words that the word time is higher. 
# The item type does not provide any extra information in this plot. 

ggplot(data=genderData, aes(x=WORD_TIME, fill=ITEM_TYPE, col=ITEM_TYPE)) + 
  geom_density(alpha=0.2)+
  facet_grid(.~as.factor(RELWDINDEX))
# For incorrect sentences (GB type, red plot) we expect word time to increase 
# as we move away from the critical word, i.e., for plots with RELWDINDEX from 1 to 7.
# in these plots however the blue and red plots greatly overlap across all RELWDINDEX
# except maybe at RELWDINDEX of 7 where GB item type seems to have a bimodal type distribution.
# We can loosely conclude that word time of participants do not change much after reading
# the GB sentence critical word until quite later into the sentence where the word may depend 
# the critical word.

ggplot(data=genderData, aes(x=RELWDINDEX, y=WORD_TIME, col=ITEM_TYPE))+
  geom_point(position='jitter', alpha=0.2)+
  geom_smooth(method='lm')
# The word time for item type GG is shorter in the beginning of a sentence and increases as
# the sentence progresses. For the item type GB, word time of the participants does not 
# seem affected since the regression line is almost horizontal.
# Maybe GG and GB does not stand for 'gender good' and 'gender incorrect' respectively,
# but, something else?




#    Below you also find the plot for the dataset 'sleepstudy' from the package 'lme4'.
#    The figure shows relationships between days without sleeping and reaction 
#    time for each participant (subject) separately.

summary(sleepstudy)
print(xyplot(Reaction ~ Days | Subject, sleepstudy, aspect = "xy",
             layout = c(9,2), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "Days of sleep deprivation",
             ylab = "Average reaction time (ms)"))

#    Your task is also to figure out how to adapt this plot for our data. What do you 
#    conclude regarding the reading sentences experiment?
# we have 24 participants in this study. We need to check whether any conclusions made about the 
# effect of a predictor variable say, ITEM_TYPE, RELWDINDEX, etc., on the response variable WORD_TIME
# is generalizable across participants, i.e., the effects (slopes and intercept) when studied in 
# each participant do not differ greatly.

print(xyplot(WORD_TIME ~  as.numeric(ITEM_ID)| PARTICIPANT, genderData, aspect = "fill",
             layout = c(6,4), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "Item ID",
             ylab = "Average reaction time (ms)"))
# The plot of each participant on item id shows item_id has little or no correlation to word time.
# scatter plot is horizontally spread and the regression line is more or less horizontal.
# The participant 'p24m26' is vey fast(variance along y axis is very low). For participant 'p0620mr'
# the data point on the top right corner seems a very high leverage point because this point may
# induce a negative correlation in the data which otherwise seems uncorrelated to item_id predictor.
# The intercept value of different partici[ants is different. The intercept value is more toward 0
# for the participants in the last row when compared to those in the first row.
# Also the variance along the y axis (predictor variable) is less for some participants and more 
# for others. Thus using this predictor generally for all participants may not be a good idea.

print(xyplot(WORD_TIME ~  ITEM_TYPE| PARTICIPANT, genderData, aspect = "fill",
             layout = c(6,4), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "Type of item (SB/SG)",
             ylab = "Average reaction time (ms)"))
# the predictor item_type has a very weak to almost no positive correltaion to the response.
# The intercept value is also similar across participants.
# Thus, this predictor is generalizable over participants.

print(xyplot(WORD_TIME ~  as.numeric(EXPWORD)| PARTICIPANT, genderData, aspect = "fill",
             layout = c(6,4), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "Exp Words",
             ylab = "Average reaction time (ms)"))
# This predictor is also generalizable across participants. The intercept across particiapnts is 
# different across participants though. The slower particiapnts (1st row with high intercept value)
# are in general slow with all words. The faster responding participants (last row, low intercept) 
# are generally faster and slower on some words (more scatter/variance in the y axis).

print(xyplot(WORD_TIME ~  itemOrder| PARTICIPANT, genderData, aspect = "fill",
             layout = c(6,4), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "item order",
             ylab = "Average reaction time (ms)"))
# There seems to be a consistent but very weak negative correlation between the predictor and response
# variables,  For fast responding participants (last row), the correlation is almost absent.

print(xyplot(WORD_TIME ~ RELWDINDEX | PARTICIPANT, genderData, aspect = "fill",
             layout = c(6,4), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "Relative word index",
             ylab = "Average reaction time (ms)"))
# For participant 'p2220fr' the word time for words at 4th position after the critical word
# is generally higher than the preceding words. Other than this the relation is consistently
# weak across all particiapnts. Here again the fastest responders are generaaly fast on all 
# word positions(small intercepts, last row). 

print(xyplot(WORD_TIME ~  (as.numeric(ITEM_ID)+itemOrder+RELWDINDEX)| PARTICIPANT, 
             genderData, aspect = "fill",
             layout = c(6,4), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "Predictors",
             ylab = "Average reaction time (ms)"))

## In general it can be said that the relation between predictors and response variables
# is more evident in the slow responders than the fast responders. For the fast responders,
# the intercept and slope are both close to 0. 


# c) Decide whether you want to exclude any data points (provide not only the code,
#    but also a detailed (!) explanation);
lm.mod1 <- lm(WORD_TIME ~ ITEM_ID+ITEM_TYPE+EXPWORD+itemOrder+RELWDINDEX, data=genderData)
summary(lm.mod1)
par(mfcol=c(2,3))
plot(lm.mod1, which=seq(1,6))
# We have seen in earlier plots that the data had high positive skew, this becomes evident in
# the QQ plot because the residuals deviate highly at the borders. 
# Observations labelled 54241, 112126, 102569 seems troublesome and may be removed from the data.
# The R-squared (0.1859) and adjusted R-squared (0.1558) values are very low which means 
# that the model is not a very good fit to the data. Residual std error = 300.7
# Let us remove these observations

remove_data <- c('54241', '112126', '102569')
genderData2<-genderData[ !(rownames(genderData) %in% remove_data), ]
lm.mod2 <- lm(WORD_TIME ~ ITEM_ID+ITEM_TYPE+EXPWORD+itemOrder+RELWDINDEX, data=genderData2)
summary(lm.mod2)
par(mfcol=c(2,3))
plot(lm.mod2, which=seq(1,6))
# Rsq (0.1858) and adjusted R-squared (0.1557)
# Residual Standard error - 285.8
# The R-squared value does not improve. The residual standard error inproves slightly.
# Removing the observations does no tgreatly improve the R squared value
# On the contrary the QQ plot deviates greatly from the normal and the model identifies more 
# extreme values. This may be due to the positive skew in the data distribution.



# d) Try to make a plot where for each word, the average reading 
#    (collapsing across items and subjects) is shown; in this plot all violations 
#    are at point 0. Of course, you should not collapse the semantically good vs. 
#    bad condition.
sum(is.na(genderData$ITEM_TYPE))
sum(is.na(genderData$EXPWORD))
genderdata_casted <- cast(genderData, EXPWORD + ITEM_TYPE ~., mean, value='WORD_TIME')

colnames(genderdata_casted)[colnames(genderdata_casted)=="(all)"] <- "Average_Reading_Time"
summary(genderdata_casted)

ggplot(genderdata_casted, aes(x=EXPWORD, y=Average_Reading_Time, col=ITEM_TYPE))+
  geom_point()+
  geom_smooth(method='lm', aes(group=ITEM_TYPE), se=F)


# e) Experiment with calculating a linear mixed effects model for this study, 
#    and draw the appropriate conclusions (give a detailed explanation 
#    for each model).
lme_model1 = lmer(WORD_TIME ~ ITEM_TYPE + (ITEM_TYPE|PARTICIPANT) + (ITEM_TYPE|EXPWORD), genderData)
summary(lme_model1)
# the linear mixed effect model built studies the fixed effect of item type and random effect of participants and the words on the word time. 
# the summary results, we can infer that the speed of the participant speed affects the word time due that variance value for the intercept
# Also, the item type is affected by the words used in the experiment

#    Let's get back to the dataset 'sleepstudy'. The following plot shows 
#    subject-specific interception and slope. Adapt this plot for our study 
#    and make conclusions.

model = lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
print(dotplot(ranef(model,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["Subject"]])


lme_model2 = lmer(WORD_TIME ~ ITEM_TYPE * RELWDINDEX + (RELWDINDEX*ITEM_TYPE|PARTICIPANT) + (ITEM_TYPE|EXPWORD), genderData)
summary(lme_model2)
print(dotplot(ranef(lme_model2,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["PARTICIPANT"]])
# the linear mixed effect model built studies the fixed effect of relative word index on the word time
# from this model, we can infer that the relative word index is not a good predictor for fixed effects owing to the negative correlation 
# between item type and RELWINDEX
