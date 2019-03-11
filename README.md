# AMADisparities
---
title: "AMA Results"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Libraring packages
```{r}
library(stringr)
library(prettyR)
library(Zelig)
```
Loading data
```{r}
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/AMAData")
AMAData = read.csv("AMAData.csv", header = TRUE, na.strings = c("NULL"))
```
Try to look at the variables
Variables to keep:
Client_ID, AgeAt_ASSESS_Date, DischargeType, ClientEncounter_NUM

A292_Q1	A292_Q59	A292_Q62	A292_Q66	A298_Q5	A298_Q6	A298_Q7	A292_Q1	A298_Q2	A298_Q5	A298_Q6	A298_Q7	A292_Q59	A292_Q62	A292_Q66	A294_Q20	A294_Q41	A294_Q43	A294_Q49	A294_Q50	A294_Q51	A294_Q53	A294_Q55	A294_Q57	A294_Q60	A294_Q62	A294_Q63	A294_Q64	A294_Q66	A294_Q67	A294_Q68	A294_Q69	A294_Q70	A294_Q72	A294_Q73	A294_Q74	A294_Q78	A294_Q90	A294_Q94	A294_Q95	A294_Q102	A294_Q104	A294_Q106	A294_Q108	A294_Q110	A294_Q125	A294_Q127	A294_Q129	A294_Q137	A294_Q141	A294_Q143	A294_Q145	A294_Q149	A294_Q158	A294_Q166	A294_Q174	A294_Q183	A294_Q184	A294_Q193	A294_Q200	A294_Q204	A294_Q207	A294_Q213	A294_Q216	A294_Q225	A294_Q230	A294_Q231	A294_Q233	A295_Q5	A296_Q5	A297_Q1	A297_Q3	A297_Q5

NULL is a value need to get rid of it at some point

Clean them one by one
Alcohol, Heroin, Cocaine/Crack, Marijuana/Hashish, Methamphetamines/Amphetamines, Benzodiazepines, Percocet


So I think what I need to do is create some type of if statement that says if this variable contains the word "X", then this new variable get this value and if not then...until you code everything that you want then you can factor the variable 
Try with alcohol


```{r, include=FALSE}
dim(AMAData)
head(AMAData)
##A292_Q1
head(AMAData$A292_Q1)
AMAData$A292_Q1 = factor(AMAData$A292_Q1)


Alcohol = str_detect(AMAData$A292_Q1, "Alcohol")
Heroin =  str_detect(AMAData$A292_Q1, "Heroin")
Cocaine_Crack = str_detect(AMAData$A292_Q1, "Cocaine/Crack")
Marijuana_Hashish = str_detect(AMAData$A292_Q1, "Marijuana/Hashish")
Methamphetamines_Amphetamines = str_detect(AMAData$A292_Q1, "Methamphetamines/Amphetamines")
Benzodiazepines = str_detect(AMAData$A292_Q1, "Benzodiazepines")
Percocet = str_detect(AMAData$A292_Q1, "Percocet")

### Add back variables
AMAData$Alcohol = Alcohol
AMAData$Heroin = Heroin
AMAData$Cocaine_Crack = Cocaine_Crack
AMAData$Marijuana_Hashish = Marijuana_Hashish
AMAData$Methamphetamines_Amphetamines = Methamphetamines_Amphetamines
AMAData$Benzodiazepines = Benzodiazepines
AMAData$Percocet = Percocet

##Look at the rest of the variables and see which ones are not dicot
##DischargeType
describe(AMAData)
dim(AMAData)

###Deal with any data errors before this point. 
###Get count and percentages for the descirptives 
describe.factor(AMAData$A298_Q5)
describe.factor(AMAData$A298_Q6)
describe.factor(AMAData$A298_Q7)
describe.factor(AMAData$A294_Q41)
describe.factor(AMAData$A294_Q57)
describe.factor(AMAData$A294_Q137)
describe.factor(AMAData$A297_Q5)
dim(AMAData)

#A298_Q5 = Sex_Orien: Hetero versus all else including not stated
### Non-hetero
describe.factor(AMAData$A298_Q5)
Sex_Orien = ifelse(AMAData$A298_Q5 == "Heterosexual", 0, 1)
describe.factor(Sex_Orien)
AMAData$Sex_Orien = Sex_Orien

#A298_Q6
#White or Caucasian versus all else including undisclosed
## Non-white versus white
describe.factor(AMAData$A298_Q6)
Race = ifelse(AMAData$A298_Q6 == "White or Caucasian", 0, 1)
AMAData$Race = Race

#A298_Q7
#Gender = Female 1 male and undisclosed 0
describe.factor(AMAData$A298_Q7)
Gender = ifelse(AMAData$A298_Q7 == "Female", 1, 0)
AMAData$Gender = Gender

#A294_Q41
#Parents or Partner and everything else 0
describe.factor(AMAData$A294_Q41)
Parents = ifelse(AMAData$A294_Q41 == "Parents", 1, 0)
Partner = ifelse(AMAData$A294_Q41 == "Partner", 1, 0)
AMAData$Parents= Parents
AMAData$Partner = Partner

#A294_Q57, use autocontent analysis to figure this out
#describe.factor(AMAData$A294_Q57)
#length(AMAData$A294_Q57)

#A294_Q137
#None = 1 versus everything else 
describe.factor(AMAData$A294_Q137)
No_trouble = ifelse(AMAData$A294_Q137 == "None", 1, 0)
AMAData$No_trouble = No_trouble
describe.factor(AMAData$No_trouble)

#A297_Q5
#If Polysubstance = 1 all else 0
describe.factor(AMAData$A297_Q5)
Polysubstance = str_detect(AMAData$A297_Q5, "Polysubstance")
describe.factor(Polysubstance)
AMAData$Polysubstance = Polysubstance

#DischargeType
#AMA Against Medical Advice = 1; completed program is 0
describe.factor(AMAData$DischargeType)
AMA = ifelse(AMAData$DischargeType == "AMA Against Medical Advice", "Class1", "Class2")
AMAData$AMA = AMA
describe.factor(AMA)

#Now get rid of old variables and extra variables
AMAData$A292_Q1 = NULL 
AMAData$A298_Q5 = NULL
AMAData$A298_Q6 = NULL
AMAData$A294_Q137 = NULL
AMAData$A297_Q5 = NULL
AMAData$DischargeType = NULL
### Keep this null for now
AMAData$A294_Q57 = NULL
AMAData$A294_Q41 = NULL
AMAData$A298_Q7 = NULL
describe.factor(AMAData$ClientEncounter_NUM)
head(AMAData$A294_Q57)
### Don't need ClientID can create training and test on client encounter
#ClientBiopsychosocial_NUM, 

#Need to recode the Yes No variables into 1 and zero's
#These variables come after the sequence
#A294_Q233, A297_Q1, A297_Q3

head(AMAData)
head(AMAData[c(13:55, 58:59)])
AMAData_bin_con = AMAData[c(13:55, 58:59)]
sum(is.na(AMAData_bin_con))


AMAData_bin_con = apply(AMAData_bin_con, 2, function(x){ifelse(x == "Yes", 1, 0)})
describe(AMAData_bin_con)
sum(is.na(AMAData_bin_con))

#Ok now get the corrected binary data back with the original data
head(AMAData)

### Data I want to keep from the first data set to combine with the second
AMAData_keep = AMAData[c(2, 5, 60:74)]
head(AMAData)

AMAData_analysis = data.frame(AMAData_keep, AMAData_bin_con)
head(AMAData_analysis)

#### Changeing trues to ones and false to zeros for later data analysis
AMAData_analysis_true =AMAData_analysis[,3:16]
head(AMAData_analysis_true)
AMAData_analysis_true = apply(AMAData_analysis_true,2, function(x){as.numeric(x)})
head(AMAData_analysis_true)

AMAData_analysis[,3:16] = AMAData_analysis_true
head(AMAData_analysis)
```

Now do missing data analysis
```{r}
AMAData_analysis_complete = na.omit(AMAData_analysis)
dim(AMAData_analysis_complete)

1-dim(AMAData_analysis_complete)[1]/dim(AMAData_analysis)[1]

AMAData_analysis_complete$AMA = factor(AMAData_analysis_complete$AMA)
```
Drop variables with with near zero variance
```{r}
lowVarAMA =  nearZeroVar(AMAData_analysis_complete)
lowVarAMA

describe(AMAData_analysis_complete[c(26, 31, 34, 41, 42, 46, 51, 52, 57)])

AMAData_analysis_complete[c(26, 31, 34, 41, 42, 46, 51, 52, 57)] = NULL
head(AMAData_analysis_complete)

nearZeroVar(AMAData_analysis_complete)
```
Need a correlation matrix to justify the logistic pca

Pearson's r is fine for two binary variables: https://www.dummies.com/business/customers/associations-between-binary-variables/
```{r}
library(psych)
head(AMAData_analysis_complete)
head(AMAData_analysis_complete)
cor(AMAData_analysis_complete[,-c(17)])

```
Now try Rare event

Sex_Orien = Non-herto
Race = Non-white
Gender = Female
Trouble = Trouble ==1

```{r}
AMAData_analysis_complete$AMA = ifelse(AMAData_analysis_complete$AMA == "Class1", 1, 0)
AMAData_analysis_complete$Trouble = ifelse(AMAData_analysis_complete$No_trouble == 1, 0,1)

re_model = zelig(AMA ~ AgeAt_ASSESS_Date + Sex_Orien + Race + Gender + Parents + Partner + No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model)

logit_model = glm(AMA ~ AgeAt_ASSESS_Date + Sex_Orien + Race + Gender + Parents + Partner + No_trouble, data = AMAData_analysis_complete, family = "binomial")

summary(logit_model)
```
Try drug variables
This demonstrates that even if you drop insignificant variables there is no change in the significant ones
```{r}
re_model_final = zelig(AMA ~ Sex_Orien + Race + Gender + Parents + Partner + Polysubstance + Alcohol + Heroin + Cocaine_Crack + Marijuana_Hashish + Methamphetamines_Amphetamines + Benzodiazepines + Percocet, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_final)


re_model_final = zelig(AMA ~ Sex_Orien + Race + Gender + Parents + Partner + Polysubstance + Alcohol + Heroin + Cocaine_Crack  + Benzodiazepines + Percocet, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_final)


re_model_final = zelig(AMA ~ Sex_Orien + Race + Gender + Parents + Partner  + Alcohol + Heroin + Cocaine_Crack  + Benzodiazepines + Percocet, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_final)


re_model_final = zelig(AMA ~ Sex_Orien + Race + Gender + Parents + Partner  + Alcohol + Heroin   + Benzodiazepines + Percocet, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_final)
```
Testing the effect of adding substances and it looks like it takes away age
```{r}
re_model_final = zelig(AMA ~ AgeAt_ASSESS_Date + Sex_Orien + Race + Gender + Parents + Partner + Trouble + Polysubstance + Alcohol + Heroin + Cocaine_Crack + Marijuana_Hashish + Methamphetamines_Amphetamines + Benzodiazepines + Percocet , model = "relogit", data = AMAData_analysis_complete)

summary(re_model_final)

re_model_final = zelig(AMA ~ AgeAt_ASSESS_Date + Sex_Orien + Race + Gender + Parents + Partner + Trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_final)

```
Final Model
```{r}
re_model_final = zelig(AMA ~ AgeAt_ASSESS_Date + Sex_Orien + Race + Gender + Parents + Partner + Trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_final)
```
Logit Final Model
Do some sensitivty analyses
```{r}
model_final = glm(AMA  ~ AgeAt_ASSESS_Date + Sex_Orien + Race + Gender + Parents + Partner + Trouble, family = "binomial", data = AMAData_analysis_complete)

summary(model_final)
```
Do Bayesian version and evaluate which of three effects is different each other
Do linear regression with subset data for interventions (intervention is the difference between parameter estimates)


################################################################
Interactions term testing
################################################################


Try interaction terms
Age versus sex_orien
```{r}
re_model_age_sexOrien = zelig(AMA ~ AgeAt_ASSESS_Date*Sex_Orien + Race + Gender + Parents + Partner + No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_age_sexOrien)
```
Try interaction terms
Age versus race
```{r}
re_model_age_race = zelig(AMA ~ AgeAt_ASSESS_Date*Race + Sex_Orien  + Gender + Parents + Partner + No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_age_race)
```
Try interaction terms
Age versus gender
```{r}
re_model_age_gender = zelig(AMA ~ AgeAt_ASSESS_Date*Gender + Race + Sex_Orien + Parents + Partner + No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_age_gender)
```
Try interaction terms
Age versus parents
```{r}
re_model_age_parents = zelig(AMA ~ AgeAt_ASSESS_Date*Parents + Gender + Race + Sex_Orien + Parents + Partner + No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_age_parents)
```
Try interaction terms
Age versus partner
```{r}
re_model_age_partner = zelig(AMA ~ AgeAt_ASSESS_Date*Partner + Parents + Gender + Race + Sex_Orien + No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_age_partner)
```
Try interaction terms
Age versus partner
```{r}
re_model_age_no_trouble = zelig(AMA ~ AgeAt_ASSESS_Date*No_trouble + Partner + Parents + Gender + Race + Sex_Orien + No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_age_no_trouble)
```
Try interaction terms
No_trouble versus partner
```{r}
re_model_no_trouble_partner = zelig(AMA ~ No_trouble*Partner + AgeAt_ASSESS_Date*No_trouble  + Parents + Gender + Race + Sex_Orien + No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_no_trouble_partner)
```
Try interaction terms
No_trouble versus partner
```{r}
re_model_no_trouble_parents = zelig(AMA ~ No_trouble*Parents + Partner + AgeAt_ASSESS_Date*No_trouble  + Parents + Gender + Race + Sex_Orien + No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_no_trouble_parents)
```
Try interaction terms
No_trouble versus partner
```{r}
re_model_no_trouble_gender = zelig(AMA ~ No_trouble*Gender + Partner + Parents + AgeAt_ASSESS_Date*No_trouble + Race + Sex_Orien + No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_no_trouble_gender)
```
Try interaction terms
No_trouble versus partner
```{r}
re_model_no_trouble_race = zelig(AMA ~ No_trouble*Race + Gender + Partner + Parents + AgeAt_ASSESS_Date*No_trouble + Sex_Orien + No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_no_trouble_race)
```
Try interaction terms
No_trouble versus partner
```{r}
re_model_no_trouble_Sex_Orien = zelig(AMA ~ No_trouble*Sex_Orien + Parents + Race + Gender + Partner + AgeAt_ASSESS_Date*No_trouble + No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_no_trouble_Sex_Orien)
```
Try interaction terms
Sex orien and race
```{r}
re_model_Sex_Orien_race = zelig(AMA ~ Sex_Orien*Race + Gender + Partner + Parents+ AgeAt_ASSESS_Date*No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_Sex_Orien_race)
```
Try interaction terms
Sex orien versus gender
```{r}
re_model_Sex_Orien_gender = zelig(AMA ~ Sex_Orien*Gender + Race + Gender + Partner + Parents+ AgeAt_ASSESS_Date*No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_Sex_Orien_gender)
```
Try interaction terms
Sex_orien and partner
```{r}
re_model_Sex_Orien_Partner = zelig(AMA ~ Sex_Orien*Partner + Race + Gender + Partner + Parents+ AgeAt_ASSESS_Date*No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_Sex_Orien_Partner)
```
Try interaction terms
Sex_orien and partner
```{r}
re_model_Sex_Orien_Parents = zelig(AMA ~ Sex_Orien*Parents + Race + Gender + Partner + Parents+ AgeAt_ASSESS_Date*No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_Sex_Orien_Parents)
```
Try interaction terms
Sex_orien and partner
```{r}
re_model_Sex_Orien_AgeAt_ASSESS_Date = zelig(AMA ~ Sex_Orien*AgeAt_ASSESS_Date + Race + Gender + Partner + Parents+ AgeAt_ASSESS_Date*No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_Sex_Orien_AgeAt_ASSESS_Date)
```
Try interaction terms
Sex_orien and no trouble
```{r}
re_model_Sex_Orien_No_trouble = zelig(AMA ~ Sex_Orien*No_trouble + Race + Gender + Partner + Parents+ AgeAt_ASSESS_Date*No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_Sex_Orien_No_trouble)
```
Try interaction terms
Gender and race
```{r}
re_model_Sex_Orien_Race = zelig(AMA ~ Gender*Race + Sex_Orien  + Race + Gender + Partner + Parents+ AgeAt_ASSESS_Date*No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_Sex_Orien_Race)
```
Try interaction terms
Gender and race
```{r}
re_model_Sex_Orien_Partner = zelig(AMA ~ Gender*Partner + Sex_Orien  + Race + Gender + Partner + Parents+ AgeAt_ASSESS_Date*No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_Sex_Orien_Partner)
```
Try interaction terms
Gender and race
```{r}
re_model_Sex_Orien_Parents = zelig(AMA ~ Gender*Parents + Sex_Orien  + Race + Gender + Partner + Parents+ AgeAt_ASSESS_Date*No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_Sex_Orien_Parents)
```
Try interaction terms
Gender and race
```{r}
re_model_Sex_Orien_AgeAt_ASSESS_Date = zelig(AMA ~ Gender*AgeAt_ASSESS_Date + Sex_Orien  + Race + Gender + Partner + Parents+ AgeAt_ASSESS_Date*No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_Sex_Orien_AgeAt_ASSESS_Date)
```
Try interaction terms
Gender and race
```{r}
re_model_Gender_No_trouble = zelig(AMA ~ Gender*No_trouble + Sex_Orien  + Race + Gender + Partner + Parents+ AgeAt_ASSESS_Date*No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_Gender_No_trouble)
```











