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
library(MCMCpack)
library(MissMech)
```
Loading data
```{r}
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/AMAData")
AMAData = read.csv("AMAData.csv", header = TRUE, na.strings = c("NULL", "N/A"))
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



#A298_Q5 = Sex_Orien: Hetero versus all else including not stated
### Non-hetero
describe.factor(AMAData$A298_Q5)
Sex_Orien = ifelse(AMAData$A298_Q5 == "Heterosexual", 0, 1)
describe.factor(Sex_Orien)
AMAData$Sex_Orien = Sex_Orien

#A298_Q6
#White or Caucasian versus all else including undisclosed
describe.factor(AMAData$A298_Q6)
White = ifelse(AMAData$A298_Q6 == "White or Caucasian", 1, 0)
AMAData$White = White

describe.factor(AMAData$A298_Q6)
Black = ifelse(AMAData$A298_Q6 == "Black or African American", 1, 0)
AMAData$Black = Black

#Hispanic or Latino
describe.factor(AMAData$A298_Q6)
Hispanic = ifelse(AMAData$A298_Q6 == "Hispanic or Latino", 1, 0)
AMAData$Hispanic = Hispanic

## Another racial identity
#White or Caucasian
describe.factor(AMAData$A298_Q6)
Another = ifelse(AMAData$A298_Q6 != "Hispanic or Latino" & AMAData$A298_Q6 != "Black or African American" & AMAData$A298_Q6 != "White or Caucasian", 1, 0)
describe.factor(Another)
AMAData$Another = Another
33.000000+26.0000000+21.0000000+16.0000000+8.0000000+2.00000000+1.00000000+261.000000

#A298_Q7
#Gender = Female 1 male and undisclosed 0
describe.factor(AMAData$A298_Q7)
Gender = ifelse(AMAData$A298_Q7 == "Female", 1, 0)
AMAData$Gender = Gender


#A294_Q57, use autocontent analysis to figure this out
#describe.factor(AMAData$A294_Q57)
#length(AMAData$A294_Q57)

#A294_Q137
#None = 1 versus everything else 
describe.factor(AMAData$A294_Q137)
Trouble = ifelse(AMAData$A294_Q137 == "None", 0, 1)
AMAData$Trouble = Trouble
describe.factor(AMAData$Trouble)

#DischargeType
#AMA Against Medical Advice = 1; completed program is 0
describe.factor(AMAData$DischargeType)
AMA = ifelse(AMAData$DischargeType == "AMA Against Medical Advice", 1, 0)
AMAData$AMA = AMA
describe.factor(AMA)

describe.factor(AMAData$Edu)
### Those with less than and a high school versus everyone else so 1 and 0 versus everyone else
AMAData$Edu = ifelse(AMAData$Edu <= 1,1,0)
describe.factor(AMAData$Edu)

### Now get veteran status
describe.factor(AMAData$A294_Q108)

Vet = data.frame(Vet =AMAData$A294_Q108)
AMAData$Vet = apply(Vet, 2, function(x){ifelse(x == "Yes", 1, 0)})
describe.factor(AMAData$Vet)
### Just grab the variables that you want

head(AMAData)
describe(AMAData$AgeAt_ASSESS_Date)
range(AMAData$AgeAt_ASSESS_Date, na.rm = TRUE)
#### Some values below 18 need to get rid of them
AMAData$AgeAt_ASSESS_Date = ifelse(AMAData$AgeAt_ASSESS_Date < 18, NA, AMAData$AgeAt_ASSESS_Date)
range(AMAData$AgeAt_ASSESS_Date, na.rm = TRUE)

AMAData_analysis = AMAData[c("AgeAt_ASSESS_Date", "Sex_Orien", "White", "Black", "Hispanic","Another", "Gender", "Trouble", "Edu", "Vet", "AMA")]

describe.factor(AMAData_analysis$Edu)

```

Now do missing data analysis
```{r}
AMAData_analysis_complete = na.omit(AMAData_analysis)
dim(AMAData_analysis_complete)
1-(dim(AMAData_analysis_complete)[1]/dim(AMAData_analysis)[1])
TestMCARNormality(AMAData_analysis)
```
Demographics
Update this with the missing data and then get a column for missingness by variable
```{r}
factor_dat = apply(AMAData_analysis_complete[-c(1)], 2, function(x){factor(x)})
factor_dat = data.frame(factor_dat)
describe(factor_dat)
describe.factor(factor_dat$Edu)
describe(AMAData_analysis_complete[c(1)])
sd(AMAData_analysis_complete$AgeAt_ASSESS_Date)
```
Now try Rare event

Sex_Orien = Non-herto
Race = Non-white
Gender = Female
Trouble = Trouble ==1

```{r}
AMAData_analysis_complete$AMA = as.factor(AMAData_analysis_complete$AMA)
write.csv(AMAData_analysis_complete, "AMAData_analysis_complete.csv", row.names = FALSE)

AMAData_analysis_complete = read.csv("AMAData_analysis_complete.csv", header = TRUE)

AMAData_analysis_complete$Vet = as.factor(AMAData_analysis_complete$Vet)
logit_model = glm(AMA ~  AgeAt_ASSESS_Date*Trouble + Sex_Orien  + Black + Hispanic+ Another + Gender + Vet + Trouble + Edu, data = AMAData_analysis_complete, family = "binomial")
summary(logit_model)
library(car)
vif(logit_model)
```
Interactions in Freq, because packages are available
```{r}
library(interactions)

johnson_neyman(logit_model, pred = "Trouble", modx = "AgeAt_ASSESS_Date", control.fdr = TRUE)
```


Do Bayesian version and evaluate which of three effects is different each other
Do linear regression with subset data for interventions (intervention is the difference between parameter estimates)
```{r}

model_bayes = MCMClogit(AMA  ~ Sex_Orien  + Black + Hispanic + Another  + Gender + Edu+ AgeAt_ASSESS_Date +Vet + Trouble+ AgeAt_ASSESS_Date*Trouble, data = AMAData_analysis_complete)

summary(model_bayes)
sum_bayes = summary(model_bayes)
sum_bayes
sum_bayes_exp =  round(data.frame(par_est = sum_bayes$statistics[,1], se = sum_bayes$statistics[,2],  odds = exp(sum_bayes$statistics[,1]), ci = exp(sum_bayes$quantiles[,c(1,5)])),3)
sum_bayes_exp = sum_bayes_exp[-1,]
sum_bayes_exp
write.csv(sum_bayes_exp, "sum_bayes_exp.csv", row.names = FALSE)
```




