library(stringr)
library(prettyR)
library(Zelig)
library(BEST)
library(MCMCpack)
library(MissMech)

setwd("C:/Users/jessica.mitroi/Desktop/AMA")
AMAData = read.csv("AMAData.csv", header = TRUE, na.strings = c("NULL", "N/A"))

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
## Non-white versus white
describe.factor(AMAData$A298_Q6)
Race = ifelse(AMAData$A298_Q6 == "White or Caucasian", 0, 
                ifelse(AMAData$A298_Q6 == "Black or African American",1, 
                ifelse(AMAData$A298_Q6 == "Hispanic or Latino",2,3)))
AMAData$Race = Race
describe.factor(AMAData$Race)

#A298_Q6
#White or Caucasian versus all else including undisclosed
## Non-white versus white
#describe.factor(AMAData$A298_Q6)
#Race = ifelse(AMAData$A298_Q6 == "White or Caucasian", 0, 1)
#AMAData$Race = Race

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
AMAData$Edu = ifelse(AMAData$Edu < 1,1,0)
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

AMAData_analysis = AMAData[c("AgeAt_ASSESS_Date", "Sex_Orien", "Race", "Gender", "Trouble", "Edu", "Vet", "AMA")]

describe.factor(AMAData_analysis$Edu)

#missing data analysis

AMAData_analysis_complete = na.omit(AMAData_analysis)
dim(AMAData_analysis_complete)
1-(dim(AMAData_analysis_complete)[1]/dim(AMAData_analysis)[1])
TestMCARNormality(AMAData_analysis)

#drop variables with near zero variance
head(AMAData_analysis_complete)
lowVarAMA =  nearZeroVar(AMAData_analysis_complete)
lowVarAMA

#Demographics Update this with the missing data and then get a column for missingness by variable

factor_dat = apply(AMAData_analysis_complete[-c(1)], 2, function(x){factor(x)})
factor_dat = data.frame(factor_dat)
describe(factor_dat)

describe(AMAData_analysis_complete[c(1)])
sd(AMAData_analysis_complete$AgeAt_ASSESS_Date)

#Now try Rare event

Sex_Orien = Non-herto Race = Non-white Gender = Female Trouble = Trouble ==1

AMAData_analysis_complete$AMA = as.factor(AMAData_analysis_complete$AMA)
write.csv(AMAData_analysis_complete, "AMAData_analysis_complete.csv", row.names = FALSE)

AMAData_analysis_complete = read.csv("AMAData_analysis_complete.csv", header = TRUE)

re_model = zelig(AMA ~ AgeAt_ASSESS_Date + Sex_Orien + Race + Gender + Vet + Trouble + Edu, model = "relogit", data = AMAData_analysis_complete)
summary(re_model)
describe.factor(AMAData_analysis_complete$AMA)

summary(re_model)
AMAData_analysis_complete$Vet = as.factor(AMAData_analysis_complete$Vet)
logit_model = glm(AMA ~  AgeAt_ASSESS_Date*Trouble + Sex_Orien + Race + Gender + Vet + Trouble + Edu, data = AMAData_analysis_complete, family = "binomial")


summary(logit_model)

#Interactions in Freq, because packages are available

library(interactions)

interact_plot(logit_model, pred = "AgeAt_ASSESS_Date", modx =  "Trouble", data = AMAData_analysis_complete)

sim_slopes(logit_model, pred = "Trouble", modx = "AgeAt_ASSESS_Date", data = AMAData_analysis_complete, jnplot = TRUE)

johnson_neyman(logit_model, pred = "Trouble", modx = "AgeAt_ASSESS_Date", control.fdr = TRUE)

#Do Bayesian version and evaluate which of three effects is different each other Do linear regression with subset data for interventions (intervention is the difference between parameter estimates)

model_bayes = MCMClogit(AMA  ~AgeAt_ASSESS_Date*Trouble + Sex_Orien + factor(Race) + Gender+ Vet  + Edu, data = AMAData_analysis_complete)
model_bayes
summary(model_bayes)

#Get quantiles to get the odds ratios to evaluate practically significant differences

sum_model_bayes = summary(model_bayes)
quant_exp= exp(sum_model_bayes$quantiles)
quant_exp

#Now grab the distributions of the Sex_Orien and gender

sex_orien_gender_posts = model_bayes[,c(3,5)]
sex_orien_gender_posts = data.frame(sex_orien_gender_posts)
head(sex_orien_gender_posts)


sex_orien_gender_posts = sex_orien_gender_posts[sample(nrow(sex_orien_gender_posts),500),]
sex_orien_gender_posts$Sex_Orien = exp(sex_orien_gender_posts$Sex_Orien)
sex_orien_gender_posts$Gender = exp(sex_orien_gender_posts$Gender)

describe.factor(sex_orien_gender_posts$Gender)

test_Best =  BESTmcmc(sex_orien_gender_posts$Sex_Orien, sex_orien_gender_posts$Gender)
test_Best
plot(test_Best)

summary(test_Best)


#Interaction

re_model_age_sexOrien = zelig(AMA ~ AgeAt_ASSESS_Date*Sex_Orien + Race + Gender + Parents + Partner + No_trouble, model = "relogit", data = AMAData_analysis_complete)

summary(re_model_age_sexOrien)



