library(tidyverse)
library(colorRamps)
library(car)
library(effects)
library(olsrr)

#PART A
#initial model
lp.initialmodel <- lm(acrePrice~region+improvements+acres+tillable+financing+crpPct+productivity,data=LandPrices)
summary(lp.initialmodel)

#PART B
#check assumptions
residualPlots(lp.initialmodel,type="rstudent",pch=16,quadratic=FALSE,id=FALSE)
qqPlot(lp.initialmodel,pch=16,envelope=FALSE,id=FALSE)
# Shapiro-Wilk test for non-normality
shapiro.test(rstudent(lp.initialmodel))
#multicollinearity
ols_vif_tol(lp.initialmodel)
#influential observations
#outliers
influenceIndexPlot(lp.initialmodel,vars="Studentized",id=FALSE)
#leverage
influenceIndexPlot(lp.initialmodel,vars="hat",id=FALSE)
#influence
influence.cutoff <- qf(.5,8,805)
influence.cutoff
influenceIndexPlot(lp.initialmodel,vars="Cook",id=FALSE)

#PART C
#starting transformation (y needs to be transformed)
(power1 <- powerTransform(lp.initialmodel,family="bcPower")) #0.3629592
#suggests sqrt transformation of acrePrice 
lp.model1 <- lm(basicPower(acrePrice,0.5)~improvements+acres+tillable+crpPct+productivity+region+financing,data=LandPrices)
residualPlots(lp.model1,type="rstudent",quadratic=FALSE,tests=FALSE,id=FALSE,pch=16)
qqPlot(lp.model1,pch=16,envelope=FALSE,id=FALSE)
shapiro.test(rstudent(lp.model1))
# tranforming x as well - acres - logarithm
lp.model2 <- lm(basicPower(acrePrice,0.5)~improvements+basicPower(acres,0)+tillable+crpPct+productivity+region+financing,data=LandPrices)
residualPlots(lp.model2,type="rstudent",quadratic=FALSE,tests=FALSE,id=FALSE,pch=16)
qqPlot(lp.model2,pch=16,envelope=FALSE,id=FALSE)
shapiro.test(rstudent(lp.model2))

# PART D
#first try at variable reduction
#model2 from above but with all interactions possible
lp.model3 <- lm(sqrt(acrePrice)~(improvements+log(acres)+tillable+crpPct+productivity+region+financing)^2,data=LandPrices)
summary(lp.model3)
#model with only significant interactions based on p-values from model3 output
lp.model4 <- lm(sqrt(acrePrice)~(improvements+log(acres)+tillable+crpPct+productivity+region+financing+improvements*region+log(acres)*region+tillable*region+productivity*region+log(acres)*tillable), data=LandPrices)
summary(lp.model4)
#model with variable reduction from model4
lp.model5 <- lm(sqrt(acrePrice)~improvements+log2(acres)+tillable+productivity+region+improvements*region+log2(acres)*region+tillable*region+productivity*region+log2(acres)*tillable, data=LandPrices)
summary(lp.model5)
ols_vif_tol(lp.model5)
#we see very high multicollinearity, so we need to figure out how to reduce this
#checking significant interactions to compare and choose simpler model
#we use a stepwise function to try another way to find only significant interactions
step1 <- step(lp.model3,scope=~.^2,direction="both",trace=1,k=log(n))
summary(step1)
ols_vif_tol(step1)
#very high multicollinearity! something is going wrong here
#now we will try centering all of our quantitative variables to reduce multicollinearity
#center quantitative variables to reduce multicollinearity
LandPrices$cTill <- scale(LandPrices$tillable,scale=FALSE)
LandPrices$cPct <- scale(LandPrices$crpPct,scale=FALSE)
LandPrices$cProd <- scale(LandPrices$productivity,scale=FALSE)
LandPrices$cImp <- scale(LandPrices$improvements,scale=FALSE)
LandPrices$logAcre <- log(LandPrices$acres)
LandPrices$cAcre <- scale(LandPrices$logAcre,scale=FALSE)
#if we center acre first, then we will have negative values
#we cannot do a log transformation on negative values
#so we must do the log transformation first and then center the variable
#model with centered variables
lp.model6 <- lm(sqrt(acrePrice)~cImp+cAcre+cTill+cPct+cProd+region+financing,data=LandPrices)
summary(lp.model6)
#we see from the output above that cAcre is the only term that is not significant
#so we will eliminate it from our model and continue
#we will now run a model with all possible interactions
lp.model7 <- lm(sqrt(acrePrice)~(cImp+cTill+cPct+cProd+region+financing)^2,data=LandPrices)
summary(lp.model7)
#from this output, will will eliminate insignificant predictor variables
#we see that cProd and all of its interactions are insignificant
#we also see that financing and all of its interactions are insignificant
#so we will eliminate both of these variables from our model
lp.model8 <- lm(sqrt(acrePrice)~(cImp+cTill+cPct+region+region*cImp+region*cTill+region*cPct),data=LandPrices)
summary(lp.model8)
#now we see that all terms and their interactions are significant
#so this is the model that we will want to stick with
#we do not want to eliminate any more variables or interactions
#define this to be our final model
lp.finalmodel <- lp.model8

# PART E
#check assumptions
residualPlots(lp.finalmodel,type="rstudent",pch=16,quadratic=FALSE,id=FALSE)
qqPlot(lp.finalmodel,pch=16,envelope=FALSE,id=FALSE)
# Shapiro-Wilk test for non-normality
shapiro.test(rstudent(lp.finalmodel))
#multicollinearity
ols_vif_tol(lp.finalmodel)
#influential observations
#outliers
influenceIndexPlot(lp.finalmodel,vars="Studentized",id=FALSE)
#leverage
influenceIndexPlot(lp.finalmodel,vars="hat",id=FALSE)
#influence
influence.cutoff <- qf(.5,8,805)
influence.cutoff
influenceIndexPlot(lp.finalmodel,vars="Cook",id=FALSE)
#verifying that our final model is the best possible
all.sub8 <- ols_step_all_possible(lp.model8)
best.sub8 <- ols_step_best_subset(lp.model8)
view(all.sub8)
view(best.sub8)
#comparing reduced and full model with partial F-test
anova(lp.finalmodel, lp.initialmodel)
