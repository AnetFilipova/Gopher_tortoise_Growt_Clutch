setwd("/Users/anetfilipova/Library/CloudStorage/Box-Box/TS_Lab_GopherTortoises/Data_Analysis")

datum1 <- read.csv("Tortoise_mass.csv")
head(datum1)

install.packages("nlme")
library(nlme)


## #Describe/Define your variable
##Clutch_size means all 81 eggs including the ones that were found rotten in the field, as well as those that were brought to the lab but didn't hatch##
##Growth rate is taking into account the mass at hatching and the mass up until 14 weeks of measurement##

#plotting the differences in tortoise mass as a response of clutch size##
plot(Hatching_Mass~as.factor(Clutch_Size), data=datum1)

#plotting the differences in tortoise mass as a response of clutch size##
plot(Hatching_Mass~as.factor(Clutch_ID), data=datum1)


###Depth is defined at depth to the top (1st) egg found in the nest; it ranges from 5 to 22cm ##
##testing the effects of clutch size and nest depth on the individuals' growth rates##
#### Is there a Relationship between clutch size and tortoise hatching mass?
## We use Clutch_ID as a random effect to take into account that clutch effect since there multiple hatchlings from each clutch?
##Fit a linear mixed-effects model
#No interactions

lme_results_Mass.Clutch <- lme(Hatching_Mass ~ Clutch_Size + Depth, data = datum1, random = ~1|Clutch_ID)
summary(lme_results_Mass.Clutch)
intervals(lme_results_Mass.Clutch)
plot (results_Mass.Clutch)

lme_results_SCL.Clutch <- lme(SCL ~ Clutch_Size+Depth, data = datum1, random = ~1|Clutch_ID)
summary(lme_results_SCL.Clutch)
plot(results_SCL.clutch)

lme_results_Growth.Clutch <- lme(Growth_rate_measurement_1_10 ~ Clutch_Size + Depth, data = datum1, random = ~1|Clutch_ID)
summary(lme_results_Growth.Clutch)
intervals(lme_results_Growth.Clutch)
plot(lme_results_Growth.Clutch)

###running an F-drop to see whether the more complicated model including the random effect (Clutch_ID) is a better fit to the data##
lme_results_Mass.Clutch <- lme(Hatching_Mass ~ Clutch_Size + Depth, data = datum1, random = ~1|Clutch_ID)
results_Mass.Clutch_1 <-lm(Hatching_Mass ~ Clutch_Size + Depth, data = datum1)
anova(results_Mass.Clutch,results_Mass.Clutch_1)

lme_results_SCL.Clutch <- lme(SCL ~ Clutch_Size+Depth, data = datum1, random = ~1|Clutch_ID)
lme_results_SCL.Clutch_1 <-lm(SCL ~ Clutch_Size+Depth, data = datum1)
anova(lme_results_SCL.Clutch,lme_results_SCL.Clutch_1)

lme_results_Growth.Clutch <- lme(Growth_rate_measurement_1_10 ~ Clutch_Size + Depth, data = datum1, random = ~1|Clutch_ID)
lme_results_Growth.Clutch_1 <-lm(Growth_rate_measurement_1_10 ~ Clutch_Size + Depth, data = datum1)
anova(lme_results_Growth.Clutch,lme_results_Growth.Clutch_1)

##testing for the effect of egg position (top, middle, bottom) in the nest on the hatchling success from all 81 eggs##
datum2=read.csv('Egg_position.csv')
head(datum2)

datum2$Position <- as.factor(datum2$Position)

results_hatch.success=glm(Survived ~ Position, data = datum2, family = binomial)
summary(results_hatch.success)
exp(confint(results_hatch.success))

##running a likelihood ratio test to compare whether the F-drop test and the likelihood ratio test produce the same p-value to check for the effect of the random effect##
install.packages("lme4")
install.packages("lmerTest")
library(lme4)
library(lmerTest)

#lme_results_Mass.Clutch <- lmer(Hatching_Mass ~ Clutch_Size + Depth,random = + (1|Clutch_ID), data = datum1)
lme_results_Mass.Clutch <- lmer(Hatching_Mass ~ Clutch_Size + (1 | Clutch_ID), data = datum1)
likelihood_ratio_test <- anova(lme_results_Mass.Clutch, refit = FALSE)

print(likelihood_ratio_test) 
