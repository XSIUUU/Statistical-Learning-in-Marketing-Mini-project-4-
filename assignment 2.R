rm(list=ls())

# import table and value assign----
group9 <- SLIM2425a_team_9_glm
library(GPArotation)
install.packages("dplyr")
library(dplyr)

# life style scores extraction and combine to origin table----
ls <- group9[,11:23]
ls.sd <- data.frame(scale(ls))
summary(ls.sd)

(ls.sd.ob <- factanal(ls.sd, factors=3, rotation="oblimin",scores = "Bartlett"))   
ls.scores <- data.frame(ls.sd.ob$scores)
names(ls.scores) <- c("open_minded","cooking_passion","social_conformity")
group9 <- bind_cols(group9, ls.scores)

# perception scores extraction and combine to origin table----
pc <- group9[,24:34]
pc.sd <- data.frame(scale(pc))
summary(pc.sd)

pc.sd.ob <- factanal(pc.sd, factors=3, rotation="oblimin",scores="Bartlett")   #p-value of chi square is too small, recommend more factors
pc.scores <- data.frame(pc.sd.ob$scores)
names(pc.scores) <- c("bio_knowledge","bio_familiarity","bio_inaccessibility")
group9 <- bind_cols(group9, pc.scores)

summary(group9[,39:44])
var(group9[,39])
var(group9[,40])
var(group9[,41])
var(group9[,42])
var(group9[,43])
var(group9[,44])


# MODEL 1: DV:PI1. IV:6 factors----
m1 <- lm(PI1 ~ open_minded + cooking_passion + social_conformity + bio_knowledge + bio_familiarity + bio_inaccessibility, data=group9)
summary(m1) # only social_conformity has positive significant influence on PI1, 1 unit growing in social conformity factor increase 0.37 unit of Purchase Intention. 

#Testing for multicollinearity
library(car)
#create vector of VIF values and a vector of tolerance values
vif_values <- vif(m1)
tolerance <- 1/vif_values
vif_values
tolerance


# MODEL 2: DV:WTR. IV:6 factors----
m2 <- lm(WTR ~ open_minded + cooking_passion + social_conformity + bio_knowledge + bio_familiarity + bio_inaccessibility, data=group9)
summary(m2) # social_conformity(positive) & bio_inaccessibility(negative) have strongly significant influence on WTR, open minded and familiarity have moderately Significant, inaccessibility have the largest influence, 1 unit growth decrease 0.24 unit of WTR.
vif_values <- vif(m2)
vif_values

# Mean-centering moderator----
mc_moneyspend <- scale(group9[,8],scale= FALSE)
colnames(mc_moneyspend) <- "mc_moneyspend"
group9 <- cbind(group9, mc_moneyspend)

# MODEL 3: DV:PI1. IV: social_conformity. Moderator: MoneySpent.----
m3 <- lm(PI1 ~ social_conformity + MoneySpent + social_conformity*MoneySpent, data=group9)
summary(m3) # Moderate effect not exist

# M3.2 with mean-centering----
m3.2 <- lm(PI1 ~ social_conformity + mc_moneyspend + social_conformity*mc_moneyspend, data=group9)
summary(m3.2)

# MODEL 4: DV:WTR. IV: bio_inaccessibility. Moderator: MoneySpent.----
m4 <- lm(WTR ~ bio_inaccessibility + MoneySpent + bio_inaccessibility*MoneySpent, data=group9)
summary(m4) # Moderate effect not exist

# M4.2 with mean-centering----
m4.2 <- lm(WTR ~ bio_inaccessibility + mc_moneyspend + bio_inaccessibility*mc_moneyspend, data=group9)
summary(m4.2) # Moderate effect not exist
