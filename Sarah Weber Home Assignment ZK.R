#Home Assignment Zoltan Kekecs

library(dplyr)
library(plyr)
library(psych)
library(tidyverse)
library(r2glmm) # for r2beta\t
library(lme4) # for lmer
library(lmerTest) # to get singificance test in lmer
library(MuMIn) # for r.squaredGLMM
library(lm.beta)
library(lsr)
library(car)
library(gridExtra)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(lmtest)
library(optimx)
library(r2glmm)
library(cAIC4)

#Assignment Part 1
data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1") 

data_sample_1 %>% 
  summary

data_sample_1[data_sample_1$STAI_trait < 20, ]

data_sample_1[data_sample_1$pain > 10, ]

ds1 <- data_sample_1 %>% 
  filter(ID != "ID_34" & ID != "ID_88")

View(ds1)

as.factor(ds1$sex)

#Comment on not excluding patient with very low IQ and low weight because not able to follow test instructions???

#Building regression models
model1 <- lm(pain ~ age + sex, data = ds1)
model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = ds1)

#Looking at distributions
ds1 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = age, y = pain, label = rownum) + geom_point() + geom_text()

ds1 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = STAI_trait, y = pain, label = rownum) + geom_point() + geom_text()

ds1 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = pain_cat, y = pain, label = rownum) + geom_point() + geom_text()

ds1 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = cortisol_serum, y = pain, label = rownum) + geom_point() + geom_text()

ds1 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = cortisol_saliva, y = pain, label = rownum) + geom_point() + geom_text()

ds1 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = mindfulness, y = pain, label = rownum) + geom_point() + geom_text()

#Cooks distance
model2 %>% 
  plot(which = 4)

ds1 %>%
  slice(c(46, 73, 85))

#Nothing unusual with those participants, look at normality first before excluding

#Normality

model2 %>% 
  plot(which = 2)

residuals_model2 <- enframe(residuals(model2))

residuals_model2 %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model2))

ds1 %>% 
  slice(c(73, 84, 102))

#There are a few cases showing up on the qq plot and with high leverage, but
#its not the same cases and skew and kurtosis are good, also nothing unusual so no exclusions


#Linearity

model2 %>% 
  residualPlots()

#Homoscedasticity

model2 %>% 
  plot(which = 3)

model2 %>%
  ncvTest()

model2 %>% 
  bptest()

#Multicollinearity

model2 %>%
  vif()

ds1 %>%
  select(pain, age, sex, pain_cat, STAI_trait, cortisol_saliva, cortisol_serum, mindfulness) %>%
  pairs.panels(col = "red", lm = T)

#build model without cortisol_saliva
model2_corr <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum - cortisol_saliva + mindfulness, data = ds1)

model2_corr %>% 
  vif()

#Model comparison
#smaller AIC means less error and better model fit, so in this case we accept the model with the
#smaller AIC. However, if the difference in AIC does not reach 2, we can retain either of the two models.
#In this case, theoretical considerations and previous results should should be considered when doing model
#selection. If both models seem plausible theoretically, we can retain the model containing less predictors.
summary(model1)
summary(model2_corr)

summary(model1)$adj.r.squared
summary(model2_corr)$adj.r.squared
AIC(model1)
AIC(model2_corr)
anova(model1, model2_corr)

#table
tab_model(model1, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)
tab_model(model2_corr, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

##########################################################################################################

#Assignment Part 2

ds1 %>% 
  summary()

#define model with all predictors
all_pred <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = ds1)

#Model diagnostics
all_pred %>% 
  plot(which = 4)

ds1 %>% 
  slice(c(46, 84, 85)) #nothing unusual, check for normality

all_pred %>% 
  plot(which = 2)

residuals_all_pred <- enframe(residuals(all_pred))

residuals_all_pred %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(all_pred))

ds1 %>% 
  slice(c(84, 85, 102))

#There are a few outliers with high leverage that also show up in the QQ Plot.
#However, there is nothing unusual with the data and skew and kurtosis are good.
#Therefore I'm not excluding them

#Linearity
all_pred %>% 
  residualPlots()
#Homoscedasticity
all_pred %>% 
  plot(which = 3)

all_pred %>%
  ncvTest()

all_pred %>% 
  bptest()

#Multicollinearity
all_pred %>%
  vif()

#perform backward stepwise regression
backward <- step(all_pred, direction = "backward")


back_mod <- lm(pain ~ mindfulness + age + cortisol_serum + pain_cat, data = ds1)
theory_mod <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum - cortisol_saliva + mindfulness, data = ds1)

summary(back_mod)
AIC(back_mod)

#compare to initial backward model
AIC(all_pred)
anova(all_pred, back_mod)

#compare to theory model
AIC(theory_mod)
anova(theory_mod, back_mod)

#table
tab_model(back_mod, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

#Next part

data_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_2.csv")

ds2 <- data_sample_2

ds2 %>% 
  summary()

as.factor(ds2$sex)

#Looking at distributions
ds2 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = age, y = pain, label = rownum) + geom_point() + geom_text()

ds2 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = STAI_trait, y = pain, label = rownum) + geom_point() + geom_text()

ds2 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = pain_cat, y = pain, label = rownum) + geom_point() + geom_text()

ds2 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = cortisol_serum, y = pain, label = rownum) + geom_point() + geom_text()

ds2 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = mindfulness, y = pain, label = rownum) + geom_point() + geom_text()

#calculate predicted values
pred_test <- predict(theory_mod, ds2)
pred_test_back <- predict(back_mod, ds2)

#now we calculate the sum of squared residuals
RSS_test = sum((ds2[, "pain"] - pred_test)^2)

RSS_test_back = sum((ds2[, "pain"] - pred_test_back)^2)
RSS_test
RSS_test_back


#Report the prediction performance of the backward model and the theory-based model 
#on the new data (data file 2). This can be done with several measures, for example calculate 
#the sum of squared differences between the predicted and the actual pain values (or the sum 
#of absolute differences) for each model. 

#backwards model has more error

##############################################################################################################################

#Assignment Part 3

data_sample_3 = read.csv("https://tinyurl.com/b385chpu")
data_sample_4 = read.csv("https://tinyurl.com/4f8thztv")

# Checking data sample 3
summary(data_sample_3)

data_sample_3[data_sample_3$household_income <= 0, ]

ds3 <- data_sample_3 %>% 
  mutate(household_income = na_if(household_income, -7884))

summary(ds3$household_income)

ds3 = ds3 %>%
  mutate(hospital = factor(hospital)) %>% 
  mutate(sex = factor(sex))

summary(ds3$sex)

ds3[ds3$sex == "woman", ]

ds3 = ds3 %>% 
  mutate(sex = replace(sex, sex == "woman", "female"))

summary(ds3$sex)
summary(ds3$hospital)

#Looking at distributions
ds3 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = age, y = pain, label = rownum) + geom_point() + geom_text()

ds3 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = STAI_trait, y = pain, label = rownum) + geom_point() + geom_text()

ds3 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = pain_cat, y = pain, label = rownum) + geom_point() + geom_text()

ds3 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = cortisol_serum, y = pain, label = rownum) + geom_point() + geom_text()

ds3 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = mindfulness, y = pain, label = rownum) + geom_point() + geom_text()

ds3 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = IQ, y = pain, label = rownum) + geom_point() + geom_text()

ds3 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = weight, y = pain, label = rownum) + geom_point() + geom_text()

ds3 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = household_income, y = pain, label = rownum) + geom_point() + geom_text()

#Checking data sample 4
summary(data_sample_4)

ds4 <- data_sample_4 %>% 
  mutate(hospital = factor(hospital)) %>% 
  mutate(sex = factor(sex))

summary(ds4$sex)
summary(ds4$hospital)

#Looking at distributions
ds4 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = age, y = pain, label = rownum) + geom_point() + geom_text()

ds4 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = STAI_trait, y = pain, label = rownum) + geom_point() + geom_text()

ds4 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = pain_cat, y = pain, label = rownum) + geom_point() + geom_text()

ds4 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = cortisol_serum, y = pain, label = rownum) + geom_point() + geom_text()

ds4 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = mindfulness, y = pain, label = rownum) + geom_point() + geom_text()

ds4 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = IQ, y = pain, label = rownum) + geom_point() + geom_text()

ds4 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = weight, y = pain, label = rownum) + geom_point() + geom_text()

ds4 %>% 
  mutate(rownum = row.names(ds1)) %>% 
  ggplot() + aes(x = household_income, y = pain, label = rownum) + geom_point() + geom_text()


#First, build a linear mixed model on data file 3, accounting for the clustering of the data at different hospital sites. 
#Random intercept model.
pain_rnd_int = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1 | hospital), data = ds3)
summary(pain_rnd_int)

confint(pain_rnd_int)

#creating tables
tab_model(pain_rnd_int, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)
tab_model(theory_mod, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

#Also, compute the variance explained by the fixed effect predictors using marginal R2, 
#and the variance explained by the fixed and random effect terms combined using conditional 
#R2.
r2beta(pain_rnd_int, method = "nsj", data = ds3)

r.squaredGLMM(pain_rnd_int)


#Now use the regression equation obtained on data file 3 to predict pain in data file 4.
#Compute the variance explained by the model on data file 4.
pred_pain4 <- predict(pain_rnd_int, ds4, allow.new.levels = TRUE)
RSS = sum((ds4[, "pain"] - pred_pain4)^2)
model_mean <- lm(pain ~ 1, data = ds4)
TSS = sum((ds4$pain - predict(model_mean))^2)
R <- 1-(RSS/TSS)
R

#Compare this R2 to the marginal and conditional R2 values computed for the model on data file 3.  
#         R2m       R2c
#[1,] 0.3852492 0.4632079
#         R
# [1] 0.3797384

#Build a new linear mixed effects model on dataset 3 predicting pain with most influential predictor. Allow for random intercept and slope.
cort_rnd_slope = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = ds3)
cort_rnd_slope

cort_rnd_int = lmer(pain ~ cortisol_serum + (1|hospital), data = ds3)
cort_rnd_int

#Now visualize the fitted regression lines for each hospital separately.
ds3 = ds3 %>% 		
  mutate(pred_int = predict(cort_rnd_int),
         pred_slope = predict(cort_rnd_slope)) 

ds3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 2) + 
  geom_line(color='red', aes(y=pred_slope, x=cortisol_serum))+
  facet_wrap(~hospital, ncol = 2)

ds3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 2) + 
  geom_line(color='red', aes(y=pred_int, x=cortisol_serum))+
  facet_wrap(~hospital, ncol = 2)


cAIC(pain_rnd_int)$caic
cAIC(cort_rnd_slope)$caic
anova(cort_rnd_slope, pain_rnd_int)
r2beta(cort_rnd_slope, method = "nsj", data = ds3)
r2beta(pain_rnd_int, method = "nsj", data = ds3)
