# used for the preparation of the pilot study paper



# Setup -------------------------------------------------------------------

library(data.table)

user <- Sys.info()[[7]]

if(user == "ben"){
  dataF <- path.expand("~/temp/serlRecruit18000.csv")
  all_onboards <- data.table::fread(dataF)
}


# Tables ----
tr <- table(all_onboards$region, all_onboards$consent)
trp <- prop.table(tr, 1)*100
totalByQuin <- table(all_onboards$quintile)

t <- table(all_onboards$region, all_onboards$quintile)
t # table of case frequency by IMD & region
round(prop.table(t,1)*100,2)
# interesting dominance of IMD Q5 in South East

# table of row % with last row as frequency by IMD quin
# need to check if these row % distributions match the IMD * Region distribution of LSOAs?
# If not then have we got over/under sampling going on by IMD?
# requires gmodels
gmodels::CrossTable(all_onboards$region, all_onboards$quintile, 
           prop.chisq = FALSE,
           prop.r = TRUE, # row proportions
           prop.t = FALSE, # no table props
           prop.c = FALSE, # col proportions
           chisq = TRUE,
           dnn = c("Outcome","IMD quintile") )#how do we turn off observed frequencies?
           
           
# Simple consent model (full data as loaded) ----
table(all_onboards$consent, useNA = "always") # check for NA
prop.table(table(all_onboards$consent, useNA = "always")) # check for NA
nonResponseModel <- glm(formula = consent ~ as.factor(quintile) + 
                          region, 
                       family = binomial(logit), all_onboards) # wiil auto-drop the missing values

summary(nonResponseModel)
# CIs for coef not OR for plot
coefCI <- cbind(coef = coef(nonResponseModel), 
                confint(nonResponseModel))
library(broom) # tidy up regression outputs
df <- broom::tidy(nonResponseModel)
df$model <- "2. Original"

originalDT <- data.table::as.data.table(cbind(coefCI, df)) # collect for later plot

# > Diagnostics: Independence of errors ----
car::durbinWatsonTest(nonResponseModel)
# Andy Field DSUR:The test statistic can vary between 0 and 4 with a value of 2 meaning that the 
# residuals are uncorrelated. A value greater than 2 indicates a negative correlation between 
# adjacent residuals, whereas a value below 2 indicates a positive correlation. 
# The size of the Durbin-Watson statistic depends upon the number of predictors in the model and the 
# number of observations. As a very conservative rule of thumb, values less than 1 or greater than 
# 3 are definitely cause for concern; however, values closer to 2 may still be problematic depending on your sample and model.

# if p < 0.05 then a problem as implies auto-correlation

# plot to check residuals
plot(nonResponseModel$residuals)
# you what?
head(all_onboards)
# hmm

# these results suggest a problem with the model but I suspect this is caused by
# a) the data being ordered by the outcome (consent = T/F)
# b) large sample size -> so p value is going to be small...

# Simple consent model (randomly sampled) ----

# Create a smaller randomly ordered sample by randomly selecting 3000
# with replacement

dt <- all_onboards[sample(nrow(all_onboards), 2000, replace = TRUE), ]

# same % as above?
prop.table(table(dt$consent, useNA = "always")) # check for NA
# yep

# re-run model with random sample
nonResponseModelReduced <- glm(formula = consent ~ as.factor(quintile) + 
                          region, 
                        family = binomial(logit), dt) # wiil auto-drop the missing values

summary(nonResponseModelReduced)

# CIs for coef not OR for plot
coefCI <- cbind(coef = coef(nonResponseModelReduced), 
                confint(nonResponseModelReduced))

df <- broom::tidy(nonResponseModelReduced)
df$model <- "1: Reduced"

reducedDT <- data.table::as.data.table(cbind(coefCI, df)) # collect for later plot

# > Diagnostics: Independence of errors ----
car::durbinWatsonTest(nonResponseModelReduced)

# plot to confirm
plot(nonResponseModelReduced$residuals)
# noisy & random

# Simple consent model (full data randomly shuffled) ----

# OK, so let's shuffle the original data randomly
all_onboards <- all_onboards[sample(nrow(all_onboards)),]

# re-run model with random sample
nonResponseModelShuffled <- glm(formula = consent ~ as.factor(quintile) + 
                                 region, 
                               family = binomial(logit), all_onboards) # wiil auto-drop the missing values

summary(nonResponseModelShuffled) # should be identical to first model
# NB the model is not explaining a lot
# This is kind of what we want - it suggests these area level indicators are
# not strong predictors of consent


# CIs for coef not OR for plot
coefCI <- cbind(coef = coef(nonResponseModelShuffled), 
                confint(nonResponseModelShuffled))

df <- broom::tidy(nonResponseModelShuffled)
df$model <- "3: Shuffled"

shuffledDT <- data.table::as.data.table(cbind(coefCI, df)) # collect for later plot

plotDT <- rbind(originalDT, reducedDT, shuffledDT)
data.table::setnames(plotDT, c("2.5 %","97.5 %"), c("lo", "up")) # for easier ggplotting


# useful method of platting OR with CI
# need to tidy the labels etc
library(ggplot2)
p <- ggplot2::ggplot(plotDT, aes(x = coef, y = term , fill = model)) + 
  geom_col(position = "dodge") +
  geom_errorbar(aes(xmax = up, xmin = lo),position = "dodge") +
  geom_vline(xintercept = 0) +
  theme(legend.position="bottom")
p

library(here) # so useful - relative repo paths!
ggplot2::ggsave("nonResponseModelCompare.png", plot = p,
                path = here::here("plots"))

# the intercept effect is very large - so the model is not telling us a lot...

# > Diagnostics: Independence of errors ----
car::durbinWatsonTest(nonResponseModelShuffled)
# DW = 1.88, I suspect the p value = 0 due to sample size (NB also Note
# in https://www.rdocumentation.org/packages/car/versions/3.0-10/topics/durbinWatsonTest
# says p value only calculated for lm, this is not lm...?)

# plot to confirm
plot(nonResponseModelShuffled$residuals)
# noisy & random


