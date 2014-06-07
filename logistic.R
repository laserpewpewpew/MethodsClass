### Logistic Regression

# How do we analyze dichotomous DVs?
    # Not by OLS
    # The logic of logistic
    # Interpretation
        # Odds ratios?
        # Predicted Probabilities

# Setup
library(car)
library(ggplot2)

library(foreign)
ess <- read.dta("ESS6ES.dta") 
var.labels <- attr(ess,"var.labels")
data.key <- data.frame(var.name=names(ess),var.labels)
data.key[1:10,]  # The ESS asks *lots* of questions, including really good ones on protest

data.key[grep("demonstra", data.key$var.labels),] # Ah, here we go (will warn about accented characters)
table(ess$pbldmn)
table(as.numeric(ess$pbldmn))

ess$protest[as.numeric(ess$pbldmn) < 3] <- 2 - as.numeric(ess$pbldmn)[as.numeric(ess$pbldmn) < 3]
table(ess$protest)

ess$age[ess$age==999] <- NA    
ess$eduyr[ess$eduyr>20] <- 20   
ess$income[as.numeric(ess$hinctnta)<=10] <- as.numeric(ess$hinctnta)[as.numeric(ess$hinctnta)<=10] 
ess$female <- as.numeric(ess$gndr=="Female")
ess$married <- as.numeric(ess$maritalb=="Legally married")
ess$church <- as.numeric(ess$rlgatnd=="Every day" | ess$rlgatnd=="More than once a week" | 
                            ess$rlgatnd=="Once a week")

# Not by OLS

m0 <- lm(protest ~ age + eduyr + income + female + married + church,
          data = ess, weights = dweight)
summary(m0)

residualPlots(m0)       # Focus on the last plot (residuals vs. fitted).  
                        # What two things jump out?

# The Logic of Logistic

# If we want to be sure that our predictions make sense, we need a way of converting our
# dichotomous DV into something that can be modeled linearly.  There are other options 
# (e.g., probit), but the logit function starts by taking the odds of "success" (i.e., a one). 
# Odds are continuous (yay!), but they are always positive (boo!).  The *natural log* of the
# odds, though, is negative when the odds are below 1 (even odds, a 50-50 chance) and is 
# positive when the odds are above 1.  Yay! We can predict the logged odds as a linear 
# function of our IVs.

m1 <- glm(protest ~ age + eduyr + income + female + married + church,
          data = ess, family = "binomial", weights = dweight)
summary(m1)


# Interpretation

# Um, but now our coefficients are in logits: "for a one-point change in x1, the
# logged odds of the DV are predicted to change by b1."  Nobody can understand that.

# Odds ratios are one option for interpreting logistic regression results.  Just 
# exponentiate the predicted change in the logged odds, and you'll get how the odds 
# are predicted to change (as a ratio to their previous value).  

exp(cbind(OR = coef(m1), confint(m1)))

# This offers a straightforward interpretation: a person with an additional year of education
# is predicted to have odds of protesting that are 11% (+/-3%) higher than an otherwise similar
# person without that extra year of education.  Someone with two years more, then, 
exp(coef(m1)[3])^2

# has predicted odds 24% higher.  The odds of regular churchgoers protesting are predicted to
# be just 57% of the odds of those who don't attend regularly (or put differently, their odds
# are predicted to be 43% lower). Totally straightforward.  Well, sort of: it's hard to
# wrap your mind around odds.  And if you don't know what the original odds of protesting were
# (for the less educated person), you don't know if 11 or 24% higher odds is really a big deal. 
# For these reasons, odds ratios are strongly disfavored among political scientists.

# Predicted probabilities

church.fake <- with(ess, data.frame(age = weighted.mean(age, na.rm = T, w = dweight),
                                    eduyr = weighted.mean(eduyr, na.rm = T, w = dweight), 
                                    income = weighted.mean(income, na.rm=T, w = dweight), 
                                    female = 1, 
                                    married = 1,
                                    church = 0:1))
church.pp <- cbind(church.fake, predict(m1, newdata = church.fake, type = "link", se = TRUE))
church.pp <- within(church.pp, {
    pp <- plogis(fit)*100
    lb <- plogis(fit - (1.96 * se.fit))*100
    ub <- plogis(fit + (1.96 * se.fit))*100
})
church.pp[ , 6:12]

# So *among otherwise typical married women*, those who don't attend religious services 
# regularly are predicted to have a 26% (+/-4%) probability of having participated in a  
# demonstration in the past year, but churchgoers are predicted to have just a 17% (+/-5%) 
# probability of having done so.

edu.fake <- with(ess, data.frame(age = weighted.mean(age, na.rm = T, w = dweight),
                                 eduyr=0:20, 
                                 income = weighted.mean(income, na.rm=T, w = dweight),
                                 female = 1, 
                                 married = 1,
                                 church = 0))
edu.pp <- cbind(edu.fake, predict(m1, newdata = edu.fake, type = "link", se = TRUE))
edu.pp <- within(edu.pp, {
    pp <- plogis(fit)*100
    lb <- plogis(fit - (1.96 * se.fit))*100
    ub <- plogis(fit + (1.96 * se.fit))*100
})

ggplot(edu.pp, aes(x = eduyr, y = pp)) + 
    geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.2) + 
    geom_line(size = 1)

# In logistic regression, the assumption of additivity is only made in the logit space.
# When transforming results into probabilities, just the opposite is true: the predicted 
# always vary with the values of the other variables.  This is why we have to make up
# fake data for all of the other IVs in the model to calculate how the predicted probabilities 
# change for when any one IV changes . . .

edu.fake2 <- with(ess, data.frame(age=mean(age, na.rm=T), eduyr=rep(0:20, times=2), 
                                  income=mean(income, na.rm=T), female = 1, married = 1,
                                  church = rep(0:1, each=21)))
edu.pp2 <- cbind(edu.fake2, predict(m1, newdata = edu.fake2, type = "link", se = TRUE))
edu.pp2 <- within(edu.pp2, {
    pp <- plogis(fit)*100
    lb <- plogis(fit - (1.96 * se.fit))*100
    ub <- plogis(fit + (1.96 * se.fit))*100
})

ggplot(edu.pp2, aes(x = eduyr, y = pp)) + 
    geom_ribbon(aes(ymin = lb, ymax = ub, fill = as.factor(church)), alpha = 0.2) + 
    geom_line(aes(colour = as.factor(church)), size = 1)
