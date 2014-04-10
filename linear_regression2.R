###  Classical Linear Regression, Assumptions, and Violations (Part 2)

# What are the most important assumptions of the linear regression model?
# And how do we diagnose violations and correct problems?  
#   1. VALIDITY!
#   2. LINEARITY
#   3. ADDITIVITY
#   4. Independent Errors
#   5. Homoskedasticity
#   6. A nuisance: normality of errors

# 1. VALIDITY!
# The single most important issue encountered in doing regression analysis regards
# validity: the fit between the measures employed and the concepts that one seeks
# to investigate.  This is frequently skipped over in discussions of
# methods because problems of validity are often difficult to overcome.
# Nevertheless, the quality of your data should be your foremost concern (Gelman
# and Hill 2007, 45-46).

# 2. LINEARITY
# Recall that the regression model is of the form y = XB + e. It therefore assumes 
# that the DV is appropriately modeled as a linear function of the IVs--that any
# similarly-sized change in an IV predicts the same amount of change in the DV.
# One should consider theoretical reasons this may not be so (e.g., one might 
# expect 30-year-olds to participate in politics more than 20-year-olds, but 
# 80-year-olds to participate *less* than 70-year-olds).  We can also check
# the linearity assumption empirically.
library(car)
m1 <- lm(prestige ~ education + income + type, data=Prestige)
summary(m1)

residualPlots(m1)
# This function returns, first, a lack-of-fit test for each numerical predictor.
# These indicate the p-value of the square of that predictor if it were added
# to the model (Fox and Weisberg 2011, 288-89).  A *lack* of a statistically 
# significant value, then, indicates the assumption is not violated.  The
# function also produces a plot of the residuals--the difference, for each
# observation between the the actual value of the DV and the predicted value--
# against the range of values of each IV.  These plots allow a visual inspection
# to confirm the results of the lack-of-fit tests.

# What to do if the linearity assumption fails?  One can, of course, just
# add the square of the predictor:

m2 <- lm(prestige ~ education + income + I(income^2) + type, data=Prestige)
summary(m2)
residualPlots(m2)

# This still isn't great--the results imply that including the fourth power of 
# income would improve fit!  Breaking a continuous variable into a series of
# dichotomous indicator variables that together identify its full
# range is often a better option (Gelman and Hill 2007, 66-67).  This
# approach trades degrees of freedom for increased flexibility.

    # Generate indicators for each quintile of income
quantile(Prestige$income, probs = seq(0, 1, 0.2))
q <- quantile(Prestige$income, probs = seq(0, 1, 0.2))
for (i in c(1:5)) {
    assign(paste0("inc",i), as.numeric(Prestige$income>=q[i] & Prestige$income<q[i+1]))
}

m2a <- lm(prestige ~ education + inc1 + inc2 + inc4 + inc5 + type, data=Prestige)
summary(m2a)
residualPlots(m2a)

# These results reveal that the predicted change in prestige for a change in income
# is pretty linear except that occupations in the poorest quintile have especially 
# low prestige.  Knowing this, we can now take back most of our degrees of freedom.
# Note the improvement in adjusted R2 from m2a to m2b.

m2b <- lm(prestige ~ education + inc1 + income + type, data=Prestige)
summary(m2b)
residualPlots(m2b)

# Given that increases in income predict large changes at the low end of the range,
# but smaller ones as income increases, one can also use a log transformation.  Note
# that m2c further improves on the adjusted R2 of m2b.

m2c <- lm(prestige ~ education + log(income) + type, data=Prestige)
summary(m2c)
residualPlots(m2c)

# 3. ADDITIVITY
# The related assumption is the effects of the IVs are additive--that the predicted
# change in the DV for a change in one IV does not depend on the values of the other
# IVs.  If there are theoretical reasons to expect nonadditivity, one can use
# multiplicative interaction terms.  If the theorized interactions are extensive,
# it may make sense to log transform *all* of the variables: if y = a*b*c, then 
# log(y) = log(a) + log(b) + log(c), which re-establishes additivity (Gelman 
# and Hill 2007, 46).  Such log-log models have an advantage over multiplicative
# interactions in interpretability: the coefficient estimates are interpreted as 
# the change *in percent* in the DV predicted by a 1% change in the IV.

# 4. Independent errors
# Linear regression assumes that the errors are independent--that knowing how well
# one observation is predicted provides no information about how well any other
# observations are predicted.  This assumption is violated in a number of easily 
# recognized cases:
#   (a) dichotomous and ordinal DVs (e.g., Solt 2001, 87),
#   (b) time series data (e.g., Mitchell and Moore 2002, 442-443),
#   (c) spatially dependent data (e.g., Boehmke and Skinner 2012, 321-322),
#   (d) hierarchically organized data (e.g., Solt 2008, 53-54; Pacheco 2012, 191).  
# Each of these cases has specialized methods to address the nonindependence of
# errors.

# 5. Homoskedasticity
# Linear regression assumes that the errors have uniform variance, that is, that
# predictions fit the DV equally well over the range of the IVs.  Plotting the
# residuals against the IVs makes heteroskedasticity (that is, the absence of equal
# error variance) fairly easy to spot.  If the residuals exhibit an obvious fan-
# shaped pattern (with smaller variance at one end of the IV's range) or bulging
# pattern (with smaller variance at both ends of the range), then the 
# assumption of homoskedasticity is violated.  

residualPlots(m2b)

# In m2b, the residuals for income exhibit a reverse-fan pattern, with smaller
# error variance at high values (compare the income plot with the plot for 
# education).  Heteroskedacity isn't a really big deal (Gelman and Hill 2007, 46):
# estimates are not biased, but they are not as efficient as they could be.
# It can often be addressed simply by respecifying the model and/or transforming
# the variable: in m2c, for example, log(income) does not exhibit much if any 
# heteroskedacity.

residualPlots(m2c)

# Another option is to use weighted least squares, which weights each observation
# inversely proportional to its variance.  This, however, requires the variance
# to be known.  In very large datasets, one might estimate the variance empirically,
# but the Prestige dataset (N=102) used here isn't particularly big.  As it appears
# that the variance decreases with income, we can calculate weights that also  
# decrease with income:

wt <- with(Prestige, 1/(max(income)-income+1))
m2d <- lm(prestige ~ education + inc1 + income + type, data=Prestige, weight=wt)
summary(m2d)
residualPlots(m2d)

# The residual plot looks better: the previous fan shape is not evident.  Now,
# however, the lack-of-fit tests for both education and income indicate linearity
# is violated in this model.  As is often the case, we're better off using a
# transformation (as in m2c) rather than weighted least squares.

# 6. A nuisance: normality of errors
# A lack of normally distributed errors makes OLS regression inefficient.  Errors
# may not be normally distributed because the DV and IVs are themselves very 
# far from normally distributed and the problem is really with the linearity 
# assumption.  If this is not the case, the presence of a few large outliers
# can skew the distribution of errors--OLS estimation is based on minimizing 
# the squared error, so a few extreme observations can exert a disproportionate 
# influence on parameter estimates.  To have influence, a point must be both
# poorly predicted and have leverage due to outlying values on one or more 
# IVs (Fox and Weinberg 2011, 294-298).  How well a point is predicted can 
# be determined from its studentized residual (see Fox and Weinberg 2011, 286-287);
# leverage is measure with the hat-value (see Fox and Weinberg 2011, 296-297).
# Influence is conveniently summarized as the difference in the coefficients 
# with and without the observation included in the sample, a statistic known 
# as Cook's D.

influencePlot(m2c, id.n=3) 

m2c2 <- update(m2c, subset = row.names(Prestige) != "medical.technicians")
compareCoefs(m2c, m2c2)

# Although many have the knee-jerk reaction to want to remove highly influential 
# data points from the sample, this should not be one's first response.  Instead,
# such points should be examined to determine if they are erroneous, perhaps due 
# to a data-entry error.  If the data for the observation are indeed correct, the
# observation should then be scrutinized for its theoretical implications (see 
# Stephens 1979 for a pioneering example).  Observations should *never* be deleted
# without discussion!  If one excludes points from a sample merely on grounds of
# influence, one has to explain this and its effect on the results.
