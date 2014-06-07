### Regression and Causal Inference

# When can regression be used to make causal claims rather than predictive ones?  
    # Causal vs. predictive inference
    # The Fundamental Problem
    # Assumptions: overlap
    # Assumptions: balance
    # Assumptions: ignorability
    # Matching: an example

# Causal vs. predictive inference

# So far, we have used regression to make only predictive inferences: e.g, to figure 
# out the predicted difference in the DV, given a one-point increase in an IV, all
# else equal.  These inferences are comparisons *across* different observations, for
# example, between the registration deadlines of those states with low levels of 
# ethnic diversity and those with high levels of ethnic diversity.  Causal inference
# instead seeks to figure out what would happen to the DV of *same* observation if
# its value on the IV were different.  This implies that, to make a causal inference
# regarding a particular IV--for it to be a *cause*, not just a predictor--it has to
# be amenable to actually taking on a different value for the same observation.
# Policy interventions that would change the ethnic diversity of a state are 
# imaginable, so making a causal inference is at least hypothetically possible for 
# that variable, but individual-level gender or ethnicity can only ever be used for 
# predictive inferences.

# 2. The Fundamental Problem

# Because causal inference seeks what effect changing the value of IV for an 
# observation would have on the DV in that observation, it runs into a fundamental
# problem: either the IV changed for that ob or it didn't.  That is, causal 
# inference always involves an unobservable counterfactual (Gelman and Hill 2007, 
# 170-171).  We inevitably come back to making comparisons across observations 
# rather than on the same observation, and the difference in the DV between 
# observations should not be inferred to have been caused by a difference in
# the IV unless those observations are otherwise entirely similar.  If some
# other variable that is (a) not itself caused by the IV, (b) but correlated with 
# the IV, and (c) has a causal effect on the DV, then that variable's effect will  
# be mistakenly attributed to the IV unless it is included in the model (Gelman
# and Hill, 169-170).  This is called "omitted variable bias."

# But how can we ensure that two groups of observations are *entirely* similar,
# even with regard to potential confounders that haven't even been thought of?
# Randomized experiments provide a solution: if obs are assigned to groups at random, 
# and the groups are then assigned different values of the IV, with enough obs, the 
# distribution of *all* other variables will be similar within each group--making 
# the average difference between the DVs of the groups attributable to their
# different values on the IVs.  Okay, sure, but we can't randomize and manipulate
# many, almost certainly most, of the things political scientists care about,
# so for the moment, we'll look at issues of causal inference in observational
# studies.

# Overlap: the range of X across groups has to be equal or some observations won't
# have a counterfactual observation in the other group to compare to (Gelman & Hill 
# 2007, 201).

# Balance: the distributions of X across groups have to be equal or our inferences
# will depend on getting the model exactly right (Gelman & Hill 2007, 200-201).

# Ignorability: no omitted confounders.  Do your best to think of what you might
# need to control for--there's no way to assess ignorability empirically.

# An Example
# Suppose we are in the leadership of the Canadian teacher's union, and we want 
# our primary-school teachers to enjoy greater prestige than they do currently
# (that is, in 1971 when these data were collected).  Can we boost teachers' 
# prestige by adopting certification requirements that will require teachers
# to have at least 14 years of education?
library(car)
data(Prestige)

# Currently primary-school teachers average 13.6 years of education
Prestige[which(row.names(Prestige) == "primary.school.teachers"), ]

# Generate indicator variables for average education levels
m <- c(6, 9, 12, 14, 16)
for (i in 1:4) {
    Prestige[ , paste0("educ.m", m[i])] <- as.numeric(Prestige$education>=m[i] & Prestige$education<m[i+1])
}

apply(Prestige[, 7:10], 2, mean)

# Exclude teachers' current category, educ.m12, to serve as the reference category:
# we are, after all, interested in the comparison between educ.m12 and educ.m14.
m1 <- lm(prestige ~ educ.m6 + educ.m9 + educ.m14 + log(income) + type, data=Prestige)
summary(m1)

# This doesn't look promising: prestige is not predicted to be statistically 
# significantly higher for jobs that average more than 14 years than those that
# average between 12 and 14 years of education.  But there may be differences in
# the distribution of the other variables in the data that are working to obscure
# the proposed positive causal effect of shifting into the next education category.  

summary(Prestige[Prestige$educ.m12==1, c(2, 6)])

summary(Prestige[Prestige$educ.m14==1, c(2, 6)])

# There's a lack of complete overlap in type and there's imbalance in income.
# We can correct the overlap issue by including only professional jobs in our
# sample.  Teachers are professionals, so that's fine for our purposes: we only
# care about the effect more education would have on a professional occupation.
# We can also drop the least educated professional jobs, because we're interested 
# in only the comparison between educ.m12 and educ.m14.

P.pro <- Prestige[Prestige$type=="prof" & !is.na(Prestige$type) & 
                      Prestige$educ.m9==0, ]
summary(P.pro)

# We can drop type, educ.m6, and educ.m9 from the model--they don't vary in our new,
# fully-overlapping sample.
m2 <- lm(prestige ~ educ.m14 + log(income), data=P.pro)
summary(m2)

# A one-tailed test--appropriate since we hypothesized that more education would
# generate *more* prestige--is supported at the 5% level: moving up to the next
# category of education yields a predicted increase in prestige of 5.5 points.
# That's would be enough to put teachers past those darn health-care-providing 
# nurses!  But the data are still imbalanced with regard to income:

summary(P.pro[P.pro$educ.m12==1, c(2, 6)])

summary(P.pro[P.pro$educ.m14==1, c(2, 6)])

# and, as we know, imbalance leads to bias if our specification isn't exactly right.
# We can address the balance issue with matching.  Load the MatchIt package:
if (require(MatchIt)==F) install.packages("MatchIt")
library(MatchIt)

# Perform the match
P.matched <- matchit(educ.m14 ~ log(income), data=P.pro, replace=T)
summary(P.matched)
plot(P.matched)
P.m <- match.data(P.matched) # Extract the matched data for analysis

# Analyze the matched data
m3 <- lm(prestige ~ educ.m14 + log(income), data=P.m)
summary(m3)

# Wow--by ensuring balance on income, the matched data allows a sharper
# comparison: boosting education is estimated to cause a 6.8 point increase
# in prestige.  Let's push those credentials up!  What could go wrong?  Well,
# besides barristas with MFAs . . .

# This is a small dataset so we can check the plausibility of the match directly:
P.matched$match.matrix

P.m[order(P.m$income), c("income", "educ.m14", "prestige")]
