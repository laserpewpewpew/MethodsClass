### Classical Linear Regression, Assumptions, and Violations

# Load packages, installing if necessary
if(require(ggplot2) == FALSE) {
    install.packages("ggplot2")
    require(ggplot2)
}

if(require(car) == FALSE) {
    install.packages("car")
    require(car)
} 

# Load data
states <- read.csv("statereg.csv")

# Do states with more income inequality have earlier registration deadlines?
qplot(y = regdays, x= stategini, data=states)

m1 <- lm(regdays ~ stategini + stdiversity + over64 + college + stincpc +
             south, data=states)

summary(m1)



### Diagnostics

residualPlots(m1)

# Okay, the residuals indicate that the linearity assumption is problematic,
# so we'll add a squared term.  Atheoretical, but let's see what we get.
m2 <- lm(regdays ~ stategini + I(stategini^2) + stdiversity + over64 + 
             college + stincpc + south, data=states) 

summary(m2)

residualPlots(m2) # Looks good, but what do these results mean?

fake.state <- data.frame(stategini = seq(40,50,.1),
                          stdiversity = mean(states$stdiversity),
                          over64 = mean(states$over64),
                          college = mean(states$college),
                          stincpc = mean(states$stincpc),
                          south = 0)
fake.state$regdays <- predict(m2, fake.state)

ggplot(fake.state, aes(stategini, regdays)) + geom_line() 

# Back to m1

# Nuisances
# Influence: outlying observations with leverage
influenceIndexPlot(m1, id.n=3, labels=states$state)

influencePlot(m1, id.n=3, labels=states$state)

m1a <- update(m1, subset = states$state != "Connecticut" & states$state != "WestVirginia")
compareCoefs(m1, m1a)

# Multicollinearity
vif(m1)
sqrt(vif(m1)) #confidence intervals are increased by a factor of root VIF

