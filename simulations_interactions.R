### Linear Regression, cont.; Simulations; Interactions

# Load packages, installing if necessary
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

packages <- c("ggplot2", "RCurl", "arm")
ipak(packages)

# Again with the state registration deadlines: first load the data
states <- read.csv(text = getURL("https://raw2.github.com/fsolt/POLI5003/master/statereg.csv"))

# Do states with more income inequality have earlier registration deadlines?
m1 <- lm(regdays ~ stategini + stdiversity + over64 + college + stincpc +
             south, data=states)

summary(m1)

# What are the relative magnitudes of the estimated effects?
# Maximum effects
states.range <- apply(states[,4:9], 2, function(x) max(x) - min(x)) 
max.effects <- states.range * m1$coef[2:7]

# Of course, you can choose a different range, like twice
# the standard deviation (roughly the range of the middle 2/3s of observations)
states.range.2sd <- apply(states[,4:9], 2, function(x) 2*sd(x)) 
sd2.effects <- states.range.2sd * m1$coef[2:7]

# An alternative: standardized coefficients (actually not really popular in polisci)
states.st <- data.frame(scale(states[,c(1,3:9)])) # scale() standardizes variables (subtracts the mean and divides by sd)
states.st$state <- states$state
m1.st <- lm(regdays ~ stategini + stategini + over64 + college + stincpc +
             south, data=states.st)
summary(m1.st)

# Simulation
# Note that we have point estimates for max.effects or sd2.effects, but no
# confidence intervals.  How do we find them?  Simulation.
set.seed(324)  # Always set the seed when generating pseudorandom numbers
n.sims <- 10000
m1.sims <- sim(m1, n.sims)
apply(m1.sims@coef, 2, mean) # double check against coef estimates . . .

coefs.no.intercept <- m1.sims@coef[,2:7]

max.effects.2 <- matrix(nrow = n.sims, ncol = length(states.range))
for(i in 1:length(states.range)) {
    max.effects.2[,i] <-  states.range[i] * coefs.no.intercept[,i]
}
colnames(max.effects.2) <- names(m1$coef[2:7])
ci <- apply(max.effects.2, 2, function(x) quantile(x, c(.025,.975)))
ci

# Interactions: How to model conditional hypotheses
# Is effect of inequality affected by level of ethnic diversity?
m2 <- lm(regdays ~ stategini + stdiversity + stdiversity:stategini + over64 + college + stincpc +
             south, data=states)
summary(m2)

set.seed(324)
m2.sims <- sim(m2, n.sims)
apply(m2.sims@coef, 2, mean)


coef.gini <- data.frame(fake_stdiversity = seq(min(states$stdiversity), max(states$stdiversity), 
                        length.out=100), coef_gini = NA, ub_gini = NA, lb_gini = NA)


for(i in 1:100) {   
    coef.gini$coef_gini[i] <- mean(m2.sims@coef[,2] + 
            coef.gini$fake_stdiversity[i]*m2.sims@coef[,8])
    coef.gini$ub_gini[i] <- quantile(m2.sims@coef[,2] + 
            coef.gini$fake_stdiversity[i]*m2.sims@coef[,8], .975)
    coef.gini$lb_gini[i] <- quantile(m2.sims@coef[,2] + 
            coef.gini$fake_stdiversity[i]*m2.sims@coef[,8], .025)    
}

gini.coef.plot <- ggplot(coef.gini, aes(x = fake_stdiversity, y = coef_gini)) + 
    geom_line() + geom_ribbon(aes(ymin=lb_gini, ymax=ub_gini), alpha=.5) +
    xlab("State Ethnic Diversity") + ylab("Coefficient for State Income Inequality") +
    scale_x_continuous(limits=c(0,80))

gini.coef.plot

# This also means effect of diversity is affected by level of inequality
coef.div <- data.frame(fake_stategini = seq(min(states$stategini), max(states$stategini), 
                                               length.out=100), coef_div = NA, ub_div = NA, lb_div = NA)

for(i in 1:100) {   
    coef.div$coef_div[i] <- mean(m2.sims@coef[,2] + coef.div$fake_stategini[i]*m2.sims@coef[,8])
    coef.div$ub_div[i] <- quantile(m2.sims@coef[,2] + coef.div$fake_stategini[i]*m2.sims@coef[,8], .975)
    coef.div$lb_div[i] <- quantile(m2.sims@coef[,2] + coef.div$fake_stategini[i]*m2.sims@coef[,8], .025)    
}

div.coef.plot <- ggplot(coef.div, aes(x = fake_stategini, y = coef_div)) + 
    geom_line() + geom_ribbon(aes(ymin=lb_div, ymax=ub_div), alpha=.5) +
    xlab("State Income Inequality") + ylab("Coefficient for State Ethnic Diversity") +
    scale_x_continuous(limits=c(40,50))

div.coef.plot