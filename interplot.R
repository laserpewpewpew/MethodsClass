interplot <- function(m, var1, var2, seed=324, sims=1000, steps=100, plot=TRUE) {
    require(arm)
    require(ggplot2)
    set.seed(seed)
    m.sims <- sim(m, sims)
    
    var12 <- paste0(var2,":",var1)
    if (!var12 %in% names(m$coef)) var12 <- paste0(var1,":",var2)
    if (!var12 %in% names(m$coef)) stop(paste("Model does not include the interaction of",var1 ,"and",var2))
    coef <- data.frame(fake = seq(min(m$model[var2], na.rm=T), max(m$model[var2], na.rm=T), length.out=steps), coef1 = NA, ub = NA, lb = NA)
    
    for(i in 1:steps) {   
        coef$coef1[i] <- mean(m.sims@coef[,match(var1, names(m$coef))] + 
                                  coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))])
        coef$ub[i] <- quantile(m.sims@coef[,match(var1, names(m$coef))] + 
                                   coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))], .975)
        coef$lb[i] <- quantile(m.sims@coef[,match(var1, names(m$coef))] + 
                                   coef$fake[i]*m.sims@coef[,match(var12, names(m$coef))], .025)    
    }

    if(plot==TRUE) {
        if(steps>5) {
            coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) +                       
                geom_line() + geom_ribbon(aes(ymin=lb, ymax=ub), alpha=.5) 
        } else {
            coef.plot <- ggplot(coef, aes(x = fake, y = coef1)) +                       
                geom_point() + geom_errorbar(aes(ymin=lb, ymax=ub), width=0) + 
                scale_x_continuous(breaks = 0:steps)
        }
        return(coef.plot)
    } else {
        names(coef) <- c(var2, "coef", "ub", "lb")
        return(coef)
    }
}

require(arm)
require(foreign)
obama <- read.dta("Obama.dta")
m2 <- lm(obama ~ income + age + educ + female + black + dem + rep + income:dem, data=obama)

p1 <- interplot(m=m2, var1="dem", var2="income")
p1 <- p1 + ylab("Coefficient for Democrat") + xlab("Income")
p1

p2 <- interplot(m=m2, var1="income", var2="dem", steps=2)
p2 <- p2 + ylab("Coefficient for Income") + xlab("Democrat")
p2

interplot(m=m2, var1="income", var2="dem", steps=2, plot=F)
