# SmarteR not HardeR Workshop
# R in the Psychological Science Production Cycle
#
# Multilevel Modeling Basics - Regression of Regressions
#
# Questions? contact mbarstead@deadreckoning.consulting

# Loading in packages
# ----------------------------------------------------------------------------------------------------------------------
if(!require(pacman))
    install.packages('pacman')

pacman::p_load(tidyverse, 
               lme4, 
               merTools, 
               influence.ME)

# Note that you should have already explored your data overall and by grouping variable before modeling
# ----------------------------------------------------------------------------------------------------------------------
data("hsb")

# The RQ we'll explore: the association between SES and math achievement

# Start by fitting a "null" model: 
fit_null <- lmer(mathach ~ 1 + (1|schid), 
                 data = hsb)
summary(fit_null)

# Note there is no p-value here (largely becasue it is hard to know what the degrees of freedom should be)
# An alternative and my preferred method would be to boostrap here, luckily somewhere already built that tool
confint.merMod(fit_null, 
               method = 'boot',
               nsim=1000)

# But the intercept isn't really all that interesting, and this is just a base model for comparison 
fit_ses_fixed <- lmer(mathach ~ 1 + ses + (1|schid), 
                      data = hsb)
summary(fit_ses_fixed)
anova(fit_null, fit_ses_fixed) # One way to test for improvement in model fit. 

# Normality assumptions
# ----------------------------------------------------------------------------------------------------------------------
# Still have the basic normality of residuals expectations but now at two levels
# Level-1 residuals
hist(residuals(fit_ses_fixed))

# Level-2 residuals
hist(ranef(fit_ses_fixed)$schid[,1])

# Influential cases
# ----------------------------------------------------------------------------------------------------------------------
# But what about influential cases?? 
inf_lv2 <- influence(fit_ses_fixed, 
                     group="schid")

n_groups <- length(unique(hsb$schid))
cook_thresh <- 4/n_groups

par(mfrow=c(2,1)) # basic plotting window
# Plot DF Betas for ses fixed effects
plot(dfbetas.estex(inf_lv2)[,2], 
     ylab = "DFBetas: SES")
abline(a=0, b=0, lty='dashed', col='black')

# One common threshold for cook's distance is 4/n - here we use grouping count
plot(cooks.distance.estex(inf_lv2), 
     ylab = "Cook's Distance")
abline(a=cook_thresh, b=0, lty='dashed', col='red')

par(mfrow=c(1,1))
# Default plots can help identify how many school IDs are outside of thresholds 
plot(inf_lv2,
     which = "cook",
     cutoff = cook_thresh,
     sort = TRUE,
     xlab = "Generalized Cook's Distance", 
     ylim = c(n_groups-15.5, n_groups+.5))

# So which are the offending cases here? Difficult to tell with default plotting options
cook_results <- cooks.distance.estex(inf_lv2)
inf_groups <- rownames(inf_lv2$alt.fixed)[cook_results>cook_thresh]

# Do the "influential" cases matter? Percent change in 
plot(inf_lv2, 
     which = 'pchange', 
     xlab = 'Percent Change in Parameter', 
     ylab = 'School ID', 
     groups = inf_groups)

# Not going to go through it but you can do the same for every "row" as well... 

# ----------------------------------------------------------------------------------------------------------------------
# Adding model complexity: 

# Still focused on ses, but now want to understand separate contributions of "contextual" effects vs. individual effects
# Going to "group-mean" center variables
# This changes our understanding of what the intercept means: now the estimated mathach for an individual at mean ses 
# for their school.

# simple function for group-centering


group.center <- function(var,grp) {
    return(var-tapply(var, grp, mean, na.rm=T)[grp])
}

group.center(hsb$ses, hsb$schid)

hsb$c.ses <- group.center(hsb$ses, hsb$schid)
mean(hsb$c.ses[hsb$schid=='1224'])
mean(hsb$c.ses[hsb$schid=='2336'])
# Looks like it worked

fit_final <- lmer(mathach ~ 1 + c.ses + meanses + (1 + c.ses|schid), 
                  data = hsb)
anova(fit_null, fit_final)

summary(fit_final)
confint.merMod(fit_final, parm='beta_', nsim = 1000)

# From here I would work on partitioning variance using Rights and Sterba Approach

# Bayesian Version: (Requires setup steps we don't have time to review)
# ----------------------------------------------------------------------------------------------------------------------
fit_final_bayes <- brm(mathach ~ 1 + c.ses + meanses + (1 + c.ses|schid), 
                       data = hsb, 
                       iter=2000, 
                       warmup=1000, 
                       chains=3)
