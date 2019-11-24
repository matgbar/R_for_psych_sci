# SmarteR not HardeR Workshop
# R in the Psychological Science Production Cycle
#
# Exploratory Analyses - Dimension Reduction
#
# Questions? contact mbarstead@deadreckoning.consulting

# Loading in packages
# ----------------------------------------------------------------------------------------------------------------------
if(!require(pacman))
    install.packages('pacman')

pacman::p_load(tidyverse, 
               lavaan,
               nFactors, 
               psych, 
               corrgram)

# Exploratory Analysis
# ----------------------------------------------------------------------------------------------------------------------
#Quick Exploration 
data("HolzingerSwineford1939")
df <- HolzingerSwineford1939 # mostly so I don't have to keep writing HolzingerSwineford1939

# Using nFactors library - NOTE THIS IS PROBLEMATIC IF THERE IS A LOT OF MISSING DATA
df_miss_removed <- na.omit(df[,paste0("x", 1:9)])
pairs.panels(df_miss_removed) # snapshots

# More heat mappish approach: 
corrgram(df_miss_removed, 
         order=TRUE, 
         lower.panel = panel.conf, 
         upper.panel = panel.pie, 
         diag.panel = panel.density, 
         col.regions = colorRampPalette(c('navy', 'lightblue', 'white', 'pink', 'darkred')))

# Setting up parallel analysis - NOTE: pioneered in 1964 by Horn!!
ev <- eigen(cor(df_miss_removed)) # get eigenvalues
ap <- parallel(subject=nrow(df_miss_removed),
               var=ncol(df_miss_removed),
               rep=1000, 
               cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) 

# Eigenvalues > 1 is the age-old rule for retaining a factor that is at least one "unit" of variance
# Optimal coordinates relies on gradients in eigevalues
# Acceleration factor relies on finding the gradient with the most "abrupt" change... 
# Parallel Analysis - eigenvalues from sample data where population correlations = 0 

fit_pca <- principal(df_miss_removed, 
                     nfactors=3, 
                     rotate="oblimin")
print(fit_pca, sort=TRUE)

# Confirmatory Version 
# ----------------------------------------------------------------------------------------------------------------------

model <- ' 
visual  =~ x1 + x2 + x3 
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9 
'

fit_cfa <- cfa(model, 
               data=HolzingerSwineford1939, 
               estimator='MLR')
summary(fit_cfa, 
        fit.measures=TRUE, 
        rsquare=TRUE, 
        standardized=TRUE)

mod <- modificationindices(fit_cfa)
head(mod[order(mod$mi, decreasing=TRUE),])

# Reliability omega and alpha
# ----------------------------------------------------------------------------------------------------------------------
omega(df_miss_removed)