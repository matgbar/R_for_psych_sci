# SmateR not HardeR Workshop
# R in the Psychological Science Production Cycle
#
# Power Analysis with Simulation
#
# Questions? contact mbarstead@deadreckoning.consulting

# Loading in packages
# ----------------------------------------------------------------------------------------------------------------------
if(!require(pacman))
    install.packages('pacman')

pacman::p_load(tidyverse, 
               lavaan, 
               simsem, 
               simr, 
               lme4, 
               lmerTest)

# Bivariate Regression
# ----------------------------------------------------------------------------------------------------------------------
#Specifying a population model
pop.model<-'
Y~.2*X1+.5*X2
X1~~.4*X2
Y~~.63*Y
'

# Test model - reflection of the population model
test.model<-'
Y~X1+X2
X1~~X2
Y~~Y
'

# Just to see what happens here: 
demo <- simulateData(model = pop.model, 
                     sample.nobs = c(5, 10, 15))
demo

# Let's make a function: 
simPwr<-function(n.max=500, n.min=50, n.sims=1000, skew.val=0, verbose = TRUE,
                 kurtosis.val=0, pop.model, test.model, alpha=.01){
    
    # browser() # Let's illustrate what is happening at each step 
    # Getting just the regression paths power (my work around for only getting regression paths)
    tmp.data <- simulateData(model = pop.model, sample.nobs = 250)
    tmp.fit <- sem(test.model, data = tmp.data)
    tmp.par.table <- parameterEstimates(tmp.fit)
    n.reg.paths <- length(tmp.par.table$op[tmp.par.table$op == '~'])
    
    # Create a sequence of sample sizes from n.min to n.max by 25
    n.obs<-seq(n.min, n.max, by=25)
    
    # data.frame to retain final results
    pow <- data.frame()
    
    # Now loop through the the vector of different sample size
    for(n in 1:length(n.obs)){
        #browser()
        start_time <- Sys.time()
        est <- data.frame()
        pval <- data.frame()
        
        # Definitely some expensive for loops here. Code in need of optimization.
        for(s in 1:n.sims){
            #browser()
            tmp.data<-simulateData(pop.model, sample.nobs = n.obs[n], skewness = skew.val, kurtosis = kurtosis.val)
            tmp.fit<-sem(test.model, data=tmp.data, fixed.x = F)
            parm<-parameterestimates(tmp.fit)
            parm<-parm[parm$op=='~',]
            est<-rbind(est, parm$est)
            pval<-rbind(pval, parm$pvalue)
        }
        
        Power<-ifelse(pval < alpha, 1, 0)
        
        # Save values in a data.frame (mainly for graphing) and final analysis
        mu.est<-colMeans(est)
        mu.pow<-colMeans(Power)
        sim.N<-rep(n.obs[n], n.reg.paths)
        parm.nm <- paste(parm$lhs[parm$op=="~"], 
                         parm$op[parm$op=="~"],
                         parm$rhs[parm$op=="~"])
        dat.fin<-data.frame(parm.nm, mu.est, mu.pow, sim.N)
        colnames(dat.fin)<-c('Parameter', 'Estimate', 'Power', 'N')
        pow<-rbind(pow, dat.fin)
        time_elapsed <- round(Sys.time() - start_time, digits=3)
        print(paste("Finished simulations for", n.obs[n], "observations;", n, 'of', length(n.obs)))
        print(paste('Time Elapsed:', time_elapsed, 'seconds'))
    }
    return(pow)
}

test<-simPwr(n.max=500, 
             n.min = 25, 
             n.sims = 5, 
             pop.model = pop.model, 
             test.model = test.model)

g1 <- ggplot(aes(x=N, y=Power, group = Parameter, color=Parameter), data=test) + 
    stat_smooth(se=F) +
    geom_hline(yintercept = .8, color='red', lty='dashed') + 
    labs(main = "Power Curves for Parameters in Model", 
         y = "Power", 
         x = "Sample Size")
g1    
