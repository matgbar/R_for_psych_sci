# SmateR not HardeR Workshop
# R in the Psychological Science Production Cycle
#
# Exploratory Analyses - Clustering Data
#
# Questions? contact mbarstead@deadreckoning.consulting

# Loading in packages
# ----------------------------------------------------------------------------------------------------------------------
if(!require(pacman))
    install.packages('pacman')

pacman::p_load(tidyverse, 
               mclust)

# K-Means
# ----------------------------------------------------------------------------------------------------------------------
data("iris")
df_iris <- iris[,1:4]

wss <- (nrow(df_iris)-1)*sum(apply(df_iris,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(df_iris,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

# So what is the right number of clusters, 2? Or Maybe 3? 
fit_kmeans <- kmeans(df_iris, 2) # 5 cluster solution
# get cluster means
aggregate(df_iris, by=list(fit_kmeans$cluster),FUN=mean)
# append cluster assignment
df_iris_2clust <- data.frame(df_iris, 
                             cluster = fit_kmeans$cluster) 

table(iris$Species, 
      df_iris_2clust$cluster)

# So we see a two-cluster solution is distinguishing setosa from the two "v"s

# Beyond the mans - latent classes with covariance AND means
# ----------------------------------------------------------------------------------------------------------------------
library(mclust)

BIC <- mclustBIC(df_iris)
plot(BIC)

fit_mclust <- Mclust(df_iris, x = BIC)
summary(fit_mclust, parameters = TRUE)

# All patterns are dependent on inputs, but really true of these sorts of EDAs - always defining in relation to other 
# inputs. 

plot(fit_mclust, what='classification')
