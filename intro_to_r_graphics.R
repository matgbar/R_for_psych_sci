# SmateR not HardeR Workshop
# R in the Psychological Science Production Cycle
#
# Exploratory Data Analysis 1 of X 
#
# Questions? contact mbarstead@deadreckoning.consulting

# Loading in packages
# ----------------------------------------------------------------------------------------------------------------------
if(!require(pacman))
    install.packages('pacman')

pacman::p_load(tidyverse, 
               plotly, 
               corrgram, 
               psych, 
               cowplot, 
               psych, 
               yarrr)

# Exploring toy iris data 
# ----------------------------------------------------------------------------------------------------------------------
data("iris")
head(iris)

# Simple Univariate Case: 
# ----------------------------------------------------------------------------------------------------------------------
# HISTOGRAM:
g_uni_1 <- ggplot(data = iris, 
                  aes(x = Sepal.Width))+
    geom_histogram(bins = 12)
g_uni_1

# DENSITY PLOT: 
g_uni_2 <- ggplot(data = iris, 
                  aes(x = Sepal.Width))+
    geom_histogram(bins = 12,
                   aes(y = ..density..))+
    stat_density()
g_uni_2

# Notice that layers are added sequentially
# I also needed to add `y=..density..` to the `aes()` argument in `geom_histogram` layer to scale y axis 
# (Try on your own without adding `y=..density..` and see what happens to the plot)

# A BETTER DENSITY PLOT: 
g_uni_3 <- ggplot(data = iris, 
                  aes(x = Sepal.Width))+
    geom_histogram(bins = 12,
                   aes(y = ..density..))+
    stat_density(alpha = .5)
g_uni_3

# CONSEQUENCES OF SCALING: 
g_uni_4a <- ggplot(data = iris, 
                   aes(x = Sepal.Width))+
    geom_histogram(bins = 12)+
    scale_y_continuous(limits = c(0,100))

g_uni_4b <- ggplot(data=iris, 
                   aes(x = Sepal.Width))+
    geom_histogram(bins = 12)+
    scale_x_continuous(limits = c(0, 4))

g_uni_4c <- ggplot(data=iris, 
                   aes(x=Sepal.Width))+
    geom_histogram(bins=12)+
    coord_cartesian(xlim=c(0,4))

cowplot::plot_grid(g_uni_4a, g_uni_4b, g_uni_4c, nrow = 3)

# Bi-Variate Plotting
# ----------------------------------------------------------------------------------------------------------------------

# SIMPLE BIVARIATE PLOT
g_bi_1 <- ggplot(data=iris, 
                 aes(x = Sepal.Width, 
                     y = Sepal.Length))+
    geom_point()
g_bi_1

# IMPROVING ON THE BIVARIATE PLOT
g_bi_2 <- ggplot(data = iris, 
                 aes(x = Sepal.Width, 
                     y = Sepal.Length, 
                     group = Species, 
                     color = Species, 
                     fill = Species))+
    geom_point(aes(shape = Species))+
    stat_smooth(se = FALSE, 
                method = 'lm')+
    stat_ellipse(geom = 'polygon',
                 alpha = .45, 
                 type = 'norm')+
    labs(title = 'The Association between Sepal Length and Sepal Width as a Function of Species', 
         x = 'Sepal Width', 
         y = 'Sepal Length', 
         subtitle = 'Examples from the iris data set', 
         caption = 'Presented on November 24, 2019')
g_bi_2

# If two dimensions are good... 
# ----------------------------------------------------------------------------------------------------------------------
plot_ly(data = iris,
        x = ~Sepal.Width, 
        y = ~Sepal.Length, 
        z = ~Petal.Length, 
        type = "scatter3d", 
        mode = "markers", 
        color = ~Species)

# Plotly is likely best for presentations, websites, and true exploration 

# Quick Exploratory Summaries
# ----------------------------------------------------------------------------------------------------------------------

# CORRGRAM 
corrgram(iris[,1:4], 
         order=TRUE, 
         lower.panel = panel.conf, 
         upper.panel = panel.pie, 
         diag.panel = panel.density, 
         col.regions = colorRampPalette(c('navy', 'lightblue', 'white', 'pink', 'darkred'))
         )
# lots and lots of options here. 

# PSYCH PACKAGE PAIRS.PANELS() 
pairs.panels(iris[,1:4])

# YARRR PACKAGE AND PIRATE PLOTS

# Requires data be reshaped (wants "stacked" data)
iris_stacked <- reshape::melt(iris[,1:4])
str(iris_stacked)

pirateplot(value~variable, 
           data=iris_stacked, 
           ylab = 'Measurement (inches)', 
           xlab = 'Variable', 
           main = 'Sepal and Petal Measurement Distributions')

# But the variable names are a little annoying: 
iris_stacked$variable <- gsub(".", " ", iris_stacked$variable, fixed = TRUE)
str(iris_stacked)

pirateplot(value~variable, 
           data=iris_stacked, 
           ylab = 'Measurement (inches)', 
           xlab = 'Variable', 
           main = 'Sepal and Petal Measurement Distributions')

vignette('pirateplot', package = 'yarrr')