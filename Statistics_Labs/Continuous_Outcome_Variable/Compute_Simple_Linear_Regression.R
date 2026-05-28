
#Linear models with continuous predictors

library(MASS) #to create the randomized datasets
library(lme4) #to check answers
library(ggplot2) #to plot the linear model


#Simple linear regression
#One continuous predictor variable x and one outcome variable y


#Assumptions include that both variables are normally distributed,
#that the residuals have evenly distributed variance, and 
#that the relationship between the two variables is linear.

#Create two variables of 100 values with a set correlation coefficient (in this case, r = 0.36) 

data <- as.data.frame(mvrnorm(100, mu = c(0,0), 
                             Sigma = matrix(c(1,0.36,0.36,1),, ncol = 2), 
                             empirical = TRUE))

#Scale these into positive integer variables
data$var_x <- (data$V1 - min(data$V1))*1000+10
data$var_y <- (data$V2 - min(data$V2))*200+30

#Check the correlation coefficient
cor.test(data$var_x, data$var_y) #r = .36, p = .0002.

#Check that the variables are normally distributed.
#The histograms should appear roughly in the shape of a normal curve. 

hist(data$var_x)
hist(data$var_y)


#Compute means and standard deviations using base functions in R.

slope <- .3600*sd(data$var_y)/sd(data$var_x)

intercept <- mean(data$var_y) - slope*mean(data$var_x)

#The regression function is: y = slope(x) + intercept


#Compute variables needed for the linear model

#Create empty variables for the for loop below. 

data$pred_y <- NA
data$total_x_var <- NA
data$es_var <- NA
data$rs_var <- NA
data$total_y_var <- NA


mean_x <- mean(data$var_x) 
mean_y <- mean(data$var_y) 


for (i in 1:nrow(data)) {
  data$pred_y[i] <- slope*data$var_x[i] + intercept #predicted y values
  data$total_x_var <- (data$var_x - mean_x)^2 #total variance in x
  data$es_var[i] <- (data$pred_y[i] - mean_y)^2 #explained variance in model 
  data$rs_var[i] <- (data$var_y[i] - data$pred_y[i])^2 #residual, or unexplained, variance in model
  data$total_y_var[i] <- (data$var_y[i] - mean_y)^2 #total variance in y
}

ssx <- sum(data$total_x_var) #sum of total variance in x
ess <- sum(data$es_var) #sum of explained variance
rss <- sum(data$rs_var) #sum of residual variance
total_variance_1 <- variance <- ess + rss #sum of total variance in y
total_variance_2 <- sum(data$total_y_var) #sum of total variance in y


#Compute F and t stats and corresponding p values.

n_params <- 2 #x and y 
n <- 100 #100 observations
msm <- ess/(n_params - 1) #mean square model
mse <- rss/(n - n_params) #mean square error
rmse <- sqrt(mse) #root mean square error
se <- sqrt(rss / (n - 2)) #standard error of model


R_squared <- 1 - rss/total_variance_1

R_squared_adjusted <- 1 - ((1-R_squared)*(n-1)/(n-n_params))


  
F_test <- msm/mse #the F test stat, should be around 14.59

pF <- pf(14.59, (n_params - 1), (n - n_params), lower.tail = FALSE) #p value for F test


  
t_intercept <- intercept / sqrt(mse*(1/n+mean_x^2/ssx)) #The t test stat for the intercept

pt <- 2*pt(-abs(t_intercept),df=length(data$var_x)-2) # p value for intercept t test

t_var_x <- (slope-0) / (rmse / sqrt(ssx)) #The t test stat for the slope

pt <- 2*pt(-abs(t_var_x),df=length(data$var_x)-2) # p value for slope t test



#Plot the regression line and data points, the fitted values and the residuals

#Plot the regression line and the data
p <- ggplot(data = data, mapping = aes(x = var_x, y = var_y)) #requires the ggplot2 package
lm_eq <- function(x) slope * x + intercept
p + stat_function(fun = lm_eq) + geom_point()

#Plot the fitted values
fitted_values <- slope*data$var_x + intercept
plot(fitted_values) #plots the fitted values

#Plot the residuals
residuals <- data$var_y - (slope*data$var_x + intercept)
plot(residuals) +
  abline(0, 0)

#Check the quantiles of the residuals
quantile(residuals)



#Check your answers

simple.lm <- lm(data$var_y ~ data$var_x)
summary(simple.lm) #The values here should correspond to the above regression line, t stat and R squared estimates
#influence(simple.lm) 

anova(simple.lm) #The values here should correspond to the above F stat and sum of square estimates. 


#Plot the data points with the regression line, the fitted values and the residuals from the lm function

plot <- ggplot(data = data, mapping = aes(var_x, var_y)) #requires ggplot2
plot + geom_point() +
  geom_smooth(method='lm', formula= y~x)

plot(simple.lm$fitted.values)

plot(simple.lm$residuals)
abline(0, 0)

plot(simple.lm) #additional plots
