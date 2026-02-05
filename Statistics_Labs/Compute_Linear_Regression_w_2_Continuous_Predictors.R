
#Linear models with continuous predictors

library(MASS) #to create the randomized datasets
library(ggplot2) #to plot the data
library(dplyr) #to carry out operations on data frames

#Fixed effects linear regression with two continuous predictor variables (x1 and x2) and a continuous outcome variable (y). 


#Assumptions include that all variables (x1, x2, y) are normally distributed,
#That the predictors (x1 and x2) are not correlated with each other,
#that the residuals are evenly distributed, and 
#that the relationships between the x and y variables are linear.

#Create two variables of 100 values and set correlation coefficients between each pair. 

set.seed(0)
data <- as.data.frame(mvrnorm(100, mu = c(0,0,0), 
                             Sigma = matrix(c(1, -.05, .50,-.05, 1, -.25, 0.50, -.25, 1),, ncol = 3), 
                             empirical = TRUE))

#Scale these into positive integer variables
data$var_x1 <- (data$V1 - min(data$V1))*1000+10
data$var_x2 <- (data$V2 - min(data$V2))*100+20
data$var_y <- (data$V3 - min(data$V3))*200+30

#Check the correlation coefficient
cor.test(data$var_x1, data$var_y) #r = .50, p =  <.001.
cor.test(data$var_x2, data$var_y) #r = -.25, p = .0121.
cor.test(data$var_x1, data$var_x2) #r = -.05, p = .6213.

#Check that the variables are normally distributed.
#The histograms should appear roughly in the shape of a normal curve. 

hist(data$var_x1)
hist(data$var_x2)
hist(data$var_y)

#Round all values to the nearest decimal place

data$var_x1 <- round(data$var_x1, 0)
data$var_x2 <- round(data$var_x2, 0)
data$var_y <- round(data$var_y, 0)

#Compute the mean of each variable
mean_x1 <- mean(data$var_x1)
mean_x2 <- mean(data$var_x2)
mean_y <- mean(data$var_y)



#Compute the model coefficients

#Set up the normal equation

n = 100

# Create design matrix X of predictive variables with the intercept set to 1
X <- cbind(1, data$var_x1, data$var_x2, data$var_x1*data$var_x2)  # First column is intercept (1s)
y = data$var_y

# Compute the vector theta using the normal equation
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y

# Output model coefficients
beta_hat #Use this output below

intercept <- 252.6 #These are from beta_hat
slope_x1 <- .1675
slope_x2 <- .2563
slope_x1x2 <- -.00027
 
#Compute the predicted values and square error terms from these coefficients.
#The equation is: y_pred = 252.6 + .1675(X1) + .2563(X2) - .00027(X1*X2)

for (i in 1:nrow(data)) {
  data$y_pred[i] <- (252.6 + .1675*data$var_x1[i] + .2563*data$var_x2[i] -.00027*data$var_x1[i]*data$var_x2[i]) 
  data$model_variance[i] <- (data$y_pred[i] - mean_y)^2
  data$residuals[i] <- (data$var_y[i] - data$y_pred[i]) #residuals
  data$residuals_squared[i] <- (data$var_y[i] - data$y_pred[i])^2 #squared residuals
}

n_params <- 4 #intercept, x1, x2, x1:x2
n_predictors <- 3 #x1, x2, x1:x2
n <- 100 #100 observations
rss <- sum(data$residuals_squared)
ess <- sum(data$model_variance)
msm <- ess/(n_predictors) #mean square model
mse <- rss/(n - n_predictors - 1) #mean square error
rmse <- sqrt(mse) #root mean square error
se <- sqrt(rss / (n - n_params)) #standard error of the full model
total_variance <- sum(data$total_y_var)


#Compute F and t stats and corresponding p values for the whole model

R_squared <- 1 - rss/total_variance

R_squared_adjusted <- 1 - ((1-R_squared)*(n-1)/(n-n_predictors-1))
  
F_test <- msm/mse #the F test stat, should be around 14.95. Note, it is also possible to find the F stat using R squared. 

pF <- pf(14.95, (n_params - 1), (n - n_params), lower.tail = FALSE) #p value for F test.



# Calculate the standard error for each parameter

vcov <- solve(t(X) %*% X) * sum(data$residuals^2)/(nrow(X) - ncol(X))

se <- sqrt(diag(vcov)) #This takes the square root of each of the the values along this matrix diagonal (where the row number equals the column number)



#Compute test statistics and p values for the intercept, X1, X2, and X1:X2

t_intercept <- intercept/se[[1]]

pt_intercept <- 2*pt(-abs(t_intercept),df=nrow(data)-n_params) # p value for intercept t test

t_var_x1 <- slope_x1/se[[2]] #The t test stat for the slope

pt_var_x1 <- 2*pt(-abs(t_var_x1),df=nrow(data)-n_params) # p value for slope t test

t_var_x2 <- slope_x2/se[[3]] #The t test stat for the slope

pt_var_x2 <- 2*pt(-abs(t_var_x2),df=nrow(data)-n_params) # p value for slope t test

t_var_x1x2 <- slope_x1x2/se[[4]] #The t test stat for the slope

pt_var_x1x2 <- 2*pt(-abs(t_var_x1x2),df=nrow(data)-n_params) # p value for slope t test



#Plot the regression line and data points, the fitted values and the residuals


plot(data$var_x1, data$var_y)
plot(data$var_x2, data$var_y)
plot(data$var_x1*data$var_x2, data$var_y) #This does not have a typical distribution. 

#Plot the fitted values
fitted_values <- slope_x1*data$var_x1 + slope_x2*data$var_x2 + slope_x1x2*data$var_x1*data$var_x2 + intercept
plot(fitted_values) #plots the fitted values

#Plot the residuals
residuals <- data$var_y - fitted_values
plot(residuals) +
  abline(0, 0)

#Check the quantiles of the residuals
quantile(residuals)



#Check your answers

fixedeffect.lm <- lm(data$var_y ~ data$var_x1*data$var_x2)
summary(fixedeffect.lm) #The values here should correspond to the above regression line, t stat and R squared estimates

#Check the variance/covariance matrix
all.equal(vcov(fixedeffect.lm), 
          solve(t(X) %*% X) * sum(data$residuals^2)/(nrow(X) - ncol(X)),
          check.attributes = FALSE)

#Plot the fitted values and the residuals from the lm function

plot(fixedeffect.lm$fitted.values)

plot(fixedeffect.lm$residuals)
abline(0, 0)

plot(fixedeffect.lm) #additional plots
