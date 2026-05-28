
#Random effect model with continuous outcome variable

library(MASS) #to create the randomized datasets
library(lme4) #to check answers
library(ggplot2) #to plot the linear model
library(dplyr) #to format data frame
library(reshape2) #to format data frame



#Assumptions include that:
#Errors are normally distributed
#Equal spread, or variance, across different levels of the random effect
#Errors in the model within a level of the random effect are correlated
#Errors across different levels of the random effect are uncorrelated because, 
#within the random effect aside, observations are independently drawn at random from a larger population

#If unequal variances are found between levels of a random effect, use a robust estimation method
#or a more general method for computing squared errors (such as generalized least squares). 


#Create 10 datasets of 20 values each with different means

# Set parameters
n_levels <- 10
n_bylevel <- 20
n_obs <- 20
common_sd <- 2
means <- seq(5, 15, length.out = n_levels)  # Different means from 5 to 15

# Generate datasets
set.seed(0)  # For reproducibility
dataset <- lapply(means, function(m) rnorm(n_bylevel, mean = m, sd = common_sd))

# Name the list elements (e.g., RElevel1, RElevel2, ...)
names(dataset) <- paste0("RELevel", 1:n_levels)
# Access individual levels: dataset[[1]] for first level, etc.   

#Format the list into a dataframe with random effect leves A:J
data <- as.data.frame(dataset)
colnames(data) <- c("A","B", "C", "D", "E", "F", "G", "H", "I", "J")
data <- melt(data, variable.name ="random_effect", value.name ="var_y")





#Create a dataset with different means AND SDs
#set.seed(0)

#data_info <- data.frame(random_effect = LETTERS[1:10],
 #                        MEAN = runif(10, 5, 10),
 #                        SD = runif(10, 1, 3))

#library(data.table)
#data <- setDT(data_info)[, .(var_y = rnorm(20, mean = MEAN, sd = SD)), by = .(random_effect)]

#n_levels = 10
#n_obvs = 200

#data <- as.data.frame(data)





#Check that the variables are normally distributed.
#The histograms should appear roughly in the shape of a normal curve. 

#Check full distribution
hist(data$var_y)

#Check that the levels have similar spread
plot <- ggplot(data = data, mapping = aes(var_y)) #requires ggplot2
plot + geom_histogram() +
  facet_wrap(~random_effect)


#Compute the means of each level of the random effect

level_means <- data %>%
  group_by(random_effect) %>%
  summarize(level_means = mean(var_y)) %>%
  ungroup()

#Scale the means

scaled_level_means <- scale(level_means$level_means) #This is different from the intercept values created in lme4



#Compute variance components

data$var_y <- round(data$var_y, 2) #For convenience, first round the y values in the dataset to 2 decimal places


overall_mean <- mean(data$var_y)
overall_sd <- sd(data$var_y)
group_size <- 20 #Use the average group size if sizes are unequal

within_level_variance <-  data %>%
  group_by(random_effect) %>%
  summarize(variance = sd(var_y)^2) %>%
  ungroup() %>%
  summarize(residual_variance = mean(variance)) #the residual variance is the mean of the variance within each level of the random effect. 

between_level_variance <- data %>% #This is the between-subjects or between-levels variance
  group_by(random_effect) %>% 
  summarize(level_mean = mean(var_y)) %>%
  mutate(squared_residuals = (level_mean-overall_mean)^2) %>%
  summarize(SS_residuals_between = sum(squared_residuals)/(n_levels-1))
  
random_effect_variance <- between_level_variance - within_level_variance/(group_size)
  
random_effect_sd <- sqrt(random_effect_variance)

#Compute the total variance without random effect
total_variance <-  data %>%
  summarize(variance = sd(var_y)^2) %>%
  summarize(total_variance = mean(variance)) 

total_variance_sd <- sqrt(total_variance)
  
model_intercept <- overall_mean # the mean of the y values - the coefficients of the fixed effects, but there is no fixed effect here. 
model_intercept_se <- sd(level_means$level_means)/sqrt(n_levels) #the SE of the group means.
model_intercept_se
model_intercept_t <- model_intercept/model_intercept_se
model_intercept_t

#Check your answers
  
random.lm <- lmer(var_y ~ (1|random_effect), data) #allows the outcome variable to have a different intercept at each level of the random effect.   
summary(random.lm) #The values here should correspond to the above.

#Plot the full model

plot(random.lm)
abline(0, 0)

#Reproduce intercept values using components from the model

zt <- getME(random.lm, "Zt")
random_intercepts <- random_effect_variance$SS_residuals_between/within_level_variance$residual_variance * zt %*% residuals(random.lm) #It would be nice to add this entirely by-hand
random_intercepts

#Check your answers
scaled_intercepts <- ranef(random.lm) #Output scaled intercept values by level
scaled_intercepts
