
#Calculations with one sample (Z scores and one-sample t tests)

library(MASS) #to create the randomized datasets

#Assumptions: data are normally distributed (left-right symmetrical and distributed in the shape of a normal curve). 

#Create a normal distribution of 100 values with a mean of 100 and a standard deviation of 15.  

var <- rnorm(100, 100, 15) #n = 100, mean = 100, sd = 15

var <- as.data.frame(var)

n <- 100

#Confirm that the mean is approximately 100, that the standard deviation is approximately 15 and 
#visualize to confirm that the distribution is approximately in the shape of a normal curve. 
sample_mean <- mean(var$var)
sample_standard_deviation <- sd(var$var)
hist(var$var)
qqnorm(var$var)
qqline(var$var) #If the Q-Q plot follows a straight line, then the distribution is normal.

#Assume that this sample was randomly drawn from an underlying population 
#and that the population mean and standard deviation are already known. 
#Compute a z score for the sample. 

population_mean <- 100
population_standard_deviation <- 15

z = (sample_mean - population_mean)/population_standard_deviation #Should be very close to 0, showing that the sample 
#mean is consistent with the population mean. 

#Compute the 95% CI with the critical z value of 1.96. 
upper_bound <- sample_mean + 1.96*population_standard_deviation/n
lower_bound <- sample_mean - 1.96*population_standard_deviation/n


#Now, assume that the sample and population mean may be different. 
#In this case, we already have a hypothesized population mean of 90. 
#Conduct a t test to see whether the sample mean is significantly different from the population mean.

#Set a hypothesized population mean that might be different from the sample mean.
#The population standard deviation in this case is unknown. 
population_mean <- 90


#Conduct a one-sample t-test
t <- (sample_mean - population_mean) / (sample_standard_deviation/sqrt(n))
p <- 2*pt(-abs(t),df= length(var$var)-1)

#Compute the 95% CI

#Compute the critical t value 
crit_t <- qt(1-.05/2, length(var$var)-1) #.05 is the alpha

#Compute the upper and lower bounds of the confidence interval. 
upper_bound <- sample_mean + crit_t*sample_standard_deviation/sqrt(n)
lower_bound <- sample_mean - crit_t*sample_standard_deviation/sqrt(n)


#Check your answers.

t.test(var$var, mu = population_mean, alternative = "two.sided")

