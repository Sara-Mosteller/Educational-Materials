
#Linear models with continuous predictors

library(MASS) #to create the randomized datasets


#Correlation coefficient and confidence intervals.
#Assumptions: data consists of two normally distributed, continuous variables with a linear relationship. 
#NOTE: This is a very high correlation coefficient (r) for educational purposes, while most relationships
#that are observed in actual data would not be so strong. 

#Create a dataframe with two variables, each with 10 observations, and a correlation coefficient of 0.94. 

cor_data <- as.data.frame(mvrnorm(10, mu = c(0,0), 
                                  Sigma = matrix(c(1,0.94,0.94,1),, ncol = 2), 
                                  empirical = TRUE))

#Scale these into positive integer variables

cor_data$var_a <- (cor_data$V1 - min(cor_data$V1))*100+10
cor_data$var_b <- (cor_data$V2 - min(cor_data$V2))*100+10

#Here are the final variables generated:

var_a <- c(10, 188, 130, 297, 114, 109, 64, 332, 192, 110)
var_b <- c(72, 166, 177, 298, 119, 115, 10, 349, 161, 127)
n <- 10

mean_a <- (10 + 188 + 130 + 297 + 114 + 109 + 64 + 332 + 192 + 110) / 10
standard_deviation_a <-  sqrt(((10 - mean_a)^2 + (188 - mean_a)^2 + (130 - mean_a)^2 + (297 - mean_a)^2 + (114 - mean_a)^2 +
                                 (109 - mean_a)^2 + (64 - mean_a)^2 + (332 - mean_a)^2 + (192 - mean_a)^2 + (110 - mean_a)^2) / 9 )
standard_a <-  c((10 - mean_a)/standard_deviation_a, (188 - mean_a)/standard_deviation_a, (130 - mean_a)/standard_deviation_a,
                 (297 - mean_a)/standard_deviation_a, (114 - mean_a)/standard_deviation_a, (109 - mean_a)/standard_deviation_a,
                 (64 - mean_a)/standard_deviation_a, (332 - mean_a)/standard_deviation_a, (192 - mean_a)/standard_deviation_a,
                 (110 - mean_a)/standard_deviation_a)

mean_b <- (72 + 166 + 177 + 298 + 119 + 115 + 10 + 349 + 161 + 127) / 10
standard_deviation_b <- sqrt(((72 - mean_b)^2 + (166 - mean_b)^2 + (177 - mean_b)^2 + (298 - mean_b)^2 + (119 - mean_b)^2 +
                                (115 - mean_b)^2 + (10 - mean_b)^2 + (349 - mean_b)^2 + (161 - mean_b)^2 + (127 - mean_b)^2) / 9)
standard_b <- c((72 - mean_b)/standard_deviation_b, (166 - mean_b)/standard_deviation_b, (177 - mean_b)/standard_deviation_b,
                (298 - mean_b)/standard_deviation_b, (119 - mean_b)/standard_deviation_b, (115 - mean_b)/standard_deviation_b,
                (10 - mean_b)/standard_deviation_b, (349 - mean_b)/standard_deviation_b, (161 - mean_b)/standard_deviation_b,
                (127 - mean_b)/standard_deviation_b)

standard_a_times_b <- standard_a*standard_b

#Compute correlation coefficient and t statistic

r <- sum(standard_a_times_b)/ (n-1) #.9398

t <- r*sqrt(n-2) / sqrt(1-r^2) #7.7796

#Approximate p using the t distribution

p <- 2*pt(-abs(t),df=length(var_a)-2) #.00005


#Compute the 95% CI

z_from_r = log((1+r) / (1-r)) / 2 #Convert r to a z score

lower_bound_z <- z_from_r - (1.96)/sqrt(n-3) #find the lower bound in terms of z

upper_bound_z <- z_from_r + (1.96)/sqrt(n-3) #find the lower bound in terms of z

lower_bound <- (exp(2*lower_bound_z)-1)/(exp(2*lower_bound_z)+1) #convert back to r

upper_bound <- (exp(2*upper_bound_z)-1)/(exp(2*upper_bound_z)+1) #convert back to r




#Check answers

cor.test(var_a, var_b) # r = .9398, p = .00005, 95% CI .7598-.9860

mean(var_a) # 154.6
mean(var_b) #159.4
sd(var_a) #99.8
sd(var_b) #100.1
