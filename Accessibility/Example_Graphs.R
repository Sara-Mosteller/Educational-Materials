
#load the libraries
#install.packages("dplyr")
library(dplyr) #To carry out operations on data frames
#install.packages("ggplot2")
library(ggplot2) #To create customizable graphs
#install.packages("sn")
library(sn) #To create the skewed smooth curve
#install.packages("MASS")
library(MASS) #to create randomized datasets

#Figures Example 1-Normal and skewed curves

#Plot a normal curve

curve(dnorm(x, 0, 1), from=-4, to=4, xlab = "Standard Units", ylab = "Probability", main="Example Normal Curve")


#Plot a skewed curve

x <- seq(-3, 3, 0.01)
# Right-skewed curve
plot(x, dsn(x, xi = 0, omega = 1, alpha = 3), type = "l", 
     col = "black", lwd = 2, 
     xlab = "Standard Units", ylab = "Density", main = "Example Skewed Curve")



#Figure Example 2-Histogram

data <- rnorm(100, 0, 1)

#Create a histogram in base R

hist1 <- hist(data, main="Example Histogram", 
        ylab = "Frequency", xlab="x values", col = c('blue'),
        breaks = seq(min(data), max(data), length.out = 6))
        axis(1, at = seq(-4, 4, 1), labels = seq(-4, 4, 1), lwd = 0, lwd.ticks = 1)
        axis(2, at = seq(0, 50, 10), labels = seq(0, 50, 10), lwd = 0, lwd.ticks = 1)

hist1

#Figure Example 3: Box plot

data <- c(20, 35, 40, 41, 45, 50, 50, 50, 50, 50, 50, 50, 50, 51, 51, 51, 55, 55, 56, 56, 56, 56, 60, 60, 60, 60, 60, 60, 60, 62, 65, 65, 66, 67, 68, 68, 68, 68, 70, 70, 70, 70, 74, 75, 77, 80, 80, 80, 96)

boxplot <- boxplot(data, horizontal=TRUE, main="Ages of Club Members", xlab="Age in Years", ylab = "")
boxplot

boxplot <- boxplot(data, horizontal=FALSE, main="Ages of Club Members", xlab="", ylab = "Age in Years")
boxplot




#Figure Example 4-Bar graph

categories <- c("Category A", "Category B", "Category C", "Category D", "Category E")
percentages <- c(25, 45, 20, 3, 7)
data <- cbind(categories, percentages)
data <- as.data.frame(data)
data$categories <- as.factor(data$categories)
data$percentages <- as.numeric(data$percentages)

#Plot a bar graph in alphabetical order

bar1 <- barplot(height=data$percentages, names = data$categories, main="Example Bar Graph",
                xlab="Categories", ylab = "Percentage")
bar1


#Plot a bar graph in descending order

data <- data[order(data$percentages, decreasing = TRUE),] 

bar1 <- barplot(height=data$percentages, names = data$categories, main="Example Bar Graph",
                xlab="Categories", ylab = "Percentage")
bar1




#Figure Example 5-Pie chart

games <- c("Monopoly", "Candyland", "Jenga", "Chess", "Twister", "Poker", "Uno")
data <- c(11, 12, 6, 8, 10, 4, 2)
percentage <- data/sum(data)*100

pie1 <- pie(data/sum(data)*100, labels = games, main = "Percentage of Players by Game Type", col = rainbow(length(games)))
pie1


#Figure Example 6-Dot plot or dot chart

values <- as.numeric(c(0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 5, 5))
values <- as.data.frame(values)

#Vertical 
ggplot(values, aes(x=values)) +
  geom_dotplot(method = 'dotdensity') +
  scale_y_continuous(NULL, breaks = NULL) +
  ggtitle("Example Dot Plot")

#Horizontal
ggplot(values, aes(x=1, y = values)) +
  geom_dotplot(method = 'dotdensity', binaxis="y") +
  scale_x_continuous(NULL, breaks = NULL) +
  ggtitle("Example Dot Plot")

#Example Figure 7: Line Graph

averages_1 = c(5, 10, 20, 12, 25, 19, 30)
averages_2 = c(20, 23, 18, 15, 10, 16, 7)


range <- length(averages_1)
legend <- c("Variable 1", "Variable 2")

# set up graph
xrange <- range(1:7)
yrange <- round(range(c(5,30)))
colors <- rainbow(length(1:2))
plot(xrange, yrange, type="n",
     xlab="Sampling Points",
     ylab="Averages")

# add the lines
for (j in 1:1){
  lines(x = c(1:7), y = averages_1, type="l", lwd=2, lty=1, col=colors[j])
}

for (j in 2:2){
  lines(x = c(1:7), y = averages_2, type="l", lwd=2,lty = 1, col=colors[j])
}


#add annotation

cex.axis=300
title("Example Line Graph")
legend("topleft", cex=.8, xpd=TRUE,
       as.character(legend),
       fill=colors)



#Example Figure 8: Scatter plot

#Create two variables of 100 values with a set correlation coefficient (in this case, r = 0.36) 
set.seed(0)
data <- as.data.frame(mvrnorm(100, mu = c(0,0), 
                              Sigma = matrix(c(1,0.36,0.36,1),, ncol = 2), 
                              empirical = TRUE))

#Scale these into positive integer variables
data$variable_x <- (data$V1 - min(data$V1))*1000+10
data$variable_y <- (data$V2 - min(data$V2))*200+30

#Check the correlation coefficient
cor.test(data$variable_x, data$variable_y) #r = .36, p = .0002.


#Plot
plot <- ggplot(data = data, mapping = aes(variable_x, variable_y)) #requires ggplot2
plot + geom_point() +
  coord_cartesian(xlim = c((min(data$variable_x)-100), (max(data$variable_x)+100)), ylim = c((min(data$variable_y)-100), (max(data$variable_y)+100))) +
  labs(
    title = "Example Scatterplot",
    x = "X Variable",
    y = "Y Variable"
  )



#Example Figure 9: Linear regression plot

slope <- .3600*sd(data$variable_y)/sd(data$variable_x)
intercept <- mean(data$variable_y) - slope*mean(data$variable_x)
  
p <- ggplot(data = data, mapping = aes(x = variable_x, y = variable_y)) #requires the ggplot2 package
lm_eq <- function(x) slope * x + intercept
p + stat_function(fun = lm_eq) + geom_point() +
  coord_cartesian(xlim = c((min(data$variable_x)-100), (max(data$variable_x)+100)), ylim = c((min(data$variable_y)-100), (max(data$variable_y)+100))) +
  labs(
    title = "Example Linear Regression",
    x = "Independent Variable",
    y = "Dependent Variable"
  )



#Example Figure 9: Logit regression plot

#Create the data
set.seed(0)
n <- 1000
x <- rnorm(n)
z <- 1 + 2*x  # linear predictor
#Transform to probabilities
pr <- 1 / (1 + exp(-z))
#Generate binomial response variable
y <- rbinom(n, size = 1, prob = pr)

data <- cbind(x, y)
data <- as.data.frame(data)

#Fit the model

model <- glm(data$y ~ data$x, data = data, family = binomial)
coefficients <- coef(model)

prob_y <- 1/(1+exp(-(coefficients[1]+coefficients[2]*data$x)))


#Plot the logit model
ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  coord_cartesian(xlim = c(min(data$x), max(data$x)), ylim = c(min(data$y), max(data$y))) +
  labs(
    title = "Example Logistic Regression",
    x = "",
    y = "P(y=1)"
  )

range_zero <- data %>%
  filter(y==0)

min(range_zero)
max(range_zero)

range_one <- data %>%
  filter(y==1)

min(range_one)
max(range_one)

