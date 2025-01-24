#Install needed packages

library(dplyr) #Formats and runs sequences of operations on dataframes
library(ggplot2) #Used to create graphs
library(lme4) #Optional-linear models use the base R code

###############################################################################################################
#Generate a normal distribution for the histograms

data <- rnorm(100, mean=0, sd=1) #Makes the distribution

data <- as.data.frame(data)

#Replace one value in the distribution with an outlying value

data1 <- data
data1[98,] <- 10

#write.csv(data, "rnorm_orig_distribution.csv")
#write.csv(data1, "rnorm_outlierrep_distribution.csv")

#Manually create data for the boxplot

boxplot <- c(20, 28, 30, 30, 35, 37, 40, 41, 41, 42, 42, 42, 43, 44, 44, 46, 46, 47, 47, 47, 47, 48, 48, 49, 50, 50, 51, 51, 52, 54, 55, 55, 55, 56, 57, 58, 58, 59, 60, 79)

boxplot <- as.data.frame(boxplot)


#Generate an original dataset for the regression line and set the correlation strength

complement <- function(y, rho, x) {
  if (missing(x)) x <- rnorm(length(y)) # Optional: supply a default if `x` is not given
  y.perp <- residuals(lm(x ~ y))
  rho * sd(y.perp) * y + y.perp * sd(y) * sqrt(1 - rho^2)
}

y <- rnorm(25, sd=10) #25 is the number of values
x <- 1:25 # Optional
r <- seq(.6, length.out=1) #Set the correlation strength here. Currently it is set to .6.
X <- data.frame(z=as.vector(sapply(r, function(r) complement(y, r, x))),
                r=ordered(rep(signif(r, 2), each=length(y))),
                y=rep(y, length(r)))

#Save the data
#write.csv(X, "regressionline_data.csv")

#Read in the data
X <- read.csv("regressionline_data.csv")

#Add outliers as separate dataframes

Xnochange <- data.frame(matrix(ncol = 3, nrow = 26))
colnames(Xnochange) <- c("z", "r", "y")
Xnochange$z <- c(X$z, 175)
Xnochange$r <- .60
Xnochange$y <- c(X$y, 50)

Xflat <- data.frame(matrix(ncol = 3, nrow = 26))
colnames(Xflat) <- c("z", "r", "y")
Xflat$z <- c(X$z, -80)
Xflat$r <- .6
Xflat$y <- c(X$y, 50)

Xsteep <- data.frame(matrix(ncol = 3, nrow = 26))
colnames(Xsteep) <- c("z", "r", "y")
Xsteep$z <- c(X$z, 375)
Xsteep$r <- .6
Xsteep$y <- c(X$y, 50)

#Check the model strengths and fits

modX <- lm(z~y, X)
summary(modX)

modXnochange <- lm(z~y, Xnochange)
summary(modXnochange)

modXsteep <- lm(z~y, Xsteep)
summary(modXsteep)

modXflat <- lm(z~y, Xflat)
summary(modXflat)

###############################################################################################################
#FIGURES



#Make a histogram of the normal distribution

#Compute the mean to be plotted as a dashed line
vline.data <- data1 %>% summarize(grp.mean=mean(data)) %>% ungroup()
head(vline.data)

#Output the SDs of both distributions

sd(data$data)
sd(data1$data)


p <- ggplot(data = data, aes(x=data)) +
  geom_histogram(position="identity", alpha=0.5,bins=10) +
  geom_vline(data = vline.data, aes(xintercept=grp.mean),linetype="dashed",size=1)

p + 
  
  theme(legend.title = element_blank(), legend.position = 'none',
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "value", y ='frequency') +
  theme(strip.text.x = element_blank()) +
  theme_classic() +
  theme(legend.title = element_blank())



#Make a boxplot

p <- boxplot %>%
  ggplot(aes(x = , y = boxplot)) +
  geom_boxplot(size = 1)

p + 
  
  labs(x = "", y = "") +
  theme(strip.text.x = element_blank()) +
  theme(strip.background = element_blank()) +
  theme_classic() +
  scale_y_continuous(limits = c(20, 80), breaks = c(20,30,40,50,60,70,80)) +
  theme(text = element_text(size=20)) +
  theme(legend.title = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.line.x.bottom=element_line(size=1)) +
  theme(axis.line.y.left=element_line(size=1))




#Create a scatterplot for the automatically generated regression line

ggplot(Xnochange, aes(y,z), color = "black") + 
  geom_smooth(method="lm", color="black", se = FALSE, show.legend = FALSE, size = 1.5) + 
  geom_point(aes(fill="r"), color="black", fill="black", alpha=1, shape=21, size = 3) +
  scale_y_continuous(limits = c(-100, 200), breaks = c(-100,-50,0,50,100,150, 200, 250,300,350,400)) +
  scale_x_continuous(limits = c(-30, 60), breaks = c(-30,0,30,60)) +
  labs(x="", y = "") +
  theme_classic() +
  xlab("x-axis") +
  ylab("y-axis") +
  theme(text = element_text(size=25)) +
  theme(axis.text = element_text(size=25)) +
  theme(legend.title = element_blank()) +
  guides(fill="none")

