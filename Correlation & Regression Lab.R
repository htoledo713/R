##Correlation & Regression Lab

#1. Enter the data below in a data frame called "data" (review previous notes if you forget how to do this).

data <- data.frame("R1.x" = c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5), "R1.y" = c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68),
                   "R2.x" = c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5), "R2.y" = c(9.14, 8.14, 8.74, 8.77, 9.26, 8.1, 6.13, 3.1, 9.13, 7.26, 4.74),
                   "R3.x" = c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5), "R3.y" = c(7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.42, 5.73),
                   "R4.x" = c(8, 8, 8, 8, 8, 8, 8, 19, 8, 8, 8), "R4.y" = c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 12.5, 5.56, 7.91, 6.89))

#2. Use ggplot to create scatterplots for R1, R2, R3, and R4. Name each one "R1.scatter", "R2.scatter" etc. Make sure to use ggplot to add the 
# least-squares regression line for each plot. (4 pts).

ggplot(data, aes(R1.x, R1.y))+
  geom_point()+
  geom_smooth(method="lm", colour="red", alpha = 0.1)+
  labs(title= "R1.scatter",
       x= "R1.x", y= "R1.y")

ggplot(data, aes(R2.x, R2.y))+
  geom_point()+
  geom_smooth(method="lm", colour="red", alpha = 0.1)+
  labs(title= "R2.scatter",
       x= "R2.x", y= "R2.y")

ggplot(data, aes(R3.x, R3.y))+
  geom_point()+
  geom_smooth(method="lm", colour="red", alpha = 0.1)+
  labs(title= "R3.scatter",
       x= "R3.x", y= "R3.y")

ggplot(data, aes(R4.x, R4.y))+
  geom_point()+
  geom_smooth(method="lm", colour="red", alpha = 0.1)+
  labs(title= "R4.scatter",
       x= "R4.x", y= "R4.y")

#4 Create linear models (use lm() to create an object) and use the summary() function to acquire the intercept, regression coefficient (slope),
# and F value for each regression. List each of those values. (6 pts).

R1.lm <- lm(R1.y ~ R1.x, data = data)
summary(R1.lm)

R2.lm <- lm(R2.y ~ R2.x, data = data)
summary(R2.lm)

R3.lm <- lm(R3.y ~ R3.x, data = data)
summary(R3.lm)

R4.lm <- lm(R4.y ~ R4.x, data = data)
summary(R4.lm)

library(ggplot2)
mammal_sleep <- as.data.frame(msleep)
cor(mammal_sleep$awake, mammal_sleep$bodywt)

mammal_sleep.mr <- lm(sleep_cycle ~ sleep_rem + bodywt + brainwt, data = mammal_sleep)

summary(mammal_sleep.mr)

