##Logistic Regression Lab

#1. 1. Open the Lusitania data and open the Titanic data.  (5 pts).

Lusitania_Data <- read.csv(file.choose(), header = TRUE)
Titanic_Data <- read.csv(file.choose(), header = TRUE)

#2. In the Lusitania data, count the number of 'NAs' per column. Which column has the most NAs? 
# Remove the NAs from the Age column (5 pts).

colSums(is.na(Lusitania_Data))
subset(Lusitania_Data, Age != "NA")
Lusitania_Data_noNA <-subset(Lusitania_Data, Age != "NA")

#3. In the Lusitania data, turn both Fate and Class into factors by creating new columns 
# in the dataframe (5 pts).

Lusitania_Data$Fate_Factor <- as.factor(Lusitania_Data$Fate)
levels(Lusitania_Data$Fate_Factor)

Lusitania_Data$Class_Factor <- as.factor(Lusitania_Data$Department.Class)
levels(Lusitania_Data$Class_Factor)

#4. In the Titanic data, remove the NAs from the Age column and turn
# the 'Survived' variable in to a factor. (5 pts)

colSums(is.na(Titanic_Data))
subset(Titanic_Data, Age != "NA")
Titanic_Data_noNA <-subset(Titanic_Data, Age != "NA")

Titanic_Data$Survived_Factor <- as.factor(Titanic_Data$Survived)
levels(Titanic_Data$Survived_Factor)

#5. In the Titanic data create two logistic regression models that will predict survival. 
# One model with Age and Sex, and a second model Age, Sex, and Fare as predictive variables. 
# What is the VIF for the second model with all three variables?

Titanic_Model <- glm(Survived_Factor ~ Age + Sex, family = binomial, data=Titanic_Data)
summary(Titanic_Model)

Titanic_Model_2 <- glm(Survived_Factor ~ Age + Sex + Fare, family=binomial, data=Titanic_Data)
summary(Titanic_Model_2)

library(ggplot2)
library(car)
vif(Titanic_Model_2)

Overdisptest <- function(LogModel) {
  devi <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  resi <- LogModel$df.residual
  phi <- devi/resi
  R.l <- 1 - devi / nullDev
  cat ("phi =", phi, "\n")
  cat("Hosmer and Lemeshow's R^2 = ", R.l)
}

Overdisptest(Titanic_Model_2)

Overdisptest(Titanic_Model)

#6. In the Lusitania data, create a logistic regression model with Age and Sex as predictive variables
# and the survival (your new Fate factor variable). 

Lusitania_Data_Model <- glm(Fate_Factor ~ Age + Sex, family=binomial, data=Lusitania_Data)
summary(Lusitania_Data_Model)

#7. Plot three graphs using ggplot with sigmoid curves. For the Titanic data, plot Age vs Survived and Fare vs Survived. 
# For the Lusitania data, plot Age vs Survived. What do you notice about the Fare vs Survived graph? (10 pts)

library(ggplot2)
ggplot(Titanic_Data, aes(Age, Survived))+
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family="binomial"))

ggplot(Titanic_Data, aes(Fare, Survived))+
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family="binomial"))

ggplot(Lusitania_Data, aes(Age, Fate_Factor))+
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family="binomial"))

write.csv(Lusitania_Data, "C:/Users/htole/Desktop/BIO 670//Lusitania.csv", row.names=FALSE)

Lusitania_Data <- read.csv(file.choose(), header = TRUE)

