##ANOVA Lab##

ChickWeight<- data.frame(ChickWeight)

anova.ChickWeight<-lm(ChickWeight$weight~factor(ChickWeight$Diet))

anova(anova.ChickWeight)

ChickWeight$Diet_factor<- factor(ChickWeight$Diet)

anova_CW<- aov(ChickWeight$weight~ChickWeight$Diet_factor)
summary(anova_CW)

Diet_1_Mean<- mean(subset(ChickWeight, Diet_factor == "1")$weight)
Diet_2_Mean<- mean(subset(ChickWeight, Diet_factor == "2")$weight)
Diet_3_Mean<- mean(subset(ChickWeight, Diet_factor == "3")$weight)
Diet_4_Mean<- mean(subset(ChickWeight, Diet_factor == "4")$weight)

Tukey.Test<- TukeyHSD(anova_CW, ordered= TRUE)

plot(Tukey.Test)

library(ggplot2)

ggplot(ChickWeight, aes(Diet_factor,weight))+
  stat_summary(fun= mean, geom= "bar", fill= "white", colour= "Black") +
  stat_summary(fun.data=mean_se, geom = "pointrange") +
  labs(x= "Diet Type", y= "Chick Weight") +
  scale_x_discrete(labels=c("Diet 1", "Diet 2", "Diet 3", "Diet 4")) +
  annotate("text", x=c(1.25,2.25,3.25,4.25), y=c(Diet_1_Mean+1,Diet_2_Mean+1,Diet_3_Mean+1, Diet_4_Mean+1),
           label=c("A","B","C", "D"))

library(car)
Prestige_Data<- data.frame(Prestige)

anova_Prestige_Data<-lm(Prestige_Data$prestige~factor(Prestige_Data$type))
anova(anova_Prestige_Data)