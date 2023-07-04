##T-test Lab 

#2
library(ggplot2)
library(pastecs)

Bitter_End <- c(52,44,55,59,44,51,45,47,52,48)
Hopmaster <- c(42,41,52,54,28,40,52,57,57,46)

t.test(Bitter_End,Hopmaster)

Hops_Data<- c(Bitter_End,Hopmaster)

Factor<- factor(c(rep('Bitter End',10), rep('Hopmaster', 10)))
Hops.plot<- data.frame(Hops_Data,Factor)

ggplot(data=Hops.plot, aes(x=Factor,y=Hops_Data))+
  stat_summary(fun.data=mean_se, geom="pointrange")+
  labs(x= "Brand", y= "Hops")

#3
Verns_Cove<- c(4,9,8, 7,4,5,9,10,1,7)
Jills_Cove<- c(4,6,3,9,9,8,11,12,10,13)

t.test(Verns_Cove,Jills_Cove)

#4
blood_pressure<- c(110,170,115,150,108,112,100,175,111,160)
t.test(blood_pressure, mu=120)

#5
pre<- c(8,8,7,7,7,8,9,9,9,7)
post<- c(8,10,9,8,9,9,9,11,9,10)
t.test(pre, post, paired= T)

#6
pre_eggs<- c(2,2,0,3,1,1,2,2,1,1,2,0 )
post_eggs<- c(4,3,2,3,2,2,2,3,2,3,2,2)
t.test(pre_eggs, post_eggs, paired= T)

##Effect Size##
#put the t-test info into an object

Egg_test<- t.test(pre_eggs, post_eggs, paired= T)
Egg_test
#t-value
t<- Egg_test$statistic[[1]]

#degrees of freedom
df<- Egg_test$parameter[[1]]

#calculate r (effect size)
r<- sqrt(t^2/(t^2+df))
round(r,3)
##^^ effect size is the number this gives
