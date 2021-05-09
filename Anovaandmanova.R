data1 <- read.csv("DietWeightLoss.csv")
View(data1)
names(data1)
class(data1$WeightLoss)
class(Diet)
data1$Diet <- as.factor(data1$Diet)
str(data1)
levels(data1$Diet)
boxplot(data1$WeightLoss~data1$Diet)

#H0 : Mean weight loss is same for all deits#

model <- aov(data1$WeightLoss~data1$Diet)
summary(model)

#Null hypothesis is rejected 


#tukey test

plot(TukeyHSD(model))

#kruskal test

kruskal.test(data1$WeightLoss~data1$Diet)


###MANOVA

data2 <- read.csv("test.csv")
View(data2)
str(data2)
data2$teaching_method <- as.factor(data2$teaching_method)
summary(data2)

m1 <- lm(data2$t1 ~ data2$teaching_method)
m2 <- lm(data2$s1 ~ data2$teaching_method)

summary(m1)
summary(m2)

y <- cbind(data2$s1,data2$t1)
model1 <- manova(y~data2$teaching_method)
summary(model1)

##################

data3 <- read.csv("skulls.csv")
View(data3)
str(data3)
data3$epoch <- as.factor(data3$epoch)
levels(data3$epoch)
m1 <- lm(data3$mb~data3$epoch)
m2 <- lm(data3$bh~data3$epoch)
m3 <- lm(data3$bl~data3$epoch)
m4 <- lm(data3$nh~data3$epoch)

summary(m1) 
## epochc4000BC is significant
summary(m2)
## epochcAD150 is significant
summary(m3)
## epochc3300BC,epochc4000BC,epochcAD150 are significant
summary(m4)
## no significant variables


y <- cbind(data3$mb,data3$bh,data3$bl,data3$nh)
model <- manova(y~data3$epoch)
summary(model)



