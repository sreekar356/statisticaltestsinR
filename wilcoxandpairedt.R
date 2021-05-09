data <- read.csv("data.balanced.over.csv")
summary(data)
View(data)
str(data)
library(caret)
data$Is_sanctioned <- as.factor(data$Is_sanctioned)

log <- c(0.9995,0.9997,0.9997,0.9869,0.9998,0.9998,0.9958,0.9983,0.9989)


split <- floor(0.90 * nrow(data))

set.seed(123)
train_Sample <- sample(seq_len(nrow(data)),size = split)
train <- data[train_Sample,]
test <- data[-train_Sample,]

control <- trainControl(method = "cv" , summaryFunction = twoClassSummary , classProbs = T ,savePredictions = T )
model_log <- train(Is_sanctioned~.,method = 'glm', data = train ,trControl = control)
predict <- predict(model_log,test,type = 'raw')
CM_log <- table(predict,test$Is_sanctioned,dnn = c('Predicted','Actual'))
confusionMatrix(CM_log)

################### lda

str(data)


lda <- c(0.9786,0.9776,0.9780,0.9771,0.9777,0.9782,0.9783,0.9772,0.9813)


split <- floor(0.90 * nrow(data))

set.seed(123)
train_Sample <- sample(seq_len(nrow(data)),size = split)
train <- data[train_Sample,]
test <- data[-train_Sample,]

model_lda <- train(Is_sanctioned~.,method = 'lda', data = train ,trControl = control)
predict1 <- predict(model_lda,test,type = 'raw')
CM_lda <- table(predict1,test$Is_sanctioned,dnn = c('Predicted','Actual'))
confusionMatrix(CM_lda)


## Null hypothesis: Median difference b/w both of the classifiers is zero


data1 <- data.frame(classifier = rep(c("log","lda"), each = 9 ),sensitivity = c(log,lda))

library(ggpubr)

ggboxplot(data1,x = "classifier",y = "sensitivity",color = "classifier",palette = c("#00AFBB","#E7B800"),order = c("log","lda"),ylab = "sensitivity",xlab = "classifier")


#subset of sensitivity data

log <- subset(data1,classifier == "log",sensitivity)

lda <- subset(data1,classifier == "lda",sensitivity)

library(PairedData)

a <- paired(log,lda)

plot(a,type = "profile")

results <- wilcox.test(log$sensitivity,lda$sensitivity,paired = TRUE)


results2 <- wilcox.test(sensitivity~classifier,data1,paired = TRUE)

###unpaired sample

s1 <- c(160,180,210,230,150,260,150,190,110,300)
s2 <- c(450,290,50,110,350,250,330,320,360,350)

data2 <- data.frame(group = rep(c("s1","s2"), each = 10 ),sugarlevel = c(s1,s2))
ggboxplot(data2,x = "group",y = "sugarlevel",color = "group",palette = c("#00AFBB","#E7B800"),order = c("s1","s2"),ylab = "sugarlevel",xlab = "group")


s1 <- subset(data2,group == "s1",sugarlevel)

s2 <- subset(data2,group == "s2",sugarlevel)

library(PairedData)


results <- wilcox.test(s1$sugarlevel,s2$sugarlevel)

####### paired t test #########

##Alternate hypothesis : The data is not normally distributed

##shapiro wilk test(normality test)

sw <- with(data1,sensitivity[classifier == "log"] - sensitivity[classifier == "lda"])
shapiro.test(sw)

sw <- with(data2,sugarlevel[group == "s1"] - sugarlevel[group == "s2"])
shapiro.test(sw)

## paired t test is used in second case as there is  in the sample

t <- with(data2,sugarlevel[group == "s1"] - sugarlevel[group == "s2"])
t.test(s1,s2,paired = TRUE)
