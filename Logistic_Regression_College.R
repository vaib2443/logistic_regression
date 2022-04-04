##### Generalized linear model######

#importing libraries
install.packages("ISLR")
install.packages("caret")
install.packages("caTools")
install.packages("ggcorrplot")
library(ISLR)
library(ggplot2)
library(caret)
library(gridExtra)
library(pROC)
library(caTools)
library(dplyr)
library(GGally)
library(ggpubr)
library(ggcorrplot)


#loading data
clg<-College
#understanding structure of data and data types
str(clg)
#summary of data to check NAs
summary(clg)


############################################################
#Exploratory Data Analysis
############################################################

#pair &correlation plot of continuous variables
ggpairs(clg[, 2:18])

#pair plot for select variables filled with Private categorical variable
ggpairs(clg, columns = c(2, 5, 7 :9,13:18), ggplot2::aes(colour=Private)) 

#creating multiple box plot of continuous variables
bp1<-qplot(Private, Apps, fill=Private, geom='boxplot')
#box plot of Private and Accept
bp2<-qplot(Private, Accept, fill=Private, geom='boxplot')
#box plot of Private and Enroll
bp3<-qplot(Private, Enroll, fill=Private, geom='boxplot')
#box plot of Private and Top 10 percent students of high school 
bp4<-qplot(Private, Top10perc, fill=Private, geom='boxplot')
#box plot of Private and Full time undergraduate
bp5<-qplot(Private, F.Undergrad, fill=Private, geom='boxplot')
#box plot of Private and Part time undergraduate
bp6<-qplot(Private, P.Undergrad, fill=Private, geom='boxplot')
#box plot of Private and out of state students
bp7<-qplot(Private, Outstate, fill=Private, geom='boxplot')
#box plot of Private and doctorate students
bp8<-qplot(Private, PhD, fill=Private, geom='boxplot')
#box plot of Private and Student Faculty ratio
bp9<-qplot(Private, S.F.Ratio, fill=Private, geom='boxplot')

#creating grid view of box plots 
ggarrange(bp1, bp2,bp3,bp4,bp5,bp6,bp7,bp8,bp9,
          ncol = 3, nrow = 3)

############################################################
#Splitting Data train and test
############################################################
#initialize randomizer
set.seed(345)
#splitting training data into 80/20
traindata<-createDataPartition(clg$Private, p=0.80, list = F)
#creating training data to include 80% rows
train<-clg[traindata,]
#creating test data to include 20% rows
test<-clg[-traindata,]

############################################################
#Fitting Generalized Linear Model (GLM)
############################################################

# create model1
model1<-glm(Private~., data=train, family = binomial(link='logit'))
summary(model1)

#model2 based on significant variables of model 1
model2<-glm(Private~Apps+F.Undergrad+Outstate+PhD+perc.alumni+Expend, data=train, family = binomial(link='logit'))
summary(model2)

#regression coefficients (logarithmic odds)
coef(model2)

#regression coefficients(odds)
exp(coef(model2))


############################################################
#Making Predictions Against train data set 
############################################################
prob.train<-predict(model2, newdata=train, type='response')
#setting 50% probability for train data
pred.class.min<-as.factor(ifelse(prob.train>=0.5, "Yes", "No"))

#Creating Confusion Matrix for training data set
cm<-confusionMatrix(pred.class.min, train$Private, positive = 'Yes')
cm

#Finding Recall, Precision Values from the confusion matrix using byClass
cm$byClass

############################################################
#Making Predictions Against test data set 
############################################################
prob.test<-predict(model2, newdata=test, type='response')
#setting 50% probability for test data
pred.class.min<-as.factor(ifelse(prob.test>=0.5, "Yes", "No"))

#Creating Confusion Matrix for test data set to calculate model accuracy
confusionMatrix(pred.class.min, test$Private, positive = 'Yes')

#Creating ROC Curve: Receiver Operator Characteristic
ROC1<-roc(test$Private, prob.test)
plot(ROC1, col="red")

#Calculating Area Under the Curve for ROC curve
auc<-auc(ROC1)
auc
