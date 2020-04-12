########################wine data set##############
library(readr)
wine <- read_csv("F:/data science r studio/proj practice/wine/winequality-red.csv")

#check na vaue
sum(is.na(wine))# no na vaue
str(wine)
summary(wine)
dim(wine)

##########EDA##########
#1.1,2,3,4 movment
install.packages("moments")
library(moments)

skewness(wine$quality)
kurtosis(wine$quality)

boxplot(wine$quality,horizontal = TRUE)


hist(wine$quality)
barplot(wine$quality)

table(wine$quality)

###used linear regretion
plot(wine)
pairs(wine)
model1<-lm(quality~.,data=wine)
model1
summary(model1)
##many eement are not significant
model2<-lm(quality~.,data=wine)
summary(model2)
str(model2)

#Aply the vif funcation variance inflaction factore
#install a package car check colinearity  
install.packages("cars")
library(cars)
vif(start)

avPlots(start)

install.packages("installr") # install 
setInternet2(TRUE) # only for R versions older than 3.3.0
installr::updateR() # updating R.
# If you wish it to go faster, run: installr::updateR(T)
# check the separaty it is significatnt or not

mode<-lm(quality~`fixed acidity`,data=wine)
summary(mode)
str(mode)

# check 
model3<-lm(quality~`citric acid`,data=wine)
summary(model3)


model3<-lm(quality~`residual sugar`,data=wine)
summary(model3)# it is not significant

model3<-lm(quality~`density`,data=wine)
summary(model3)

model3<-lm(quality~`density`,data=wine)
summary(model3)#R-squared:  0.3601,

?rtool


#buied the 2 mode
model2<-lm(quality~.-`citric acid` ,data=wine)
summary(model2)
quality=T

####used svm
wine_train<-wine[1:960,]
wine_test<-wine[961:1599,]
# Building model 

library(kernlab)
library(caret)
model1<-ksvm(quality ~.,data = wine_train,kernel = "vanilladot")
model1

help(kvsm)
??kvsm
# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(quality ~.,data = wine_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata= wine_test)
mean(pred_rfdot==wine_test$quality) #0
dim(wine_test)
dim(wine_train)

model_rfdot<-ksvm(quality ~.,data = wine_train,kernel = "laplacedot")
pred_rfdot<-predict(model_rfdot,newdata= wine_test)
mean(pred_rfdot==wine_test$quality) #0

#############svm mean is 0
#############3used the pca############3
#check the relation 
cor(wine)
plot(wine)
pcaobj<-princomp(wine,cor=TRUE,scores = TRUE,covmat = NULL)
##check summary
summary(pcaobj)
str(pcaobj)

###loading pca data
library(loading)
loadings(pcaobj)
sum(pcaobj$scores[,4])
#um(pcaobj$n.obs[,4])*2

##no relation all element
plot(pcaobj$scores[,6],pcaobj$scores[,3])

cor(pcaobj$scores[,6],pcaobj$scores[,3])
##all corelation is 0 means no relation
plot(pcaobj)

#c bind used bined col wise 
wine_ata<-cbind(wine,pcaobj$scores[,1:4])
View(wine_ata)
pcaobj$scores[1:3]

##preaparing the data for clustring
clus_data<-wine_ata[,13:16]
## normalization data
normalization<-scale(clus_data)
head(normalization)
View(normalization)
dist<-dist(normalization,method="euclidean")

##considering data hclust
fit1<-hclust(dist,method = "complete")
plot(fit1,hang=-1)
rect.hclust(fit1,k=9,border="red")
group<-cutree(fit1,9)
group

membership<-as.matrix(group)
head(membership)

View(membership)

final1<-cbind(membership,wine)
head(final1)
View(final1)
View( aggregate(final1(-c(,9:12)),by=list(membership),FUN=mean)
     
      