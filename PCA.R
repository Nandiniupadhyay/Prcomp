##load data and summary
data <- read.csv("C:/Users/com/Downloads/data.csv")
data$diagnosis<-ifelse(data$diagnosis=="M",1,0)
df=subset(data, select=-c(id,diagnosis, X))
head(df)

##standardize the data to PCA
library(factoextra)
df_std=transform(df)
head(df_std)
##PCA with correlation matrix
all_pca=princomp(df_std,cor=TRUE)
head(all_pca)
summary(all_pca)
##sceeplot for variation explained by 10 principal component
fviz_eig(all_pca, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", barcolor="grey",linecolor = "red", ncp=10)+
  labs(title = "Cancer All Variances - PCA",
       x = "Principal Components", y = "% of variances")
nComp<-10
dfComponents<-predict(all_pca,newdata=df_std)[,1:10]
dfEvaluate<-cbind(diagnosis=data$diagnosis,as.data.frame(dfComponents))
head(dfEvaluate)
nrows <- NROW(dfEvaluate)
##Train and test dataset
set.seed(218)                           ## fix random value
index <- createDataPartition(dfEvaluate$diagnosis, times = 1, p = 0.8, list = FALSE)   ## shuffle and divide

#train <- dfEvaluate                          ## 569 test data (100%)
train <- dfEvaluate[index,]                   ## 398 test data (70%)
test <- dfEvaluate[-index,]                   ## 171 test data (30%)
X_train<- subset(train,select = -diagnosis)
Y_train<- train$diagnosis

X_test<- subset(test,select= -diagnosis)
Y_test<- test$diagnosis

prop.table(table(train$diagnosis)) ##prop of diagnosis
prop.table(table(test$diagnosis))   ##proportin of diagnosis
library(caret)
##Fitting of Multiple logistic regression
model<- glm(diagnosis~.,family = binomial(link = logit),train)
summary(model)
pre_glm=predict(model,test)
cm_glm <- confusionMatrix(pre_glm, test$diagnosis)

A=cor(df_std)
A
B=eigen(A)
