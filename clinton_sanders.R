library(ggplot2)
library(ggthemes)
train<-read.csv("googletrends_dem_delegates_trainingset.csv")
train[!is.na(train$Day1),]
p2 <- ggplot(train, aes(x = Clinton_Google_average, y = Sanders)) +
  geom_point(color="blue") +geom_text(aes(label=ifelse(Sanders>.17,as.character(State),''),color="white",show_guide=F),hjust=-.2,size=3.5)+
  ggtitle("Sanders Percentage compared to Clinton's Average Google Trend Score")
p2+ theme_hc(bgcolor = "darkunica")+stat_smooth(method = "loess", formula = y ~ x, size = 1)+labs(x="Clinton Google Average",y="Sanders Voting Percentage")

library(e1071)
set.seed(60)
##get training set
smp_size<-floor(.6*nrow(train))
train_ind<-sample(seq_len(nrow(train)),size=smp_size)
training<-train[train_ind,]
testing<-train[-train_ind,]
##also change x to see how it changes based on certain things
x<-training[,c(7:34)]
x<-x[,c(1:4,15:18)]
y<-training$Sanders


svm_tune<-tune(svm,train.x=x,train.y=log(y),kernel="radial",ranges=list(cost=10^(-1:5), gamma=c(.5,.6,.7,.8,.9,1,2)))
sv<-svm(x,log(y),cost=svm_tune$best.parameters$cost,gamma=svm_tune$best.parameters$gamma,kernel="radial")
svm_predict<-predict(sv,newdata=x)

sum(((training$Sanders)-exp(svm_predict))^2)*(1/nrow(training))


x_test<-testing[,c(7:34)]
x_test<-x_test[,c(1:4,15:18)]
y_test<-testing$Sanders
prediction<-predict(sv,newdata=x_test)
sum(((y_test)-exp(prediction))^2)*(1/length(y_test))



library(randomForest)
fit <- randomForest(x=x,y=y)
rf<-predict(fit,newdata=x)
sum((training$Sanders-rf)^2)*(1/nrow(training))
pre_rf=predict(fit,newdata=x_test)
sum((y_test-(pre_rf))^2)*(1/length(y_test))


lineartest<-cbind(y,x)

linearmodel<-lm(y~.,data=lineartest)
loessmodel<-loess(y~Day1+Day1.1+Day2+Day2.1,data=lineartest)



lm_predict<-predict(linearmodel,newdata=x_test)
sum((y_test-(lm_predict))^2)*(1/length(y_test))

x_loess_test<-x_test[,c(1,2,5,6)]
loess_predict<-predict(loessmodel,newdata=x_loess_test)
sum((y_test-(loess_predict))^2)*(1/length(y_test))




(lm_predict+pre_rf+exp(prediction))/4

##ensemble mean squared error
mse_ensemble=sum((y_test-(2.77*lm_predict+1.5*pre_rf+exp(prediction))/4.2)^2)*(1/length(y_test))

##then predict outcomes for new york and next states
statestodo<-read.csv("googletrends_dem_delegates_testingset.csv")
ny<-statestodo[1,c(3:6,17:20)]
ny_lm<-predict(linearmodel,newdata=ny)
ny_rf<-predict(fit,newdata=ny)
ny_svm<-predict(sv,newdata=ny)
ny_predcition<-(2.77*ny_lm+1.5*ny_rf+exp(ny_svm))/4.2
