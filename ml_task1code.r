# Attaching the required packages

```{r}
library(tidyverse)
library(caTools)
library(gridExtra)
library(Metrics)
```

# Read the data
data<-read_csv("score.csv")

head(data)

# Basic summary of the data

summary(data)


  
data%>% ggplot()+ geom_point(aes(x=Hours,y=Scores),colour="blue")+
  geom_smooth(aes(x=Hours,y=Scores),method = "lm",formula = y~x,se=FALSE,colour="red")+
  ggtitle("Score vs Hours of the data")+theme(plot.title=element_text(hjust=0.5))




  

p1<-data%>% ggplot()+geom_boxplot(aes(y=Hours))

p2<-data%>% ggplot()+geom_boxplot(aes(y=Scores))
grid.arrange(p1,p2,nrow=1)



#Splitting the data in training set and test set.


set.seed(2)
split<-sample.split(data$Hours,SplitRatio = 0.8)
training_data<-subset(data,split==TRUE)
test_data<-subset(data,split==FALSE)


### Fitting the linear model




regressor<-lm(formula=Scores~Hours,data = training_data)
summary(regressor)



cor(data$Hours,data$Scores)


#Prediction scores for test set


score_prediction<-predict(regressor,newdata=test_data)
act_pred_data<-data.frame(Hours=test_data$Hours,Actual_score=test_data$Scores,Predicted_score=score_prediction)
act_pred_data

# Some plots related to test data and training data


pl1<-ggplot()+geom_point(aes(x=training_data$Hours,y=training_data$Scores),colour="blue")+
  geom_line(aes(x=training_data$Hours,y=predict(regressor,training_data)))+ggtitle("Score vs Hours(Training_set) ")+xlab("Hours") +
  ylab("Scores")
pl2<-ggplot()+geom_point(aes(x=test_data$Hours,y=test_data$Scores),colour="blue")+
  geom_line(aes(x=test_data$Hours,y=score_prediction),colour="red")+ggtitle("Score vs Hours(Test_set") +
  xlab("Hours") +
  ylab("Scores")
grid.arrange(pl1,pl2,nrow=1)





#The predicted score for a student who studied for 9.5 hours


new_data<-data.frame(Hours=9.5,Score=NA)
Predicted_score<-predict(regressor,new_data)
Predicted_score


# Model Accuracy



mean_absolute_error<-mae(act_pred_data$Actual_score,act_pred_data$Predicted_score)
root_mean_squared_error<-rmse(act_pred_data$Actual_score,act_pred_data$Predicted_score)
root_mean_squared_error
mean_absolute_error
