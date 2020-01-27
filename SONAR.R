#library(readxl)
sonar= read_csv("sonar.csv")
library(e1071)
library(caret)
data(sonar)

model <- train(TARGET~.,sonar,
               method = "rf",TuneLength=3,
               trControl = trainControl(
                 method="cv",number= 10,
                 verboseIter = TRUE
                 )
             )


model$results

pred<- predict(model,sonar,"prob")



S<-sonar

X=ifelse(pred$M > pred$R,0 ,1 )
 
S<-mutate(S ,Expected = X )
 
N<-S

N$TARGET=factor(N$TARGET,
                        levels = c('R','M'),
                     labels = c(1,0))


N$Expected=ifelse(N$Expected>0.5 ,0,1)

library(ggplot2)
ggplot()+
  geom_point(aes(x=S$Expected,y=S$TARGET),
            color='black')+
  geom_line(aes(x=N$Expected,y=S$TARGET),
             color='red')+
  ggtitle('Expected VS ACTUAL')

#install.packages("varhandle")

library(varhandle)

M1=mean(S$Expected)

x<- N$TARGET
x<-unfactor(x)

M2=mean(x)

print(M1-M2)



