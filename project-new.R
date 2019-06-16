fake=read.csv("projectdataF18.csv")
summary(fake)
str(fake)


fake$Fake=as.factor(fake$Fake)
set.seed=(123)
split=sample.split(fake$Fake,SplitRatio = 0.8)
train=subset(fake,split==TRUE)
test=subset(fake,split==FALSE)
table(train$Fake)

#Balancing: over sampling using ROSE package

BD=SMOTE(Fake~.,data,perc.over=14950,perc.under = 100.9)
table(BD$Fake)

#Clustering

c1=BD
c1$Fake=NULL
c2=test
c2$Fake=NULL
k=2
clusterpart=kmeans(c1,centers=k)
table(clusterpart$cluster)
clusterpart$centers

#Modeling and Testing

clustermodel=as.kcca(clusterpart,c1)
train1=predict(clustermodel)
test1=predict(clustermodel,newdata=c2)
Train=subset(BD,train1==1)
Train2=subset(BD,train1==2)
Test=subset(test,test1==1)
Test1=subset(test,test1==2)


mrf1=tuneRF(Train[,-31],Train[,31],stepFactor =0.5,plot=TRUE,ntreeTry = 300,trace=TRUE,improve = 0.00001)
mrf2=tuneRF(Train2[,-31],Train2[,31],stepFactor =0.5,plot=TRUE,ntreeTry = 300,trace=TRUE,improve = 0.00001)


rfmodel1=randomForest(Fake~.,data=Train,mtry=5,ntree=300)
rfmodel2=randomForest(Fake~.,data=Train2,mtry=5,ntree=300)
predicttest=predict(rfmodel1,newdata=Test)
predicttest1=predict(rfmodel2,newdata=Test1)
table(Test$Fake,predicttest)
table(Test1$Fake,predicttest1)
AllPredictions=c(predicttest,predicttest1)
AllOutcomes=c(Test$Fake,Test1$Fake)
table(AllPredictions,AllOutcomes)
(17977+13)/(17977+10+0+13)  #Accuracy of Model


#min
clusters <- function(x, centers) {
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  max.col(-t(tmp))  
}
  
  
clusterpart=kmeans(c1,centers=k)
all.equal(clusterpart[["cluster"]], clusters(c1, clusterpart[["centers"]]))
fake2=read.csv("projectdataF18+validation.csv.")
fdata=clusters(fake2,clusterpart[["centers"]])
datatestfinal1=subset(fake2,fdata==1)
datatestfinal2=subset(fake2,fdata==2)


Predicttest2= predict(rfmodel1, newdata =datatestfinal1)
Predicttest3= predict(rfmodel2, newdata =datatestfinal2)
finalpredictions=c(Predicttest2,Predicttest3)
table(finalpredictions)

