#reading And Exploring the data
DiamondPricesData=read.csv('D:/DS/Python/Python Datasets Assignment2/Python Datasets/Regression Datasets/DiamondPricesData.csv', na.strings =c(""," ","NA","Null"))
dim(DiamondPricesData)
str(DiamondPricesData)
names(DiamondPricesData)
#Changing the Categorical values into Factors
DiamondPricesData$cut=factor(DiamondPricesData$cut)
DiamondPricesData$color=factor(DiamondPricesData$color)
DiamondPricesData$clarity=factor(DiamondPricesData$clarity)
str(DiamondPricesData)
#checking for Missing values
colSums(is.na(DiamondPricesData))
DiamondPricesData$depth[is.na(DiamondPricesData$depth)]=median(DiamondPricesData$depth, na.rm = TRUE)
max(table(DiamondPricesData$color))
DiamondPricesData$color[is.na(DiamondPricesData$color)]="G"
colSums(is.na(DiamondPricesData))
#Grouping the variables
summary(DiamondPricesData)
Continouscols=c("price","carat","depth","table","x","y","z")
Categoricalcols=c("cut","color","clarity")

#Uni and Bi Variate Analysis
#Cont-Hist
library(RColorBrewer)
par(mfrow=c(2,4))
for (histcols in Continouscols) {
  hist(DiamondPricesData[,histcols], main = paste("The Hist of",histcols), col='blue')
  
}
#Cat- Bar

par(mfrow=c(2,3))
for (barcols in Categoricalcols) {
  barplot(table(DiamondPricesData[,barcols]), main =paste("The Barplt of",barcols),cols=brewer.pal(8,"Paired"))
  }

#Bi variate Anlaysis
#Cont vs Cont
dev.off()
plot(DiamondPricesData[,Continouscols], col='green')

x=10

#Cont Vs Cat
for (plottobox in Categoricalcols) {
  boxplot(DiamondPricesData$price ~ DiamondPricesData[,plottobox] ,main=paste("The Boxplot of",plottobox), col=brewer.pal(8,"Paired"))
}

#Correlation analysis
#p value>0.5
CorrData=cor(DiamondPricesData[,Continouscols],use="complete.obs")
CorrData

names(CorrData["price",][abs(CorrData["price",])>0.5])
#Selected variables ="carat", "x","y","z"

#Anova Analysis
#p value<0.05
options(scipen = 999)
summary(aov(price~cut+color+clarity, data = DiamondPricesData))
#Selected variables=cut+color+clarity

#Creating a New dataset for ML
InputData=DiamondPricesData
TargetVariableName='price'
BestPredictorName=c("carat", "x","y","z","cut","color","clarity")
TargetVariable=InputData[,TargetVariableName]
TargetVariable
BestPredictor=InputData[,BestPredictorName]
BestPredictor

DataForML=data.frame(TargetVariable,BestPredictor)
head(DataForML)

#splitting the data

TrainingSampleIndex=sample(1:nrow(DataForML), size = 0.7*nrow(DataForML))
length(TrainingSampleIndex)

DataForMLTrain=DataForML[TrainingSampleIndex,]
dim(DataForMLTrain)
DataForMLTest=DataForML[-TrainingSampleIndex,]
dim(DataForMLTest)

#linear Reg

startTime=Sys.time()
Model_Reg=lm(TargetVariable~carat, data = DataForMLTrain)
endTime=Sys.time()
Model_Reg

plot(DataForMLTrain$carat, DataForMLTrain$TargetVariable,xlab="carat", ylab="price", col='green')
abline(Model_Reg, col='blue')

#MLR
startTime=Sys.time()
Model_Reg=lm(TargetVariable~., data = DataForMLTrain)
endTime=Sys.time()
Model_Reg

#Checking accuracy on test data
head(DataForMLTest)
DataForMLTest$Predict_Lm=predict(Model_Reg,DataForMLTest)
head(DataForMLTest)

DataForMLTest$LM_APE=100*(abs(DataForMLTest$TargetVariable- DataForMLTest$Predict_Lm)/DataForMLTest$TargetVariable)
head(DataForMLTest)

MeanAPE=mean(DataForMLTest$LM_APE)
MedianAPE=median(DataForMLTest$LM_APE)
print(paste("the mean accuracy",100- MeanAPE))#60.07%
print(paste("the median accuracy",100- MedianAPE))#79.08

#DT
startTime=Sys.time()
Model_CTREE=ctree(TargetVariable~., data = DataForMLTrain)
endTime=Sys.time()
Model_Reg

plot(Model_CTREE)

#checking Accuracy
DataForMLTest$Pred_CTREE=as.numeric(predict(Model_CTREE, DataForMLTest))
head(DataForMLTest)

DataForMLTest$CTREE_APE= 100 *(abs(DataForMLTest$TargetVariable-DataForMLTest$Pred_CTREE)/DataForMLTest$TargetVariable)
head(DataForMLTest)

print(paste("the mean accuracy",100- mean(DataForMLTest$CTREE_APE)))#91.98%
print(paste("the median accuracy",100-median(DataForMLTest$CTREE_APE)))#94.21%




