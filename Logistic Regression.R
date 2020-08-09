


mydata <- read.csv("https://raw.githubusercontent.com/CGUIM-BigDataAnalysis/BigDataCGUIM/master/binary.csv")
str(mydata)

mydata$admit <- factor(mydata$admit) # 類別變項要轉為factor
mydata$rank <- factor(mydata$rank) # 類別變項要轉為factor


mydata$Test<-F 
#新增一個參數紀錄分組
mydata[sample(1:nrow(mydata),nrow(mydata)/3),"Test"]<-T 
#隨機取1/3當Test set
c(sum(mydata$Test==F),sum(mydata$Test==T)) 
# Training set : Test set學生數
## [1] 267 133



#修改一下factor的level
mydata$admit<-factor(mydata$admit,levels=c(1,0))
mydata$admit




# GRE:某考試成績, GPA:在校平均成績, rank:學校聲望
library(MASS)

mylogit <- glm(admit ~ gre + gpa + rank,
               data = mydata[mydata$Test==F,], family = "binomial")

finalFit<-stepAIC(mylogit,direction = "both",trace=FALSE) 
# 雙向逐步選擇模型

summary(finalFit)





#用預測組預測新學生可不可以錄取，並驗證答案

AdmitProb<-predict(finalFit, # 用Training set做的模型
                   newdata = mydata[mydata$Test==T,], #Test==T, test data
                   type="response") #結果為每個人被錄取的機率

#AdmitProb
head(AdmitProb)

table(AdmitProb<0.5,mydata[mydata$Test==T,]$admit) 
# 混淆矩陣 Confusion Metrix
#
# Accuracy(準確度):正確預測的機率 
(2+36)/(2+8+87+36)
# Recall(召回率):實際為TRUE也被預測為TRUE的機率
36/(87+36)
# Specificity(明確率):實際為FALSE也被預測為FALSE的機率
2/(2+8)
# Precision(精確度):被預測為True之中，實際為True的機率
36/(8+36)



#計算預測效能參數
install.packages("caret")

library(caret) 

AdmitAns = ifelse(AdmitProb > 0.5,1,0)
AdmitAns <- factor(AdmitAns)

sensitivity(AdmitAns,mydata[mydata$Test==T,]$admit)

specificity(AdmitAns,mydata[mydata$Test==T,]$admit)

posPredValue(AdmitAns,mydata[mydata$Test==T,]$admit)

negPredValue(AdmitAns,mydata[mydata$Test==T,]$admit)


