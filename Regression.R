
#以NBA資料為例，首先先將資料讀入


install.packages("SportsAnalytics")
library(SportsAnalytics)

NBA1516<-fetch_NBAPlayerStatistics("15-16")

str(NBA1516)


#移除有遺失值的欄位
NBA1516<-NBA1516[complete.cases(NBA1516),] 
str(NBA1516)


#分訓練組與測試組

# 從1到10，隨機取三個數字
sample(1:10,3) 
#從第一行到最後一行，隨機取1/3行數
sample(1:nrow(NBA1516),nrow(NBA1516)/3) 
#新增一個欄位儲存FALSE
NBA1516$Test<-F 
#再把其中1/3改為TRUE
NBA1516[sample(1:nrow(NBA1516),nrow(NBA1516)/3),]$Test<-T 


# Training set : Test set球員數
c(sum(NBA1516$Test==F),sum(NBA1516$Test==T))
## [1] 317 158






#用訓練組的資料（NBA1516$Test==F），訓練一個多變數線性迴歸模型
fit<-glm(TotalPoints~TotalMinutesPlayed+FieldGoalsAttempted+
             Position+ThreesAttempted+FreeThrowsAttempted,
         data =NBA1516[NBA1516$Test==F,])

#fit
 summary(fit)$coefficients





#逐步選擇模型 stepwise 後退學習：
library(MASS)

##根據AIC，做逐步選擇, 預設倒退學習 direction = "backward"
finalModel_B<-stepAIC(fit,direction = "backward",trace=FALSE) #trace=FALSE: 不要顯示步驟

summary(finalModel_B)$coefficients



#逐步選擇模型 stepwise 往前學習：

##根據AIC，做逐步選擇, 往前學習 direction = "forward"
finalModel_F<-stepAIC(fit,direction = "forward",trace=FALSE)

summary(finalModel_F)$coefficients




#逐步選擇模型 stepwise 雙向學習
##根據AIC，做逐步選擇, 雙向學習 direction = "both"
finalModel_Both<-stepAIC(fit,direction = "both",trace=FALSE)

summary(finalModel_Both)$coefficients





#用Test set來評估模型
#使用predict函數
#將測試組資料放入預測模型中

predictPoint<-predict(finalModel_Both, 
                      newdata = NBA1516[NBA1516$Test==T,])


predictPoint<-predict(fit, 
                      newdata = NBA1516[NBA1516$Test==T,])                                        
#str(predictPoint)
#str(NBA1516[NBA1516$Test==T,])





# self-defined 的 R-squared 函式
R_squared <- function(actual, predict){
    mean_of_obs <- rep(mean(actual), length(actual))
    
    SS_tot <- sum((actual - mean_of_obs)^2)  #總變異
    
    SS_reg <- sum((predict - mean_of_obs)^2) #可解釋之變異
    
    #SS_res <- sum((actual - predict)^2)     #不可解釋之變異
    
    R_squared <- SS_reg/SS_tot               #1 - (SS_res/SS_tot)
    R_squared
}


c(R_squared(NBA1516[NBA1516$Test==T,]$TotalPoints, predictPoint))




#相關係數
cor(x=predictPoint,y=NBA1516[NBA1516$Test==T,]$TotalPoints) 

plot(x=predictPoint,y=NBA1516[NBA1516$Test==T,]$TotalPoints)

























