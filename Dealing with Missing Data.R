
data("BostonHousing",package="mlbench")
data<-BostonHousing
original<-BostonHousing
str(data)  #'data.frame':	506 obs. of  14 variables
complete.cases(original)


set.seed(100)
data[sample(1:nrow(data),40),"rad"] <- NA
data[sample(1:nrow(data),40), "ptratio"] <- NA
head(data,15)
complete.cases(data)



# 刪除資料列法
data_backup<-data
complete.cases(data_backup)
data_backup[!complete.cases(data_backup),] #有遺失值的row
MissingValue_Row<-data_backup[!complete.cases(data_backup),] 

data_backup<-data_backup[complete.cases(data_backup),]
data_backup
str(data_backup) #'data.frame':	430 obs. of  14 variables



# 刪除特徵變數法 #不推薦使用


# 填補法
data_backup2<-data

#Hmisc package:
# install.packages('Hmisc')
# library(Hmisc)
# 
# impute(data_backup2$ptratio, mean)  # replace with mean
# impute(data_backup2$ptratio, median)# median
# impute(data_backup2$ptratio, 20)    # replace specific number

# Hand:
data_backup2$ptratio[is.na(data_backup2$ptratio)==TRUE]<-mean(data_backup2$ptratio, na.rm=TRUE)


# 評估填補正確率
install.packages('DMwR')
library(DMwR)

actuals <- original$ptratio[is.na(data$ptratio)]  #從本來沒有遺失值的資料中選出被插入遺失值的row >> 即為真實值
predicteds <- rep(mean(data_backup2$ptratio, na.rm = TRUE), length(actuals)) #預測值皆為平均值
regr.eval(trues = actuals,preds = predicteds)
#MAPE(平均絕對誤差百分比)為0.0957



# KNN預測法
#可一次填補完所有特徵變數中的遺失值
library(DMwR)

#將目標變數移除再進行預測填補
names(data)
names(data)%in% 'medv'

knnoutput<-knnImputation(data=data[,!names(data)%in% 'medv'])
anyNA(knnoutput)  #所有遺失值都已經填補完畢

#評估準確度:
actuals <- original$ptratio[is.na(data$ptratio)]  #從本來沒有遺失值的資料中選出被插入遺失值的row >> 即為真實值
predicteds <- rep(mean(knnoutput$ptratio, na.rm = TRUE), length(actuals)) #預測值皆為平均值
regr.eval(trues = actuals,preds = predicteds)
#MAPE(平均絕對誤差百分比)為0.0953



# 決策樹法
# 即使遺失值屬類別變數也可處理
library(rpart)

str(data)

# 預測類別變數
class_mod <- rpart(formula = rad ~ . -medv, 
                   data = data[!is.na(data$rad),], 
                   method = "class", 
                   na.action = na.omit)

# 預測數值變數
anova_mod <- rpart(formula = ptratio ~ . -medv,
                   data = data[!is.na(data),], 
                   method = "anova", 
                   na.action = na.omit)

rad_predict <- predict(object = class_mod,
                       newdata = data[is.na(data$rad),])#rad欄位有NA的row

ptratio_predict <- predict(object = anova_mod, 
                           newdata = data[is.na(data$ptratio),])#ptratio欄位有NA的row

# 評估準確度:
actuals <- original$ptratio[is.na(data$ptratio)]
predicteds <- ptratio_predict
regr.eval(trues = actuals, preds = predicteds)
#MAPE(平均絕對誤差百分比)為0.0259

actuals <- original$rad[is.na(data$rad)]

data$rad
str(rad_predict)
colnames(rad_predict)
colnames(rad_predict)[1]
colnames(rad_predict)[9]

x<-apply(rad_predict, 1, which.max)#把rad_predict矩陣中每一row中最大的值(機率)挑出
colnames(rad_predict)[x]  #去找出每一個最大值所對應的欄位
as.numeric(colnames(rad_predict)[x])

predicteds <- as.numeric(colnames(rad_predict)[apply(rad_predict, 1, which.max)])
actuals != predicteds
mean(actuals != predicteds)
# 平均錯誤率為0.225


# MICE法
install.packages('mice')
library(mice)


# #常見的幾種內建填補模型包括：
# pmm (Predictive mean matching) : 任何變數型態
# cart (Classification and regression trees) : 任何變數型態
# rf (Random forest imputations) : 任何變數型態
# logreg (Logistic regression) : 二元類別型態 (binary)


#先使用mice()來建立預測模型
miceMod <- mice(data = data[,!names(data) %in% "medv"],
                method = "rf", #可以指定單一或多個補值法for不同的欄位；只給一個補值法的話，就會用到所有欄位上
                maxit = 5 #可以指定迭代次數，預設為五次
) 

#使用complete()來產生填補完整後的資料集
miceOutput <- complete(miceMod,action = 1) #預設action為第一組資料
anyNA(miceOutput)


#評估準確度:
actuals <- original$ptratio[is.na(data$ptratio)]
predicteds <- miceOutput[is.na(data$ptratio),"ptratio"]
regr.eval(actuals,predicteds)
#MAPE(平均絕對誤差百分比)為0.0240

actuals <- original$rad[is.na(data$rad)]
predicteds <- miceOutput[is.na(data$rad),"rad"]
mean(actuals != predicteds) 
# 平均錯誤率為0.2












