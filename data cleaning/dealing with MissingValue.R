naVec<-c("a","b",NA,"d","e")
is.na(naVec)

naVec[!is.na(naVec)] 
##保留所有在is.na()檢查回傳FALSE的元素

head(airquality) 
#資料框
#使用complete.cases來選出完整的資料列，如果資料列是完整的，則會回傳真TRUE

complete.cases(airquality) 
head(airquality[complete.cases(airquality),]) 
##保留所有在complete.cases()檢查回傳TRUE的元素






#############################################################################################
#利用演算法補值也是一種解決辦法，
#可參考_skydome20_的R筆記–(10)遺漏值處理(Impute Missing Value)教學。

tmp <- c(1,5,8,NA,5,NA,6)
is.na(tmp)
## [1] FALSE FALSE FALSE  TRUE FALSE  TRUE FALSE
# 計算遺漏值的個數
sum(is.na(tmp))


install.packages("missForest") # prodNA() function

library(missForest)
# 在iris資料內，隨機產生10%的遺漏值
data <- prodNA(iris, noNA = 0.1)
# 可以注意到，資料裡面有NA的存在，代表Not-Available(遺漏值)
head(data)

#############################################################################################
#  1. 直接移除有遺漏值的資料

# 當一筆資料是完整的，回傳TRUE；當一筆資料有遺漏值，回傳FALSE
complete.cases(data)
# 移除有遺漏值的資料
rm.data <- data[complete.cases(data), ] #預設為TRUE
rm.data



#############################################################################################
# 2. 用「平均數」、「第一四分位數」…來填補遺漏值
# 以下用平均數，來填補某一欄位的遺漏值

mean.data <- data

mean.1 <- mean(mean.data[, 1], na.rm = T)  # 第一欄位的平均數
na.rows <- is.na(mean.data[, 1])           # 第一欄位中，有遺漏值存在的資料

# 用第一欄位的平均數，填補第一欄位的遺漏值
mean.data[na.rows, 1] <- mean.1





#############################################################################################
# 3. 用K-Nearest Neighbours填補遺漏值
#找和自己很像的K個鄰居，然後從他們身上"複製"自己所沒有的東西

install.packages("DMwR")
library(DMwR)

imputeData <- knnImputation(data)
head(imputeData)



#############################################################################################
# 4. 用MICE填補遺漏值
#現在我們有欄位V1,V2,V3……Vn，每個欄位裡面都有遺漏值。
# 當我們要填補V1的遺漏值時，就先把V2,V3……Vn的欄位當作自變數(X)，把V1當作應變數(Y)，並且進行建模，然後用預測的結果來填補V1的遺漏值。
# 同理，針對V2，就用V1,V3……Vn建模，然後用預測的結果來填補V2的遺漏值。

install.packages("mice")
library(mice)

mice.data <- mice(data,
                  m = 3,           # 產生三個被填補好的資料表
                  maxit = 50,      # max iteration
                  method = "cart", # 使用CART決策樹，進行遺漏值預測
                  seed = 188)      # set.seed()，令抽樣每次都一樣

# 原始資料(有遺漏值)
data

# 填補好的資料：因為m=3，所以會有三個填補好的資料集，可以用以下方式取出

complete(mice.data, 1) # 1st data
complete(mice.data, 2) # 2nd data
complete(mice.data, 3) # 3rd data


#可以任取其中一個「填補好的資料」，來進行後續的建模了！

# e.g. 拿第二個資料，作為我後續分析的資料
df <- complete(mice.data, 2)
head(df)
# 然後以df進行線性迴歸、類神經網路、主成份分析...等等

