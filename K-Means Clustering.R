

#K-means clustering

#執行步驟:
    #指定要分幾群
    #計算每一群的中心點
    #將各個物件/觀察值指定給最近的中心點
    #依照新的分群計算新的中心點
#輸入
    #計算距離的資料（數值）
    #要分成幾群 
    #預設個群的中間點位置
#產出
    #計算出每’群‘的中心點
    #指定每個觀察值所在的’群‘

mtcars

x<-scale(mtcars$hp[-1]);y<-scale(mtcars$mpg[-1]) #標準化
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=labelCar)

dataFrame <- data.frame(x,y)
kmeansObj <- kmeans(dataFrame,centers=3)

names(kmeansObj)
#kmeansObj
kmeansObj$cluster

par(mar=rep(0.2,4))
plot(x,y,col=kmeansObj$cluster,pch=19,cex=2)
points(kmeansObj$centers,col=1:3,pch=3,cex=3,lwd=3)

#Heatmaps
set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
kmeansObj <- kmeans(dataMatrix,centers=3)
par(mfrow=c(1,2), mar = c(2, 4, 0.1, 0.1))
image(t(dataMatrix)[,nrow(dataMatrix):1],yaxt="n")
image(t(dataMatrix)[,order(kmeansObj$cluster)],yaxt="n")

#K-means注意事項:
#需要決定# of clusters
    #用眼睛/人工/特殊要求選
    #用 cross validation/information theory選
    #Determining the number of clusters
#K-means 沒有一定的結果
    #不同的 # of clusters
    #不同的 # of iterations






