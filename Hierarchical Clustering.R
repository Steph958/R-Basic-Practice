

#Hierarchical clustering

#可快速瀏覽觀察值與各欄位的關係
#分群結果可能被以下參數影響：
    #計算距離的方法
    #分群依據
    #更改數值的大小
#可能會遇到的問題：
    #有時會不太清楚要如何切割分群結果



mtcars.mxs<-as.matrix(mtcars)
d<-dist(mtcars.mxs) #預設為euclidean
head(d)

d<-dist(mtcars.mxs, method="manhattan") #計算manhattan距離
head(d)

par(mar=rep(2,4),mfrow=c(1,1))
hc<-hclust(dist(mtcars.mxs)) 
#可用method參數設定聚合方法，預設為complete
plot(hc)

par(mar=rep(2,4),mfrow=c(1,1))
hc<-hclust(dist(mtcars.mxs),method="average") 
#聚合方法為計算平均距離
plot(hc)

clusterCut <- cutree(hc, k=5) #分5群
sort(clusterCut)

ggplot()+geom_point(data=mtcars,
                    aes(x=hp,y=mpg,color=as.factor(clusterCut)))


clusterCut <- cutree(hc,h =4) #切在高度=4的地方（距離=4）
sort(clusterCut)



par(mar=rep(0.2,4),mfrow=c(1,1))
heatmap(mtcars.mxs)


distxy <- dist(mtcars.mxs)
hClustering <- hclust(distxy)
plot(hClustering)

