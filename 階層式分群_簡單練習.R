


#聚合式階層群聚法(AGNES)

head(iris)

#刪除標籤欄位
inputData<-iris[,-5]
head(inputData)


#不同的資料點間距離矩陣的運算
E.dist <- dist(x = inputData, method = "euclidean")
M.dist <- dist(x = inputData, method = "manhattan")

par(mfrow=c(1,2))

h.E.cluster <- hclust(E.dist)
plot(h.E.cluster, xlab="歐式距離",family="黑體-繁 中黑")

h.M.cluster <- hclust(M.dist) 
plot(h.M.cluster, xlab="曼哈頓距離", family="黑體-繁 中黑")


#不同歐式距離搭配不同聚合演算法的分群結果
dev.off()
par(mfrow= c(3,2),family="黑體-繁 中黑")

plot(hclust(E.dist, method="single"),xlab = "最近聚合法:single-linkage")   # 最近法
plot(hclust(E.dist, method="complete"), xlab = "最遠聚合法:complete-linkage")  # 最遠法
plot(hclust(E.dist, method="average"), xlab = "平均聚合法: average-linkage")  # 平均法
plot(hclust(E.dist, method="centroid"), xlab = "中心法: centroid-linkage") # 中心法
plot(hclust(E.dist, method="ward.D2"), xlab = "華德法: Ward's Method")  # 華德法


dev.off()
par(family="黑體-繁 中黑")
plot(hclust(E.dist, method="ward.D2"), xlab = "華德法: Ward's Method")





# Compute with agnes
library(cluster)
hc2 <- agnes(E.dist, method = "ward")
# Agglomerative coefficient(聚合係數)
#衡量群聚結構被辨識的程度
hc2$ac  
## [1] 0.9908772

#用聚合係數來比較多組分群連結演算法的效果
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

ac <- function(x) {
    agnes(E.dist, method = x)$ac
}

map_dbl(m, ac) #Apply a function to each element of a vector 
#
#   average    single  complete      ward 
# 0.9300174 0.8493364 0.9574622 0.9908772

#繪圖
dev.off()
hc2 <- agnes(E.dist, method = "ward")
pltree(hc2, cex = 0.6, hang = -1, main = "Dendrogram of agnes")



#分支方法

#指定分群數目
h.E.Ward.cluster <- hclust(E.dist, method="ward.D2")
plot(h.E.Ward.cluster)
rect.hclust(tree =h.E.Ward.cluster, k = 3, border = "red")
rect.hclust(tree =h.E.Ward.cluster, k = 13, border = "blue")

#指定分支的高度
h.E.Ward.cluster <- hclust(E.dist, method="ward.D2")
plot(h.E.Ward.cluster)
rect.hclust(tree =h.E.Ward.cluster, h = 4, border = "red")
rect.hclust(tree =h.E.Ward.cluster, h = 10, border = "blue")

#將資料標記上分群結果
h.E.Ward.cluster <- hclust(E.dist, method="ward.D2")
cut.h.cluster <- cutree(tree = h.E.Ward.cluster, k = 3)
cut.h.cluster


#混淆矩陣
table(cut.h.cluster, iris$Species)
# #
# cut.h.cluster setosa versicolor virginica
#         1     50          0         0
#         2      0         49        15
#         3      0          1        35


plot(table(iris$Species, cut.h.cluster),
     main = "Confusion Matrix for Species Clustering",
     xlab="Species",ylab="Cluster")

#原始資料分布情況
ggplot(data = iris,
       mapping = aes(x = Petal.Length, y = Petal.Width)) +
    geom_point(aes(col = Species))



#分列式階層群聚法(DIANA)

head(USArrests)
inputData<-USArrests%>%na.omit()%>%scale()

diana_cluster<-diana(inputData)
diana_cluster$dc #聚合係數
# 0.8514345

# plot dendrogram
pltree(diana_cluster, cex = 0.6, 
       hang = -1, main = "Dendrogram of diana")


# Cut diana() tree into 4 groups
diana_cluster <- diana(inputData)
group <- cutree(diana_cluster, k = 4)
group


install.packages('factoextra')
library(factoextra)

fviz_cluster(list(data = inputData, cluster = group))



#決定最適分群群聚數

# Elbow Method
fviz_nbclust(inputData, 
             FUN = hcut, #階層式分群法 
             method = "wss")#組內平方誤差


# Average Silhouette Method
fviz_nbclust(x = inputData,
             FUNcluster = hcut, 
             method = "silhouette")


# Gap Statistic Method
gap_stat <- clusGap(x = inputData,
                    FUNcluster = hcut, 
                    nstart = 25, 
                    K.max = 10, 
                    B = 50)

fviz_gap_stat(gap_stat)











