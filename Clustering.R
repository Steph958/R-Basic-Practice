

# 本篇目錄

    # 階層式分群(Hierarchical Clustering)
    # 切割式分群(Partitional Clustering)
        # K-Means
        # K-Medoid

    # 分群的最佳數目(Optimal number of clusters)
        # Elbow Method
        # Average Silhouette Method

    # 譜分群(Spectral Clustering)
        # 手動實踐譜分群
        # 使用套件kernlab


##################################################################################################################
# 主要可以分成兩種類型：
# 
# 緊緻性(Compactness):會希望「個體之間的距離越小越好」，讓群體內部越緊緻越好：
    # 階層式分群(Hierarchical Clustering)：不需指定分群數目，讓資料自動由上往下/由下往上結合起來。
    # 分割式分群(Partitional Clustering)：需事先指定分群數目，經過不斷的迭代，直到群內的變異最小。
# 連通性(Connectedness)，會希望「可以串接的個體分在同一群」：
    # 譜分群(Spectral Clustering)：基於圖論跟Graph Laplacian的方法，能把「資料的形狀(shape)」考量進來


##################################################################################################################
#階層式分群(Hierarchical Clustering)

head(iris)

# 由於分群屬於「非監督式學習」的演算法，
# 因此我們先把iris內的品種(Species)欄位拿掉，以剩下的資料進行分群：
data <- iris[, -5]  
head(data)       



E.dist <- dist(data, method="euclidean") # 歐式距離
M.dist <- dist(data, method="manhattan") # 曼哈頓距離



#根據資料間的距離，來進行階層式分群，使用hclust()：

par(mfrow=c(1,2)) # 讓圖片以1x2的方式呈現，詳情請見(4)繪圖-資料視覺化

# 使用歐式距離進行分群
h.E.cluster <- hclust(E.dist)
plot(h.E.cluster, xlab="歐式距離")

# 使用曼哈頓距離進行分群
h.M.cluster <- hclust(M.dist) 
plot(h.M.cluster, xlab="曼哈頓距離")





#在hclust()裡面可以調整參數method，選擇不同的結合方法：

# hclust(E.dist, method="single")   # 最近法
# hclust(E.dist, method="complete") # 最遠法
# hclust(E.dist, method="average")  # 平均法
# hclust(E.dist, method="centroid") # 中心法
# hclust(E.dist, method="ward.D2")  # 華德法

#在這個例子中，我們就用歐式距離搭配華德法，來進行階層式分群：

E.dist <- dist(data, method="euclidean")      # 歐式距離
h.cluster <- hclust(E.dist, method="ward.D2") # 華德法






# 視覺化
plot(h.cluster)
abline(h=9, col="red")

# 由上圖，可以觀察最佳的分群數目是3個，
# 因此我們可以利用cutree()，讓整個階層的結構縮減，變成分成三群的狀態：

cut.h.cluster <- cutree(h.cluster, k=3)  
cut.h.cluster                            
# [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [40] 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3
# [79] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 3 3 3 3 2 3 3 3 3 3 3 2 2 3 3
# [118] 3 3 2 3 2 3 2 3 3 2 2 3 3 3 3 3 2 2 3 3 3 2 3 3 3 2 3 3 3 2 3 3 2

table(cut.h.cluster, iris$Species)       
# 分群結果和實際結果比較
# cut.h.cluster setosa versicolor virginica
#         1     50          0         0
#         2      0         49        15
#         3      0          1        35


library(ggplot2)
qplot(x=Petal.Length,                               
      y=Petal.Width,                              
      data=iris,                      
      geom="point",                  # 圖形=scatter plot
      main = "Scatter Plot of Flower Species", #標題
      xlab="Petal.Length",                          
      ylab="Petal.Width",                    
      color= Species    # 以顏色標註品種，複合式的散布圖
)

# 圖中的右上角，有一些virginica(藍色)和versicolor(綠色)靠得十分近



##################################################################################################################
#切割式分群(Partitional Clustering)



#K-Means
#使用函式是kmeans()：
# 分成三群
kmeans.cluster <- kmeans(data, centers=3) 

# 群內的變異數
kmeans.cluster$withinss

# 分群結果和實際結果比較
table(kmeans.cluster$cluster, iris$Species)  
# setosa versicolor virginica
# 1      0          2        36
# 2      0         48        14
# 3     50          0         0


# 視覺化 k-means 分群結果(基於ggplot2的語法)
install.packages("factoextra")
library(factoextra)

fviz_cluster(kmeans.cluster,           # 分群結果
             data = data,              # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             frame.type = "norm")      # 框架型態






# K-Medoid
# 使用函式是pam()，在cluster這個套件裡面：

require(cluster)

# pam = Partitioning Around Medoids
kmedoid.cluster <- pam(data, k=3) 

# 群內的變異數
kmedoid.cluster$objective

# 分群結果和實際結果比較
table(kmedoid.cluster$clustering, iris$Species) 
# setosa versicolor virginica
# 1     50          0         0
# 2      0         48        14
# 3      0          2        36


# 視覺化 k-medoid 分群結果(基於ggplot2的語法)
require(factoextra)
fviz_cluster(kmedoid.cluster,       # 分群結果
             data = data,           # 資料
             geom = c("point"),     # 點 (point)
             frame.type = "norm")   # 框架型態



##################################################################################################################
#分群的最佳數目


#Elbow Method
#找出一個數字n，使得資料被分成n群時，群內的總變異(SSE)會最小
#那麼n = 最佳的分群數目(optimal number for clusters)

require(factoextra)

# Elbow Method 應用在階層式分析
# 注意：這裡使用的是hcut()，屬於factoextra套件，並非上面提的hclust()
fviz_nbclust(data, 
             FUNcluster = hcut,  # hierarchical clustering
             method = "wss",     # total within sum of square
             k.max = 12          # max number of clusters to consider
) + labs(title="Elbow Method for HC") +
    geom_vline(xintercept = 3,       # 在 X=3的地方 
               linetype = 2)         # 畫一條虛線


# Elbow Method 應用在 K-Means
fviz_nbclust(data, 
             FUNcluster = kmeans,# K-Means
             method = "wss",     # total within sum of square
             k.max = 12          # max number of clusters to consider
) +labs(title="Elbow Method for K-Means") +
    geom_vline(xintercept = 3,        # 在 X=3的地方 
               linetype = 2)          # 畫一條垂直虛線


# Elbow Method 應用在 K-Medoid
fviz_nbclust(data, 
             FUNcluster = pam,   # K-Medoid
             method = "wss",     # total within sum of square
             k.max = 12          # max number of clusters to consider
) +labs(title="Elbow Method for K-Medoid") +
    geom_vline(xintercept = 3,       # 在 X=3的地方 
               linetype = 2)         # 畫一條垂直虛線






# Average Silhouette Method
#
# 除了計算SSE以外，另一個衡量分群效果的方法，稱為平均側影法(Average silhouette Method)。
# 
# 側影系數(Silhouette Coefficient)會根據每個資料點(i)的內聚力和分散力，衡量分群的效果(quality)。

#在R裡面，寫法和Elbow Method完全一模一樣，
#差別只在於參數method="silhouette"而已：

#舉K-Means為例:

require(factoextra)

# Avg. Silhouette 應用在 K-Means
fviz_nbclust(data, 
             FUNcluster = kmeans,   # K-Means
             method = "silhouette", # Avg. Silhouette
             k.max = 12             # max number of clusters
) + labs(title="Avg.Silhouette Method for K-Means") 





##################################################################################################################
#譜分群(Spectral Clustering)
























