

library(dplyr)
library(magrittr)   #pipelines
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization


inputData <- 
    USArrests %>% 
    na.omit() %>% # 忽略遺失值
    scale() # 資料標準化

head(inputData)


distance <- get_dist(x = inputData)

fviz_dist(dist.obj = distance, 
          gradient = list(
              low = "#00AFBB", 
              mid = "white", 
              high = "#FC4E07"))



set.seed(101)
k_clust <- kmeans(inputData, 
                  centers = 2, #指定分群數目k或指定初始中心點數目
                  nstart = 25) #隨機起始中心點的選擇
str(k_clust)
# #
# List of 9
# $ cluster     : Named int [1:50] 1 1 1 2 1 1 2 2 1 1 ...
# ..- attr(*, "names")= chr [1:50] "Alabama" "Alaska" "Arizona" "Arkansas" ...
# $ centers     : num [1:2, 1:4] 1.005 -0.67 1.014 -0.676 0.198 ...
# ..- attr(*, "dimnames")=List of 2
# .. ..$ : chr [1:2] "1" "2"
# .. ..$ : chr [1:4] "Murder" "Assault" "UrbanPop" "Rape"
# $ totss       : num 196 
# $ withinss    : num [1:2] 46.7 56.1
# $ tot.withinss: num 103
# $ betweenss   : num 93.1
# $ size        : int [1:2] 20 30
# $ iter        : int 1
# $ ifault      : int 0
# - attr(*, "class")= chr "kmeans"



fviz_cluster(k_clust, data = inputData)

inputData %>%
    as_tibble() %>%   #將data.frame轉換為tbl_df/tibble
    mutate(cluster = k_clust$cluster, #新增分群結果
           state = row.names(USArrests) #將列名稱指定為原始資料標籤
    ) %>%
    ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
    #使用ggplot套件繪圖（指定x,y軸），標記國家名稱，並依據分群結果上色
    geom_text()









#需預設分群數目k

set.seed(101)

k_clust <- kmeans(inputData, centers = 2, nstart = 25)
k_clust_3 <- kmeans(inputData, centers = 3, nstart = 25)
k_clust_4 <- kmeans(inputData, centers = 4, nstart = 25)
k_clust_5 <- kmeans(inputData, centers = 5, nstart = 25)


# plots to compare
p1 <- fviz_cluster(k_clust, geom = "point", data = inputData) + ggtitle("k = 2")
p2 <- fviz_cluster(k_clust_3, geom = "point",  data = inputData) + ggtitle("k = 3")
p3 <- fviz_cluster(k_clust_4, geom = "point",  data = inputData) + ggtitle("k = 4")
p4 <- fviz_cluster(k_clust_5, geom = "point",  data = inputData) + ggtitle("k = 5")


library(gridExtra)

grid.arrange(p1, p2, p3, p4, nrow = 2)


#使用K-Medoid替代K-Mean

str(inputData)

kmedoid.cluster <- pam(x = inputData, #數值矩陣、data frame、相異度矩陣
                       k=3,          #分群數目
                       #diss=TRUE,  #是否為相異度矩陣
                       # metric=”euclidean”   #計算相異度矩陣的距離方式
                       )  

# 分群結果視覺化
fviz_cluster(kmedoid.cluster, data = inputData,main = 'K-Medoid')

# 繪製側影圖
plot(kmedoid.cluster,which.plots = 2)



#決定最適分群數目

#Elbow Method

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
    kmeans( x = inputData, 
            centers =  k, 
            nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)
# 
# > wss_values
# [1] 196.00000 102.86240  78.32327  56.40317  48.94420  42.87954
# [7]  38.35275  34.44327  30.44225  26.18348  23.66980  22.31869
# [13]  20.10611  18.37856  16.81152

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


#將上述數行程式碼縮減到一行指令:
set.seed(123)
fviz_nbclust(x = inputData,FUNcluster = kmeans, method = "wss")



# Average Sihouette Method
km.res <- kmeans(x = inputData, 
                 centers = 5, 
                 nstart = 25)

#計算每個觀測值的silhouette width
ss <- silhouette(km.res$cluster, dist(inputData))
ss

#取整體平均:
mean(ss[ ,3])
# 0.3030781

plot(ss)


#根據silhouette，比較不同k值的分群表現:

avg_sil <- function(k) {
    
    km.res <- kmeans(x = inputData, centers = k, nstart = 25)
    
    ss <- silhouette(km.res$cluster, dist(inputData))
    
    mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)
# #
# > avg_sil_values
# [1] 0.4084890 0.3094312 0.3396889 0.3030781 0.2904337 0.2936182
# [7] 0.2624851 0.2576100 0.2543804 0.2528853 0.2679385 0.2613045
# [13] 0.2762587 0.2769522


plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")


#使用fviz_nbclust函數將上述程式碼指令濃縮成一行:
fviz_nbclust(inputData, kmeans, method="silhouette")



# Gap Statistic Method
set.seed(123)
gap_stat <- clusGap(x = inputData,
                    FUN = kmeans, 
                    nstart = 25,
                    K.max = 10, 
                    B = 50)

# Print the result
print(gap_stat, method = "firstmax") #找最大化Gap(k)的最小k值
# #
#         logW   E.logW       gap     SE.sim
# [1,] 3.458369 3.640154 0.1817845 0.04422857
# [2,] 3.135112 3.372283 0.2371717 0.03559601
# [3,] 2.977727 3.233771 0.2560446 0.03749193
# [4,] 2.826221 3.119172 0.2929511 0.04067348
# [5,] 2.738868 3.019965 0.2810969 0.04185469
# [6,] 2.666967 2.930002 0.2630347 0.04105040
# [7,] 2.609895 2.852152 0.2422572 0.04184725
# [8,] 2.539156 2.778562 0.2394054 0.04292750
# [9,] 2.468162 2.711752 0.2435901 0.04344197
# [10,] 2.407265 2.647595 0.2403307 0.04548446


fviz_gap_stat(gap_stat)




















