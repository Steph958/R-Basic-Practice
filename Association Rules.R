
# 關聯式規則
#
# 用於從大量數據中挖掘出有價值的數據項之間的相關關係，
# 原則為不考慮項目的次序，而僅考慮其組合。
# 著名的購物籃分析 (Market Basket Analysis)即為關聯式規則分析的應用。
# Apriori演算法是挖掘布林關聯規則 (Boolean association rules) 頻繁項集的算法

install.packages("arules")

library(arules)

#以下以超市資料為例，使用關聯式規則分析執行購物籃分析。
#首先先讀入超市消費資料

# Load the libraries
if (!require('arules')){
    install.packages("arules");
    library(arules) #for Apriori演算法
}
if (!require('datasets')){
    install.packages("datasets");
    library(datasets) #for Groceries data
}

data(Groceries) # Load the data set
Groceries@data@Dim #169 種商品，9835筆交易資料



#可使用arules套件中的apriori函數來實作apriori演算法
# Get the rules

rules <- apriori(Groceries, # data= Groceries
                 parameter = list(supp = 0.001, conf = 0.8), #參數最低限度
                   control = list(verbose=F)) #不要顯示output

# 支持度(min support)：「規則」在資料內具有普遍性，也就是這些 A 跟 B 同時出現的機率多少
# 信賴度(min confidence)：「規則」要有一定的信心水準，也就是當購買 A 狀態下，也會購買 B 的條件機率

#rules
options(digits=2) # Only 2 digits
inspect(rules[1:5]) # Show the top 5 rules









#根據計算結果，解讀模型的方法如下：
#
#啤酒=>尿布
#
# Support: 一次交易中，包括規則內的物品的機率。買啤酒同時買尿布的機率。交集
# Confidence: 包含左邊物品A的交易也會包含右邊物品B的條件機率。在買了啤酒的顧客中，有買尿布的比例。
# Lift: 規則的信心比期望值高多少。（買了啤酒以後，有買尿布的機率）/（在所有顧客群中買尿布的機率）
    # lift=1: items on the left and right are independent.



#用排序功能排序後，列出最有關連（confidence最高）的幾條規則

rules<-sort(rules, by="confidence", decreasing=TRUE) #按照confidence排序
inspect(rules[1:5]) # Show the top 5 rules



#特別針對某項商品（右側變數）
#像是：買了什麼東西的人，會買牛奶呢？
rulesR<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08),
                appearance = list(default="lhs",rhs="whole milk"), #設定右邊一定要是牛奶
                control = list(verbose=F)) #不要顯示output

rulesR<-sort(rulesR, decreasing=TRUE,by="confidence") #按照confidence排序
inspect(rulesR[1:5]) # Show the top 5 rules


#特別針對某項商品（左側變數）
#像是：買了牛奶的人，會買什麼呢？
rulesL<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2),
                appearance = list(default="rhs",lhs="whole milk"), #設定左邊一定要是牛奶
                control = list(verbose=F)) #不要顯示output

rulesL<-sort(rulesL, decreasing=TRUE,by="confidence") #按照confidence排序
inspect(rulesL[1:5]) # Show the top 5 rules







#冗規則判斷與去除

# 先根據 support 大小排序 rules:

sort.rules <- sort(rules, by="support")

# 'arules' version = 1.4-2 , under R-3.2.5
subset.matrix <- is.subset(x=sort.rules, y=sort.rules)
subset.matrix 

# 'arules' version = 1.5-2 , under R-3.4.0
subset.matrix <- as.matrix(is.subset(x=sort.rules, y=sort.rules))
subset.matrix  #在X的項目，如果是Y項目的子集(subset)，就會回傳TRUE。
              #當你用RStudio打開subset.matrix這個變數時，會看見一個kxk的矩陣


#再進行以下步驟：

# 把這個矩陣的下三角去除，只留上三角的資訊
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
subset.matrix

# 計算每個column中TRUE的個數，若有一個以上的TRUE，代表此column是多餘的
redundant <- colSums(subset.matrix, na.rm=T) >= 1
str(redundant)

# 移除多餘的規則
sort.rules <- sort.rules[!redundant]
sort.rules

inspect(sort.rules)









#規則視覺化

if (!require('arulesViz')){
    install.packages("arulesViz"); 
    library(arulesViz)
}

plot(sort.rules)
plot(sort.rules, method="graph")
plot(sort.rules,method="graph",interactive=TRUE,shading=NA) 
plot(sort.rules, method="grouped")
#會跑一陣子








