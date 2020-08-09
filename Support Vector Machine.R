
# 本篇目錄

    # 簡言
    # Support Vector Machine(SVM)：如何用SVM來分類(類別)

    # SVM在多元分類(multi-class)的技巧
        # One-against-Rest
        # One-against-One

    # Support Vector Regression(SVR)：如何用SVR來預測(連續)

    # 參數討論
        # C(cost)
        # Epsilon (ε)
        # Gamma

    # 參數調整(Tune Parameters)：如何調整SVM的參數，讓模型表現出最佳狀態
        # Tune parameters in SVM(soft-margin)
        # Tune parameters in SVR

#####################################################################################

# SVM是一種知名的二元分類器(binary classifier)，
# 由俄羅斯的統計學家Vapnik 等人所提出，一種基於統計學習理論的演算法。
# 
# 簡單地說，SVM是一種監督式學習的演算法，
# 試圖從資料中建構一個超平面(hyperplane)，將資料區分成兩個類別(2 classes)，
# 最後進行預測/分類。
#
# 一直以來，SVM就是資料科學中最受歡迎的分類演算法之一。
#
# 無論是小數據的運用(和深度學習需要大數據Big Data在背後支援不同)；
# 非線性可分(non-linear separability)的問題；
# 高維模式識別問題上(醫學、圖像辨識…)，SVM都有不錯的表現。
# 
# 其概念是建構一個超平面(hyperplane)，讓資料在空間中能夠被區分成兩類，所以又被稱為二元分類器(binary classifier)。
# 
# 在二維的空間中，超平面指的就是「一條線」；
# 三維空間中，則是「一個平面」；
# 之所以有一個「超」字，是因為資料往往不會只有二維、三維。
# 在更高維的空間中，我們無法觀察這個平面的形狀為何，於是就用「超平面(hyperplane)」一詞來概括。

#####################################################################################

install.packages("e1071")

# 使用套件mlbench中的資料Glass來簡單示範。
# 
# 這是一個分類問題，
# 資料中記錄了六種玻璃材質內的化學元素含量，共有214個觀測值，10個變數：
install.packages("mlbench")
data(Glass, package = "mlbench")
data = Glass
str(data)

#分成80%的訓練資料(Train)，20%的測試資料(Test)：
smp.size = floor(0.8*nrow(data)) 
set.seed(516)                     
train.ind = sample(seq_len(nrow(data)), smp.size)
train = data[train.ind, ] # 80%
str(train)
test = data[-train.ind, ] # 20%
str(test)


#####################################################################################


#使用svm()訓練SVM的分類模型：
library(e1071)
model = svm(formula = Type ~ .,  # 依變數(在這裡是Type)的資料形態要是Factor
            data = train)


summary(model)   # 可以看到SVM預設的參數設定
#
# Call:
#     svm(formula = Type ~ ., data = train)
# 
# 
# Parameters:
#     SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  1 
# 
# Number of Support Vectors:  149
# 
# ( 14 9 52 7 50 17 )
# 
# 
# Number of Classes:  6 
# 
# Levels: 
#     1 2 3 5 6 7

#####################################################################################

#針對訓練資料、測試資料進行預測，使用predict()函式：

# 預測
train.pred = predict(model, train)
test.pred = predict(model, test)

# 訓練資料的混淆矩陣
table(real=train$Type, predict=train.pred)
#
#        predict
# real  1  2  3  5  6  7
# 1    47 10  0  0  0  0
# 2    11 47  0  0  0  0
# 3     9  7  1  0  0  0
# 5     0  0  0 10  0  0
# 6     0  0  0  0  7  0
# 7     1  0  0  0  0 21

# 測試資料的分類準確率
confus.matrix = table(real=test$Type, predict=test.pred) #注意:不是訓練資料的混淆矩陣!
#
#        predict
# real  1  2  3  5  6  7
# 1    12  1  0  0  0  0
# 2     4 12  0  0  1  1
# 3     0  0  0  0  0  0
# 5     0  0  0  2  0  1
# 6     0  0  0  0  2  0
# 7     0  0  0  0  0  7

sum(diag(confus.matrix))/sum(confus.matrix)
# 0.8139535


# 在訓練及測試資料上都得到76%左右的準確率，效果還不錯，而且模型看起來並沒有發生overfitting的問題。
# 而且，這僅僅只是使用預設參數(default)所建立的SVM分類模型。
# 只要懂得進一步調整參數，便可以讓模型的表現更勝一層樓。



#####################################################################################

#SVM在多元分類(multi-class)的技巧
#
#「一個二元分類器SVM，怎麼解決有六個類別的多元分類(multi-class)問題呢？」


# (1) One-against-Rest(One-vs-All, OvA, OvR)：一對多

# 這個策略的想法是，就是針對每一個類別，分別建立一個SVM(或其他二元分類器)：
#屬於此類別的樣本視為(+1)，其他類別的樣本視為(-1)，
#如此一來，就轉換成一個二元分類的問題了
# 
# 以第1節的資料為例，資料中有六個類別(1~6，玻璃材質)，那我們便會建立六個SVM：
# 
# 第一個SVM：屬於類別1的資料為(+1)，其他類別為(-1)，這個SVM用來區別這兩者
# 
# 第二個SVM：屬於類別2的資料為(+1)，其他類別為(-1)，這個SVM用來區別這兩者
# 
# 第三個SVM：屬於類別3的資料為(+1)，其他類別為(-1)，這個SVM用來區別這兩者
# 
# 以此類推…
# 
# 換句話說，針對有t個類別的資料，就會存在著t個SVM。
# 
# 當有一筆新資料要預測時，會分別丟進這t個SVM，得到t組值(v1, v2, v3…vt)，
#再從中判別最大的值(如果類別是+1,-1)出現第幾個位置，那這筆資料便是屬於那一類。
# 
# 這樣的做法很直覺，而且執行時間與記憶體並不會消耗太多。
# 
# 但缺點是，將「剩下類別」視為同一個類別(-1)的這種做法，很容易導致(+1,-1)之間的資料筆數差距很大，也就是類別不平衡(class imbalance)的問題。




# (2) One-against-One(OvO)：一對一
# 這個策略的想法，很像高中數學的排列組合：
#從T種類別中任取2種類別，共會有幾種組合？
# 
# 答案是：C(T,2) = T(T-1)/2
# 
# 所以在這個策略底下，我們便會從多元類別(multi classes)的資料中，任選某兩個類別(2 classes)的資料，訓練一個SVM(只能區分這兩個類別)，
#並重複這樣的動作，直到所有的類別組合，都有其對應的SVM為止。
# 
# 因此，最後會有 T(T-1)/2 個SVM模型。
# 
# 當有一筆新資料要預測時，會分別丟進這 T(T-1)/2 個SVM，每一個SVM都會將這筆資料分到某一類，
#就像是投票一樣，該類別會記錄+1，最後判斷哪一個類別獲得最多票數，即可預測這筆資料屬於哪一個類別。
# 
# 和One-against-Rest不同，這樣比較並不會造成類別不平衡的問題；
# 
# 但相對的，這個策略所需的運行時間較長；也吃較多的記憶體；而且有時候會發生兩個以上的類別獲得同票數的狀況，造成判斷上的困擾。



#在使用svm()的時候，裡面其實已經做了One-against-One。





#####################################################################################

#Support Vector Regression(SVR)
# 
# SVR是本來SVM的延伸型態，能夠處理連續的預測問題。
# 
# 在e1071套件裡面，並沒有一個函式叫做svr()，而是一樣用svm()。
# 
# 差別只在於：
# 
    # 依變數的型態是factor時，svm()會建立SVM的超平面，來處理分類問題
    # 依變數的型態是numeric，svm()會轉為SVR，進行連續值的預測。


data = data.frame(x=1:20,
                  y=c(3,4,8,2,6,10,12,13,15,14,17,18,20,17,21,22,25,30,29,31))

# 資料的原始值
plot(data$x, data$y, pch=16, xlab="X", ylab="Y")


########################################################
#先拉一條簡單的線性迴歸：

model <- lm(y ~ x , data) 

# lm預測
lm.pred = predict(model, data)

# 資料的原始值(黑點)
plot(data$x, data$y, pch=16, xlab="X", ylab="Y")
# lm的預測值(紅三角形)
points(lm.pred, pch=2, col="red")
abline(model, col="red")


########################################################

#直接用SVR來建模、預測：

model <- svm(y ~ x , data) # 依變數的型態要是numeric

# 預測
svr.pred = predict(model, data)

# 資料的原始值(黑點)
plot(data$x, data$y, pch=16, xlab="X", ylab="Y")
# SVR的預測值(藍叉)
points(svr.pred, pch=4, col="blue")


#########################################################

#比較一下線性迴歸和SVR的表現，同時計算RMSE(root mean square error)：

# 資料的原始值(黑點)
plot(data$x, data$y, pch=16, xlab="X", ylab="Y")
# lm的預測值(紅三角形)
points(lm.pred, pch=2, col="red")
# SVR的預測值(藍叉)
points(svr.pred, pch=4, col="blue")

# (lm, SVR) in RMSE
c(sqrt(mean((data$y - lm.pred)^2)),
  sqrt(mean((data$y - svr.pred)^2))
)
## [1] 1.914203 1.795094
#可以發現到，在這個例子，SVR比lm的效果還要好一些些(1.79 < 1.91)。


#####################################################################################
# Most Important~
#
#參數討論:
#
# 
# svm(...
#   type    = 決定svm是要用來classification(類別)、還是regression(連續)。
#   scale   = 將資料正規化成(平均值, 標準差) = (0,1) 的分佈。
#   kernel  = 將資料映射到特徵空間的kernel-fun，用來處理「非線性可分」的問題。
#     

#   *cost    = 在Lagrange formulation中的大C，
    #決定給被誤差/分錯的資料「多少」懲罰值。

    #表示有多重視離群值

    # 一開始的SVM，是要尋找一個能夠完美將「所有」資料分成兩邊，具有最大margin的超平面，這又被稱為“hard-margin SVM”。
    
    # 但由於hard-margin SVM，追求要將資料完美分好，因此很容易有overfitting的風險。
    
    # 於是1995年，Vapnik等人提出了“soft-margin SVM”，讓SVM能容許一些被分錯的資料存在。
    
    # 在soft-margin SVM的損失函數(loss function)中，這個大C的存在，就是容錯項。
    
    #藉由C，我們能給予那些被分錯的資料懲罰值，控制support vectors(用來決定超平面的那些資料點)的影響力
    
    #換句話說：
        # C越大，代表容錯越小，越少support vectors，越接近hard-margin SVM的概念，卻容易overfitting
        
        # C越小，代表容錯越大，越多support vectors，可以追求更大的margin，容易underfitting




#   *epsilon = margin of tolerance。越大，表示在容忍範圍內的誤差/分錯的資料，不會被懲罰；反之，越接近0，每一個誤差/分錯的資料都會被懲罰。
    
    # 這個參數主要影響的會是SVR，而非SVM。因為在SVR的損失函數中，使用的是epsilon intensive hinge loss。
    # 
    # Epsilon(ε)的概念是給予一個margin of tolerance，創造一個「絕對領域」的感覺。
    # 
    # 在「絕對領域」內的資料點會被忽視，它們的殘差(error)一點幫助也沒有，換句話說：它們對訓練SVR一點幫助也沒有。
    #
    #因此我們可以這樣說：
        # Epsilon越大，代表容忍區塊越大，越多資料會被忽視，造成模型的準確度越低，support vectors的數量減少
          #(記得，所謂的support vectors是那些across margin的資料點，它們的殘差會被納入考量，用來決定最後的margin)。
        
        # Epsilon越低(→0+)，所有的資料殘差(error)都會被考慮，卻也容易造成overfitting。




#    *gamma   = 在kernel-fun裡面的參數(linear-fun除外)。
    # Gamma的意義比較難以說明，當使用kernel function將原始資料映射到特徵空間(feature space)時，
    #它隱含地決定了資料在特徵空間的分佈狀況。
    # 
    # 以幾何的觀點來看，當gamma增加時，會讓Radial Basis Function(RBF)裡面的σ變小，
    # 而σ很小的高斯分佈會又高又瘦，讓只在附近的資料點有所作用。(參考)
    # 
    # 在定義中，Gamma = How far the influence of a single training example reaches，意思是(參考)：
        # 
        # gamma大，資料點的影響力範圍比較近，對超平面來說，近點的影響力權重較大，容易勾勒出擬合近點的超平面，也容易造成overfitting。
        # 
        # gamma小，資料點的影響力範圍比較遠，對超平面來說，較遠的資料點也有影響力，因此能勾勒出平滑、近似直線的超平面。
#     ...
# )


#####################################################################################

#參數調整(Tune Parameters)
# 
# 
# 在調參數的階段，所使用的手法被稱為grid search，
#概念是針對每一種參數組合，都會訓練一個對應的模型，最後觀察模型的表現，挑出表現最佳的模型。
# 
# 在訓練的過程中，R會自動引入cross validation的手法，確保模型的可靠度(Robustness)，讓tune出來的參數是可以採用的。
# 
# 


#####################################################################################
# Tune parameters in SVM(soft-margin)
# 在SVM中，一般會去調的參數是(cost, gamma)：

# data
require("mlbench")
data(Glass, package="mlbench")
data = Glass

# tune cost and gamma in SVM(soft-margin)
tune.model = tune(svm,
                  Type~.,
                  data=data,
                  kernel="radial", # RBF kernel function
                  range=list(cost=10^(-1:2), gamma=c(.5,1,2))# 調參數的最主要一行
)


summary(tune.model)
# 
# Parameter tuning of ‘svm’:
#     
#     - sampling method: 10-fold cross validation 
# 
# - best parameters:
# cost gamma
# 10   0.5
# 
# - best performance: 0.3181818 
# 
# - Detailed performance results:
#     cost gamma     error dispersion
# 1    0.1   0.5 0.5807359 0.11632977
# 2    1.0   0.5 0.3456710 0.08564338
# 3   10.0   0.5 0.3181818 0.10774303
# 4  100.0   0.5 0.3746753 0.11663545
# 5    0.1   1.0 0.5852814 0.09453697
# 6    1.0   1.0 0.3599567 0.09478361
# 7   10.0   1.0 0.3694805 0.12618654
# 8  100.0   1.0 0.4021645 0.13339298
# 9    0.1   2.0 0.5803030 0.09526242
# 10   1.0   2.0 0.3883117 0.10576915
# 11  10.0   2.0 0.3701299 0.12517570
# 12 100.0   2.0 0.3699134 0.12305110



# 在訓練的過程中，我們訓練的是一堆模型，
# 包含cost=10^-1, 10^0, 10^1, 10^2，gamma=0.5, 1, 2，這兩種參數的排列組合，
# 換句話說，會有 4x3=12 個SVM模型。
# 
# 裡面的值是classification error


plot(tune.model)
# 進一步，可以根據這張圖，縮小cost和gamma的範圍，再去去tune更多的參數組合。
# 
# 最後，要挑出表現最佳的模型，可以直接從tune()回傳的結果中取出($best.model)：

# Best model in set of tuning models
tune.model$best.model
#
# Call:
#     best.tune(method = svm, train.x = Type ~ ., data = data, ranges = list(cost = 10^(-1:2), 
#                                                                            gamma = c(0.5, 1, 2)), kernel = "radial")
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
#       cost:  10 
# 
# Number of Support Vectors:  162


#####################################################################################

#Tune parameters in SVR
#在SVR中，一般會去調的參數是(cost, epsilon)：
# data
data = data.frame(x=1:20,
                  y=c(3,4,8,2,6,10,12,13,15,14,17,18,20,17,21,22,25,30,29,31))

# tune cost and epsilon in SVR
tune.model = tune(svm,
                  y~x,
                  data=data,
                  range=list(cost=2^(2:9), epsilon = seq(0,1,0.1))# 調參數的最主要一行
)

# 在訓練的過程中，我們訓練的是一堆模型，包含cost=2^2., 2^3. … 2^9.，epsilon=0, 0.1, 0.2…1，這兩種參數的排列組合，
#換句話說，會有8x11=88個SVR模型。
# 
# 裡面的值是mean squared error

tune.model
#
# Parameter tuning of ‘svm’:
#     
#     - sampling method: 10-fold cross validation 
# 
# - best parameters:
# cost epsilon
# 4     0.1
# 
# - best performance: 4.267714 


plot(tune.model)

# Best model in set of tuning models
tune.model$best.model
# Call:
# best.tune(method = svm, train.x = y ~ x, data = data, ranges = list(cost = 2^(2:9), 
#                                                                     epsilon = seq(0, 1, 0.1)))
# 
# 
# Parameters:
#   SVM-Type:  eps-regression 
# SVM-Kernel:  radial 
#       cost:  4 
#      gamma:  1 
#    epsilon:  0.1 
# 
# 
# Number of Support Vectors:  13

#####################################################################################

#結論:
#
# SVM是資料科學中最重要的演算法之一，它同時具有機器學習和統計理論的長處，在分類及預測的問題上都表現得不錯。
# 
# 但要充分發揮SVM/SVR的長處，需要將其中的許多細節(參數)知道得十分清楚，同時具有對資料的敏銳度才行。
# 
# 在e1071套件裡的svm()，演算法便是 soft-margin SVM

