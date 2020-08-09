
# 簡言

    # 資料

# Bagging
    # R Code for Bagging Implement
    # Random Forest

# Boosting
    # Gradient Boosting Machine(XGboost)

# Stacking
    # R Code for Stacking Implement
        # 第一階段(Stacking)
        # 第二階段(Blending)

##############################################################################
install.packages("lasso2")

data(Prostate, package="lasso2")
str(Prostate)

data <- Prostate


# 先把資料區分成 train=0.8, test=0.2 
set.seed(22)
train.index <- sample(x=1:nrow(data), size=ceiling(0.8*nrow(data) ))

train = data[train.index, ]
test = data[-train.index, ]


##############################################################################


# Bootstrap aggregating (Bagging)
#
#從字面上來看，就是將資料裝成一個袋子一個袋子(Bag)，然後將每個袋子的結果結合在一起。
# 
# 演算法上，是將樣本重複抽樣(取後放回)，產生多個子資料集(Subsets)後，依序建立多個模型，最後再將所有模型的結果彙整在一起。
#
# 在模型 Bias 跟 Variance 的議題上，Bagging 的手法有助於降低 variance。
# 
# 當每個子集資料在建模的時候，每個模型若獨自拿出來看，會發現都是一個「強模型」(較複雜的模型)，具有低 bias 跟高 variance 的特性；
# 而如今，我們把不同高 variance 的模型結合在一起後，因為是平均(投票)的概念，其結果就會趨近於整體的平均表現，因此 variance 就不會太大。
# 






#Bagging 的一開始，是要先把資料做重複抽樣，
#因此先將 Train 分成三個子資料集(n=40)：

# Subset-1
set.seed(1)
ind_1 = sample(1:nrow(train), 40)
subset_1 = train[ind_1, ]

# Subset-1
set.seed(2)
ind_2 = sample(1:nrow(train), 40)
subset_2 = train[ind_2, ]

# Subset-3
set.seed(3)
ind_3 = sample(1:nrow(train), 40)
subset_3 = train[ind_3, ]



#對subset_1，subset_2，subset_3分別建立線性迴歸模型，
#再將 Test 的預測結果平均起來：

# Model-1 : linear regression
model_1 = lm(lpsa~., subset_1)
y1 = predict(model_1, test)

# Model-2 : linear regression
model_2 = lm(lpsa~., subset_2)
y2 = predict(model_2, test)

# Model-3 : linear regression
model_3 = lm(lpsa~., subset_3)
y3 = predict(model_3, test)

# Average Prediction Results
ave_y = (y1+y2+y3)/3


#由於lpsa是連續值，因此使用 MSE 來比較原本模型與 Bagging 後的表現好壞。

# MSE Comparision between three models and the bagging model
c(mean((y1 - test$lpsa)^2),   # linear regression of subset_1
  mean((y2 - test$lpsa)^2),   # linear regression of subset_2
  mean((y3 - test$lpsa)^2),   # linear regression of subset_3
  mean((ave_y - test$lpsa)^2))  # bagging model

#[1] 0.6269814 0.5254751 0.5822511 0.5169099


##############################################################################

# Random Forest
# 
# 所謂的隨機森林，就是運用 Bagging + CART決策樹，
#也就是說Model-1 ~ Model-n全都都是用決策樹來建模，而這麼多棵的樹組合在一起，所以才稱為「森林」。
# 
# 要注意的是，隨機森林在抽樣過程中，不只是對 Row 進行抽樣，同時也會對 Column 抽樣，
#因此產生的子集資料，其實是對欄跟列抽樣後的結果。
#之後再針對這些子集資料，各自訓練一棵決策樹，形成隨機森林。
# 
# 事實上，在面對資料中有共線性(collinearity)跟類別不平衡(Class Imbalance Problem)，
#而這些問題會對預測結果造成不良影響時，隨機森林是倍受青睞的演算法。
#
#(若是探討對「變數解釋性」的影響，則需要用 Lasso 或 Stepwise 來解決)，
#
#其概念應該不難理解：
    #對 Row 抽樣時，可以部份解決類別不平衡來影響預測的問題；
    #對 Column 抽樣時，可以部份解決共線性來影響預測的問題

require(randomForest)

rf_model = randomForest(lpsa~.,
                        data=train,
                        ntree=150  # 決策樹的數量
)
# #
# Call:
#     randomForest(formula = lpsa ~ ., data = train, ntree = 150) 
# Type of random forest: regression
# Number of trees: 150
# No. of variables tried at each split: 2
# 
# Mean of squared residuals: 0.674582
# % Var explained: 48.74




# 預測
rf_y = predict(rf_model, test)
mean((rf_y - test$lpsa)^2) # MSE
#[1] 0.6547685




# Observe that what is the best number of trees
plot(rf_model) 
#70棵樹就夠了






# 在決定好ntree的數量後，另一個參數mtry其實也需要去 Tune，
# 這個參數代表每次抽樣時需要抽「多少個變數」的意思。
#  
# 我們可以使用tuneRF()來 tune mtry的值，
# 並根據下面的結果與圖，得知每次抽樣時「抽2個變數」會是比較好的選擇：

tuneRF(train[,-9], train[,9])
#
# mtry = 2  OOB error = 0.6186665 
# Searching left ...
# mtry = 1 	OOB error = 0.7621107 
# -0.2318603 0.05 
# Searching right ...
# mtry = 4 	OOB error = 0.6615442 
# -0.06930667 0.05 
# mtry  OOBError
# 1    1 0.7621107
# 2    2 0.6186665
# 4    4 0.6615442




#根據獲得參數重新跑一次模型
rf_model = randomForest(lpsa~.,
                        data = train,
                        ntree = 50, # 決策樹的數量
                        mtry = 2    #每次抽樣時需要抽的變數數量
)

# 預測
rf_y = predict(rf_model, test)
mean((rf_y - test$lpsa)^2) # MSE
#[1] 0.6341545




#變數挑選
#
# Variable Importance of Random Forest
rf_model$importance
#
# IncNodePurity
# lcavol      31.993850
# lweight     10.468467
# age          7.444329
# lbph         4.398892
# svi          8.350435
# lcp         14.816933
# gleason      6.295688
# pgg45       11.632984

varImpPlot(rf_model)





##############################################################################

#  Boosting
# 
# 跟 Bagging 使用多個「強模型」不同， Boosting 會強調使用上需要多個「弱模型」才可以。
# 
# 今天 M1 M2 M3 如果太複雜(太強)，那彼此之間就會互相干擾，影響最後預測/分類結果；
#唯有彼此都是「弱模型」，才能好好專注在自己本身的預測/分類，然後再把彼此的成果結合一起，這就是 Boosting 的概念。
#(當然以計算效率的考量，這樣做也比較快)
# 
# 在演算法中，要從資料中找 M1 M2 M3…的模型是有順序的，概念跟 Bagging完全不一樣：
# 
# 在 Bagging 時，我們是將資料做抽樣，因此獲得許多子集資料，並各別建模後，把結果平均/投票。
# 
# 在 Boosting 時:
    #一開始先建一個簡單的模型 M1 ，此時會有預測錯誤的資料，
    #把這些資料的權重加大，建 M2 模型，
    #然後又會有預測錯的資料，再把資料的權重加大，建 M3 模型…
# 
# 在模型 Bias 跟 Variance 的議題上，Boosting 的手法有助於降低 bias。
# 
# 由於使用上是拿「弱模型」來用，這些弱模型其實是高 bias 跟 低 variance 的，
# 並且每次迭代的時候，都會基於先前的模型上進行優化
# (用梯度下降法，決定這次模型建在哪裡能使損失函數下降最多)。
# 既然是降低損失函數，表示過程中會越來越逼近實際值，
# 換句話說，就是逐漸降低 bias 的意思。
# 

##############################################################################

# Gradient Boosting Machine(XGboost)

# 所謂的 GBM 算是一種概念，是將梯度下降法(Gradient Descending)跟 Boosting 套件節合在一起的演算法，而後面的 Machine 指不特定的模型，只要能用梯度下降法找尋方向的模型都可以。
# 
# 目前市面上，如果使用 gbm 的套件，基本上都是 Tree-based 為主，也就是將數百個弱決策樹(CART)，跟梯度下降法和 Boosting 結合在一起。
# 
# 而 XGboost 又有些許不同，是本來的 Gradient Boosting Decision Tree(GBDT)的改良版本



##############################################################################
# 1. 將資料格式(Data.frame)，用`xgb.DMatrix()`轉換為 xgboost 的稀疏矩陣
install.packages("xgboost")

require(xgboost)

dtrain = xgb.DMatrix(data = as.matrix(train[,1:8]),
                     label = train$lpsa)

dtest = xgb.DMatrix(data = as.matrix(test[,1:8]),
                    label = test$lpsa)
#
# dtrain
# xgb.DMatrix  dim: 78 x 8  info: label  colnames: yes
# 
#dtest
# xgb.DMatrix  dim: 19 x 8  info: label  colnames: yes



##############################################################################
# 2. 設定xgb.params，也就是 xgboost 裡面的參數
# 
xgb.params = list(
#     #col的抽樣比例，越高表示每棵樹使用的col越多，會增加每棵小樹的複雜度
     colsample_bytree = 0.5,

#     # row的抽樣比例，越高表示每棵樹使用的col越多，會增加每棵小樹的複雜度
     subsample = 0.5, 
     booster = "gbtree",

#     # 樹的最大深度，越高表示模型可以長得越深，模型複雜度越高
     max_depth = 2,    

#     # boosting會增加被分錯的資料權重，而此參數是讓權重不會增加的那麼快，因此越大會讓模型愈保守
     eta = 0.03,
#     # 或用'mae'也可以

     eval_metric = "rmse",                      
     objective = "reg:linear",

#     # 越大，模型會越保守，相對的模型複雜度比較低
     gamma = 0)               




##############################################################################
# 3. 使用xgb.cv()，tune 出最佳的決策樹數量
#
# 過程中，程式會根據 Train 和 Validation 的平均表現，自動判斷模型是否有 overfitting，
# 最後找出較好的 nrounds，會是一個最不 overfitting 的模型。
# 
# 要注意的是，這個最不 overfitting 的模型，是建立在一開始的基本參數設定之下，所以不一定是最好的。
# 如果發生某些情況，就得回去調 xgb.params，才有機會獲得更好的模型。

cv.model = xgb.cv(
    params = xgb.params, 
    data = dtrain,
     nfold = 5,    
    # 5-fold cv
    # 當nround=1，表示現在訓練「只有一棵樹」的模型。
    # 資料會分成5等份(因為是做5-fold cv)。
    # 這時，把第一等份的資料當作 Validation，剩下當作Train，訓練一次模型，得到Train 跟 Validation 的預測表現；
    # 以上做法重複 5 次，再取平均跟取 stand deviation，就會是 nround=1 時的模型平均表現；
    # nrounds=2 時，重複上面的動作。以此類推
    
    nrounds=200,   # 測試1-100，各個樹總數下的模型
    # 如果當nrounds < 30 時，就已經有overfitting情況發生，那表示不用繼續tune下去了，可以提早停止                
    early_stopping_rounds = 30, 
    print_every_n = 20 # 每20個單位才顯示一次結果，
) 
#
# [1]	train-rmse:2.170414+0.073478	test-rmse:2.169813+0.289608 
# Multiple eval metrics are present. Will use test_rmse for early stopping.
# Will train until test_rmse hasn't improved in 30 rounds.
# 
# [21]	train-rmse:1.397099+0.035320	test-rmse:1.428450+0.234440 
# [41]	train-rmse:0.982208+0.019527	test-rmse:1.038109+0.184029 
# [61]	train-rmse:0.761991+0.008893	test-rmse:0.862965+0.175182 
# [81]	train-rmse:0.640592+0.016733	test-rmse:0.787176+0.154625 
# [101]	train-rmse:0.570471+0.025216	test-rmse:0.765982+0.142871 
# [121]	train-rmse:0.522613+0.024496	test-rmse:0.753904+0.123841 
# [141]	train-rmse:0.488789+0.023782	test-rmse:0.751154+0.114101 
# [161]	train-rmse:0.461967+0.024659	test-rmse:0.752928+0.104080 
# [181]	train-rmse:0.438543+0.024053	test-rmse:0.756316+0.098537 
# Stopping. Best iteration:
# [153]	train-rmse:0.471549+0.023408	test-rmse:0.748672+0.108083


#畫圖觀察 CV 過程中Train 跟 Validation 資料的表現
#(紅色：Train，藍色：Validation)：

tmp = cv.model$evaluation_log

plot(x=1:nrow(tmp), y= tmp$train_rmse_mean, 
     col='red', 
     xlab="nround", 
     ylab="rmse", 
     main="Avg.Performance in CV") 

points(x=1:nrow(tmp), y= tmp$test_rmse_mean, col='blue') 

legend("topright", 
       pch=1, 
       col = c("red", "blue"), 
       legend = c("Train", "Validation") )
# 
# 一般來說，Train 的表現會比 Validation 還要好，這時有兩種情況要思考：
# 
    # 如果 Train 跟 Validation 相近，表示模型其實還可以訓練得更好(更複雜)，
    #藉由提高 Train 的表現，觀察 Validation 是否有機會提升，
    #因此可以調以下參數，以提高模型複雜度的概念進行：
        #
        # max_depth 調高 1 單位 (最建議調這個)
        # 
        # colsample_bytree、subsample調高比例 (調這個也不錯)
        # 
        # eta調低 (不得以才調這個)
        # 
        # gamma 調低 (不得以才調這個)
 
    # 如果 Train 比 Validation 好太多，就表示有 ovrfitting的問題發生，
    #這時候上面的參數就要反過來調，以降低模型複雜度的概念來進行。

# 獲得 best nround
best.nrounds = cv.model$best_iteration 
best.nrounds
# 153


##############################################################################
# 4. 用xgb.train()建立模型
xgb.model = xgb.train(paras = xgb.params, 
                      data = dtrain,
                      nrounds = best.nrounds) 

# 如果要畫出 xgb 內的所有決策樹，可以用以下函式(但因為會很多，這裡就不畫了)
# xgb.plot.tree(model = xgb.model) 

# 預測
xgb_y = predict(xgb.model, dtest)
mean((xgb_y - test$lpsa)^2) # MSE
# 0.7854097


##############################################################################

#  Stacking
# 
# 「在訓練多個模型、得到多個預測值/分類結果後，與其使用投票法(hard voting)或平均法(average)將這些結果整合(ensemble)起來，
#為何不多訓練一個模型來做這樣的整合呢？」
# 
# 舉例：
# 
# 「今天已經訓練好三個機器學習的模型，分別是 linear regression, support vector regression 跟 CART decision tree。
# 當有一筆新資料需要預測時，會各自得到三個預測值(y1, y2, y3)，
# 然後接下來作為最終模型(又稱 meta-model, blender, meta learner)的輸入值，
# 得到最終預測結果(y.final)」
#
#傳統上的做法，我們直覺上會把(y1, y2, y3)的結果直接拿來平均(預測問題)或投票法(分類問題)，得到最後的結果。
# 
# 不過 Stacking 採用另一個模型(blender)來取代這樣的概念。
#換句話說，也就是「把本來的預測結果再進一步做預測」的感覺。
# 
# 因此，Stacking 的演算法可以分成兩個階段，應該不難理解：
    # 
    # Stacking：先訓練多個初始模型，其預測結果叫做 Meta-Data，作為最終模型(Meta-Model; Blender)的的輸入。
    # 
    # Blending：最終模型會取得 Meta-Data ，整合出最後結果(Predicted Results)。

#############################################################################################################################################################################################################################################

#R Code for Stacking Implementing


#第一階段(Stacking)


#一開始，我們把訓練資料 Train 分成三份(3-folds)：
# 3-folds
n = 3
n.folds = rep(1:n, each=nrow(train)/n)

# [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [44] 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
train.folds = split(train, n.folds)

# $`1`
# lcavol  lweight age       lbph svi        lcp gleason pgg45     lpsa
# 9  -0.7765288 3.539509  47 -1.3862944   0 -1.3862944       6     0 1.047319
# 88  1.7316555 3.369018  62 -1.3862944   1  0.3001046       7    30 3.712352
# 74  1.8389611 3.236716  60  0.4382549   1  1.1786550       9    90 3.075005
# 94  3.8210036 3.896909  44 -1.3862944   1  2.1690537       7    40 4.684443
# 44  1.7715568 3.896909  61 -1.3862944   0  0.8109302       7     6 2.374906
# 59  0.5423243 4.178226  70  0.4382549   0 -1.3862944       7    20 2.806386
# 81  1.4678743 3.070376  66  0.5596158   0  0.2231436       7    40 3.516013
# 67  2.0228712 3.878466  68  1.7833912   0  1.3217558       7    70 2.920470
# 48  1.1631508 4.035125  68  1.7137979   0 -0.4307829       7    40 2.568788
# 16  1.5411591 3.061052  66 -1.3862944   0 -1.3862944       6     0 1.446919
# 58  0.4637340 3.764682  49  1.4231083   0 -1.3862944       6     0 2.794228
# 72  1.1600209 3.341093  77  1.7491999   0 -1.3862944       7    25 3.037354
# 62  1.9974177 3.719651  63  1.6193882   1  1.9095425       7    40 2.853593
# 31  0.2851789 4.090169  65  1.9629077   0 -0.7985077       6     0 1.924249
# 65  2.0731719 3.623007  64 -1.3862944   0 -1.3862944       6     0 2.882004
# 49  1.7457155 3.498022  43 -1.3862944   0 -1.3862944       6     0 2.591516
# 21  1.1474025 3.419365  59 -1.3862944   0 -1.3862944       6     0 1.638997
# 68  2.1983351 4.050915  72  2.3075726   0 -0.4307829       7    10 2.962692
# 33  1.2753628 3.037354  71  1.2669476   0 -1.3862944       6     0 2.008214
# 32  0.1823216 6.107580  65  1.7047481   0 -1.3862944       6     0 2.008214
# 56  1.2669476 4.280132  66  2.1222615   0 -1.3862944       7    15 2.718001
# 51  1.0919233 3.993603  68 -1.3862944   0 -1.3862944       7    50 2.656757
# 77  2.0108950 4.433789  72  2.1222615   0  0.5007753       7    60 3.392829
# 38  0.4574248 2.374906  64 -1.3862944   0 -1.3862944       7    15 2.191654
# 97  3.4719665 3.974998  68  0.4382549   1  2.9041651       7    20 5.582932
# 23 -0.5447272 3.375880  59 -0.7985077   0 -1.3862944       6     0 1.695616
# 
# $`2`
# lcavol  lweight age        lbph svi         lcp gleason pgg45       lpsa
# 25  0.385262401 3.667400  69  1.59938758   0 -1.38629436       6     0  1.7316555
# 11  0.254642218 3.604138  65 -1.38629436   0 -1.38629436       6     0  1.2669476
# 84  2.677590994 3.838376  65  1.11514159   0  1.74919985       9    70  3.5709402
# 79  2.648300197 3.582129  69 -1.38629436   1  2.58399755       7    70  3.4578927
# 1  -0.579818495 2.769459  50 -1.38629436   0 -1.38629436       6     0 -0.4307829
# 13  1.613429934 3.022861  63 -1.38629436   0 -0.59783700       7    30  1.2669476
# 95  2.907447359 3.396185  52 -1.38629436   1  2.46385324       7    10  5.1431245
# 63  2.775708850 3.524889  72 -1.38629436   0  1.55814462       9    95  2.8535925
# 34  0.009950331 3.267666  54 -1.38629436   0 -1.38629436       6     0  2.0215476
# 15  1.205970807 3.442019  57 -1.38629436   0 -0.43078292       7     5  1.3987169
# 29  1.040276712 3.128951  67  0.22314355   0  0.04879016       7    80  1.8484548
# 50  1.220829921 3.568123  70  1.37371558   0 -0.79850770       6     0  2.5915164
# 55  3.153590358 3.516013  59 -1.38629436   0 -1.38629436       7     5  2.7047113
# 40  0.797507196 3.013081  56  0.93609336   0 -0.16251893       7     5  2.2772673
# 76  3.141130476 3.263849  68 -0.05129329   1  2.42036813       7    50  3.3375474
# 53  0.512823626 3.633631  64  1.49290410   0  0.04879016       7    70  2.6844403
# 20  0.182321557 3.825375  70  1.65822808   0 -1.38629436       6     0  1.5993876
# 70  1.193922468 4.780383  72  2.32630162   0 -0.79850770       7     5  2.9729753
# 19 -0.562118918 3.267666  41 -1.38629436   0 -1.38629436       6     0  1.5581446
# 42  1.442201993 3.682610  68 -1.38629436   0 -1.38629436       7    10  2.3075726
# 39  2.660958594 4.085136  68  1.37371558   1  1.83258146       7    35  2.2137539
# 54  2.127040520 4.121473  68  1.76644166   0  1.44691898       7    40  2.6912431
# 66  1.458615023 3.836221  61  1.32175584   0 -0.43078292       7    20  2.8875901
# 3  -0.510825624 2.691243  74 -1.38629436   0 -1.38629436       7    20 -0.1625189
# 96  2.882563575 3.773910  68  1.55814462   1  1.55814462       7    80  5.4775090
# 24  1.781709133 3.451574  63  0.43825493   0  1.17865500       7    60  1.7137979
# 
# $`3`
# lcavol  lweight age       lbph svi        lcp gleason pgg45       lpsa
# 73  1.21491274 3.825375  69 -1.3862944   1  0.2231436       7    20  3.0563569
# 30  2.40964417 3.375880  65 -1.3862944   0  1.6193882       6     0  1.8946169
# 43  0.58221562 3.865979  62  1.7137979   0 -0.4307829       6     0  2.3272777
# 35 -0.01005034 3.216874  63 -1.3862944   0 -0.7985077       6     0  2.0476928
# 80  2.77944020 3.823192  63 -1.3862944   0  0.3715636       7    50  3.5130369
# 91  3.24649099 4.101817  68 -1.3862944   0 -1.3862944       6     0  4.0298060
# 12 -1.34707365 3.598681  63  1.2669476   0 -1.3862944       6     0  1.2669476
# 27  0.51282363 3.719651  65 -1.3862944   0 -0.7985077       7    70  1.8000583
# 14  1.47704872 2.998229  67 -1.3862944   0 -1.3862944       7     5  1.3480731
# 71  1.86408013 3.593194  60 -1.3862944   1  1.3217558       7    60  3.0130809
# 82  2.51365606 3.473518  57  0.4382549   0  2.3272777       7    60  3.5307626
# 45  1.48613970 3.409496  66  1.7491999   0 -0.4307829       7    20  2.5217206
# 2  -0.99425227 3.319626  58 -1.3862944   0 -1.3862944       6     0 -0.1625189
# 22  2.05923883 3.501043  60  1.4747630   0  1.3480731       7    20  1.6582281
# 83  2.61300665 3.888754  77 -0.5276327   1  0.5596158       7    30  3.5652984
# 28 -0.40047757 3.865979  67  1.8164521   0 -1.3862944       7    20  1.8164521
# 47  2.72785283 3.995445  79  1.8794650   1  2.6567569       9   100  2.5687881
# 6  -1.04982212 3.228826  50 -1.3862944   0 -1.3862944       6     0  0.7654678
# 64  2.03470565 3.917011  66  2.0082140   1  2.1102132       7    60  2.8820035
# 46  1.66392610 3.392829  61  0.6151856   0 -1.3862944       7    15  2.5533438
# 8   0.69314718 3.539509  58  1.5368672   0 -1.3862944       6     0  0.8544153
# 7   0.73716407 3.473518  64  0.6151856   0 -1.3862944       6     0  0.7654678
# 36  1.30833282 4.119850  64  2.1713368   0 -1.3862944       7     5  2.0856721
# 4  -1.20397280 3.282789  58 -1.3862944   0 -1.3862944       6     0 -0.1625189
# 86  3.30284926 3.518980  64 -1.3862944   1  2.3272777       7    60  3.6309855
# 41  0.62057649 3.141995  60 -1.3862944   0 -1.3862944       9    80  2.2975726






#######################################################################################
#用 linear regression 為例，撰寫一個 stacking 架構，並且儲存meta-x 跟 meta-y的值：

meta.x = vector()
meta.y = list()

# 1st fold for validation
stacking.train = rbind(train.folds[[2]], train.folds[[3]])  #第1,2份為訓練組
stacking.valid = train.folds[[1]]                        #第3份為驗證組
stacking.test = test                                  #Test Data

#第1個模型處理
#第1次交叉驗證
model_1 = lm(lpsa~., stacking.train)
tmp.meta.x = predict(model_1, stacking.valid)
tmp.meta.y = predict(model_1, stacking.test)

meta.x = c(meta.x, tmp.meta.x) #置入最初創建的空向量meta.x
meta.y[[1]] = tmp.meta.y       #置入最初創建的空向量meta.y

# 2nd fold for validation
stacking.train = rbind(train.folds[[1]], train.folds[[3]])
stacking.valid = train.folds[[2]]
stacking.test = test

#第1個模型處理
#第2次交叉驗證
model_1 = lm(lpsa~., stacking.train)
tmp.meta.x = predict(model_1, stacking.valid)
tmp.meta.y = predict(model_1, stacking.test)

meta.x = c(meta.x, tmp.meta.x)
meta.y[[2]] = tmp.meta.y

# 3rd fold for validation
stacking.train = rbind(train.folds[[1]], train.folds[[2]])
stacking.valid = train.folds[[3]]
stacking.test = test

#第1個模型處理
#第3次交叉驗證
model_1 = lm(lpsa~., stacking.train)
tmp.meta.x = predict(model_1, stacking.valid)
tmp.meta.y = predict(model_1, stacking.test)

meta.x = c(meta.x, tmp.meta.x)
meta.y[[3]] = tmp.meta.y

# 所以第一個線性模型跑完三次之後，就會得到一組meta.x跟meta.y，
# 再跟本來的實際值(train$lpsa, test$lpsa)結合，
# 成為第一個模型輸出的 meta.train.1跟 meta.test.1：

# Average Meta.X of Test
mean.meta.y = (meta.y[[1]] + meta.y[[2]] + meta.y[[3]]) / 3

meta.train.1 = data.frame(`meta.x` = meta.x, 
                          y=train$lpsa)

meta.test.1 = data.frame(`mete.y` = mean.meta.y, 
                         y = test$lpsa)

#######################################################################################
#接下來，第二模型用support vector regression，也是一樣的流程：

require(e1071)
meta.x = vector()
meta.y = list()

# 1st fold for validation
stacking.train = rbind(train.folds[[2]], train.folds[[3]])
stacking.valid = train.folds[[1]]
stacking.test = test

model_2 = svm(lpsa~., stacking.train)

tmp.meta.x = predict(model_2, stacking.valid)
tmp.meta.y = predict(model_2, stacking.test)

meta.x = c(meta.x, tmp.meta.x)
meta.y[[1]] = tmp.meta.y

# 2nd fold for validation
stacking.train = rbind(train.folds[[1]], train.folds[[3]])
stacking.valid = train.folds[[2]]
stacking.test = test

model_2 = svm(lpsa~., stacking.train)

tmp.meta.x = predict(model_2, stacking.valid)
tmp.meta.y = predict(model_2, stacking.test)

meta.x = c(meta.x, tmp.meta.x)
meta.y[[2]] = tmp.meta.y

# 3rd fold for validation
stacking.train = rbind(train.folds[[1]], train.folds[[2]])
stacking.valid = train.folds[[3]]
stacking.test = test

model_2 = svm(lpsa~., stacking.train)

tmp.meta.x = predict(model_2, stacking.valid)
tmp.meta.y = predict(model_2, stacking.test)

meta.x = c(meta.x, tmp.meta.x)
meta.y[[3]] = tmp.meta.y

# Average Meta.X of Test
mean.meta.y = (meta.y[[1]] + meta.y[[2]] + meta.y[[3]]) / 3

meta.train.2 = data.frame(`meta.x` = meta.x, 
                          y=train$lpsa)

meta.test.2 = data.frame(`mete.y` = mean.meta.y, 
                         y = test$lpsa)


#######################################################################################
#仿照上面的流程，第三個模型則用 CART 決策樹：

require(rpart)
meta.x = vector()
meta.y = list()

# 1st fold for validation
stacking.train = rbind(train.folds[[2]], train.folds[[3]])
stacking.valid = train.folds[[1]]
stacking.test = test

model_3 = rpart(lpsa~., stacking.train)

tmp.meta.x = predict(model_3, stacking.valid)
tmp.meta.y = predict(model_3, stacking.test)

meta.x = c(meta.x, tmp.meta.x)
meta.y[[1]] = tmp.meta.y

# 2nd fold for validation
stacking.train = rbind(train.folds[[1]], train.folds[[3]])
stacking.valid = train.folds[[2]]
stacking.test = test

model_3 = rpart(lpsa~., stacking.train)

tmp.meta.x = predict(model_3, stacking.valid)
tmp.meta.y = predict(model_3, stacking.test)

meta.x = c(meta.x, tmp.meta.x)
meta.y[[2]] = tmp.meta.y

# 3rd fold for validation
stacking.train = rbind(train.folds[[1]], train.folds[[2]])
stacking.valid = train.folds[[3]]
stacking.test = test

model_3 = rpart(lpsa~., stacking.train)

tmp.meta.x = predict(model_3, stacking.valid)
tmp.meta.y = predict(model_3, stacking.test)

meta.x = c(meta.x, tmp.meta.x)
meta.y[[3]] = tmp.meta.y

# Average Meta.X of Test
mean.meta.y = (meta.y[[1]] + meta.y[[2]] + meta.y[[3]]) / 3

meta.train.3 = data.frame(`meta.x` = meta.x, 
                          y=train$lpsa)

meta.test.3 = data.frame(`mete.y` = mean.meta.y, 
                         y = test$lpsa)

########################################################################################
#上述流程概述:

# 1.先分出Training Data, Test Data

# 2.Training Data分別置入3個模型>> lm(), SVR(), CART() 

#For lm():

    #將Training Data分成三個subset(3-folds) 
    # 一部分用於訓練一部份用於預測
    # 產生3個預測結果 >> 結合轉換為Predict_X，也就是Meta-x  
    
    # Test Data丟入模型後也可以得到Predict(共3個)
    # 直接用平均法(連續問題)/投票法(分類問題)整合為Predict_Y，也就是Meta-y

    # 第1個線性模型跑完三次之後，就會得到一組meta.x跟meta.y，
    # 再跟本來的實際值(train$lpsa, test$lpsa)結合，
    # 成為第1個模型輸出的 meta.train.1跟 meta.test.1

#For SVR():

    #將Training Data分成三個subset(3-folds) 
    # 一部分用於訓練一部份用於預測
    # 產生3個預測結果 >> 結合轉換為Predict_X，也就是Meta-x  
    
    # Test Data丟入模型後也可以得到Predict(共3個)
    # 直接用平均法(連續問題)/投票法(分類問題)整合為Predict_Y，也就是Meta-y
    
    # 第2個SVR模型跑完三次之後，就會得到一組meta.x跟meta.y，
    # 再跟本來的實際值(train$lpsa, test$lpsa)結合，
    # 成為第2個模型輸出的 meta.train.2跟 meta.test.2

#For CART():

    #將Training Data分成三個subset(3-folds) 
    # 一部分用於訓練一部份用於預測
    # 產生3個預測結果 >> 結合轉換為Predict_X，也就是Meta-x  
    
    # Test Data丟入模型後也可以得到Predict(共3個)
    # 直接用平均法(連續問題)/投票法(分類問題)整合為Predict_Y，也就是Meta-y
    
    # 第3個CART模型跑完三次之後，就會得到一組meta.x跟meta.y，
    # 再跟本來的實際值(train$lpsa, test$lpsa)結合，
    # 成為第3個模型輸出的 meta.train.3跟 meta.test.3



#上面示範兩階段的狀況而已。
#實際上在 Kaggle 比賽中，很常看見發展成三、四階段的 Stacking 模型。

#######################################################################################

#第二階段(Blending)



#現在我們手中有三組 Meta-Train 跟 三組 Meta-Test：
c(dim(meta.train.1), dim(meta.test.1))


#建構第二階段的 Meta-Model
#
### Meta- Model Construction 

# 先把三個 Meta-Train合併一起：
big.meta.train = rbind(meta.train.1, meta.train.2, meta.train.3)

# 轉換成 xgboost 的格式
dtrain = xgb.DMatrix(data = as.matrix(big.meta.train[,1]), label = big.meta.train[, 2])
#xgb.DMatrix  dim: 234 x 1  info: label  colnames: no





# 訓練 XGboost 模型

# 其中xgb.params 直接拿第二節(Boosting)的設定
xgb.params = list(
    #     #col的抽樣比例，越高表示每棵樹使用的col越多，會增加每棵小樹的複雜度
    colsample_bytree = 0.5,
    
    #     # row的抽樣比例，越高表示每棵樹使用的col越多，會增加每棵小樹的複雜度
    subsample = 0.5, 
    booster = "gbtree",
    
    #     # 樹的最大深度，越高表示模型可以長得越深，模型複雜度越高
    max_depth = 2,    
    
    #     # boosting會增加被分錯的資料權重，而此參數是讓權重不會增加的那麼快，因此越大會讓模型愈保守
    eta = 0.03,
    #     # 或用'mae'也可以
    
    eval_metric = "rmse",                      
    objective = "reg:linear",
    
    #     # 越大，模型會越保守，相對的模型複雜度比較低
    gamma = 0)   

# 簡單用 nrounds = 100
xgb.model = xgb.train(paras = xgb.params, data = dtrain, nrounds = 100) 





# 對三組 Meta-Test進行預測：
dtest.1 = xgb.DMatrix(data = as.matrix(meta.test.1[,1]), label = meta.test.1[, 2])
# 轉換成 xgboost 的格式
final_1 = predict(xgb.model, dtest.1)


dtest.2 = xgb.DMatrix(data = as.matrix(meta.test.2[,1]), label = meta.test.2[, 2])
# 轉換成 xgboost 的格式
final_2 = predict(xgb.model, dtest.2)


dtest.3 = xgb.DMatrix(data = as.matrix(meta.test.3[,1]), label = meta.test.3[, 2])
# 轉換成 xgboost 的格式
final_3 = predict(xgb.model, dtest.3)


# 把三組結果平均起來，然後算 MSE
final_y = (final_1 + final_2 + final_3)/3
mean((final_y - test$lpsa)^2) # MSE
# 0.6444626







