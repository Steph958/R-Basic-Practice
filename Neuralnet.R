

#類神經網路(neuralnet)

#(倒傳遞)類神經網路

#tune parameters:
#針對層數和節點數進行調整，看怎麼樣的組合會有最小的MSE(RMSE)

####################################################################################################################################################
install.packages("neuralnet")
install.packages("nnet")
install.packages("caret")

library("caret")
library("nnet")
library("neuralnet")


####################################################################################################################################################
#載入資料
data <- iris
head(data)
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
# 3          4.7         3.2          1.3         0.2  setosa
# 4          4.6         3.1          1.5         0.2  setosa
# 5          5.0         3.6          1.4         0.2  setosa
# 6          5.4         3.9          1.7         0.4  setosa

str(data)
# 'data.frame':	150 obs. of  5 variables:
#     $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
# $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
# $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
# $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
# $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...


# 因為Species是類別型態，這邊轉換成三個output nodes
#使用class.ind函式()
#class.ind(data$Species)
head(class.ind(data$Species))
##      setosa versicolor virginica
## [1,]      1          0         0
## [2,]      1          0         0
## [3,]      1          0         0
## [4,]      1          0         0
## [5,]      1          0         0
## [6,]      1          0         0

# 並和原始的資料合併在一起，cbind意即column-bind
data <- cbind(data, class.ind(data$Species))

# 原始資料就會變成像這樣
head(data) 
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species setosa versicolor
# 1          5.1         3.5          1.4         0.2  setosa      1          0
# 2          4.9         3.0          1.4         0.2  setosa      1          0
# 3          4.7         3.2          1.3         0.2  setosa      1          0
# 4          4.6         3.1          1.5         0.2  setosa      1          0
# 5          5.0         3.6          1.4         0.2  setosa      1          0
# 6          5.4         3.9          1.7         0.4  setosa      1          0
# virginica
# 1         0
# 2         0
# 3         0
# 4         0
# 5         0
# 6         0

#多了後方的三個虛擬變數


####################################################################################################################################################
#建立模型:
formula.bpn <- setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
formula.bpn
#訓練bpn模型:
bpn <- neuralnet(formula = formula.bpn, 
                 data = data,
                 hidden = c(2),       # 一個隱藏層：2個node
                 learningrate = 0.01, # learning rate
                 threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
                 stepmax = 5e5        # 最大的ieration數 = 500000(5*10^5)
                 
)

plot(bpn)


####################################################################################################################################################
#Tuning Parameters
#
#當使用不同的隱藏層數和節點數，類神經網路的模型表現與可靠度就會改變。
# 
# tuning parameters時，需要觀察不同參數組合的MSE(RMSE)；
# 當MSE最小的情況發生時，我們就可以視為是最佳的參數組合(optimal parameters)。



#先把原始的資料集，分成80%的train set和20%的test set

# nrow()是用來擷取資料筆數，乘上0.8後，表示我們的train set裡面要有多少筆資料(data size)
smp.size <- floor(0.8*nrow(data)) 

# 因為是抽樣，有可能每次抽樣結果都不一樣，因此這裡規定好亂數表，讓每次抽樣的結果一樣
set.seed(131)                     

# 從原始資料裡面，抽出train set所需要的資料筆數(data size)

str(data)
# 'data.frame':	150 obs. of  8 variables:
seq_len(nrow(data))
# [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19
# [20]  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38
# [39]  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57
# [58]  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76
# [77]  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
# [96]  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114
# [115] 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133
# [134] 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150

train.ind <- sample(seq_len(nrow(data)), smp.size)
#train.ind

# 分成train/test
train <- data[train.ind, ]
test <- data[-train.ind, ]

str(train)
# 'data.frame':	120 obs. of  8 variables:
#     $ Sepal.Length: num  6.3 5.4 7.4 5.8 4.7 7.6 5.4 6.7 5.1 5 ...
# $ Sepal.Width : num  3.4 3.7 2.8 4 3.2 3 3.4 2.5 3.7 3.2 ...
# $ Petal.Length: num  5.6 1.5 6.1 1.2 1.6 6.6 1.5 5.8 1.5 1.2 ...
# $ Petal.Width : num  2.4 0.2 1.9 0.2 0.2 2.1 0.4 1.8 0.4 0.2 ...
# $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 3 1 3 1 1 3 1 3 1 1 ...
# $ setosa      : num  0 1 0 1 1 0 1 0 1 1 ...
# $ versicolor  : num  0 0 0 0 0 0 0 0 0 0 ...
# $ virginica   : num  1 0 1 0 0 1 0 1 0 0 ...
str(test)
# 'data.frame':	30 obs. of  8 variables:
#     $ Sepal.Length: num  4.6 4.9 4.8 4.8 5.1 5.1 5.1 5.5 4.9 4.4 ...
# $ Sepal.Width : num  3.4 3.1 3.4 3 3.5 3.8 3.3 3.5 3.6 3 ...
# $ Petal.Length: num  1.4 1.5 1.6 1.4 1.4 1.5 1.7 1.3 1.4 1.3 ...
# $ Petal.Width : num  0.3 0.1 0.2 0.1 0.3 0.3 0.5 0.2 0.1 0.2 ...
# $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ setosa      : num  1 1 1 1 1 1 1 1 1 1 ...
# $ versicolor  : num  0 0 0 0 0 0 0 0 0 0 ...
# $ virginica   : num  0 0 0 0 0 0 0 0 0 0 ...




#註：下面的code實際上會運行比較長的時間
# tune parameters
model <- train(form=formula.bpn,     # formula
               data=train,           # 資料
               method="neuralnet",   # 類神經網路(bpn)
               tuneGrid = expand.grid(.layer1=c(1:4), .layer2=c(0:4), .layer3=c(0)), 
   # 最重要的步驟：觀察不同排列組合(第一層1~4個nodes ; 第二層0~4個nodes)
   # 看何種排列組合(多少隱藏層、每層多少個node)，會有最小的RMSE
                 
               # 以下的參數設定，和上面的neuralnet內一樣
               learningrate = 0.01,  # learning rate
               threshold = 0.01,     # partial derivatives of the error function, a stopping criteria
               stepmax = 5e5)        # 最大的ieration數 = 500000(5*10^5)


model  # 會告訴你最佳的參數組合是什麼：第一隱藏層1個node，第二隱藏層2個node
# Neural Network 
# 
# 120 samples
# 6 predictor
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 120, 120, 120, 120, 120, 120, ... 
# Resampling results across tuning parameters:
#     
#     layer1  layer2  RMSE          Rsquared  MAE         
# 1       0       0.0013208000  NaN       0.0009876937
# 1       1       0.0010166729  NaN       0.0008354612
# 1       2       0.0010057010  NaN       0.0007813751
# 1       3       0.0009925572  NaN       0.0007824129
# 1       4       0.0008576919  NaN       0.0006786483
# 2       0       0.0036054729  NaN       0.0025334549
# 2       1       0.0010373944  NaN       0.0008176877
# 2       2       0.0022374755  NaN       0.0017016574
# 2       3       0.0015217974  NaN       0.0011749680
# 2       4       0.0018645444  NaN       0.0014878869
# 3       0       0.0029499425  NaN       0.0019499897
# 3       1       0.0018257584  NaN       0.0014639236
# 3       2       0.0017514974  NaN       0.0013480232
# 3       3       0.0031176666  NaN       0.0021670284
# 3       4       0.0023180993  NaN       0.0015811333
# 4       0       0.0050154881  NaN       0.0035079685
# 4       1       0.0017474107  NaN       0.0013711849
# 4       2       0.0024613837  NaN       0.0019705235
# 4       3       0.0030448406  NaN       0.0022817609
# 4       4       0.0035558763  NaN       0.0024758154
# 
# Tuning parameter 'layer3' was held constant at a value of 0
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were layer1 = 1, layer2 = 4 and layer3 = 0.
plot(model)

#所以我們就以兩層隱藏層(1,1)，重新訓練類神經網路模型：

bpn <- neuralnet(formula = formula.bpn, 
                 data = train,
                 hidden = c(1,4),     # 第一隱藏層1個node，第二隱藏層2個nodes
                 learningrate = 0.01, # learning rate
                 threshold = 0.01,    # partial derivatives of the error function, a stopping criteria
                 stepmax = 5e5        # 最大的ieration數 = 500000(5*10^5)
                 
)

# 新的bpn模型會長得像這樣
plot(bpn)


####################################################################################################################################################
#預測:
#
#用訓練好的模型(bpn)預測test set：
#
# 使用bpn模型，輸入test set後進行預測
# 需要注意的是，輸入的test資料只能包含input node的值，新增的虛擬變數不能放
# 所以取前四個欄位，丟入模型進行預測
pred <- compute(bpn, test[, 1:4])  

# 預測結果
pred$net.result
# [,1]         [,2]          [,3]
# 7    0.9948961312  0.005054252  1.358899e-05
# 10   0.9987729238  0.001482251 -7.417371e-05
# 12   0.9945597156  0.005364207  2.120793e-05
# 13   0.9986209315  0.001622298 -7.073420e-05
# 18   1.0067056873 -0.005827609 -2.535380e-04
# 20   1.0079004627 -0.006928660 -2.805263e-04
# 24   0.9789911787  0.019706394  3.743170e-04
# 37   1.0139322583 -0.012487675 -4.166666e-04
# 38   1.0092906030 -0.008209782 -3.119187e-04
# 39   0.9880655277  0.011347270  1.683806e-04
# 43   0.9937457029  0.006114188  3.964527e-05
# 47   1.0075289284 -0.006586268 -2.721346e-04
# 55   0.0036878291  1.008058977 -3.022223e-03
# 60   0.0030079117  1.007830412 -2.746100e-03
# 71  -0.0108559598  0.056598567  9.546370e-01
# 73  -0.0101450658  0.111593488  8.997573e-01
# 74   0.0051238036  1.008631660 -3.753218e-03
# 77   0.0025657477  1.007679958 -2.572353e-03
# 81  -0.0062537124  1.001917904  1.572307e-03
# 84  -0.0017963897 -0.006447847  1.007374e+00
# 85   0.0175919290  0.888348114  1.045475e-01
# 89  -0.0033358483  1.004745421 -5.798478e-05
# 101  0.0024148935  0.002423426  9.959333e-01
# 110  0.0018372575  0.001810103  9.967794e-01
# 111 -0.0061607758 -0.013690599  1.018723e+00
# 117 -0.0001697105 -0.002101635  1.001802e+00
# 139 -0.0100678157  0.008792786  1.000784e+00
# 140 -0.0012876907 -0.005065799  1.005584e+00
# 144  0.0019158445  0.001906968  9.966489e-01
# 148 -0.0015236747 -0.005708610  1.006413e+00

# 四捨五入後，變成0/1的狀態
pred.result <- round(pred$net.result)
pred.result
# [,1] [,2] [,3]
# 7      1    0    0
# 10     1    0    0
# 12     1    0    0
# 13     1    0    0
# 18     1    0    0
# 20     1    0    0
# 24     1    0    0
# 37     1    0    0
# 38     1    0    0
# 39     1    0    0
# 43     1    0    0
# 47     1    0    0
# 55     0    1    0
# 60     0    1    0
# 71     0    0    1
# 73     0    0    1
# 74     0    1    0
# 77     0    1    0
# 81     0    1    0
# 84     0    0    1
# 85     0    1    0
# 89     0    1    0
# 101    0    0    1
# 110    0    0    1
# 111    0    0    1
# 117    0    0    1
# 139    0    0    1
# 140    0    0    1
# 144    0    0    1
# 148    0    0    1

# 把結果轉成data frame的型態
pred.result <- as.data.frame(pred.result)

# 建立一個新欄位，叫做Species
pred.result$Species <- ""

# 把預測結果轉回Species的型態
for(i in 1:nrow(pred.result)){
    if(pred.result[i, 1]==1){ pred.result[i, "Species"] <- "setosa"}
    if(pred.result[i, 2]==1){ pred.result[i, "Species"] <- "versicolor"}
    if(pred.result[i, 3]==1){ pred.result[i, "Species"] <- "virginica"}
}

pred.result
# V1 V2 V3    Species
# 7    1  0  0     setosa
# 10   1  0  0     setosa
# 12   1  0  0     setosa
# 13   1  0  0     setosa
# 18   1  0  0     setosa
# 20   1  0  0     setosa
# 24   1  0  0     setosa
# 37   1  0  0     setosa
# 38   1  0  0     setosa
# 39   1  0  0     setosa
# 43   1  0  0     setosa
# 47   1  0  0     setosa
# 55   0  1  0 versicolor
# 60   0  1  0 versicolor
# 71   0  0  1  virginica
# 73   0  0  1  virginica
# 74   0  1  0 versicolor
# 77   0  1  0 versicolor
# 81   0  1  0 versicolor
# 84   0  0  1  virginica
# 85   0  1  0 versicolor
# 89   0  1  0 versicolor
# 101  0  0  1  virginica
# 110  0  0  1  virginica
# 111  0  0  1  virginica
# 117  0  0  1  virginica
# 139  0  0  1  virginica
# 140  0  0  1  virginica
# 144  0  0  1  virginica
# 148  0  0  1  virginica


####################################################################################################################################################
#評估:

# 混淆矩陣 
table(real    = test$Species, 
      predict = pred.result$Species)
#               predict
# real         setosa versicolor virginica
# setosa         12          0         0
# versicolor      0          7         3
# virginica       0          0         8

#預測正確率
27/30



# 
# 總結:
# 類神經網路是一個很強大的方法，屬於機器學習的範疇，
# 因此在預測上有很好的效果，可是最大的問題則是難以解釋。
# 在資工的領域中，人工智慧就是類神經網路的一個分支，
# 屬於深度學習(deep learning)的範疇。















