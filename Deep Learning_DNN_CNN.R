
# R有很多可以實現深度學習/類神經網路的套件(neuralnet、nnet、h2o、mxnet)
# 
# 使用mxnet套件建構以下模型：
    # 深度神經網路(DNN: Deep Neuron Networks)
    # 卷積類神經網路(CNN: Convolutional Neural Networks)

# 相較於其他套件，我看中mxnet的優點有四個：
    # 具有強大的彈性，允許自行設計符合需求的類神經網路模型(下面會有更詳細介紹)。
    # 支援GPU的運算。
    # 能實現DNN, CNN, RNN-LSTM…等深度學習演算法。
    # 同時具有python的版本，寫法幾乎一模一樣，而且和python另一個深度學習的套件keras寫法也相近；若未來有機會以python實作深度學習，能很快就上手。

########################################################################################################################################################
#資料預處理:


# 42000個觀測值，每一筆代表一張手寫數字的圖片
# 784個自變數： 28 x 28 pixels，以灰階值0~255表示。
# 應變數label，代表這張圖片象徵的數字，
# 也是在測試資料(test.csv)中要預測的值。
train <- read.csv("C:/Users/USER/Desktop/R resourse/R Training/train.csv")
dim(train)
# [1] 42000   785


# 28000個觀測值，每一筆代表一張手寫數字的圖片
# 784個自變數： 28 x 28 pixels，以灰階值0~255表示。
# 無label，要預測的
test <- read.csv('C:/Users/USER/Desktop/R resourse/R Training/test.csv')
dim(test)
# [1] 28000   784







# 資料轉換成 28x28 的矩陣
obs.matrix <- matrix(unlist(train[1, -1]), # ignore 'label'
                     nrow = 28,            
                     byrow=T)
str(obs.matrix)
# int [1:28, 1:28] 0 0 0 0 0 0 0 0 0 0 ...


# 用 image 畫圖，顏色指定為灰階值 0~255
image(obs.matrix, 
      col=grey.colors(255))


# 由於原本的圖是倒的，因此寫一個翻轉的函式：
# rotates the matrix
rotate <- function(matrix){
    t(apply(matrix, 2, rev)) 
} 

# 畫出第1筆~第8筆資料的圖
par(mfrow=c(2,4))
for(x in 1:8){
    obs.matrix <- matrix(unlist(train[x, -1]), # ignore 'label'
                         nrow = 28,            
                         byrow=T)
    
    image(rotate(obs.matrix),
          col=grey.colors(255),
          xlab=paste("Label", train[x, 1], sep=": "),
          cex.lab = 1.5
    )
}




#在建模之前，因為資料裡面的pixels欄位值為0 ~ 255，
#因此先簡單做個轉換，讓pixels在0~1之間：
#這步驟很關鍵!!
#
# data preparation
train.x <- t(train[, -1]/255) # train: 28 x 28 pixels
train.y <- train[, 1]         # train: label
test.x <- t(test/255)         # test: 28 x 28 pixels

########################################################################################################################################################

#模型建構 (MXnet)

install.packages("Mxnet")


########################################################################################################################################################
# 
# DNN
#
# 所謂的DNN，就是「多層隱藏層」的類神經網路。
# 一般來說，類神經網路的基本型為筆記(8)介紹的「倒傳遞類神經網路(BPN)」。
# 
# 然而，單層或雙層的BPN，在面對大數據(Big Data)的議題上，其預測效果和處理效率沒有想像中的好(尤其是圖片處理、影音處理、語音處理…)，
# 
# 因此有人提倡引入多層隱藏層，擴展神經網路的「深度」：
# 現在的隱藏層萃取出「input的一些特徵」後，當作下一隱藏層的input。
# 隨著層數越多，能夠辨識的「特徵」也會越明確。以此概念來增加最後的預測效果。
# 
# 不過隨著隱藏層增加，許多問題也會接踵而來：「梯度殘差消失」、「權重數量遞增」…
# 因此又有人接著提出「激發函數改為relu」、「Dropout概念」、「min batch」…等想法來解決這些問題。

#關於DNN的詳情，這裡不多加贅述，可以參照台大李宏毅老師的講義： 一天搞懂深度學習。

########################################################################################################################################################

# Build Model


# 輸入層
data <- mx.symbol.Variable("data")

# 第一隱藏層: 500節點，狀態是Full-Connected
fc1 <- mx.symbol.FullyConnected(data, name="1-fc", num_hidden=500)
# 第一隱藏層的激發函數: Relu
act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")
# 這裡引入dropout的概念
drop1 <- mx.symbol.Dropout(data=act1, p=0.5)

# 第二隱藏層: 400節點，狀態是Full-Connected
fc2 <- mx.symbol.FullyConnected(drop1, name="2-fc", num_hidden=400)
# 第二隱藏層的激發函數: Relu
act2 <- mx.symbol.Activation(fc2, name="relu2", act_type="relu")
# 這裡引入dropout的概念
drop2 <- mx.symbol.Dropout(data=act2, p=0.5)

# 輸出層：因為預測數字為0~9共十個，節點為10
output <- mx.symbol.FullyConnected(drop2, name="output", num_hidden=10)
# Transfer the Evidence to Probability by Softmax-function
dnn <- mx.symbol.SoftmaxOutput(output, name="dnn")
# 
# 因為輸出層的結果是類別型態(0~9的數字)，因此最後一行使用的是softmax。
# 若現在要預測的是連續型態，就改為mx.symbol.LinearRegressionOutput())

#最後，創造了一個具有兩層隱藏層(500, 400)，搭配激發函數Relu，以及引入Dropout概念的類神經網路。





#視覺化其結構：

# 神經網路中的各個參數的資訊
arguments(dnn)

# 視覺化DNN結構
graph.viz(dnn$as.json(),
          graph.title= "DNN for Kaggle－Digit Recognizer")




########################################################################################################################################################

#Train Model

#接下來，就拿train.x來訓練剛剛創造的神經網路，內部參數的設定十分重要：

mx.set.seed(0) 

# 訓練剛剛創造/設計的模型
dnn.model <- mx.model.FeedForward.create(
    dnn,       # 剛剛設計的DNN模型
    X=train.x,  # train.x
    y=train.y,  #  train.y
    ctx=mx.cpu(),  # 可以決定使用cpu或gpu
    num.round=10,  # iteration round
    array.batch.size=100, # batch size
    learning.rate=0.07,   # learn rate
    momentum=0.9,         # momentum  
    eval.metric=mx.metric.accuracy, # 評估預測結果的基準函式*
    initializer=mx.init.uniform(0.07), # 初始化參數
    epoch.end.callback=mx.callback.log.train.metric(100)

#
## Start training with 1 devices
## [1] Train-accuracy=0.859379474940333
## [2] Train-accuracy=0.932333333333333
## [3] Train-accuracy=0.944928571428571
## [4] Train-accuracy=0.9515
## [5] Train-accuracy=0.957095238095239
## [6] Train-accuracy=0.959952380952382
## [7] Train-accuracy=0.963761904761905
## [8] Train-accuracy=0.966047619047621
## [9] Train-accuracy=0.965904761904764
## [10] Train-accuracy=0.969190476190479

# 
# 倒數第三個的參數：eval.metric。
# 
# 因為使用的是mx.metric.accuracy，用來評估「預測的準確率」，所以在訓練模型時的輸出才會是Train-accuracy。
# 
#如果現在要評估「連續數值」的預測效果，可以改使用mx.metric.mae、mx.metric.rmse，這樣就會用MSE愈小愈好的方式來訓練模型。
# 
# 最棒的是，如果自己有特殊需求(例如，我想看的是R-Squared)，
# mxnet允許我們可以自行設計屬於自己的評估函式，再將參數設定成： eval.metric = my.eval.metric就好：

# define my own evaluate function (R-squared)
my.eval.metric <- mx.metric.custom(
    name = "R-squared", 
    function(real, pred) {
        mean_of_obs <- mean(real)
        
        SS_tot <- sum((real - mean_of_obs)^2)
        SS_reg <- sum((predict - mean_of_obs)^2)
        SS_res <- sum((real - predict)^2)
        
        R_squared <- 1 - (SS_res/SS_tot)
        R_squared
    }
)




########################################################################################################################################################

#預測:

# test prediction 
test.y <- predict(dnn.model, test.x)
test.y <- t(test.y)
test.y.label <- max.col(test.y) - 1

# Submission format for Kaggle
result <- data.frame(ImageId = 1:length(test.y.label),
                     label = test.y.label)








########################################################################################################################################################
# 
# CNN


#建立模型(以LeNet為例):


# 輸入層
data <- mx.symbol.Variable('data')


# 第一卷積層，windows的視窗大小是 5x5, filter=20
conv1 <- mx.symbol.Convolution(data=data, kernel=c(5,5), num_filter=20, name="1-conv")
# 第一卷積層的激發函數：tanh
conv.act1 <- mx.symbol.Activation(data=conv1, act_type="tanh", name="1-conv.act")
# 第一卷積層後的池化層，max，大小縮為 2x2
pool1 <- mx.symbol.Pooling(data=conv.act1, pool_type="max", name="1-conv.pool",
                           kernel=c(2,2), stride=c(2,2))

# 第二卷積層，windows的視窗大小是 5x5, filter=50
conv2 <- mx.symbol.Convolution(data=pool1, kernel=c(5,5), num_filter=50, name="2-conv")
# 第二卷積層的激發函數：tanh
conv.act2 <- mx.symbol.Activation(data=conv2, act_type="tanh", name="2-conv.act")
# 第二卷積層後的池化層，max，大小縮為 2x2
pool2 <- mx.symbol.Pooling(data=conv.act2, pool_type="max", name="2-conv.pool",
                           kernel=c(2,2), stride=c(2,2))


# Flatten
flatten <- mx.symbol.Flatten(data=pool2)


#後面的部分就和DNN是一樣的


# 建立一個Full-Connected的隱藏層，500節點
fc1 <- mx.symbol.FullyConnected(data=flatten, num_hidden=500, name="1-fc")
# 隱藏層的激發函式：tanh
fc.act1 <- mx.symbol.Activation(data=fc1, act_type="tanh", name="1-fc.act")

# 輸出層：因為預測數字為0~9共十個，節點為10
output <- mx.symbol.FullyConnected(data=fc.act1, num_hidden=10, name="output")
# Transfer the Evidence to Probability by Softmax-function
cnn <- mx.symbol.SoftmaxOutput(data=output, name="cnn")


########################################################################################################################################################

#視覺化其結構：

# 神經網路中的各個參數的資訊
arguments(cnn)

# 視覺化CNN結構
graph.viz(cnn$as.json(),
          graph.title= "CNN for Kaggle－Digit Recognizer"
)


########################################################################################################################################################

#Train Model

#在訓練之前有一件事情要注意，因為LeNet有要求既定的input格式()，
#和DNN不一樣，我們不能直接拿train.x來訓練，
#得先將train.x和test.x轉換成一個四維矩陣，才能進行訓練：


# Transform matrix format for LeNet 
train.array <- train.x
dim(train.array) <- c(28, 28, 1, ncol(train.x))
test.array <- test.x
dim(test.array) <- c(28, 28, 1, ncol(test.x))
mx.set.seed(0)


# 訓練剛剛設計的CNN模型
cnn.model <- mx.model.FeedForward.create(
    cnn,       # 剛剛設計的CNN模型
    X=train.array,  # train.x
    y=train.y,  #  train.y
    ctx=mx.cpu(),  # 可以決定使用cpu或gpu
    num.round=10,  # iteration round
    array.batch.size=100, # batch size
    learning.rate=0.07,   # learn rate
    momentum=0.7,         # momentum  
    eval.metric=mx.metric.accuracy, # 評估預測結果的基準函式*
    initializer=mx.init.uniform(0.05), # 初始化參數
    epoch.end.callback=mx.callback.log.train.metric(100)
)

#
## Start training with 1 devices
## [1] Train-accuracy=0.913842482100241
## [2] Train-accuracy=0.977904761904765
## [3] Train-accuracy=0.98514285714286
## [4] Train-accuracy=0.989285714285717
## [5] Train-accuracy=0.992285714285717
## [6] Train-accuracy=0.994214285714287
## [7] Train-accuracy=0.996142857142858
## [8] Train-accuracy=0.997333333333334
## [9] Train-accuracy=0.99802380952381
## [10] Train-accuracy=0.99854761904762




########################################################################################################################################################


#Prediction


# test prediction 
test.y <- predict(cnn.model, test.array)
test.y <- t(test.y)
test.y.label <- max.col(test.y) - 1

# Submission format for Kaggle
result <- data.frame(ImageId = 1:length(test.y.label),
                     label = test.y.label)


# Kaggle Score = 0.98871













