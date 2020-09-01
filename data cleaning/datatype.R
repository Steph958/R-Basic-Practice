

#基本資料型態

###################################################################
#integer, number, logic, character,date
dateBook<-Sys.Date()
dateBook

library(lubridate)
ymd('2012/3/3')
## [1] "2012-03-03"
mdy('3/3/2012')
## [1] "2012-03-03"

?ymd   #HELP!!

a <- 3     
b <- 1.6 
str(a)   #R預設數字型態為number

num1<-1.568
num2<-2.121
round(num2,digits = 1) #2.121四捨五入至小數點第一位
## [1] 2.1
floor(num1) ##1.568
## [1] 1
ceiling(num2) ##2.121
## [1] 3

a <- as.integer(3)
str(a)

# 資料型態	轉換函式
# integer	as.integer()
# number	as.numeric()
# character	as.character()
# factor	as.factor()
# matrix	as.matrix()
# vector	as.vector()
# list	as.list()
# data frame	as.data.frame()

is.integer(a)
is.integer(b)

# 資料型態	判斷函式
# integer	is.integer()
# number	is.numeric()
# character	is.character()
# factor	is.factor()
# matrix	is.matrix()
# vector	is.vector()
# list	is.list()
# data frame	is.data.frame()


a <- TRUE
b <- FALSE
str(a)        
is.integer(b)

TRUE & TRUE
## [1] TRUE
TRUE & FALSE
## [1] FALSE
TRUE | TRUE
## [1] TRUE
TRUE | FALSE
## [1] TRUE

!TRUE
## [1] FALSE
!FALSE
## [1] TRUE

a <- "Dr.Lee"    
professor <- a   
str(professor)   

##########################################################3333333#####
#vector
#在vector裡有一個需要注意的規則
#每一個element都會是相同的型態

a <- c(5,10,15,20,25)           # 建立一個number vector
b <- c("Tom", "Henry", "John")  # 建立一個character vector
a
b

1:20 ## c(1,2,...,19,20)
seq(from=1,to=20,by=1) ##1~20，中間相隔1

a[3]      # Ans: 15           (取第3個element)
a[1:3]    # Ans: 5 10 15      (取第1~第3個element)
a[c(2,4)] # Ans: 10 20        (取第2和第4個element)

a <- c(1, "john", 3) # 若是把number和character同時放入vector裡，
a                    # R會自動將所有element的型態，轉變成character 

b <- c(T, 3, F)      # logic和number在vector裡的話
b                    # T和F會被自動轉換成1和0，變成數字的vector


#vector數學運算:

a <- c(7,8,6,9,5) # 建立一個number vector
b <- c(2,4,6,0,1) # 建立一個number vector

a * b             # a和b的第一個element相乘，第二個element相乘......
b^3               # 對b之中的每一個element三次方
b > 3             # 判斷b之中的哪些值大於 3 ，然後回傳 TRUE
a%%b              #計算餘數



###################################################################
#factor
#
# factor的型態，主要用來表示「類別變數」(category variable)。
# (例如：性別(男、女)，年級(小一、小二….碩一、碩二)，地區(北、中、南、東)…等等。)
# 在用R進行資料分析時，當遇到這類「類別變數」時，要轉換成factor的資料型態，再丟入模型(model)進行分析。
# factor的資料型態和vector很相似，差別在於factor具有額外的類別屬性(Levels)。
# 要建立factor的變數，可以使用factor()函式：
gender <- c("boy", "girl", "boy", "boy", "girl")  
gender <- factor(gender)  
gender      

levels(gender)  #查看裡面存在著哪些類別

#factor(資料向量,levels=類別次序)，levels參數可設定各類別的次序
factor(c("大學生","碩士班學生","博士班學生"),
       levels = c("大學生","碩士班學生","博士班學生"))


###################################################################
#list
#
#list可以存放「任何型態」的變數
Dr.Lee <- list(gender="man", age=18, 
               hobby=c("tease", "be teased"))
Dr.Lee 

str(Dr.Lee)
Dr.Lee[[3]] 
## [1] "tease"     "be teased"
Dr.Lee[3]   
## $hobby
## [1] "tease"     "be teased"

str(Dr.Lee[[3]] )     # Ans:使用兩個中括號，取出來的資料是vector
str(Dr.Lee[3] )       # Ans:使用一個中括號，取出來的資料是list

listSample<-list(Students=c("Tom","Kobe","Emma","Amy"),Year=2017,
                 Score=c(60,50,80,40),School="CGU")

listSample$Students
#[1] "Tom"  "Kobe" "Emma" "Amy"

listSample$Year
#[1] 2017

listSample$Score
#[1] 60 50 80 40

listSample$School
#[1] "CGU"

listSample[[1]] ##取得中表中第一個變量的值
## [1] "Tom"  "Kobe" "Emma" "Amy"

#如果只使用單中括號，回傳的資料型態會是列表list，並非列表中的值
listSample[1] ##取得中表中第一個變量（列表型態）
## $Students
## [1] "Tom"  "Kobe" "Emma" "Amy"


#列表資料也可和向量資料一樣，重新編輯設定
listSample[[1]] 
## [1] "Tom"  "Kobe" "Emma" "Amy"
listSample[[1]]<-c("小明","大雄","胖虎","小新","大白") ##將Students變量重新設定
listSample[[1]] 
## [1] "小明" "大雄" "胖虎" "小新" "大白"

#除了編輯以外，列表資料也能用$符號與<-變數設定符號新增
listSample$Gender<-c("M","F","M","F","M") ##新增Gender變量，並設定向量值

#若需刪除某變量，可將變量值設定為NULL
listSample$Score<-NULL ##刪除Score變量



###################################################################
#matrix

a <- matrix(c(1:6), nrow=3, ncol=2) #建立一個3x2的矩陣，依照column分別填入1~6的值
a

b <- matrix(c(3:8), nrow=2, ncol=3) #建立一個2x3的矩陣，依照column分別填入3~8的值
b

a[2,2]
b[1, ] 

a %*% b #矩陣相乘

# 和矩陣相關的運算函式：
# 
# t(x)：將矩陣轉置。
# %*%：矩陣相乘。
# diag()：產生一個對角矩陣，或回傳矩陣的對角線向量
# det()：計算矩陣行列式值，一定是要對稱矩陣。
# solve()：傳回矩陣的反矩陣，非常適合解線性方程式。
# eigen()：計算矩陣的特徵向量與特徵值。

#建立一個3x2的矩陣，隨機從1~100內填入6個值
a <- matrix(sample(1:100, size=6), nrow=3, ncol=2) 
#建立一個2x3的矩陣，隨機從1~100內填入6個值
b <- matrix(sample(1:100, size=6), nrow=2, ncol=3) 
#建立一個4x4的方陣，隨機從1~100內填入16個值
c <- matrix(sample(1:100, size=16), nrow=4, ncol=4)


x = 1:30
x.matrix = matrix(x,5,6)   #預設為byrow = FALSE
x.matrix_2 = matrix(x,5,6,byrow = TRUE)

#查詢矩陣屬性
str(x.matrix)
nrow(x.matrix)
ncol(x.matrix)
dim(x.matrix)
length(x.matrix)


Vince=c(1,2,3,4,5)
Curry=c(9,8,7,6,5)
Klay=c(-1,-2,-3,-4,-5)
rmatrix = rbind(Vince,Curry,Klay)
cmatrix = cbind(Vince,Curry,Klay)
#去除部分欄位: matrix[列，欄]
#Ex:
rmatrix[-1,-2]
cmatrix[-2,-1]
rmatrix[ ,2:4]
rmatrix[ ,-2:-4]
rmatrix[-1:-2]
rmatrix[1,2] = NA


#設定欄位名稱
colnames(rmatrix) = c("hobby","figure","score","speed","jump")

#轉置矩陣
t(rmatrix)


###################################################################
#三維陣列組:Array()
high.dim = array(1:1000,dim=c(10,10,10))







###################################################################
#dataframe

#讀入:

#方法一
AV.data.csv = read.csv(file.choose())
AV.data.csv #透過顯示框直接選取要開啟的檔案

#方法二
AV.data.csv = read.csv("C:/Users/USER/Desktop/R resourse/R/AV data.csv")
AV.data.csv #透過檔案的絕對路徑呼叫

str(AV.data.csv)

#自造:
tmp <- data.frame(Student_ID=c(1,2,3,4,5),
                  name=c("Helen", "Lun", "Leon", "Kevin", "Tommy"),
                  score=c(80,36, 88.9, 97.5, 60))
tmp       

tmp[4,3]
tmp[1, ] 
tmp[, 3]
tmp$name  
tmp[tmp$name == "Leon", ]
tmp[c(FALSE, FALSE, TRUE, FALSE, FALSE), ]



name=c("前田香織","橋本有菜","明里岫","波多野結衣","Edita","Princessdolly")
nation = c("JAPAN","JAPAN","JAPAN","JAPAN","Russia","Taiwan")
genre = c("DP","Anal","Group","Gangbang","bigtit","hugecock")
breast = c("middle","proper","middle","E cup","small","D cup")
episodes = c(12,16,14,36,10,6)

AV.data=data.frame(name,nation,genre,breast,episodes)
AV.data

#取得部分資料
AV.data["genre"]
AV.data$genre

AV.data["name","breast"]

AV.data[2]
AV.data[3, ]

colnames(AV.data)
rownames(AV.data)
str(AV.data)

#更改資料
AV.data$episodes[2] <- 39
AV.data
AV.data$episodes <- NULL
AV.data

#增加資料
rbind(AV.data,c("相澤南","JAPAN","Gangbang","middle",20,18500))

#增加欄位
salary = c(10500,9500,10000,25000,16590,20500)
AV.data$salary = salary

#透過編輯器新增資料
AV.data.edit = edit(AV.data)
AV.data.edit


#路徑
getwd()
setwd("C:/Users/USER/Documents/R")

#新建檔案、編輯檔案
cat("Hello\nWorld!",file="practice.txt")
cat("\n","Do you wanna I give you an assfuck?",file="practice.txt",append = TRUE)

#將資料框架寫出檔案
write.csv(AV.data, file = "AV data.csv")



###################################################################
#資料屬性查詢
islands

head(islands)
head(names(islands))

USArrests

head(USArrests) #若為資料框，則會顯示行（欄位）名稱
head(names(USArrests))

dimnames(USArrests) 

length(USArrests) #若資料行態為資料框，則會顯示行（欄位）數
length(islands) 

dim(USArrests) #先列後行

使用class()函數可知道變數類別

class(1)
## [1] "numeric"
class("Test")
## [1] "character"
class(Sys.Date())
## [1] "Date"

#使用table()函數可知道向量中每個值出現幾次
iris$Species ##原始值
table(iris$Species) ##統計結果



