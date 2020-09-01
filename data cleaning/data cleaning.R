########################################################################################
#資料型別轉換
num<-100
cha<-'200'
boo<-T


is.numeric(num)
## [1] TRUE
is.numeric(cha)
## [1] FALSE
is.character(num)
## [1] FALSE
is.character(cha)
## [1] TRUE
is.logical(boo)
## [1] TRUE


class(num)
## [1] "numeric"
class(cha)
## [1] "character"
class(boo)
## [1] "logical"
class(Sys.Date())
## [1] "Date"


as.numeric(cha)
## [1] 200
as.numeric(boo)
## [1] 1
as.character(num)
## [1] "100"
as.character(boo)
## [1] "TRUE"
as.numeric("abc")
## Warning: NAs introduced by coercion
## [1] NA


library(lubridate)
ymd('2012/3/3')
## [1] "2012-03-03"
mdy('3/3/2012')
## [1] "2012-03-03"



########################################################################################
#文字字串處理

# 切割 strsplit()
# 子集 substr()
# 大小寫轉換 toupper() tolower()
# 兩文字連接 paste() paste0()
# 文字取代 gsub()
# 前後空白去除 str_trim() 需安裝stringr(Wickham 2019b) package

strsplit ("Hello World"," ")
## [1] "Hello" "World"
toupper("Hello World")
## [1] "HELLO WORLD"
tolower("Hello World")
## [1] "hello world"
paste("Hello", "World", sep='')
## [1] "HelloWorld"
substr("Hello World", start=2,stop=4)
## [1] "ell"
gsub("o","0","Hello World")
## [1] "Hell0 W0rld"

library(stringr)
str_trim(" Hello World ")
## [1] "Hello World"




grep("A",c("Alex","Tom","Amy","Joy","Emma")) 
##在姓名文字向量中尋找A，回傳包含"A"之元素位置
## [1] 1 3
grepl("A",c("Alex","Tom","Amy","Joy","Emma")) 
##在姓名文字向量中尋找A，回傳各元素是否包含"A"
## [1]  TRUE FALSE  TRUE FALSE FALSE


########################################################################################
#子集Subset
#一維資料 (向量)

letters 
##  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
## [20] "t" "u" "v" "w" "x" "y" "z"
letters[1] ##取出letters向量的第一個元素
## [1] "a"
letters[1:10] ##取出letters向量的前十個元素
##  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
letters[c(1,3,5)] ##取出letters向量的第1,3,5個元素
## [1] "a" "c" "e"
letters[c(-1,-3,-5)] ##取出letters向量除了第1,3,5個元素之外的所有元素
##  [1] "b" "d" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v"
## [20] "w" "x" "y" "z"

#若想要快速取得一向量的開頭與結尾元素，可使用head()和tail()函數

head(letters,5) ##取出letters向量的前五個元素
## [1] "a" "b" "c" "d" "e"
tail(letters,3) ##取出letters向量的後三個元素
## [1] "x" "y" "z"


#二維資料
# 最常見的二維資料為data.frame資料框
#以,分隔列與行的篩選條件，資料篩選原則為前Row,後Column
#若不想篩選列，則在,前方保持空白即可。
# 
# 篩選方式可輸入位置(index)、欄位名稱或輸入布林變數(TRUE/FALSE)
# 
# 輸入位置: dataFrame[row index,column index]
# 輸入布林變數: dataFrame[c(T,F,T),c(T,F,T)]
# 輸入欄位名稱: dataFrame[row name,column name]

data(iris)
iris[1,2] ##第一列Row，第二行Column
iris[1:3,] ##第1~3列Row，所有的行Column
iris[,"Species"] ##所有的列Row，名稱為Species的行Column
iris[1:10,c(T,F,T,F,T)] ##第1~10列Row，第1,3,5行Column (TRUE)
iris$Species ##所有的列Row，名稱為Species的行Column


#Row的篩選可使用subset()函數，使用方法為subset(資料表,篩選邏輯)
subset(iris,Species=="virginica") ##Species等於"virginica"的列Row，所有的行Column
#Row的篩選也可搭配字串搜尋函數grepl()
knitr::kable(iris[grepl("color",iris$Species),]) ##Species包含"color"的列，所有的行

tail(iris,3) ##取出iris資料框的後三列
head(iris,5) ##取出iris資料框的前五列


########################################################################################
#排序order


#sort()函數可直接對向量做由小到大的排序
head(islands) ##排序前的前六筆資料
head(sort(islands)) ##由小到大排序後的前六筆資料
head(sort(islands,decreasing = T)) ##由大到小排序後的前六筆資料


#如需對資料框做排序，可使用order()函數
#回傳由小到大之元素位置(index)
order(iris$Sepal.Length)
iris$Sepal.Length[14] #數值最小的元素為第14個元素

order(iris$Sepal.Length,decreasing = T)
iris$Sepal.Length[132] #數值最大的元素為第132個元素


head(iris[order(iris$Sepal.Length),]) 
##依照Sepal.Length欄位數值大小排序後的前六筆資料，預設decreasing = F
head(iris[order(iris$Sepal.Length,decreasing = T),]) 
##改為由大到小排序的前六筆資料



########################################################################################
#資料組合

rbind(c(1,2,3), #第一列
      c(4,5,6)  #第二列
) 


irisAdd<-rbind(iris, #資料框
               c(1,1,1,1,"versicolor")  #新增一列 >> 151列
) 
tail(irisAdd)


cbind(c(1,2,3), #第一行
      c(4,5,6)  #第二行
) 

irisAdd<-cbind(iris, #資料框
               rep("Add",nrow(iris))  #新增一行
) 
tail(irisAdd)


########################################################################################
#資料結合

nameDF<-data.frame(ID=c(1,2,3,4,5),
                   Name=c("Amy","Bob","Chris","David","Emma"))

scoreDF<-data.frame(ID=c(1,2,4),
                    Score=c(60,90,50))

nameDF
scoreDF

merge(nameDF,scoreDF,by="ID")
merge(nameDF,scoreDF,by="ID",all=T)
merge(nameDF,scoreDF,by="ID",all.x=T)
merge(nameDF,scoreDF,by="ID",all.y=T)


#dplyr套件提供更有效率的資料結合方法，包括:

install.packages("dplyr")
library(dplyr) #使用前須先載入套件

inner_join(nameDF,scoreDF,by="ID") #保留有對應到的資料
##   ID  Name Score
## 1  1   Amy    60
## 2  2   Bob    90
## 3  4 David    50

left_join(nameDF,scoreDF,by="ID")#保留左邊的表所有的列
##   ID  Name Score
## 1  1   Amy    60
## 2  2   Bob    90
## 3  3 Chris    NA
## 4  4 David    50
## 5  5  Emma    NA

right_join(nameDF,scoreDF,by="ID")#保留右邊的表所有的列
##   ID  Name Score
## 1  1   Amy    60
## 2  2   Bob    90
## 3  4 David    50

full_join(nameDF,scoreDF,by="ID")
##   ID  Name Score
## 1  1   Amy    60
## 2  2   Bob    90
## 3  3 Chris    NA
## 4  4 David    50
## 5  5  Emma    NA

semi_join(nameDF,scoreDF,by="ID") #留下左邊的ID也有出現在右邊的表的列，右表資料不會輸出
##   ID  Name
## 1  1   Amy
## 2  2   Bob
## 3  4 David

anti_join(nameDF,scoreDF,by="ID")




