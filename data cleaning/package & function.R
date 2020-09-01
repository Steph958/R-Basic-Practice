

##########################################################################
#函式

str(iris)
summary(iris)

# ?str
# ?summary

iris$Sepal.Length  
mean(iris$Sepal.Length)     #「花萼長度」的平均值
var(iris$Sepal.Length)      #「花萼長度」的變異數
sd(iris$Sepal.Length)       #「花萼長度」的標準差
median(iris$Sepal.Length)   #「花萼長度」的中位數
max(iris$Sepal.Length)      #「花萼長度」中的最大值
min(iris$Sepal.Length)      #「花萼長度」中的最小值
sum(iris$Sepal.Length)      #「花萼長度」加總
range(iris$Sepal.Length)    #「花萼長度」最小值和最大值(全距)
quantile(iris$Sepal.Length, probs=0.25)  # 第一四分位數 
quantile(iris$Sepal.Length, probs=0.75)  # 第三四分位數 




a <- c(1, 2, 3, 5, 8, 13, 21, NA, 55)
sum(a)
## [1] NA
sum(a, na.rm=TRUE) #表示若資料裡面有遺漏值NA，會把它忽略
## [1] 108




# paste()：串接字串(把 "Happy"" 和 "White Day"" 兩個字串拼貼起來，sep代表連結字串的符號)
paste("Happy", "White-Day", sep=" ") 




# append()：把兩個vector串接起來 
b <- c(1,2,3)
c <- c(4,5,6)
append(b, c)




#合併：rbind(), cbind()
a <- data.frame(x=c(1,2,3), y=c("Henry", "Lun", "Kevin"))
b <- data.frame(x=c(4,5,6), y=c("Helen", "Tommy", "Leon"))
a
b
rbind(a,b) # rbind()：把兩個data frame，依據row合併起來
cbind(a,b) # cbind()：把兩個data frame，依據column合併起來




#數列：sample(), seq()
sample(x=1:100, size=10)  # 從1~100數字中，隨機挑10個數字，產生一個數列(vector)
seq(from=1, to=5, by=0.5) # 產生一個「從1開始，每次加0.5，直到5為止」的數列(vector)




#預覽：head(), tail()
head(iris, n=6)  # head(): 顯示data frame中的前6筆資料
tail(iris, n=6)  # tail(): 顯示data frame中的後6筆資料




#排序：order(), sort()
a <- sample(x=1:100, size=10) # 從1~100數字中，隨機挑10個數字，產生一個數列(vector)
a
##  [1]  2  5 28 70 88 30  7 24 34 21
# 用order()，把數列由大排到小；從小排到大，decreasing = FALSE
a[order(a, decreasing=TRUE)]   
##  [1] 88 70 34 30 28 24 21  7  5  2
sort(a, decreasing=TRUE)      
##  [1] 88 70 34 30 28 24 21  7  5  2




#重複：unique(), duplicated()
a <- c("A", "B", "C", "A", "A", "B")
unique(a)       # 萃取資料中unique的element
## [1] "A" "B" "C"

duplicated(a)            # 若後面有重複的資料，函式會回傳TRUE，而第一個資料會是FALSE
a[duplicated(a)==FALSE]  # 和 unique(a)一樣效果
## [1] FALSE FALSE FALSE  TRUE  TRUE  TRUE
## [1] "A" "B" "C"

# which()：找出第幾個element是TRUE(在a裡面，第幾個element的值等於100)
a <- c(68,73,99,100,56,100,85,36)
which(a==100)

##########################################################################
#函數
# 名字<-function(參數1,參數2,...){
#     程式碼本體
#     最後一行的輸出自動設為回傳值
# }

myMean<-function(vec){
    m<-sum(vec)/length(vec)
    return(m)
}
myMean(1:6)



cal_bmi<-function(height,weight){
    if(height>5){
        height <- height/100
    }
    bmi <- weight / height**2
    return(bmi)
}
cal_bmi(160,50)



# 函式編程（Functional programming）
#可以用在所有支援函數為一等公民（First Class）的程式語言，其中包括：
    # 可以將函數指定為一個變數 Assign a function to a variable
    # 可以將函數當作參數傳遞 Pass a function as an argument
    # 可以回傳一個函數 Return a function

hello<-function(){
    print("Hello")
}
sayHello <- function() {
    return(hello())
}
sayHello()
## [1] "Hello"

# 上述支援函數為一等公民（First Class）的條件，最實用的應該為在purrr套件中，將函數當作參數傳遞
# purrr套件提供一系列功能，將向量與函數的功能搭配，解決for迴圈速度很慢且程式碼很長的問題
# 在purrr套件中最重要的功能是map家族，
# 包括map(), map_chr(), map_int(), map_dbl(), map_df(), walk()等


# Ex. map_dbl():
good_teacher_score<-function(ori_score){
    better_score<-sqrt(ori_score)*10
    return(better_score)
}

ori_score_list<-(sample(1:100,30))
#print(ori_score_list)

#install.packages("purrr")

#library(purrr)
new_score_list<-
    map_dbl(ori_score_list,
            good_teacher_score)

print(new_score_list)

# data.frame(ori_score_list,
#            new_score_list)


#以上程式等價於:
ori_score_list<- list(sample(1:100,30))
print(ori_score_list)
print(str(ori_score_list))

for (i in ori_score_list){
    new_score_list<- good_teacher_score(i)
}
print(new_score_list)
    

# Ex. map2_dbl():
# map函數的設計是輸入一組需逐一計算的向量
# 兩組成對且需逐一計算的向量:map2家族函數
# 概念為輸入兩組成對且需逐一計算的向量，成對帶入後置函數內，完成計算。

chi_score<-c(60,50,40)
eng_score<-c(60,50,40)

weight_score<-function(chi,eng){
    final_score<-chi+eng*2
    return(final_score)
}
    
weight_score_list<-
    map2_dbl(chi_score,
             eng_score,
             weight_score)

data.frame(chi_score,
           eng_score,
           weight_score_list)



#詳細介紹參見purrr之PDF檔#####################



##########################################################################
#套件

# 方法一：用RStudio介面
# 在RStudio上點開「Tools」->「Install Packages」 ：
# 在中間填入想要安裝的套件名稱，點「Install」就大功告成

# 方法二：用install.packages()
# 直接在console裡面輸入函式install.packages("套件名稱")
install.packages("ggplot2")

library(ggplot2) #匯入ggplot2套件到R裡面
ggplot(data=CO2) + geom_boxplot(data=CO2,aes(x=conc, y=uptake, colour=Plant))



#Supplement:
require(graphics)
dat<- data.frame(t=seq(0, 2*pi, by=0.1) )
xhrt <- function(t) 16*sin(t)^3
yhrt <- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
dat$y=yhrt(dat$t)
dat$x=xhrt(dat$t)
with(dat, plot(x,y, type="l"))
with(dat, polygon(x,y, col="hotpink")) 
points(c(10,-10, -15, 15), c(-10, -10, 10, 10), pch=169, font=5)


