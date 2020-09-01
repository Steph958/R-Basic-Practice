file.read = read.csv("C:/Users/USER/Desktop/Lemonade.csv")
file.read

#敘述統計
summary(file.read$Temperature)

sd(file.read$Rainfall) #標準差
var(file.read$Flyers)   #變異數
range(file.read$Price) #全距
table(file.read$Day)  #計數



# apply(x,margin,function,......)
# x:向量、矩陣、資料框架
# margin:1表示按列計算；2表示按行計算
flyer.mode= apply(file.read,2,mode)



# lapply(x,function,......)
flyer.mode_2 = lapply(file.read, mode)



#tapply(x,index,function=NULL,......,simplify=TRUE)
# X:要計算的組別(欄位) (這裡必須是向量)
# index:要計算的組別(欄位)

temp.day = c(file.read$Day) #分組的種類，按星期幾幾分為7種
file.read.sales = c(file.read$Sales) #要分組計算的欄位，這裡選擇sales
tapply(file.read.sales,temp.day,mean)



