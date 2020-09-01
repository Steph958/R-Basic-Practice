#流程控制

# 在R裡面，流程控制的指令主要可以分為三類：
# 
# 邏輯判斷：>、<、==、!=、%in%、&、|、!
# 條件指令：if、else、ifelse、switch
# 迴圈指令：for、while、repeat、break、next


##############################################################
#邏輯判斷:
x <- 5
y <- c(0,2,3)

x %in% c(1,2,3,4,5) 
y %in% c(1,2,3,4,5)

x <- 5
y <- 8

!(x > 3)         
## [1] FALSE

x > 3 & y > 10   # AND：和；交集(&)
## [1] FALSE

x > 3 | y > 10   # OR ：或；聯集(|)
## [1] TRUE

# &和&&的區別：

c(T,T,T) & c(F,T,T)    # 用一個&，會將向量內的每一個元素互相比對，判斷是True/False
## [1] FALSE  TRUE  TRUE
c(T,T,T) && c(F,T,F)   # 用兩個&，只會將向量內的「第一個元素」互相比對而已
## [1] FALSE


######################################################################
#條件指令:

if(3 > 2){
    TRUE
}else{
    FALSE
}


score<-95
if(score>=90){
    print("優秀")
}else if(score>=60){
    print("及格")
}else{
    print("不及格")
}


CHscore<-95 ##國文成績
ENscore<-55 ##英文成績
if(CHscore>=60){
    if(ENscore>=60){
        print("全部及格")
    }else{
        print("國文及格，英文再加油")
    }
}else{
    if(ENscore>=60){
        print("英文及格，國文再加油")
    }else{
        print("全部不及格")
    }
}





# 單行寫法
if(3 > 2) TRUE else FALSE

score<-80
ifelse(score>=60,"及格","不及格")

ifelse(2 > 3, T, F)

scoreVector<-c(30,90,50,60,80)
ifelse(scoreVector>=60,"及格", "不及格")


switch(2,      # 指定執行第二行程式碼，故回傳4。(請自行修改數字，看不同的結果)
       1+1,    # 第一行：1+1
       2^2,    # 第二行：2的平方
       3*6)    # 第三行：3*6

switch("Tom",  # 指定執行名稱為Tom的這行程式碼，故回傳7 (請自行修改名稱，看不同的結果)
       Tom = 2+5,         
       Susan = 1*0,       
       Helen = "Apple",
       Lee = 1024)

######################################################################
#迴圈指令:

#for-loop

result <- 0

for(i in c(1:135)){ 
    # for-loop裡，i會依序帶入1~135的值，重複進行括號內的程式碼
    
    # 迴圈內重複進行的動作
    result <- result + i
}

result

#while-loop

i <- 1
result <- 0

while(i < 136){   
    # while-loop當符合裡面的條件時，就會一直重複括號內的程式碼，直到不符合為止
    
    # 迴圈內重複進行的動作
    result <- result + i
    i <- i + 1
}

result

#repeat-loop

i <- 1
result <- 0

repeat{         
    # repeat和while很像，差別在於條件可以寫在任何地方，並且使用break跳出迴圈
    
    if(i > 135) break # 當i比135大時，用break跳出迴圈
    
    # 迴圈內重複進行的動作
    result <- result + i
    i <- i + 1
}

result



#break和next

# break 主要用來跳出迴圈
for(i in c(1:5)){
    
    if(i == 3) break  # 當i等於3的時候，跳出迴圈
    
    # 迴圈內重複進行的動作
    print(i)  
}


# next 主要用來省略此次迴圈的行為，直接進入下一次迴圈

for(i in c(1:5)){
    
    if(i == 3) next  # 當i等於3的時候，省略此次迴圈(skip)的動作，從下一個i=4開始
    
    # 迴圈內重複進行的動作
    print(i)  
}