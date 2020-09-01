# dplyr(Wickham et al. 2020) package是Hadley Wickham開發的資料處理分析套件，如要使用必須安裝並載入dplyr package
# 
# dplyr使用以下函數分析整理資料：
# 
# select(): 選要分析的欄位，欄位子集 (Column)
# filter(): 選要分析的觀察值，觀察值子集 (Row)
# mutate(): 增加新欄位
# summarise(): 計算統計值
# group_by(): 分組依據
# arrange(): 觀察值排序
# rename(): 欄位重新命名
# %>%: the “pipe” operator 連結上數函式，將所有函式計算串在一起執行

install.packages("dplyr") ##安裝

library(dplyr) ##載入

library(SportsAnalytics)
NBA1516<-fetch_NBAPlayerStatistics("15-16")





#使用select()函式可選要分析的欄位，也就是針對欄位 (Column)做子集
#
# dplyr提供幾個方便篩選名稱的函式：
# 
# starts_with()
# ends_with()
# contains()
# matches()
# num_range()
# one_of()
# everything()
#
#詳細說明可在R執行視窗中輸入?select_helpers查看。

select1<-select(NBA1516,Name,starts_with("Threes"),starts_with("FieldGoals"))
head(select1)
## 以上等同於
# NBA1516[,c("Name","ThreesMade",
#             "ThreesAttempted","FieldGoalsMade",
#             "FieldGoalsAttempted")]


select2<-select(NBA1516,Name:FreeThrowsAttempted)
head(select2)
##等同於NBA1516[,2:12]


select3<-select(NBA1516,Name:FreeThrowsAttempted,-GamesPlayed)
head(select3)
##等同於NBA1516[,c(2:4,6:12)]










#filter()函式可選要分析的觀察值，也就是針對列 (Row)做子集

filter1<-filter(NBA1516,TotalMinutesPlayed>2850)
filter1
##等同於 NBA1516[NBA1516$TotalMinutesPlayed>2850,]


filter2<-filter(NBA1516,Team %in% c("BOS","SAN","GSW"))
filter2
##等同於 NBA1516[NBA1516$Team %in% c("BOS","SAN","GSW"),]


filter3<-filter(NBA1516,FieldGoalsMade/FieldGoalsAttempted>0.7)
filter3

filter4<-filter(NBA1516,FieldGoalsMade/FieldGoalsAttempted>0.7 & GamesPlayed>30)
filter4








#使用mutate()增加新欄位

#新增新欄位FieldGoalsRate
#欄位值為FieldGoalsMade/FieldGoalsAttempted
mutate1<-mutate(NBA1516,FieldGoalsRate=FieldGoalsMade/FieldGoalsAttempted)
mutate1$FieldGoalsRate[1:10]
#mutate1
#mutate1$FieldGoalsRate


#summarise()函式用來計算統計值

sum1<-summarise(NBA1516,
                nPlayer=n(), #球員個數
                nTeam=n_distinct(Team), #不重複的隊伍數
                nPosition=n_distinct(Position)) #不重複的守備位置數
sum1


#計算出場分鐘數大於2500分鐘的球員個數、平均投進的兩分球數以及平均投出的兩分球數
filter1<-filter(NBA1516,TotalMinutesPlayed>2500)
sum2<-summarise(filter1,
                nPlayer=n(),
                meanFieldGoalsMade=mean(FieldGoalsMade),
                meanFieldGoalsAttempted=mean(FieldGoalsAttempted))
sum2









#group_by()函數的功能為設定分組依據

group1<-group_by(NBA1516,Team)%>%
    summarise(nPlayer=n(),
              meanFieldGoalsMade=mean(FieldGoalsMade),
              meanFieldGoalsAttempted=mean(FieldGoalsAttempted))
head(group1)
## # A tibble: 6 x 4
##   Team  nPlayer meanFieldGoalsMade meanFieldGoalsAttempted
##   <fct>   <int>              <dbl>                   <dbl>
## 1 ATL        15               215                     471.
## 2 BOS        15               209.                    475.
## 3 BRO        16               181.                    396.
## 4 CHA        14               199.                    451.
## 5 CHI        15               209.                    475.
## 6 CLE        16               200.                    433.



group2<-group_by(NBA1516,Team,Position)%>%
    summarise(nPlayer=n(),meanFieldGoalsMade=mean(FieldGoalsMade),
              meanFieldGoalsAttempted=mean(FieldGoalsAttempted))
head(group2)
## # A tibble: 6 x 5
## # Groups:   Team [2]
##   Team  Position nPlayer meanFieldGoalsMade meanFieldGoalsAttempted
##   <fct> <fct>      <int>              <dbl>                   <dbl>
## 1 ATL   C              1                11                      19 
## 2 ATL   PF             6               247.                    516.
## 3 ATL   PG             2               382.                    884 
## 4 ATL   SG             6               161.                    364.
## 5 BOS   C              2               196.                    423 
## 6 BOS   PF             4               182.                    386.







#arrange()
#排序功能，預設為遞增排序

arrange1<-arrange(NBA1516,TotalMinutesPlayed)
head(arrange1)

arrange2<-arrange(NBA1516,desc(TotalMinutesPlayed),desc(GamesPlayed))
head(arrange2)



#計算各隊各守備位置（以Team和Position作為分組依據）的球員數、
#平均投進的兩分球數以及平均投出的兩分球數，
#並依平均投進的兩分球數由大到小排序

arrange3<-group_by(NBA1516,Team,Position)%>%
    summarise(nPlayer=n(),
              meanFieldGoalsMade=mean(FieldGoalsMade),
              meanFieldGoalsAttempted=mean(FieldGoalsAttempted)) %>%
    arrange(desc(meanFieldGoalsMade))
head(arrange3)




#rename()

rename1<-rename(NBA1516,Po=Position)
rename1[1:5,1:5]







