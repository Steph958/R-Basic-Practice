
#data.table
#data.table是data.frame資料框型別的延伸
#如要使用必須安裝data.table (Dowle and Srinivasan 2019) package
#使用data.table讀取大型資料的速度比使用資料框快上數倍，進階處理語言也相當好用

install.packages("data.table") ##安裝
library(data.table) ##載入


#fread("檔案名稱")  #讀取檔案


library(SportsAnalytics)

NBA1516<-fetch_NBAPlayerStatistics("15-16")

NBA1516DT<-data.table(NBA1516)
class(NBA1516DT)
## [1] "data.table" "data.frame"

# 可以發現轉換後的NBA1516DT資料型態為data.table以及data.frame
# 因為data.table是data.frame資料框型別的延伸
# 所以是data.table型態的資料就一定會是data.frame型態。


#列的篩選
NBA1516DT[grepl('James',Name)]

NBA1516DT[GamesPlayed>80]



#欄位選擇 & 運算
NBA1516DT[,mean(GamesPlayed)] ##因沒有篩選需求，,前方留空

NBA1516DT[,.(mean(GamesPlayed),mean(PersonalFouls),mean(Steals))] 
#一次計算多個數值 
#此時第二個欄位j需要使用.()包起來
#因沒有篩選需求，,前方留空
##    V1  V2 V3
## 1: 55 105 41


#在計算新欄位時，可以在新欄位定義的前方加上欄位名稱=，同時替欄位取名字
NBA1516DT[,.(GamesPlayedMean = mean(GamesPlayed),
             PersonalFoulsMean = mean(PersonalFouls),
             StealsMean = mean(Steals))]
##    GamesPlayedMean PersonalFoulsMean StealsMean
## 1:              55               105         41


NBA1516DT[,.(GamesPlayedMax = max(GamesPlayed), 
             ThreesMadeMin = min(ThreesMade), 
             FieldGoalsMadeSD = sd(FieldGoalsMade))] 
##    GamesPlayedMax ThreesMadeMin FieldGoalsMadeSD
## 1:             82             0              166

#計算出所有出場數大於70的球員，平均投進幾顆三分球與兩分球
NBA1516DT[GamesPlayed>70,
          .(ThreesMadeMean=mean(ThreesMade),
            FieldGoalsMadeMean=mean(FieldGoalsMade))]


#by分組依據
NBA1516DT[,.(.N,AssistsMean=mean(Assists)),
          by=Team] #每隊計算平均助攻

# .N在data.table內是保留字，用來計算個數
#計算出NBA各隊的中鋒球員數和他們的平均三分球出手次數
NBA1516DT[Position=="C",
          .(.N,ThreesAttemptedMean=mean(ThreesAttempted)),
          by=Team]




