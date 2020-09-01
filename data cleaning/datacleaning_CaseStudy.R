
#資料清洗綜合練習#####################################################

install.packages("SportsAnalytics")

library(SportsAnalytics)

NBA1516<-fetch_NBAPlayerStatistics("15-16")

str(NBA1516)
head(NBA1516)





#資料排序後篩選#####################################################


#想要找出出場數最高的前五名選手的所有資料
NBA1516Order<-NBA1516[order(NBA1516$GamesPlayed,decreasing = T),]

NBA1516Order[1:5,] 
##逗號前方放1~5，表示取1~5列；逗號後方空白，表示要取所有欄位



#想要出出場分鐘數最高的前十名選手的名字
NBA1516OrderM<-NBA1516[order(NBA1516$TotalMinutesPlayed,decreasing = T),]
NBA1516OrderM[1:10,"Name"] 
##逗號前方取1~10列；逗號後方放"Name"，表示取名稱為Name之欄位



#欄位值篩選#####

subset(NBA1516,Team=="BOS")

#字串條件搜尋後篩選#######################################################
#將所有名字裡有“James”的選手資料取出

NBA1516[grepl("James",NBA1516$Name),]




