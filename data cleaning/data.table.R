
#data.table
#data.table�Odata.frame��Ʈث��O������
#�p�n�ϥΥ����w��data.table (Dowle and Srinivasan 2019) package
#�ϥ�data.tableŪ���j����ƪ��t�פ�ϥθ�Ʈا֤W�ƭ��A�i���B�z�y���]�۷��n��

install.packages("data.table") ##�w��
library(data.table) ##���J


#fread("�ɮצW��")  #Ū���ɮ�


library(SportsAnalytics)

NBA1516<-fetch_NBAPlayerStatistics("15-16")

NBA1516DT<-data.table(NBA1516)
class(NBA1516DT)
## [1] "data.table" "data.frame"

# �i�H�o�{�ഫ�᪺NBA1516DT��ƫ��A��data.table�H��data.frame
# �]��data.table�Odata.frame��Ʈث��O������
# �ҥH�Odata.table���A����ƴN�@�w�|�Odata.frame���A�C


#�C���z��
NBA1516DT[grepl('James',Name)]

NBA1516DT[GamesPlayed>80]



#����� & �B��
NBA1516DT[,mean(GamesPlayed)] ##�]�S���z��ݨD�A,�e��d��

NBA1516DT[,.(mean(GamesPlayed),mean(PersonalFouls),mean(Steals))] 
#�@���p��h�Ӽƭ� 
#���ɲĤG�����j�ݭn�ϥ�.()�]�_��
#�]�S���z��ݨD�A,�e��d��
##    V1  V2 V3
## 1: 55 105 41


#�b�p��s���ɡA�i�H�b�s���w�q���e��[�W���W��=�A�P�ɴ������W�r
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

#�p��X�Ҧ��X���Ƥj��70���y���A������i�X���T���y�P����y
NBA1516DT[GamesPlayed>70,
          .(ThreesMadeMean=mean(ThreesMade),
            FieldGoalsMadeMean=mean(FieldGoalsMade))]


#by���ը̾�
NBA1516DT[,.(.N,AssistsMean=mean(Assists)),
          by=Team] #�C���p�⥭���U��

# .N�bdata.table���O�O�d�r�A�Ψӭp��Ӽ�
#�p��XNBA�U�������W�y���ƩM�L�̪������T���y�X�⦸��
NBA1516DT[Position=="C",
          .(.N,ThreesAttemptedMean=mean(ThreesAttempted)),
          by=Team]



