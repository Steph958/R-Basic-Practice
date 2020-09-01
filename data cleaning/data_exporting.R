table_1 = read.csv("C:/Users/USER/Desktop/R resourse/R Training/AV data.csv")

colnames(table_1)
rownames(table_1)
str(table_1)

table_1$breast


table_1$legLength <- c(114,102,95,98,103,104)

table_1

write.csv(table_1, file = "AVMODEL_data.csv",row.names=F)