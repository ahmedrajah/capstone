install.packages("data.table")
library(data.table)


rawShootOuts <- fread ("intlShootOuts.csv")
rawKicks <- fread("intlKicks.csv")
eplPK <- fread("eplPK.csv")

psLen <- rawShootOuts[,c(3, 11, 12),]
psLen$Total<-psLen$WinTaken+psLen$LoTaken

str(rawShootOuts)
str(rawKicks)
str(eplPK)

summary(psLen$Total)
hist(psLen$Total, main = "Shootout Length Distribution", xlab = "Number of Shots")
#Minimum shootout length is 7
#Maximum shootout length is 18
#Mean shootout length is just under 10 (9.775)


eplSea<- eplPK[,1]
eplSea$Rate <- eplPK$Scored / eplPK$Total
summary (eplSea$Rate)
summary (eplPK$Total)
#Lowest conversion rate for an EPL season was 70%
#Highest conversion rate for an EPL season was 97.56%
#Mean conversion rate for an EPL season 84.61%



