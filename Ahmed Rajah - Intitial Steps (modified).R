install.packages("data.table")
library(data.table)

#set WD
setwd("/Users/flo/Documents/CKME999/Capstone/CKME136 - Data Analytics Capstone/Data/PKs and PSs - FIFA WC EU UEFA CL/")


#import 3 tables in CSV format
rawShootOuts <- read.csv ("intlShootOuts.csv", stringsAsFactors=FALSE)
rawKicks <- read.csv ("intlKicks.csv")
eplPK <- read.csv ("eplPK.csv")


#create table of Penantly Shootout Lengths
psLen <- rawShootOuts[,c(3, 7, 8, 11, 12),]
psLen$Total<-psLen$WinTaken+psLen$LoTaken

str(rawShootOuts)
str(rawKicks)
str(eplPK)

summary(psLen$Total)
hist(psLen$Total, main = "Shootout Length Distribution", xlab = "Number of Shots")
#Minimum shootout length is 7
#Maximum shootout length is 18
#Mean shootout length is just under 10 (9.775)

#Add conversion rate column to EPL PK data frame
eplPK$Rate <- eplPK$Scored / eplPK$Total

summary (eplPK$Rate)
summary (eplPK$Total)
#Lowest conversion rate for an EPL season was 70%
#Highest conversion rate for an EPL season was 97.56%
#Mean conversion rate for an EPL season 84.61%

wcRates <- data.frame ("ID" = character(), "Rate" = numeric(), stringsAsFactors = FALSE)
#for (x in 1:length(rawShootOuts$Event)){


###the 2 for sections can be deleted (i think) -- sat nov 3
#for (x in 1:71){
    if (rawShootOuts$Event =="WC") {
      wcRates$ID<- rawShootOuts$ID
      #wcRates$Rate <- (rawShootOuts$WinScored+rawShootOuts$LoScored)/(rawShootOuts$WinMiss+rawShootOuts$LoMiss)
      }
  
#}

for (x in 1:71){
  if (rawShootOuts[x,'Event'] == "WC") {
    print (rawShootOuts[x,'ID'])
    wcRates$ID <- rawShootOuts[x, 'ID']
    }
}


#wcRates <- subset (rawShootOuts, ID == "WC")
#this should be deleted (i think) -- sat nov 3

#this one works
#wcRates <- rawShootOuts[rawShootOuts$Event == "WC",]

wcShootOuts <- rawShootOuts[rawShootOuts$Event == "WC", c("ID", "WinScored","LoScored","WinTaken","LoTaken")]
wcShootOuts$Rate <- (wcShootOuts$WinScored+wcShootOuts$LoScored)/(wcShootOuts$WinTaken+wcShootOuts$LoTaken)


euShootOuts <- rawShootOuts[rawShootOuts$Event == "EU", c("ID", "WinScored","LoScored","WinTaken","LoTaken")]
euShootOuts$Rate <- (euShootOuts$WinScored+euShootOuts$LoScored)/(euShootOuts$WinTaken+euShootOuts$LoTaken)


caShootOuts <- rawShootOuts[rawShootOuts$Event == "CA", c("ID", "WinScored","LoScored","WinTaken","LoTaken")]
caShootOuts$Rate <- (caShootOuts$WinScored+caShootOuts$LoScored)/(caShootOuts$WinTaken+caShootOuts$LoTaken)

#variance and means

wcMeanRate <- mean(wcShootOuts$Rate)
wcCumRate <- sum(wcShootOuts$WinScored+wcShootOuts$LoScored)/sum(wcShootOuts$WinTaken+wcShootOuts$LoTaken)
varWCRate <- var(wcShootOuts$Rate)
#explain the difference


euMeanRate <- mean(euShootOuts$Rate)
euCumRate <- sum(euShootOuts$WinScored+euShootOuts$LoScored)/sum(euShootOuts$WinTaken+euShootOuts$LoTaken)
varEURate <- var(euShootOuts$Rate)


caMeanRate <- mean(caShootOuts$Rate)
caCumRate <- sum(caShootOuts$WinScored+caShootOuts$LoScored)/sum(caShootOuts$WinTaken+caShootOuts$LoTaken)
varCARate <- var(caShootOuts$Rate)



eplMeanRate <- mean (eplPK$Rate)
eplCumRate <- sum(eplPK$Scored.)/sum(eplPK$Total)
varEPLRate <- var (eplPK$Rate)
  
  
#boxplots
boxplot (wcShootOuts$Rate, euShootOuts$Rate, caShootOuts$Rate, eplPK$Rate,
         main = "Comparison of Conversion Rates across Events and Seasons",
         names = c("World Cup", "Euro", "Copa America", "EPL"),
         col = c("blue", "green", "red", "white"),
         border = "black",
         varwidth = TRUE
         )



#create new data.frame with first 6 kicks from every shootouts and the winning team (a or b, a odd/b is even)

soF6 <- data.frame("ID"=rawShootOuts$ID)

#get all the position 1, 2, 3, 4, 5 and 6th kicks to be combined with ID and Winner in a new data.frame

pos1 <- rawKicks[rawKicks$KickNum == 1, 'Success']
pos2 <- rawKicks[rawKicks$KickNum == 2, 'Success']
pos3 <- rawKicks[rawKicks$KickNum == 3, 'Success']
pos4 <- rawKicks[rawKicks$KickNum == 4, 'Success']
pos5 <- rawKicks[rawKicks$KickNum == 5, 'Success']
pos6 <- rawKicks[rawKicks$KickNum == 6, 'Success']

soF6<- cbind (soF6, pos1, pos2, pos3, pos4, pos5, pos6)




#Initialize blank columm
soF6$Winner <- NA

#Fill winner column - A for first team / B for Second team
win <- c()

for (i in 1:71){
  
  if (rawShootOuts[i, 'ID']==soF6[i, 'ID']) {
    win[i] <- rawShootOuts[i, 'First']   
  }
  
}

win <- replace (win, win=='W', 'A')
win <- replace (win, win=='L', 'B')

#add updated data to Winner column
soF6$Winner <- win


#create subsets for First Kick, First 2 Kicks, First 4 Kicks and First 6 kicks

soF1 <- soF6[,c('ID', 'pos1', 'Winner')]
soF2 <- soF6[,c('ID', 'pos1', 'pos2', 'Winner')]
soF4 <- soF6[,c('ID', 'pos1', 'pos2', 'pos3', 'pos4', 'Winner')]
soF6 <- soF6[,c('ID', 'pos1', 'pos2', 'pos3', 'pos4', 'pos5', 'pos6', 'Winner')]



#wilcoxon sum rank two-sided test (non-parametric) since non-normal distrubtions

#WC shootout conversion rates versus EPL season conversion rates
wilcox.test(wcShootOuts$Rate, eplPK$Rate)
## >p-value of 0.0000362 // the medians are significantly different aka reject null

#EU shootout conversion rates versus EPL season conversion rates
wilcox.test(euShootOuts$Rate, eplPK$Rate)
## >p-value of 0.1234 // the medians are not significantly different aka cannot reject null

#CA shootout conversion rates versus EPL season conversion rates
wilcox.test(caShootOuts$Rate, eplPK$Rate)
## >p-value of 0.03519 // the medians are significantly different aka reject null

#All INTL shootout conversion rates versus EPL season conversion rates
#####would need to add a Rate column to rawShootOuts
rawShootOuts$Rate <- (rawShootOuts$WinScored+rawShootOuts$LoScored)/(rawShootOuts$WinTaken+rawShootOuts$LoTaken)
wilcox.test(rawShootOuts$Rate, eplPK$Rate)
## >p-value of 0.03519 // the medians are significantly different aka reject null


boxplot (rawShootOuts$Rate, eplPK$Rate,
         main = "Comparison of Conversion Rates across Events and Seasons",
         names = c("All International", "EPL"),
         col = c("dark gray", "white"),
         border = "black",
         varwidth = TRUE
)


#density plot of (frequencies?) of conversion rates across all major international penalty shootouts
#compared to plot of EPL season rates

plot (density(rawShootOuts$Rate, cut= 0.3, to=1.0), main = "Penalty Shootout Conversion Rates", xlab = "Conversion Rate", ylim = c(0,5))
lines(density(eplPK$Rate), col = 'red')


#does it really make sense to compare the conversion rates from each shootout which
#are based on misses in order to end the penalty shootout
#whereas penalty kicks have no start or end beyond the single event - the one kick


#conversion rate across all kicks in penalty shootouts
sum(rawKicks$Success) / 694
#Conversion rate across all kicks in the 71 penalty shootouts is 75.07205%