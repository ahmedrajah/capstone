library (caret)

#set WD
setwd("/Users/flo/Documents/CKME999/Capstone/CKME136 - Data Analytics Capstone/Data/PKs and PSs - FIFA WC EU UEFA CL/")


#import 3 tables in CSV format
rawShootOuts <- read.csv ("intlShootOuts.csv", stringsAsFactors=F)
rawKicks <- read.csv ("intlKicks.csv")
eplPK <- read.csv ("eplPK.csv")


#create table of Penantly Shootout Lengths
psLen <- rawShootOuts[,c(3, 7, 8, 11, 12),]
psLen$Total<-psLen$WinTaken+psLen$LoTaken

#inspect data
str(rawShootOuts)
str(rawKicks)
str(eplPK)

summary(psLen$Total)
hist(psLen$Total, main = "Shootout Length Distribution", xlab = "Number of Shots", xlim = range(6:20), ylim = range(0:25))
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

#create data frames for each Major International Event - World Cup (WC), Euro (EU), Copa America (CA)
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
#mean rate > mean of shootout average conversion rates
#cum rate > this is the average conversion rate of all individual kicks (across all shootouts for the respective event)
#mean rate is an average of averages - the average for a shootout is taken, and mean rate is the average of the averate rates



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
#side by side comparison of World Cup Shootouts, European Championship Shootouts, Copa America Shootouts and EPL Penalty kicks
boxplot (wcShootOuts$Rate, euShootOuts$Rate, caShootOuts$Rate, eplPK$Rate,
         main = "Comparison of Conversion Rates across Events and Seasons",
         names = c("World Cup", "Euro", "Copa America", "EPL"),
         col = c("blue", "green", "red", "white"),
         border = "black",
         ylab = "Conversion Rate",
         varwidth = TRUE
         )

#OUTLIER
#In the Copa America set of shootouts there apperas to be one outlier.

caShootOuts$ID [which.min(caShootOuts$Rate)]
caShootOuts[caShootOuts$ID=='CA17',]


allShootOuts <- rawShootOuts[!rawShootOuts$ID == 'CA17',]
caShootOuts <- caShootOuts [!caShootOuts$ID == 'CA17',]

#remove CA17 kicks for data consistency
allKicks <- rawKicks [!rawKicks$ID == 'CA17',]

#save old CA mean and var for reference
caMeanRateOld <- caMeanRate
caCumRateOld <- caCumRate
varCARateOld <- varCARate

#new CA mean and var
caMeanRate <- mean(caShootOuts$Rate)
caCumRate <- sum(caShootOuts$WinScored+caShootOuts$LoScored)/sum(caShootOuts$WinTaken+caShootOuts$LoTaken)
varCARate <- var(caShootOuts$Rate)


#Add a Rate column to rawShootOuts
allShootOuts$Rate <- (allShootOuts$WinScored+allShootOuts$LoScored)/(allShootOuts$WinTaken+allShootOuts$LoTaken)

#all INTL mean and var
allMeanRate <- mean(allShootOuts$Rate)
allCumRate <- sum(allShootOuts$WinScored+allShootOuts$LoScored)/sum(allShootOuts$WinTaken+allShootOuts$LoTaken)
varAllRate <- var(allShootOuts$Rate)

#variances of Rates (Rate column):
# WC        0.02139835
# EU        0.01977132
# CA        0.01274551
# ALL INTL  0.019696
# EPL       0.0068859

#The EPL season conversion rates show a much smaller variance that the shootout conversion rates across all interionall shootouts.
# 0.006886 vs 0.019696

#wilcoxon sum rank test (non-parametric) since non-normal distrubtions
#95% // p < 0.05 will be considered statistically signifcant
#null hypothesis is that the medians are the same

#WC shootout conversion rates versus EPL season conversion rates
wilcox.test(wcShootOuts$Rate, eplPK$Rate)
## >p-value of 0.0000362 // The medians are significantly different; we reject the null hypothesis.

#EU shootout conversion rates versus EPL season conversion rates
wilcox.test(euShootOuts$Rate, eplPK$Rate)
## >p-value of 0.1234 // The medians are not significantly different; we fail to reject the null hypothesis.

#CA shootout conversion rates versus EPL season conversion rates
wilcox.test(caShootOuts$Rate, eplPK$Rate)
## >p-value of 0.05663 // The medians are not significantly different; fail to reject the null hypothesis.

#boxplots
#compare all 3 major international event shootouts to EPL penalty kicks

boxplot (allShootOuts$Rate, eplPK$Rate,
         main = "Conversion Rates - Major International Events vs EPL",
         names = c("All International", "EPL"),
         col = c("dark gray", "white"),
         border = "black",
         ylab = "Conversion Rate",
         varwidth = TRUE
)



#All INTL shootout conversion rates versus EPL season conversion rates
wilcox.test(allShootOuts$Rate, eplPK$Rate)
## >p-value of 0.000739 // The medians are significantly different; we reject the null hypothesis.



sum (eplPK$Total)/sum(c(allShootOuts$WinTaken, allShootOuts$LoTaken))
#In terms of total kicks, ther are almost 3 times as many kicks in the EPL penalty kicks data frame than the international shootouts data frame
#The boxplot for the EPL rates has a tighter range and a higher mean conversion rate



#density plot of conversion rates across all major international penalty shootouts compared to EPL season rates

plot (density(allShootOuts$Rate, cut= 0.3, to=1.0), main = "Penalty Shootout Conversion Rates", xlab = "Conversion Rate", ylim = c(0,5))
lines(density(eplPK$Rate), col = 'red')
legend ("topright", legend = c ("EPL", "International"), col = c("red", "black"), pch = 15)
#this plot is only for reference



#conversion rate across all kicks in penalty shootouts
sum(allKicks$Success) / nrow(allKicks)
#Conversion rate across all kicks in the 70 penalty shootouts is 75.54585%


#mean conversion rate for first 10 positions (note: all PS have atleast 6 kick)
posMeans <- data.frame("Pos"=c(1:10), "Avg"=NA)


for (i in 1:10){
  posMeans[i,2] <- mean (allKicks$Success[allKicks$KickNum == i])
}

print (posMeans)  

plot (posMeans$Pos, posMeans$Avg, type = "h", main = "Mean Conversion Rate by Position", ylab = "Conversion Rate", xlab = "Position within Shootout", ylim = c(0.5, 1),  axes=F)
#y-axis
axis(side = 2, at = seq(0.5,1,0.1), tck = -0.02)
axis(side = 1, at = seq (1:10))
points (posMeans$Avg, col = "dark red", pch = 16)
lines (seq(0.75,10.25, by = 0.5), rep (0.754585,20), col = "purple", lty = 3 )
#this plot shows the conversion rate, by position, for the first 10 positions
#the average across all positions (not ony the first 10) is also visible

#positons 2 and 5 have the highest rate (0.829) and position 8 has the lowest (0.6)
#the first 5 kicks all have conversion rates above the average
#4 of the 5 positions between 6 and 10 (inclusive) are below the average rate
#while we cannot say with significant certainty that the rates for positions 6-10 are lower than for 1-5
#we can say, in the real world, shooters within the first 5 positions have been more successful than in the next 5 positions





#create new data.frame with first 6 kicks from every shootouts and the winning team (a or b, a is odd/b is even)
#all shootouts consist of at least 3 full rounds (2 kicks per round - 1 from each team)

soF6 <- data.frame("ID"=allShootOuts$ID)

#get all the position 1, 2, 3, 4, 5 and 6th kicks to be combined with ID and Winner in a new data.frame

pos1 <- allKicks[allKicks$KickNum == 1, 'Success']
pos2 <- allKicks[allKicks$KickNum == 2, 'Success']
pos3 <- allKicks[allKicks$KickNum == 3, 'Success']
pos4 <- allKicks[allKicks$KickNum == 4, 'Success']
pos5 <- allKicks[allKicks$KickNum == 5, 'Success']
pos6 <- allKicks[allKicks$KickNum == 6, 'Success']


soF6<- cbind (soF6, pos1, pos2, pos3, pos4, pos5, pos6)


#Initialize blank columm
soF6$Winner <- NA

#Fill winner column - A for first team / B for Second team
win <- c()

for (i in 1:nrow(allShootOuts)){
  
  if (allShootOuts[i, 'ID']==soF6[i, 'ID']) {
    win[i] <- rawShootOuts[i, 'First']  
  }
}


#use next 2 lines for A and B to represent 1st team and 2nd team
win <- replace (win, win=='W', 'A')
win <- replace (win, win=='L', 'B')


#add updated data to Winner column
soF6$Winner <- win


#create subsets for First Kick, First 2 Kicks, First 4 Kicks and First 6 kicks

soF1 <- soF6[,c('ID', 'pos1', 'Winner')]
soF2 <- soF6[,c('ID', 'pos1', 'pos2', 'Winner')]
soF4 <- soF6[,c('ID', 'pos1', 'pos2', 'pos3', 'pos4', 'Winner')]
#Already have first 6 (soF6)




#modelling
#Naive-Bayes will be used
#Cross-fold training and testing will be used via trainControl() within train()
#we will compare the (a) accuracy and (b) kappa of the models 
#a simulation of 'random guesses' will also be run to act as baseline 
#this is to imitate a person guessing which team wins a penalty shootout



#set seed 
set.seed (1234)

#setup cross-fold setup for evaluating the model
#for confusion matrix need to use this one with savePredictions and classProbs
train_control <- trainControl(method="cv", number=7, savePredictions = "final", classProbs = T)

#the following grid settings will be used as after repated tests, these were the final values used
grid <- expand.grid(fL=c(0), usekernel=c(FALSE), adjust = 1)


cvModelF1 <- train(Winner~pos1, data=soF6, trControl=train_control, method="nb", tuneGrid = grid)
cvModelF2 <- train(Winner~pos1 + pos2, data=soF6, trControl=train_control, method="nb", tuneGrid = grid)
cvModelF4 <- train(Winner~pos1 + pos2 + pos3 + pos4, data=soF6, trControl=train_control, method="nb", tuneGrid = grid)
cvModelF6 <- train(Winner~pos1 + pos2 + pos3 + pos4 + pos5 + pos6, data=soF6, trControl=train_control, method="nb", tuneGrid = grid)

#Compare Accuracy and Kappa across the 4 models:
cvModels <- data.frame ("Model" = NA, "Accuracy" = NA, "Accuracy SD" = NA, "Kappa" = NA, "Kappa SD" = NA)
cvModels [1,] <- c("First 1", cvModelF1$results$Accuracy, cvModelF1$results$AccuracySD, cvModelF1$results$Kappa, cvModelF1$results$KappaSD)
cvModels [2,] <- c("First 2", cvModelF2$results$Accuracy, cvModelF2$results$AccuracySD, cvModelF2$results$Kappa, cvModelF2$results$KappaSD)
cvModels [3,] <- c("First 4", cvModelF4$results$Accuracy, cvModelF4$results$AccuracySD, cvModelF4$results$Kappa, cvModelF4$results$KappaSD)
cvModels [4,] <- c("First 6", cvModelF6$results$Accuracy, cvModelF6$results$AccuracySD, cvModelF6$results$Kappa, cvModelF6$results$KappaSD)

#create a data frame for each of the models and 4 statistics associated with each
cvModels$Accuracy <- as.numeric(cvModels$Accuracy)
cvModels$Accuracy.SD <- as.numeric(cvModels$Accuracy.SD)
cvModels$Kappa <- as.numeric(cvModels$Kappa)
cvModels$Kappa.SD <- as.numeric(cvModels$Kappa.SD)
cvModels [ , 2:5] <- round (cvModels[ ,2:5], 4)
summary(cvModels)


print(cvModels)

#from this we can see the following:
#The best model, after 6 kicks, produced an accuracy of 0.6571 with a kappa of 0.3143
#There is not much difference between the first 2 models (F1 vs F2) or last 2 models (F4 vs F6)


#generalized linear model
glmModelF1 <- train(Winner~pos1, data=soF6, trControl=train_control, method="glm")
glmModelF2 <- train(Winner~pos1 + pos2, data=soF6, trControl=train_control, method="glm")
glmModelF4 <- train(Winner~pos1 + pos2 + pos3 + pos4, data=soF6, trControl=train_control, method="glm")
glmModelF6 <- train(Winner~pos1 + pos2 + pos3 + pos4 + pos5 + pos6, data=soF6, trControl=train_control, method="glm")


#Compare Accuracy and Kappa across the 4 models:
glmModels <- data.frame ("Model" = NA, "Accuracy" = NA, "Accuracy SD" = NA, "Kappa" = NA, "Kappa SD" = NA)
glmModels [1,] <- c("First 1", glmModelF1$results$Accuracy, glmModelF1$results$AccuracySD, glmModelF1$results$Kappa, glmModelF1$results$KappaSD)
glmModels [2,] <- c("First 2", glmModelF2$results$Accuracy, glmModelF2$results$AccuracySD, glmModelF2$results$Kappa, glmModelF2$results$KappaSD)
glmModels [3,] <- c("First 4", glmModelF4$results$Accuracy, glmModelF4$results$AccuracySD, glmModelF4$results$Kappa, glmModelF4$results$KappaSD)
glmModels [4,] <- c("First 6", glmModelF6$results$Accuracy, glmModelF6$results$AccuracySD, glmModelF6$results$Kappa, glmModelF6$results$KappaSD)

#create a data frame for each of the models and 4 statistics associated with each
glmModels$Accuracy <- as.numeric(glmModels$Accuracy)
glmModels$Accuracy.SD <- as.numeric(glmModels$Accuracy.SD)
glmModels$Kappa <- as.numeric(glmModels$Kappa)
glmModels$Kappa.SD <- as.numeric(glmModels$Kappa.SD)
glmModels [ , 2:5] <- round (glmModels[ ,2:5], 4)
summary(glmModels)

print (glmModels)
#what we can see referencing the linear models is:
#the same order exists whereby the the Accuracy drops from F1 to F2 and then increases from F2 to F4 and F4 to F6
# >> F6 > F4 > F1 > F2 in terms of accuracy for both NB and GLM
#that said, we will be using the NB results as we believe it better fits the problem, and the existing 'ignorance' within the problem and available data



#simulation of guessing winner randomly
#set number of runs for the simulations where each run places a guess for the number of events in our data (soF6)

set.seed (1234)
numberOfRuns = 1000
numberOfGuesses = nrow(soF6)

runs <- data.frame ("RunNum" = 1:numberOfRuns, "Correct" = as.integer(NA))

for (i in 1:numberOfRuns) {
  guesses <- sample (c("A","B"), numberOfGuesses, rep = T)
  runs[i,"Correct"] <- sum(guesses == soF6$Winner)
}

runs$'Accuracy (%)' <- round(runs$Correct/nrow(soF6),3) * 100

print (head(runs))
print (tail(runs))

summary (runs$Correct)
var (runs$Correct)
sd(runs$Correct)

# Percent correct for 3 std. dev. below mean
(mean (runs$Correct) - 3 * sd(runs$Correct)) / 70
# Percent correct for 3 std. dev. above mean
(mean (runs$Correct) + 3 * sd(runs$Correct)) / 70

hist (runs$`Accuracy (%)`, breaks = seq(30, 70, 1), main = "Distribution of Guess Accuracy", xlab = "Accuracy (%)" )

#as expected, guessing a binary outcome through thousands of trials will lead towarsd a normal distribution
#in this case, after 1000 runs, we have an average of 34.86 correct guesses (out of 70) with a variance of 17.87
#over time the expected guessing rate would be 50% with 99.7% of values falling between 32.8% and 67.6% in terms of percentage of correct guesses

#The 7-fold Naive Bayes model had an accuracy of 65.71% which is near the top of the 99.7% range of guess accuracy in the simulation.


#prediction based on input using cvModelF6 - this is the Naive-Bayes model based on the First 6 kicks
#run winPredict (with no arguments) to try
#prompts the user 6 times - goal or no goal - and then provides the predicted winner
#the first, third and fifth entries apply to team A and the second, fourth and sixth entries apply to team B


testing <- data.frame(pos1=NA, pos2=NA, pos3=NA, pos4=NA, pos5=NA, pos6=NA)

winPredict <- function (x = testing) {
  for (i in 1:6){
    testing[1,i] <- readline(prompt = "Goal (1) or Miss (0)?  ")
    testing[,i] <- as.integer (testing [,i])
  }

  print(predict (cvModelF6, testing))
}

winPredict ()

