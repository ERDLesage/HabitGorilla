# wrapper for Lisa Gistelinck's data

rm(list = ls()) # clear wm 

library(pracma)   #for tic, toc
library(Rmisc) # for summarySE
library(tidyverse)
source("C:/Users/elise.000/OneDrive/Documents/r_scripts/gorilla_scripts/HabitGorilla/SageThemesNSchemes.R")
source("C:/Users/elise.000/OneDrive/Documents/r_scripts/gorilla_scripts/HabitGorilla/SageAnalysisUtils.R")

# load in data ####
D_ <- readxl::read_xlsx('C:/Users/elise.000/Documents/AAA_projects/Lisa_data/Gorilla_exp_V23_2.2_Lisa.xlsx')

# tidy the dataset ####
# puzzle with the data to figure out what the optimal choice is
# and what the rewarded choice is.
D_ <- dplyr::rename(D_, Block = Block_Volgorde)
D_ <- dplyr::rename(D_, RT = ReactionTime)
D_<- D_ %>% select(-Incorrect, -Gerandomiseerde_Block_Vogorde, -Optellende_Block_Volgorde)
blocks <- D_ %>% select(Subject, Day, Block, StabilityContext) %>% unique()


# run through blocks to add more useful variables
D <-tibble()
tic()
#for (s in 1:length(unique(D_$Subject)))
for (i in 1:nrow(blocks)){
  b <- D_ %>% filter((Subject == blocks[[i,1]] & Day == blocks[[i,2]] & Block == blocks[[i, 3]]))
  b$ResponseID <- NA
  b$AnswerID <- NA
  b$OptimalChoice <- NA
  b$ResponseID[(b$Response == "Left shovel")& !is.na(b$Response)] <- b$LeftShovel[(b$Response == "Left shovel")& !is.na(b$Response)]
  b$ResponseID[(b$Response == "Left dynamite")& !is.na(b$Response)] <- b$LeftDynamite[(b$Response == "Left dynamite")& !is.na(b$Response)]
  b$ResponseID[(b$Response == "Right shovel")& !is.na(b$Response)] <- b$RightShovel[(b$Response == "Right shovel")& !is.na(b$Response)]
  b$ResponseID[(b$Response == "Right dynamite"& !is.na(b$Response))] <- b$RightDynamite[(b$Response == "Right dynamite")& !is.na(b$Response)]
  
  b$AnswerID[(b$ANSWER == "Left shovel")] <- b$LeftShovel[(b$ANSWER == "Left shovel")]
  b$AnswerID[(b$ANSWER == "Left dynamite")] <- b$LeftDynamite[(b$ANSWER == "Left dynamite")]
  b$AnswerID[(b$ANSWER == "Right shovel")] <- b$RightShovel[(b$ANSWER == "Right shovel")]
  b$AnswerID[(b$ANSWER == "Right dynamite")] <- b$RightDynamite[(b$ANSWER == "Right dynamite")]
  
  # which are the best ones this block (most frequently correct)
  FreqReward <- summary(as.factor(b$AnswerID))
  names <- unique(b$AnswerID)
  a <- names[order(unique(b$AnswerID))]
  FreqCorrect <- tibble(FreqReward, a)
 

  # make a column that is 8 if the answer was not optimal, and 36 if it was
  b <- inner_join(b, FreqCorrect, by = c("ResponseID" = "a"))
  b <- dplyr::rename(b, OptiResp=FreqReward)
  # now add one just to indicate what's associated with the "correct"(rewarded on this trial) answer
  b <- inner_join(b, FreqCorrect, by = c("AnswerID" = "a"))
  b <- dplyr::rename(b, RewardedOnThisTrial=FreqReward)
  
  b$OptimalChoice <- ifelse(b$OptiResp > 20, 1, 0)
  b$Exception <- ifelse(b$RewardedOnThisTrial < 20, 1, 0)
  
  # this to exclude the weird 1-trial "blocks"
  # only add to data if it's more than 5 trials
  #if (sum(FreqReward, na.rm=TRUE)>5) {
    # add to the big D :p
    D <- rbind(D, b)
  #}
  
  
  print(sprintf("Block %d of %d", i, nrow(blocks)))
  rm(a, b, FreqCorrect, FreqReward, names)
}
rm(D_)
toc()

# QA: clean up weird blocks ####
# all the exact doubles can go
pre <- nrow(D)
D <- unique(D)
post <- nrow(D)
disp(sprintf("Got rid of exact replicas (doubles): %d trials, %0.2f pct of data.", pre-post, ((pre-post)/pre)*100))
rm(pre, post)
# get rid of very fast responses
D <- SageExcludeSimple(D, D$RT<150)
# the blocks with very few trials (less than 10) I think I can safely exclude
qa1 <- summarySE(D, measurevar = "OptimalChoice", groupvars = c("Subject", "Day", "Block","StabilityContext"))
targets <- qa1 %>% mutate(exclude=if_else(N<10, 1, 0)) 
D<- SageExcludeHO(D, targets, groupvars= c("Subject", "Day", "Block","StabilityContext"), remove_data = TRUE)

################# pfff still need to work on this I suppose...
qa1 <- summarySE(OptiResp, measurevar = "Optimal", groupvars = c("Subject", "Day", "Block","StabilityContext"))
plot(qa1$N)
qa1Anomalies <- filter(qa1, N!=4) # do this with the stability as grouper and without...

qa3 <- summarySE(D, measurevar = "OptimalChoice", groupvars = c("Subject", "Day", "Block","StabilityContext"))

qa3Anomalies <- filter(qa3, N>5)
# how many trials in a block
qa2toolittle <- filter(qa2, N<75)
plot(qa1$N, qa3$N)

# QA: too-fast & too bad responses; prop per block/person ####
# dp: how many (usable) datapoints per block
QA <- D %>% group_by(Subject, Day, Block, StabilityContext) %>% summarise(fastRT = mean(is.na(RT)), performance=mean(OptimalChoice, na.rm=TRUE), dp=sum(!is.na(OptimalChoice)))
QAOverBlock <- D %>% group_by(Subject, Day, StabilityContext) %>% summarise(fastRT = mean(is.na(RT)), performance=mean(OptimalChoice, na.rm=TRUE), dp=sum(!is.na(OptimalChoice)))

plot((1-QA$performance))
plot(QA$fastRT)
plot(QA$fastRT, QA$performance)
plot(QA$dp)

# switch history per block and person ####
# switch history will index for each block, how long it was been since the contingency flipped
OptiResp <- D %>% select(Subject, Day, StabilityContext, Block, AnswerID, RewardedOnThisTrial) %>% unique()
OptiResp$Optimal <- ifelse(OptiResp$RewardedOnThisTrial>20, 1,0)

# separate the conditions so that we can compare like with like
ORStable <- filter(OptiResp, StabilityContext=="StabielC")
ORStable<- arrange(ORStable, Subject, Day, Block, AnswerID) # make sure they have the same order
ORStableWide <- pivot_wider(ORStable, names_from = "AnswerID", values_from = "Optimal")
ORVolatile <- filter(OptiResp, StabilityContext=="OnstabielC")
ORVolatile<- arrange(ORVolatile, Subject, Day, Block, AnswerID) # make sure they have the same order
ORVolatileWide <- pivot_wider(ORVolatile, names_from = "AnswerID", values_from = "Optimal")

# initialise the dataframe same, with the very first 
SwitchHistoryVolatile <- tibble()
SwitchHistoryVolatile <- ORVolatileWide[,1:4]
SwitchHistoryStable <- tibble()
SwitchHistoryStable <- ORStableWide[,1:4]

for (i in 1:nrow(ORVolatileWide)) {
  # if it's a new person, everything is new
  if (i==1 || (ORVolatileWide$Subject[i]!=ORVolatileWide$Subject[i-1])) {
    SwitchHistoryVolatile[i, 5:12] <- ifelse(is.na(ORVolatileWide[i,5:12]), NA, 0)
  } else {
    SwitchHistoryVolatile[i, 5:12] <- ifelse(is.na(ORVolatileWide[i,5:12]), NA, ifelse(ORVolatileWide[i,5:12]==ORVolatileWide[i-1,5:12], (SwitchHistoryVolatile[i-1,5:12]+1), 0))
  }
}
for (i in 1:nrow(ORStableWide)) {
  # if it's a new person, everything is new
  if (i==1 || (ORStableWide$Subject[i]!=ORStableWide$Subject[i-1])) {
    SwitchHistoryStable[i, 5:12] <- ifelse(is.na(ORStableWide[i,5:12]), NA, 0)
  #} # if we switch from one stimulus-set to another, make sure we don't inherit the NA's
  #else if (sum(is.na(ORStableWide[i-1,5:12])*(!is.na(ORStableWide[i,5:12])))==4) {
  #  SwitchHistoryStable[i, 5:12] <- ifelse(is.na(ORStableWide[i,5:12]), NA, 0)
  } else {
    SwitchHistoryStable[i, 5:12] <- ifelse(is.na(ORStableWide[i,5:12]), NA, ifelse(ORStableWide[i,5:12]==ORStableWide[i-1,5:12], (SwitchHistoryStable[i-1,5:12]+1), 0))
  }
}

#rm(OptiResp, ORStable, ORVolatile, ORStableWide, ORVolatileWide)
# how many overtrained blocks

# look at patterns re: subjects, time (condition)
QA1 <- ggplot(QA, aes(x=dp, y=performance)) + scale_y_continuous(limits = c(0, 1))+
  geom_point(aes(shape = StabilityContext, colour=as.factor(Subject), group=Day), stroke=1, size = 2, alpha=.5) +
  facet_grid(StabilityContext~Day) +
  theme_ERDL_simple()+ theme(legend.position = "none", legend.title = element_blank()) 
QA1

# Identify blocks with very very low performance (<10%) ####
hist(QA$performance)
QA$ZeroBlock <- ifelse(QA$performance<.10, 1, 0)
ZeroBlock <- filter(QA, ZeroBlock == 1)  %>% select(Subject, Day, Block)

# Identify ppts who perform at chance level on day 3 ####
QADay3 <-filter(QAOverBlock, Day==3)
QADay2 <-filter(QAOverBlock, Day==2)
hist(QADay2$performance)
hist(QADay3$performance)
QADay3$Under67 <- ifelse(QADay3$performance<.67, 1, 0)

ChanceD3 <- filter(QADay3, Under67 == 1)  %>% select(Subject)
ChanceD3 <- unique(ChanceD3)
D$ChanceD3 <- ifelse(is.element(D$Subject, as.vector(as.matrix(ChanceD3))), 1, 0)
QA$ChanceD3 <- ifelse(is.element(QA$Subject, as.vector(as.matrix(ChanceD3))), 1, 0)
QAOverBlock$ChanceD3 <- ifelse(is.element(QAOverBlock$Subject, as.vector(as.matrix(ChanceD3))), 1, 0)

# General learning over time
D$BlockStr <- sprintf("%02d", D$Block)
D<- D %>% unite(DayBlock, Day, BlockStr, sep=".", remove = FALSE)
D$Subject <- as.factor(D$Subject)
AccDayBlock <- D %>% group_by(Subject, DayBlock, StabilityContext) %>% summarySE(measurevar = "OptimalChoice", groupvars = c("Subject", "DayBlock", "StabilityContext"), na.rm=TRUE)
RTDayBlock <- filter(D, OptimalChoice==1) %>% group_by(Subject, DayBlock, StabilityContext) %>% summarySE(measurevar = "RT", groupvars = c("Subject", "DayBlock", "StabilityContext"), na.rm=TRUE)
#acc_overtime <- summarySE(D, measurevar = "OptimalChoice", groupvars = c("Subject", "DayBlock", "StabilityContext"), na.rm=TRUE)

AccuracyLines <- ggplot(AccDayBlock, aes(x=DayBlock, y=OptimalChoice, Group=Subject)) + #scale_y_continuous(limits = c(0.05, 0.2))+
  geom_line(aes(group=Subject, colour=Subject), size = 0.1, alpha=.5) +
  #geom_errorbar(aes(ymin=OptimalChoice-se, ymax=OptimalChoice+se, group=StabilityContext, color=Subject), width=0, size =1) +
  #geom_point(aes(shape = Congruency, fill=Congruency, group=Congruency), stroke=1, position=position_dodge(.5), color = "Black", size = 4) +
  facet_grid(StabilityContext~.)+
  xlab("Overtraining") + ylab("Accuracy")+ggtitle("Accuracy over time")+
  theme_ERDL_simple()
AccuracyLines
