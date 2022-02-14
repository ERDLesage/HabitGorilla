# wrapper for Lisa Gistelinck's data

rm(list = ls()) # clear wm 

library(pracma)   #for tic, toc
library(Rmisc) # for summarySE
library(tidyverse)
source("C:/Users/elise.000/OneDrive/Documents/r_scripts/gorilla_scripts/HabitGorilla/SageThemesNSchemes.R")
source("C:/Users/elise.000/OneDrive/Documents/r_scripts/gorilla_scripts/HabitGorilla/SageAnalysisUtils.R")

# load in data ####
#D_ <- readxl::read_xlsx('C:/Users/elise.000/Documents/AAA_projects/Lisa_data/Gorilla_exp_V23_2.2_Lisa.xlsx')
D_ <- readxl::read_xlsx('C:/Users/elise.000/Documents/AAA_projects/Lisa_data/LG_edit2.xlsx')

# calculate Block from scratch ####
D_ <- select(D_, -Block_Volgorde, -Optellende_Block_Volgorde)
D_ <- dplyr::rename(D_, RandBlock=Gerandomiseerde_Block_Vogorde)
D_ <- dplyr::rename(D_, RT=ReactionTime)

A <- tibble()
for (n in 1:length(unique(D_$Subject))){
  disp(sprintf("Starting subject %d", unique(D_$Subject)[n]))
  a <- filter(D_, Subject == unique(D_$Subject)[n])
  # make a little "lookup table"
  l <- data.frame(unique(cbind(a$Day, a$RandBlock, a$StabilityContext)))
  l$Block <- 1:nrow(l)
  colnames(l) <- c("Day", "RandBlock","StabilityContext", "Block")
  l$Day <- as.numeric(l$Day)
  l$RandBlock <- as.numeric(l$RandBlock)
  a <- inner_join(a, l, by = c("Day", "RandBlock","StabilityContext"))
  A <- rbind(A, a)
  rm(a, l)
}
#rm(D_)
# sanity check: how many trials per block, per day, per person
s <- A %>% group_by(Subject, Day, Block, StabilityContext) %>% summarise(num=n())
plot(s$num)
s$weirdblocks <- ifelse((s$num<85|s$num>90), 1, 0)
A <- SageExcludeHO(A, s, groupvars=c("Subject", "Day", "Block", "StabilityContext"), remove_data = FALSE, var_name = "fubar")

# Go through blocks ####
# puzzle with the data to figure out what the optimal choice is
# and what the rewarded choice is.
blocks <- A %>% select(Subject, Day, Block, StabilityContext) %>% unique()

# run through blocks to add more useful variables
D <-tibble()
tic()
nBlockRedFlags <- 0
#for (s in 1:length(unique(D_$Subject)))
for (i in 1:nrow(blocks)){
  b <- A %>% filter((Subject == blocks[[i,1]] & Day == blocks[[i,2]] & Block == blocks[[i, 3]]))
  b$ResponseID <- NA
  b$AnswerID <- NA
  b$OptimalChoice <- NA
  b$BlockRedFlag <- NA
  # b$BlockRedFlag <-ifelse(length(unique(b$RandBlock))>1, length(unique(b$RandBlock)), 0)
  # if (length(unique(b$RandBlock))>1) {
  # nBlockRedFlags <- nBlockRedFlags+1
  # disp(sprintf("Already %d Blocks red flagged.", nBlockRedFlags))
  # }
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
  
  D <- rbind(D, b)
  print(sprintf("Block %d of %d", i, nrow(blocks)))
  rm(a, b, FreqCorrect, FreqReward, names)
}
#rm(D_)
time <-toc()
disp(sprintf("That took %0.1f minutes.", time/60))
rm(time)

# QA: clean up weird blocks ####
# all the exact doubles can go
pre <- nrow(D)
D <- unique(D)
post <- nrow(D)
disp(sprintf("Got rid of exact replicas (doubles): %d trials, %0.2f pct of data.", pre-post, ((pre-post)/pre)*100))
rm(pre, post)
# get rid of very fast responses
D <- SageExcludeSimple(D, D$RT<150)
# the blocks with very few trials (fewer than 10) I think I can safely exclude
qa1 <- summarySE(D, measurevar = "OptimalChoice", groupvars = c("Subject", "Day", "Block","StabilityContext"))
targets <- qa1 %>% mutate(exclude=if_else(N<10, 1, 0)) 
D<- SageExcludeHO(D, targets, groupvars= c("Subject", "Day", "Block","StabilityContext"), remove_data = TRUE)
rm(qa1, targets)

# make the lookup table for the randblocks
OptiResp <- D %>% select(Subject, Day, StabilityContext, Block, RandBlock, AnswerID, RewardedOnThisTrial) %>% unique()
OptiResp$Optimal <- ifelse(OptiResp$RewardedOnThisTrial>20, 1,0)

#detect suspicious behavior
g<- SageSlackerCatch(D, Response, groupvar = c("Subject", "Day", "Block"))

qa1 <- summarySE(OptiResp, measurevar = "Optimal", groupvars = c("Subject", "Day", "Block","StabilityContext"))
plot(qa1$N)
qa1Anomalies <- filter(qa1, N!=4) # do this with the stability as grouper and without...
qa2 <- summarySE(OptiResp, measurevar = "Optimal", groupvars = c("Subject", "Day", "Block"))
plot(qa2$N)
qa2Anomalies <- filter(qa2, N!=4) # do this with the stability as grouper and without...

# Make a flag called "BlockFuckup" for this data
targets <- qa2 %>% mutate(danger=if_else(N!=4, 1, 0)) 
D<- SageExcludeHO(D, targets, groupvars= c("Subject", "Day", "Block"), remove_data = FALSE, var_name = "BlockFuckup")

DFU <- filter(D, BlockFuckup==1)
qa3_b <- summarySE(DFU, measurevar = "OptimalChoice", groupvars = c("Subject", "Day", "Block","StabilityContext"))
plot(qa3_b$N)
qa3_rb <- summarySE(DFU, measurevar = "OptimalChoice", groupvars = c("Subject", "Day", "Gerandomiseerde_Block_Vogorde","StabilityContext"))
plot(qa3_rb$N)

a <- DFU %>% filter(Subject==46, Day==2) %>% arrange(TrialNumber)

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

# separate the conditions so that we can compare like with like
ORStable <- filter(OptiResp, StabilityContext=="StabielC")
ORStable<- arrange(ORStable, Subject, Day, Block, AnswerID) %>% select(-RewardedOnThisTrial) # make sure they have the same order
ORStableWide <- pivot_wider(ORStable, names_from = "AnswerID", values_from = "Optimal")
ORVolatile <- filter(OptiResp, StabilityContext=="OnstabielC")
ORVolatile<- arrange(ORVolatile, Subject, Day, Block, AnswerID) %>% select(-RewardedOnThisTrial) # make sure they have the same order
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
colnames(SwitchHistoryVolatile)<- colnames(ORVolatileWide)
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
colnames(SwitchHistoryStable)<- colnames(ORStableWide)

#rm(OptiResp, ORStable, ORVolatile, ORStableWide, ORVolatileWide)

# Quick and dirty using filters ####
DF <- filter(D, BlockRedFlag==0, BlockFuckup==0)
# General learning over time
DF$BlockStr <- sprintf("%02d", DF$Block)
DF<- DF %>% unite(DayBlock, Day, BlockStr, sep=".", remove = FALSE)
DF$Subject <- as.factor(DF$Subject)
AccDayBlock <- DF %>% group_by(Subject, DayBlock, StabilityContext) %>% summarySE(measurevar = "OptimalChoice", groupvars = c("Subject", "DayBlock", "StabilityContext"), na.rm=TRUE)
RTDayBlock <- filter(DF, OptimalChoice==1) %>% group_by(Subject, DayBlock, StabilityContext) %>% summarySE(measurevar = "RT", groupvars = c("Subject", "DayBlock", "StabilityContext"), na.rm=TRUE)
#acc_overtime <- summarySE(D, measurevar = "OptimalChoice", groupvars = c("Subject", "DayBlock", "StabilityContext"), na.rm=TRUE)

AccuracyLines <- ggplot(AccDayBlock, aes(x=DayBlock, y=OptimalChoice, Group=Subject)) + #scale_y_continuous(limits = c(0.05, 0.2))+
  geom_line(aes(group=Subject, colour=Subject), size = 0.1, alpha=.5) +
  #geom_errorbar(aes(ymin=OptimalChoice-se, ymax=OptimalChoice+se, group=StabilityContext, color=Subject), width=0, size =1) +
  #geom_point(aes(shape = Congruency, fill=Congruency, group=Congruency), stroke=1, position=position_dodge(.5), color = "Black", size = 4) +
  facet_grid(StabilityContext~.)+
  xlab("Overtraining") + ylab("Accuracy")+ggtitle("Accuracy over time")+
  theme_sage_simple()
AccuracyLines

RTLines <- ggplot(RTDayBlock, aes(x=DayBlock, y=RT, Group=Subject)) + #scale_y_continuous(limits = c(0.05, 0.2))+
  geom_line(aes(group=Subject, colour=Subject), size = 0.1, alpha=.5) +
  #geom_errorbar(aes(ymin=OptimalChoice-se, ymax=OptimalChoice+se, group=StabilityContext, color=Subject), width=0, size =1) +
  #geom_point(aes(shape = Congruency, fill=Congruency, group=Congruency), stroke=1, position=position_dodge(.5), color = "Black", size = 4) +
  facet_grid(StabilityContext~.)+
  xlab("Overtraining") + ylab("Accuracy")+ggtitle("Accuracy over time")+
  theme_sage_simple()
RTLines

# Identify ppts who perform at chance level on day 3 ####
AccGrp <- DF %>% group_by(Subject, Day, StabilityContext) %>% summarySE(measurevar = "OptimalChoice", groupvars = c("Subject", "Day", "StabilityContext"), na.rm=TRUE)
QADay3 <-filter(AccGrp, Day==3)
QADay2 <-filter(AccGrp, Day==2)
hist(QADay3$OptimalChoice[QADay3$StabilityContext =="StabielC"])
hist(AccDayBlock$performance)
QADay3$Under67 <- ifelse(QADay3$performance<.67, 1, 0)


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


ChanceD3 <- filter(QADay3, Under67 == 1)  %>% select(Subject)
ChanceD3 <- unique(ChanceD3)
D$ChanceD3 <- ifelse(is.element(D$Subject, as.vector(as.matrix(ChanceD3))), 1, 0)
QA$ChanceD3 <- ifelse(is.element(QA$Subject, as.vector(as.matrix(ChanceD3))), 1, 0)
QAOverBlock$ChanceD3 <- ifelse(is.element(QAOverBlock$Subject, as.vector(as.matrix(ChanceD3))), 1, 0)

