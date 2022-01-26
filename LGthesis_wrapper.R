# wrapper for Lisa Gistelinck's data


library(pracma)   #for tic, toc
library(Rmisc) # for summarySE
library(tidyverse)
source("C:/Users/elise.000/OneDrive/Documents/r_scripts/gorilla_scripts/HabitGorilla/ERDLThemesNSchemes.R")

# load in data ####
setwd("C:/Users/elise.000/Documents/AAA_projects/Lisa_data/")
D_ <- readxl::read_xlsx('Gorilla_exp_V23_2.2_Lisa.xlsx')

# tidy the dataset ####
# puzzle with the data to figure out what the optimal choice is
# and what the rewarded choice is.
D_ <- dplyr::rename(D_, Block = Block_Volgorde)
D_ %>% select(-Incorrect, -Gerandomiseerde_Block_Vogorde, -Optellende_Block_Volgorde)
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
  
  b$OptimalChoice <- ifelse(b$OptiResp > 20, 1, 0)
  b$Exception <- ifelse(b$FreqReward < 20, 1, 0)
  
  # add to the big D :p
  D <- rbind(D, b)
  print(sprintf("Block %d of %d", i, nrow(blocks)))
  rm(a, b, OptiResp, FreqCorrect, FreqReward, names)
}
rm(D_)
toc()

# # switch history per block and person ####
# # switch history will index for each block, how long it was been since the contingency flipped
OptiResp <- D %>% select(Subject, Day, StabilityContext, Block, AnswerID, FreqReward) %>% unique()
OptiResp$Optimal <- ifelse(OptiResp$FreqReward>20, 1,0)
OptiResp <- OptiResp %>% select(-FreqReward)
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
  } else {
    SwitchHistoryStable[i, 5:12] <- ifelse(is.na(ORStableWide[i,5:12]), NA, ifelse(ORStableWide[i,5:12]==ORStableWide[i-1,5:12], (SwitchHistoryStable[i-1,5:12]+1), 0))
  }
}

# SwitchHistory <- blocks
# SwitchHistory[, 5:12] <- NA
# #SwitchHistory <- D %>% select(Subject, Day, Block) %>% unique()
# names <- unique(D$AnswerID)
# s <- names[order(unique(D$AnswerID))]
# colnames(SwitchHistory) <- c("Subject", "Day", "Block", "Condition", s)
# 
# # some helper dataframes
# UsedStim <- data.frame(matrix(ncol=length(s), nrow=nrow(blocks)))
# OptiStim <- data.frame(matrix(ncol=length(s), nrow=nrow(blocks)))
# SwitchedStim <- data.frame(matrix(ncol=length(s), nrow=nrow(blocks)))
# #SwitchHistory <- tibble()
# 
# # values should be 0 if just changed
# # value should be 1 if it is changed, and last block it was different
# # value should be 2 if it is changed, and last 2 blocks was different, etc
# # value should be NA if it's the other context
# for (i in nrow(SwitchHistory)) {
#   b <- D %>% filter((Subject == blocks[[i,1]] & Day == blocks[[i,2]] & Block == blocks[[i, 3]])) %>%  select(AnswerID)
#   st <- unique(b$AnswerID)
#   # which stimuli are used this block
#   UsedStim[i, ] <- c(as.numeric(s %in% st))
#   # which stimuli are optimal
#   OptiStim <- ifelse(UsedStim[i, ]==0, NA, 1)
#   #OptiStim <- 
# }
#   

# QA: exclude RTs<150ms ####
D$RT <- D$ReactionTime
D$OptimalChoice[D$ReactionTime<150] <- NA
D$Response[D$ReactionTime<150] <- NA
D$ResponseID[D$ReactionTime<150] <- NA
D$Correct[D$ReactionTime<150] <- NA
D$RT[D$ReactionTime<150] <- NA
D <- dplyr::rename(D, RT_unfiltered = ReactionTime) # keep the old

# QA: too-fast & too bad responses; prop per block/person ####
# dp: how many (usable) datapoints per block
QA <- D %>% group_by(Subject, Day, Block, StabilityContext) %>% summarise(fastRT = mean(is.na(RT)), performance=mean(OptimalChoice, na.rm=TRUE), dp=sum(!is.na(OptimalChoice)))
QAOverBlock <- D %>% group_by(Subject, Day, StabilityContext) %>% summarise(fastRT = mean(is.na(RT)), performance=mean(OptimalChoice, na.rm=TRUE), dp=sum(!is.na(OptimalChoice)))

plot((1-QA$performance))
plot(QA$fastRT)
plot(QA$fastRT, QA$performance)
plot(QA$dp)

# look at patterns re: subjects, time (condition)
QA1 <- ggplot(filter(QA, ChancePerformers==0), aes(x=dp, y=performance)) + scale_y_continuous(limits = c(0, 1))+
  geom_point(aes(shape = StabilityContext, colour=as.factor(Subject), group=Day), stroke=1, size = 2, alpha=.5) +
  facet_grid(StabilityContext~Day) +
  theme_ERDL_simple()+ theme(legend.position = "none", legend.title = element_blank()) 
QA1

QA2 <- ggplot(filter(QAOverBlock, ChancePerformers==0), aes(x=fastRT, y=performance)) + scale_y_continuous(limits = c(0, 1))+
  geom_point(aes(shape = StabilityContext, colour=as.factor(Subject), group=Day), stroke=1, size = 2, alpha=.8) +
  facet_grid(StabilityContext~Day) +
  theme_ERDL_simple()+ theme(legend.position = "none", legend.title = element_blank()) 
QA2

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
AccDayBlock <- filter(D, ChancePerformers==0) %>% group_by(Subject, DayBlock, StabilityContext) %>% summarySE(measurevar = "OptimalChoice", groupvars = c("Subject", "DayBlock", "StabilityContext"), na.rm=TRUE)
RTDayBlock <- filter(D, OptimalChoice==1, ChancePerformers==0) %>% group_by(Subject, DayBlock, StabilityContext) %>% summarySE(measurevar = "ReactionTime", groupvars = c("Subject", "DayBlock", "StabilityContext"), na.rm=TRUE)
#acc_overtime <- summarySE(D, measurevar = "OptimalChoice", groupvars = c("Subject", "DayBlock", "StabilityContext"), na.rm=TRUE)

AccuracyLines <- ggplot(AccDayBlock, aes(x=DayBlock, y=OptimalChoice, Group=Subject)) + #scale_y_continuous(limits = c(0.05, 0.2))+
  geom_line(aes(group=Subject, colour=Subject), size = 0.1, alpha=.5) +
  #geom_errorbar(aes(ymin=OptimalChoice-se, ymax=OptimalChoice+se, group=StabilityContext, color=Subject), width=0, size =1) +
  #geom_point(aes(shape = Congruency, fill=Congruency, group=Congruency), stroke=1, position=position_dodge(.5), color = "Black", size = 4) +
  facet_grid(StabilityContext~.)+
  xlab("Overtraining") + ylab("Accuracy")+ggtitle("Accuracy over time")+
  theme_ERDL_simple()
AccuracyLines
