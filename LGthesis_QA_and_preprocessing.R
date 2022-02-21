# wrapper for Lisa Gistelinck's data

rm(list = ls()) # clear wm 

library(pracma)   #for tic, toc
library(Rmisc) # for summarySE
library(tidyverse)
library(ggvenn)
source("C:/Users/elise.000/OneDrive/Documents/r_scripts/gorilla_scripts/HabitGorilla/SageThemesNSchemes.R")
source("C:/Users/elise.000/OneDrive/Documents/r_scripts/gorilla_scripts/HabitGorilla/SageDataCleanUtils.R")

# load in data ####
#D_ <- readxl::read_xlsx('C:/Users/elise.000/Documents/AAA_projects/Lisa_data/Gorilla_exp_V23_2.2_Lisa.xlsx')
D_ <- readxl::read_xlsx('C:/Users/elise.000/Documents/AAA_projects/Lisa_data/LG_edit2.xlsx')

# Calculate Block from scratch ####
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
rm(D_)
# sanity check: how many trials per block, per day, per person
s <- A %>% group_by(Subject, Day, Block, StabilityContext) %>% summarise(num=n())
plot(s$num)
s$weirdblocks <- ifelse((s$num<85|s$num>90), 1, 0)
A <-SageExcludeHO(A, s, groupvars=c("Subject", "Day", "Block", "StabilityContext"), remove_data = FALSE, var_name = "fubar")

# Create useful variables ####
# puzzle with the data to figure out what the optimal choice is
# and what the rewarded choice is.
blocks <- A %>% select(Subject, Day, Block, StabilityContext) %>% unique()

# run through blocks to add more useful variables
# it's pretty clunky, but it works
D <-tibble()
tic()
for (i in 1:nrow(blocks)){
  b <- A %>% filter((Subject == blocks[[i,1]] & Day == blocks[[i,2]] & Block == blocks[[i, 3]]))
  b$ResponseID <- NA
  b$AnswerID <- NA
  b$OptimalChoice <- NA
  b$StimPair <-NA
  
  b$StimPair[(b$LeftShovel == "wit@4x.png" | b$RightShovel == "wit@4x.png")] <- 1
  b$StimPair[(b$LeftShovel == "roze@4x.png" | b$RightShovel == "roze@4x.png")] <- 2
  b$StimPair[(b$LeftDynamite == "GROEN@4x.png") | (b$RightDynamite == "GROEN@4x.png")] <- 3
  b$StimPair[(b$LeftDynamite == "GEEL@4x.png" | b$RightDynamite == "GEEL@4x.png")] <- 4
  
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
  
  ## To compute the lag, split according to stimpair, make the lab, then rearrange
  b_oddsp <- filter(b, StimPair%%2==1)
  b_evensp <- filter(b, StimPair%%2==0)
  b_oddsp$ExceptionLag <- lag(b_oddsp$Exception)
  b_evensp$ExceptionLag <- lag(b_evensp$Exception)
  b <- rbind(b_oddsp, b_evensp)
  b <- arrange(b, TrialNumber)
  
  D <- rbind(D, b)
  print(sprintf("Block %d of %d", i, nrow(blocks)))
  rm(a, b, FreqCorrect, FreqReward, names, b_oddsp, b_evensp)
}
rm(A)
time <-toc()
disp(sprintf("That took %0.1f minutes.", time/60))
rm(time)

# Basic QA ####
# all the exact doubles can go
pre <- nrow(D)
D <- distinct(D)
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

# detect suspicious behavior
SlackerBlocks <- SageSlackerCatch(D, "Response", groupvars = c("Subject", "Day", "Block"), cutoff = 25)
D <- SageExcludeHO(D, SlackerBlocks, groupvars = c("Subject", "Day", "Block"), remove_data = FALSE, var_name = "Slackerblock")
badapplesPerDay <- summarySE(D, measurevar = "Slackerblock", groupvars = c("Subject", "Day"))
plot(badapplesPerDay$Slackerblock)
badapples <- summarySE(D, measurevar = "Slackerblock", groupvars = c("Subject"))
plot(badapples$Slackerblock)

# if it's over 30% of the data, get rid of the entire person
badapples <- badapples %>% mutate(exclude = Slackerblock >.3)
if(sum(badapples$exclude)>0){
  D<-SageExcludeHO(D, badapples, groupvars = "Subject")
}

# Create Contingency history variables ####
OptiResp <- D %>% select(Subject, Day, StabilityContext, Block, RandBlock, StimPair, AnswerID, RewardedOnThisTrial) %>% unique()
OptiResp$Optimal <- ifelse(OptiResp$RewardedOnThisTrial>20, 1,0)

# separate the conditions so that we can compare like with like
ORStable <- filter(OptiResp, StabilityContext=="StabielC")
ORStable<- arrange(ORStable, Subject, Day, Block, AnswerID) %>% select(-RewardedOnThisTrial, -RandBlock, -StimPair) # make sure they have the same order
ORStableWide <- pivot_wider(ORStable, names_from = "AnswerID", values_from = "Optimal")
ORVolatile <- filter(OptiResp, StabilityContext=="OnstabielC")
ORVolatile<- arrange(ORVolatile, Subject, Day, Block, AnswerID) %>% select(-RewardedOnThisTrial, -RandBlock, -StimPair) # make sure they have the same order
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
  } # if we switch from one stimulus-set to another, make sure we don't inherit the NA's
  else if ((ORVolatileWide$Day[i]!=ORVolatileWide$Day[i-1]) && sum(is.na(ORVolatileWide[i-1,5:12]+ ORVolatileWide[i,5:12]))==8) {
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
  } # if we switch from one stimulus-set to another, make sure we don't inherit the NA's
  else if ((ORStableWide$Day[i]!=ORStableWide$Day[i-1]) && sum(is.na(ORStableWide[i-1,5:12]+ ORStableWide[i,5:12]))==8) {
    SwitchHistoryStable[i, 5:12] <- ifelse(is.na(ORStableWide[i,5:12]), NA, 0)
  } else {
    SwitchHistoryStable[i, 5:12] <- ifelse(is.na(ORStableWide[i,5:12]), NA, ifelse(ORStableWide[i,5:12]==ORStableWide[i-1,5:12], (SwitchHistoryStable[i-1,5:12]+1), 0))
  }
}
colnames(SwitchHistoryStable)<- colnames(ORStableWide)

# Now to integrate the switch history into D
# Make them long again, and attach them to each other
SwitchHistory <- rbind(SwitchHistoryStable, SwitchHistoryVolatile)
SwitchHistoryLong <- pivot_longer(SwitchHistory, cols = ends_with("4x.png"), names_to = "AnswerID", values_to = "SwitchHistory")
# joining the information w D
D <- inner_join(D, SwitchHistoryLong, by = c("Subject", "Day", "Block", "StabilityContext", "AnswerID"))
rm(OptiResp, ORStable, ORVolatile, ORStableWide, ORVolatileWide, SwitchHistoryStable, SwitchHistoryVolatile)

# Make DayBlock variable and make Subject a String
D$BlockStr <- sprintf("%02d", D$Block)
D<- D %>% unite(DayBlock, Day, BlockStr, sep=".", remove = FALSE)
D$Subject <- sprintf("S%02d", as.numeric(D$Subject))

#DF <- filter(D, fubar==0, Slackerblock==0)
AccDayBlock <- D %>% group_by(Subject, DayBlock, StabilityContext) %>% summarySE(measurevar = "OptimalChoice", groupvars = c("Subject", "DayBlock", "StabilityContext"), na.rm=TRUE)
RTDayBlock <- filter(D, OptimalChoice==1) %>% group_by(Subject, DayBlock, StabilityContext) %>% summarySE(measurevar = "RT", groupvars = c("Subject", "DayBlock", "StabilityContext"), na.rm=TRUE)

AccuracyLines <- ggplot(AccDayBlock, aes(x=DayBlock, y=OptimalChoice, Group=Subject)) + #scale_y_continuous(limits = c(0.05, 0.2))+
  geom_line(aes(group=Subject, colour=Subject), size = 0.1, alpha=.5) +
  facet_grid(StabilityContext~.)+
  xlab("Overtraining") + ylab("Accuracy")+ggtitle("Accuracy over time")+
  theme_sage_simple()
AccuracyLines

RTLines <- ggplot(RTDayBlock, aes(x=DayBlock, y=RT, Group=Subject)) + #scale_y_continuous(limits = c(0.05, 0.2))+
  geom_line(aes(group=Subject, colour=Subject), size = 0.1, alpha=.5) +
  #geom_errorbar(aes(ymin=OptimalChoice-se, ymax=OptimalChoice+se, group=StabilityContext, color=Subject), width=0, size =1) +
  #geom_point(aes(shape = Congruency, fill=Congruency, group=Congruency), stroke=1, position=position_dodge(.5), color = "Black", size = 4) +
  facet_grid(StabilityContext~.)+
  xlab("Overtraining") + ylab("RT")+ggtitle("RT over time")+
  theme_sage_simple()
RTLines

# Identify ppts who perform around chance level on day 3 ####
AccRaw <- D %>% filter(fubar==0, Slackerblock==0) %>% group_by(Subject, Day, StabilityContext) %>% summarySE(measurevar = "OptimalChoice", groupvars = c("Subject", "Day", "StabilityContext"), na.rm=TRUE)
AccRawBlock <- D %>% group_by(Subject, Day, Block, StabilityContext) %>% summarySE(measurevar = "OptimalChoice", groupvars = c("Subject", "Day", "Block", "StabilityContext"), na.rm=TRUE)
# histogram per Day (introduce a cutoff at .75)
PerfHistDay <- ggplot(AccRaw, aes(x=OptimalChoice)) + 
  geom_histogram(aes(fill=as.factor(Day)), binwidth = 0.02) +
  geom_vline(xintercept = .75)+
  facet_grid(.~Day) +
  theme_sage_simple()
PerfHistDay

PerfHist <- ggplot(AccRawBlock, aes(x=OptimalChoice)) + 
  geom_histogram(aes(fill=as.factor(Day)), binwidth = 0.05) +
  geom_vline(xintercept = .66)+
  facet_grid(.~Day) +
  theme_sage_simple()
PerfHist

AccRaw <- AccRaw %>% mutate(DayUnder75 = ifelse(Day>1&OptimalChoice<.75, 1, 0))
AccRawBlock <- AccRawBlock %>% mutate(BlockUnder66 = ifelse(Day>1&OptimalChoice<.66, 1, 0))
D <- SageExcludeHO(D, AccRaw, groupvars = c("Subject", "Day", "StabilityContext"), remove_data = FALSE, var_name = "DayUnder75")
D <- SageExcludeHO(D, AccRawBlock, groupvars = c("Subject", "Day", "Block", "StabilityContext"), remove_data = FALSE, var_name = "BlockUnder66")

D$DayUnder75 <- as.logical(D$DayUnder75)
D$BlockUnder66 <- as.logical(D$BlockUnder66)
ggvenn(D, columns = c("DayUnder75", "BlockUnder66"),stroke_size = 0.5)

# write a csv with the cleaned data
setwd("C:/Users/elise.000/Documents/AAA_projects/Lisa_data/")
write.table(D, "LG_overtraining.txt", sep="\t", quote  =FALSE, row.names=FALSE)

