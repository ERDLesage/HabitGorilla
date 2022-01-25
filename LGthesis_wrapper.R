# wrapper for Lisa Gistelinck's data

library(tidyverse)
library(pracma)   #for tic, toc

# load in data ####
setwd("C:/Users/elise.000/Documents/AAA_projects/Lisa_data/")
D_ <- readxl::read_xlsx('Gorilla_exp_V23_2.2_Lisa.xlsx')

# tidy the dataset ####
# puzzle with the data to figure out what the optimal choice is
# and what the rewarded choice is.
D_$Block <- D_$Block_Volgorde
D_ %>% select(-Incorrect, -Gerandomiseerde_Block_Vogorde, -Optellende_Block_Volgorde)
blocks <- D_ %>% select(Subject, Day, Block, StabilityContext) %>% unique()


# run through blocks to add more useful variables
tic()
D <-tibble()

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
  OptiResp <- summary(as.factor(b$AnswerID))
  names <- unique(b$AnswerID)
  a <- names[order(unique(b$AnswerID))]
  FreqCorrect <- tibble(OptiResp, a)

  # make a column that is 8 if the answer was not optimal, and 36 if it was
  b <- inner_join(b, FreqCorrect, by = c("ResponseID" = "a"))
  b$OptimalChoice <- ifelse(b$OptiResp > 20, 1, 0)
  
  # add to the big D :p
  D <- rbind(D, b)
  print(sprintf("Block %d of %d", i, nrow(blocks)))
  rm(a, b, OptiResp, FreqCorrect, names)
}
rm(D_)
toc()

# switch history per block and person ####
# switch history will index for each block, how long it was been since the contingency flipped
SwitchHistory <- blocks
SwitchHistory[, 5:12] <- NA
#SwitchHistory <- D %>% select(Subject, Day, Block) %>% unique()
names <- unique(D$AnswerID)
s <- names[order(unique(D$AnswerID))]
colnames(SwitchHistory) <- c("Subject", "Day", "Block", "Condition", s)

# some helper dataframes
UsedStim <- data.frame(matrix(ncol=length(s), nrow=nrow(blocks)))
OptiStim <- data.frame(matrix(ncol=length(s), nrow=nrow(blocks)))
SwitchedStim <- data.frame(matrix(ncol=length(s), nrow=nrow(blocks)))
#SwitchHistory <- tibble()

# values should be 0 if just changed
# value should be 1 if it is changed, and last block it was different
# value should be 2 if it is changed, and last 2 blocks was different, etc
# value should be NA if it's the other context
for (i in nrow(SwitchHistory)) {
  b <- D %>% filter((Subject == blocks[[i,1]] & Day == blocks[[i,2]] & Block == blocks[[i, 3]])) %>%  select(AnswerID)
  st <- unique(b$AnswerID)
  # which stimuli are used this block
  UsedStim[i, ] <- c(as.numeric(s %in% st))
  # which stimuli are optimal
  OptiStim <- ifelse(UsedStim[i, ]
}
  



# QA: exclude RTs<150ms ####
D$RT <- D$ReactionTime
D$OptimalChoice[D$ReactionTime<150] <- NA
D$Response[D$ReactionTime<150] <- NA
D$ResponseID[D$ReactionTime<150] <- NA
D$Correct[D$ReactionTime<150] <- NA
D$RT[D$ReactionTime<150] <- NA
D <- rename(D, RT_unfiltered = ReactionTime) # keep the old

# QA: too-fast & too bad responses; prop per block/person ####
# dp: how many (usable) datapoints per block
QA <- D %>% group_by(Subject, Day, Block, StabilityContext) %>% summarise(fastRT = mean(is.na(RT)), performance=mean(OptimalChoice, na.rm=TRUE), dp=sum(!is.na(OptimalChoice)))

plot((1-QA$performance))
plot(QA$fastRT)
plot(QA$fastRT, QA$performance)
plot(QA$dp)

# look at patterns re: subjects, time (condition)


