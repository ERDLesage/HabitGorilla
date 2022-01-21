# wrapper for Lisa Gistelinck's data

library(tidyverse)


# load in data ####
setwd("C:/Users/elise.000/Documents/AAA_projects/Lisa_data/")
D_ <- readxl::read_xlsx('Gorilla_exp_V23_2.2_Lisa.xlsx')

# tidy the dataset ####
# puzzle with the data to figure out what the optimal choice is
# and what the rewarded choice is.
D_$Block <- D_$Block_Volgorde
D_ %>% select(-Incorrect, -Gerandomiseerde_Block_Volgorde, -Optellende_Block_Volgorde)
blocks <- D_ %>% select(Subject, Day, Block) %>% unique()


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

# QA: exclude RTs<150ms ####
D$RT <- D$ReactionTime
D$OptimalChoice[D$ReactionTime<150] <- NA
D$Response[D$ReactionTime<150] <- NA
D$ResponseID[D$ReactionTime<150] <- NA
D$Correct[D$ReactionTime<150] <- NA
D$RT[D$ReactionTime<150] <- NA
D <- rename(D, RT_unfiltered = ReactionTime) # keep the old

# QA: too-fast & too bad responses; prop per block/person ####
QA <- D %>% group_by(Subject, Day, Block) %>% summarise(fastRT = mean(is.na(RT)), performance=mean(OptimalChoice, na.rm=TRUE))

plot((1-QA$performance))
plot(QA$fastRT)
plot(QA$performance, QA$fastRT)

