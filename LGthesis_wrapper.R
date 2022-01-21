# wrapper for Lisa Gistelinck's data

library(tidyverse)



# load in data ####
setwd("C:/Users/elise.000/Documents/AAA_projects/Lisa_data/")
D <- readxl::read_xlsx('Gorilla_exp_V23_2.2_Lisa.xlsx')

# tidy the dataset ####
# puzzle with the data to figure out what the optimal choice is
# and what the rewarded choice is.
D$Block <- D$Block_Volgorde
blocks <- D %>% select(Subject, Day, Block) %>% unique()

# run through blocks to add more useful variables
for (i in 1:length(blocks)){
  b <- D %>% filter((Subject == blocks[[i,1]] & Day == blocks[[i,2]] & Block == blocks[[i, 3]]))
  b$ResponseID <- NA
  b$AnswerID <- NA
  b$OptimalChoice <- NA
  b$ResponseID[(b$Response == "Left shovel")] <- b$LeftShovel[(b$Response == "Left shovel")]
  b$ResponseID[(b$Response == "Left dynamite")] <- b$LeftDynamite[(b$Response == "Left dynamite")]
  b$ResponseID[(b$Response == "Right shovel")] <- b$RightShovel[(b$Response == "Right shovel")]
  b$ResponseID[(b$Response == "Right dynamite")] <- b$RightDynamite[(b$Response == "Right dynamite")]
  
  b$AnswerID[(b$ANSWER == "Left shovel")] <- b$LeftShovel[(b$ANSWER == "Left shovel")]
  b$AnswerID[(b$ANSWER == "Left dynamite")] <- b$LeftDynamite[(b$ANSWER == "Left dynamite")]
  b$AnswerID[(b$ANSWER == "Right shovel")] <- b$RightShovel[(b$ANSWER == "Right shovel")]
  b$AnswerID[(b$ANSWER == "Right dynamite")] <- b$RightDynamite[(b$ANSWER == "Right dynamite")]
  
  # which are the best ones this block (most frequently correct)
  OptiResp <- summary(as.factor(b$AnswerID))
  names <- unique(b$AnswerID)
  a <- names[order(unique(b$AnswerID))]
  FreqCorrect <- tibble(OptiResp, a)
  rm(a, OptiResp, names)
  # make a column that is 8 if the answer was not optimal, and 36 if it was
  b <- inner_join(b, FreqCorrect, by = c("ResponseID" = "a"))
  b$OptimalChoice <- ifelse(b$OptiResp > 20, 1, 0)
  
}