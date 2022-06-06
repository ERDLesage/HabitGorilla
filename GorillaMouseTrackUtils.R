# Gorilla Mouse Tracking Utils
# 2022, by the Sage (elise.r.d.lesage@gmail.com)

# utils to process mouse tracking data from Gorilla.sc

# libraries
library(mousetrap)
library(readxl)
library(data.table)
library(dplyr)
library(ggplot2)

# 1.reading in all the mouse traces
# code mostly from https://support.gorilla.sc/support/walkthrough/rstudio#mousetrackingdatatoplotsinr
GorillaMouse.ReadInTraces <- function(MTpath){
  oldpath <- getwd()
  setwd(MTpath)
  print(sprintf("Reading in mouse traces from %s", MTpath))
  #Create a list of the xlsx files in your working directory.
  xlsxfiles<-list.files(pattern=".xlsx")
  # fix the order
  Sorted <- GorillaMouse.FixOrder(xlsxfiles)
  #Import all the excel files in the list.
  print(sprintf("Starting to import %d traces.",length(xlsxfiles)))
  xlsx.df.list<-lapply(as.list(Sorted$NameList),read_excel)
  #Combine all these imported excel files into a single data frame. 
  gorilla.traj<-rbindlist(xlsx.df.list, fill=TRUE)
  #limit to only the mousetracking data
  gorilla.traj<-gorilla.traj[gorilla.traj$type=="mouse",]
  setwd(oldpath)
  return(list(gorilla.traj, Sorted))
}

# 2. Putting trace file names in order of participant and trial (default = alphabetical) 
# extract the trial number from the list of files, and order the list according to
# ID and trial number
GorillaMouse.FixOrder <- function(NameList, explain=TRUE){
  if (explain) {
    cat("Ordering traces based on ID and Trial.\n
    Deriving these from the trace file (.xlsx) name, so edit code if ID and Trial 
    aren't 3rd and 8th item in the mouse trace file name.
    Returns a three column dataframe with the names of the trace files are the first,
    participant ID as the second, and Trial number as third column.
    To use the sorted list, just convert the first row of the dataframe to a list, 
    e.g. as.list(var_name$NameList)")
  }
  a<- strsplit(NameList,"-")
  Order <- vector(mode="double", length = length(NameList))
  Order <- tibble(IDOrder = numeric(length(NameList)), TrialOrder = numeric(length(NameList)))
  for (i in 1:length(NameList)){
    Order$IDOrder[i] <- as.double(a[[i]][3])
    Order$TrialOrder[i] <- as.double(a[[i]][8])
  }
  Unsorted <- tibble(NameList) %>% cbind(Order)
  Sorted_ <- Unsorted[order(Unsorted$TrialOrder), ]
  Sorted <- Sorted_[order(Sorted_$IDOrder), ]
  return(Sorted)
}

