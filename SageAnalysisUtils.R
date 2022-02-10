# 2022 by The Sage (elise.r.d.lesage@gmail.com)
# Various analysis util functions
# originally built to work with Gorilla.sc output

# function to check for missed responses ####
# input: dataframe, condition that (e.g. RT==0, Response ==NA, ...), other variables that should become NA, name for missed variable (default=Missed)
# output: amended dataframe

# simple exclude wrapper (simple: using a first-order criterion)
# INPUT:
# - data: a dataframe
# - crit_vector: a logical vector that matches the exclusion criterion
# - remove_data: default = TRUE. If set to false, the data is not removed but a binary variable is created that can act as a toggle
# - var_name: only relevant if remove_data == FALSE; the name of the column that will be 1 if included, 0 if excluded. if not supplied, default is "exclude"
# OUTPUT:
# - the amended dataframe

SageExcludeSimple <- function(data, crit_vector, remove_data = TRUE, var_name = "Exclusion") {
  # crit_vector needs to be a logical vector of the same length as data
  if (!is.logical(crit_vector)){
    stop("!! The exclusion criterion needs to be a logical vector.")
  } else if (nrow(data)!=length(crit_vector)){
    stop(sprintf("!! The exclusion criterion vector (%d) is not the same length as the number of rows in the dataframe (%d)", length(crit_vector), nrow(data)))
  }
  # give a warning if more than 50% of the data is identified for exclusion
  if (sum(crit_vector)>nrow(data)/2) {
    warning("WARNING: More than 50% of the data fits your exclusion criterion.\nCheck that you haven't accidentally supplied an inclusion criterion.")
  }
  # now the actual work :)
  if (remove_data) {
    data <- filter(data, !crit_vector)
    disp(sprintf("Excluded %d trials, %0.2f percent of data.", sum(crit_vector), (sum(crit_vector)/nrow(data))*100))
  } else {
    data$e <- crit_vector
    colnames(data)[colnames(data) == "e"] <- var_name
    disp(sprintf("Identified %d trials that match the exclusion criterion.\nThese are NOT excluded; the variable %s was created.\nFilter the data as needed using this variable.", sum(crit_vector), var_name))
  }
  return(data)
}

# a not so simple exclude wrapper (HO meaning a higher order criterion (e.g. over blocks))
# INPUT:
# - data: a dataframe
# - crit_frame: a logical vector that matches the exclusion criterion
# - remove_data: default = TRUE. If set to false, the data is not removed but a binary variable is created that can act as a toggle
# - var_name: only relevant if remove_data == FALSE; the name of the column that will be 1 if included, 0 if excluded. if not supplied, default is "exclude"
# OUTPUT:
# - the amended dataframe
# CAUTION
# this function relies on the last column of the criterion dataframe being the criterion

SageExcludeHO <- function(data, crit_frame, groupvars, remove_data = TRUE, var_name = "Exclusion"){
  # keep only the relevant columns
  crit_frame <- cbind(crit_frame[groupvars], crit_col=pull(crit_frame, -1))
  # throw an error if the provided column name already exists
  if (var_name %in% colnames(data)) {
    stop("The provided column name already exists.\nPerhaps this command was already run?\nIf not, try again with a unique column name for the targeted data.")
  }
  # throw an error if the last column wasn't binary
  if (min(crit_frame$crit_col, na.rm=TRUE)==0 & max(crit_frame$crit_col, na.rm=TRUE)==0){
    disp("No data seem selected for exclusion.\n")
  } else if (min(crit_frame$crit_col, na.rm=TRUE)!=0 || max(crit_frame$crit_col, na.rm=TRUE)!=1) {
    stop("!! Criterion column appears not to be binary.\n The last column of the criterion dataframe (second argument)\n needs to be binary, with 1 meaning exclude the data and 0 meaning keep it.\n\nAlternatiely, maybe ")
  }
  # throw an error if the criterion frame is longer than the data frame
  if (nrow(data)<nrow(crit_frame)){
    stop("!! The criterion dataframe appears longer than the actual data.\n Check you haven't swapped them.")
  }
  # add the crit column info to the dataframe using inner_join
  data <- data %>% inner_join(crit_frame, by=groupvars) 
  if (remove_data){
    disp(sprintf("Identified %d trials for exclusion, %0.2f percent of data.\n", sum(data$crit_col), (sum(data$crit_col)/nrow(data))*100))
    data <- data %>% filter(crit_col==0) %>% select(-crit_col)
  } else if (!remove_data){
    disp(sprintf("Identified %d trials that match the exclusion criterion.\nYou opted NOT to delete these trials; instead the variable %s was created.\nFilter the data as needed using this variable.\n\n", sum(data$crit_col), var_name))
    colnames(data)[colnames(data) == "crit_col"] <- var_name
  }
  return(data)
}

# this one is unlikely very useful for the future, but...

ContingencyLookUpTable <- function(data){
  
  return(table)
}

# catch multiple of the same responses
# if the response is the same for over a given number of trials
# if the response systematically (e.g. L-R-L-R ...)
# if there are many trials missing in a row

SageSlackerCatch <- function(dat, respcol, groupvars, cutoff = 20){
  # throw an error if the respcol is not a column in the dataframe
  
  chunks <- dat %>% select(all_of(groupvars)) %>% unique()
  chunks$alt <- NA
  chunks$same <- NA
  chuncks$none <- NA
  chunks$sus <- NA
  for (b in 1:5) { #nrow(blocks)){
    bd <- semi_join(dat, chunks[b,])
    nonlag <- bd %>% select(.data[[respcol]])
    lag <- bd %>% select(.data[[respcol]]) %>% lag()
    diff <- as.numeric(nonlag==lag)
    # sum of alternating (a batch of zeros)
    zeros <- vector("integer", cutoff)
    bd$alt <- ifelse((zeros %in% diff), 1, 0)
    # sum of same (a batch of ones)
    ones <- zeros+1
  }
  #return(list(dat, chunks))
  return(chunks)
}
