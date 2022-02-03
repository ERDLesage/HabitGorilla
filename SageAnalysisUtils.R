# 2022 by The Sage (elise.r.d.lesage@gmail.com)
# analysis util functions


# function to eliminate RT's under a certain threshold  ####
# input: dataframe, name of the RT variable, threshold (default = 150ms), other variables that should be NA

# function to check for missed responses ####
# input: dataframe, condition that (e.g. RT==0, Response ==NA, ...), other variables that should become NA, name for missed variable (default=Missed)
# output: amended dataframe

# function to exclude data and flag this as a separate variable ####
# keep the old as another separate variable
SageExclude(D, "ReactionTime>150", new="RT", make.na = c("OptimalChoice", "Response", "ResponseID",  "Correct"), remove=FALSE)

# simple exclude wrapper
# INPUT:
# - data: a dataframe
# - crit_vector: a logical vector that matches the exclusion criterion
# - remove_data: default = TRUE. If set to false, the data is not removed but a 
# - var_name: only relevant if remove_data == FALSE; the name of the column that will be 1 if included, 0 if excluded. if not supplied, default is "exclude"

SageTrialExclude <- function(data, crit_vector, remove_data = TRUE, var_name = Exclusion){
  # crit_vector needs to be a logical vector of the same length as data
  if (!is.logical(crit_vector)){
    stop("The exclusion criterion needs to be a logical vector.")
  } else if (nrow(data)!=length(crit_vector)){
    stop(sprintf("The exclusion criterion vector (%d) is not the same length as the number of rows in the dataframe (%d)", length(crit_vector), nrow(data)))
  }
  # give a warning if more than 50% of the data is identified for exclusion
  if (sum(crit_vector)>nrow(data)/2) {
    warning("WARNING: More than 50% of the data fits your exclusion criterion.\nCheck that you haven't accidentally supplied an inclusion criterion.")
  }
  # now the actual work :)
  if (remove_data) {
    data <- filter(data, (1-crit_vector))
    disp(sprintf("Excluded %d trials, %0.2f pct", sum(crit_vector), (sum(crit_vector)/nrow(data))*100))
  } else {
    data$e <- 1-crit_vector
    colnames(data)[colnames(data) == "e"] <- var_name
    disp(sprintf("Identified %d trials that match the exclusion criterion. These are NOT excluded; use variable %s to filter the data as needed.", sum(crit_vector, var_name)))
  }
  return(data)
}

SageGroupExclude

