
#' Get single final cumulative infected numbers from sims
#'
#' @param x a single trial run
#' @return the max cumulative infected individuals for each simulation
#' @examples
#'
last_cuminfect_local <- function(x) {
  x[nrow(x), "Cumulative_Infections"] - x[nrow(x), "Cumulative_Intro_Infections"]
}

#' Get all final cumulative infected numbers from sims
#'
#' @param trials A list with simulated zika outbreaks. Usually utput of run_n_zika_sims()
#' @return A vector of all the max cumulatively infected individuals for each simulation.
#' Calls \code{\link{last_cuminfect_local}}
#' @examples
#'
all_max_cum_infect <- function(trials) {

  unlist(plyr::laply(trials, last_cuminfect_local))
}

#' Get single maximum of local prevalence
#'
#' @param x a single trial run
#' @return the maximum local prevalence from run
#' @examples
#'
max_local_prev <- function(x){
  return(max(x[,"Total_Infections"]-x[,"Total_Intro_Infections"]))
}


#' Returns cumulative vector of locally detected cases
#'
#' @param x a single trial run
#' @return Cumulative detectections in vector that are local
#' @examples
#'
cum_detect_local <- function(x){
  ## Returns column of cumulative local detections
  x[, "Cum_Detections"] - x[, "Cum_Intro_Detections"]
}



#' Cumulative cases for detects for single simulation run
#'
#' @param df A dataframe that is the result of a single zika simulation run
#' @param max_detect the maximum number of detections desired to be analyzed
#' @return the maximum cumulative cases and max prevalence for each unique detection
cumcases_by_detects <- function(df, max_detect){
  all_detects <- cum_detect_local(df)
  unique_detects <- unique(all_detects)


  unique_detects <- unique_detects[unique_detects<=max_detect]

  data.frame(detected = unique_detects, cum_infections = last_cuminfect_local_value(df), max_prevalence = max_local_prev(df))
}

#' Get all cumcases and prevalence by detects
#'
#' Vectorized version of \code{\link{cumcases_by_detects}}, to get all trial information.
#'
#' @param trials a list of zika trial simulations
#' @param max_detect the maximum amount of detections interested in for analysis
#'
#' @return a single probability of epidemic for single detected value
get_cumcases_by_detects_all <- function(trials, max_detect){
  ## Returns data frame of all prevalence by detections for all trials
  plyr::ldply(trials, cumcases_by_detects, max_detect)
}

#' Get frequency cases above cum threshold and prev threshold for unique detected
#'
#' Returns the frequency that cases were found to be above a threshold.
#'
#' @param df dataframe formatted output from \code{\link{get_cumcases_by_detects_all}}. Has one columns for detected, cum_infections, and max_prevalence.
#' @param detected single integer number of interest
#' @param cum_threshold cumulative threhold for epidemic classification
#' @param prev_threshold prevalence threshold for epidemic classification
#' @param num_necessary number of trials necessary for epi classification usually 1\% of trials.
#'
#' @return a single probability of epidemic for single detected value.
freq_above_thresh <- function(df, detected, cum_threshold, prev_threshold, num_necessary){
  ## Takes in dataframe of all prevalence by detect
  ## Returns a single frequency of times that
  ## prevalence for a specific detection criteria is below a threshold
  rows <- which(df[,"detected"] == detected)
  if(length(rows)<=num_necessary){
    return(NA)
  }else{
    ## Return number of rows that excede both thresholds divided by the total rows
    sum(df[rows, "cum_infections"] >= cum_threshold & df[rows,"max_prevalence"] >= prev_threshold) / length(rows)
  }
}

#' Vectorizes the freq_above_thresh for the number of detected
#'
#' Called from the get_epidemic_prob_by_d
#'
#' @param detected vector of detected of interest
#' @inheritParams freq_above_thresh
#' @return A vector of epidemic probs for the given detected values.
#'
freq_above_thresh_vec <- Vectorize(freq_above_thresh, vectorize.args = "detected")


#' Get the epidemic probability as a function of number of reported cases
#'
#' @param trials A list with simulated zika outbreaks. Usually utput of \code{\link{run_n_zika_sims}}
#' @param prev_threshold The maximum autochthonous prevalence necessary to be classifed as an epidemic - depends on individual scenario run values
#' @param cum_threshold The cumulative autochthonous infections necessary to be classified as an epidemic - usually the e_thresh value of the runs
#' @param max_detect The maximum number of detections to go to.
#' @param num_necessary Number of instances of trials to be necessary before it gets a probability. Usually want to be ~1\% of total runs
#'
#' @return A dataframe of rows max_detect+1 that has column for detected, and column for prob_epidemic given that detected number
#' @export
#' @examples
#' \dontrun{
#' get_epidemic_prob_by_d(trials, 5, 100, 15, 1)
#' }
get_epidemic_prob_by_d <- function(trials, prev_threshold, cum_threshold, max_detect=50, num_necessary=1){
  detected <- seq(0, max_detect)
  ## Gets the number of cumulative cases for each run for each unique detected.
  data <- get_cumcases_by_detects_all(trials, max_detect = max_detect)

  ## Calculates the frequency that the cum_threshold and prev_thresholds are met in for each detected number
  probs <- freq_above_thresh_vec(data, detected, cum_threshold, prev_threshold, num_necessary)
  return(data.frame(detected=detected, prob_epidemic=probs))
}

