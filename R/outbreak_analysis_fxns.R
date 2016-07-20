
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
#' @return A vector of all the max cumulatively infected individuals for each simulation
#' @examples
#'
all_max_cum_infect <- function(trials) {

  unlist(plyr::laply(trials, last_cuminfect_local))
}


#' Cumulative cases for detects for single simulation run
#'
#' @return the maximum cumulative cases and max prevalence
cumcases_by_detects <- function(df, max_detect){
  ## Takes in a data frame trials, and for each
  ## First instance of a new local detection, returns the total prevalence

  all_detects <- cum_detect_local(df)
  unique_detects <- unique(all_detects)
  ## Only  interested in maximum of 100 detections
  unique_detects <- unique_detects[unique_detects<=max_detect]

  data.frame(detected = unique_detects, cum_infections = last_cuminfect_local_value(df), max_prevalence = max_nonintro_prevalence(df))
}

#' Get frequency cases above cum threshold and prev threshold
#'
#' @return a single probability of epidemic for single detected value
get_cumcases_by_detects_all <- function(trials, max_detect){
  ## Returns data frame of all prevalence by detections for all trials
  plyr::ldply(trials, cumcases_by_detects, max_detect)
}

#' Get frequency cases above cum threshold and prev threshold
#'
#' @return a single probability of epidemic for single detected value
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
#' @return A vector of epidemic probs for the given detected values
freq_above_thresh_vec <- Vectorize(freq_above_thresh, vectorize.args = "detected")

#' Get the epidemic probability as a function of number of reported cases
#'
#' @param trials A list with simulated zika outbreaks. Usually utput of run_n_zika_sims()
#' @param prev_threshold The maximum autochthonous prevalence necessary to be classifed as an epidemic (depends on individual scenario run values)
#' @param cum_threshold The cumulative autochthonous infections necessary to be classified as an epidemic (usually the e_thresh value of the runs)
#' @param max_detect The maximum number of detections to go to.
#' @param num_necessary Number of instances of trials to be necessary before it gets a probability. Usually want to be ~1% of total runs
#'
#' @return A dataframe of rows max_detect+1 that has column for detected, and column for prob_epidemic given that detected number
#' @export
#' @examples
#'
get_epidemic_prob_by_d <- function(trials, prev_threshold, cum_threshold, max_detect=50, num_necessary=1){
  detected <- seq(0, max_detect)
  ## Gets the number of cumulative cases for each run for each unique detected.
  data <- get_cumcases_by_detects_all(trials, max_detect = max_detect)

  ## Calculates the frequency that the cum_threshold and prev_thresholds are met in for each detected number
  probs <- freq_above_thresh_vec(data, detected, cum_threshold, prev_threshold, num_necessary)
  return(data.frame(detected=detected, prob_epidemic=probs))
}

