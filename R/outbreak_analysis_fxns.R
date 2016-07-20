
#' Get all final cumulative infected numbers from sims
#'
#' @param trials A list with simulated zika outbreaks. Usually utput of run_n_zika_sims()
#' @return A vector of all the max cumulatively infected individuals for each simulation
#' @examples
#'
all_max_cum_infect <- function(trials) {
  last_cuminfect_local <- function(x) {
    x[nrow(x), "Cumulative_Infections"] - x[nrow(x), "Cumulative_Intro_Infections"]
  }
  unlist(plyr::laply(trials, last_cuminfect_local))
}



