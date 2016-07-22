

#' Plot histogram of the final sizes of zika outbreaks
#'
#' @param trials A list with simulated zika outbreaks. Usually utput of run_n_zika_sims()
#' @return A ggplot object with cowplot theme final size histogram
#' @export
#' @import cowplot
#' @examples
#' sims <- run_n_zika_sims(100, zika_def_parms())
#' plot_final_sizes(sims)
plot_final_sizes <- function(trials){
  final.sizes <- all_max_cum_infect(trials)
  qplot(final.sizes, geom="histogram", bins=30) +
    scale_y_continuous(expand=c(0,0)) +
    scale_x_continuous(expand=c(0,0))+
    labs(x = "Final Outbreak Sizes")
}

#' Plot outbreaks
#'
#' Plots the cumulative autochthonously infected individuals for all outbreaks simulated
#'
#' @param trials A list with simulated zika outbreaks. Usually utput of run_n_zika_sims()
#' @param cases A string dictating what to plot c("local", "total", "all")
#' @return A ggplot object with cowplot theme outbreak curves
#' @export
#' @import cowplot
#' @examples
#' sims <- run_n_zika_sims(100, zika_def_parms())
#' plot_zika_outbreaks(sims)
plot_zika_outbreaks <- function(trials, cases="local"){
  names(trials) <- seq_along(trials)
  outbreak_data <- plyr::ldply(trials, data.frame)

  outbreak_data$cases <- switch(cases,
                                local = outbreak_data$Cum_Detections - outbreak_data$Cum_Intro_Detections,
                                total = outbreak_data$Cum_Detections,
                                all = outbreak_data$Cumulative_Infections,
                                stop("Invalid choice, only accepts cases=c('local', 'total','all')"))
  ylabel <-switch(cases,
                  local = "Reported Autochthonous Cases",
                  total = "Reported total cases",
                  all = "Cumulative total infections")
  ggplot(outbreak_data, aes(time, cases, group=.id)) +
    geom_line(alpha=0.4,size=1, color="black") +
    labs(x = "Time (days)",
         y = ylabel)
}


#' Plot epidemic risk
#'
#' Plots the probability of an epidemic as a function of the number of reported local cases
#'
#' @param df A dataframe that is the output of \code{\link{get_epidemic_prob_by_d}}
#' @param max_detect The maximum number of cases to plot on the x-axis
#' @return A ggplot object with cowplot theme final size histogram
#' @export
#' @import cowplot
#' @examples
#' travis_parms <- get_county_parms("travis")
#' travis_sims <- run_n_zika_sims(num_reps = 1000, sim_parms)
#' travis_epi_prob <- get_epidemic_prob_by_d(travis_sims, prev_threshold = 10, cum_threshold = 100)
#' plot_epi_prob(travis_epi_prob)
plot_epi_prob <- function(df, max_detect=10){

  ggplot(df, aes(detected, prob_epidemic)) +
    coord_cartesian(ylim=c(0,1), xlim = c(0, max_detect))+
    geom_line(size=1, color="red") +
    labs(x = "Reported Cases", y = "Epidemic Probability")+
    background_grid(major="xy")
}


