

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
  qplot(final.sizes, geom="histogram") +
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
#' @return A ggplot object with cowplot theme final size histogram
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

