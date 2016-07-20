

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
    labs(x = "Final Epidemic Sizes")
}


