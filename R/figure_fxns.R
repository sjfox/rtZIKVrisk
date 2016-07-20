
#' Plot Fig 1 from Zika risk assessment
#'
#' @return A
#' @export
#' @import cowplot
#' @examples
#' plot_fig1()
plot_fig1 <- function(){

  panel_b <- ggplot(ex_growing_epidemic_data, aes(x=time, y=value, color=NA, fill= variable)) +
    geom_bar(stat="identity", width=1.05)+
    geom_segment(data=growing_arrows[growing_arrows$type=="import",], aes(x=time, xend=time, y=yval+1.5, yend=yval),
                 arrow = arrow(length = unit(0.05, "npc"), angle = 35), color="red", size=1, inherit.aes=FALSE)+
    geom_vline(data=growing_arrows[growing_arrows$type=="report",], aes(xintercept=time), linetype=2, color="red")+
    theme_cowplot() %+replace% theme(legend.position="none")+
    scale_y_continuous(expand=c(0.0,0.0))+
    scale_x_continuous(expand=c(0.01,0.01),limits=c(0,101))+
    scale_color_manual(values=c("black", "grey"), guide=FALSE) +
    scale_fill_manual(values=c("black", "grey")) +
    labs(x = "Time (days)", y="Prevalence", fill="")

  ylims <- ggplot_build(panel_b)$panel$ranges[[1]]$y.range
  panel_a <- ggplot(ex_dying_epidemic_data, aes(x=time, y=value, color=NA, fill= variable)) +
    geom_bar(stat="identity", width=1.05)+
    geom_segment(data=dying_arrows[dying_arrows$type=="import",], aes(x=time, xend=time, y=yval+1.5, yend=yval),
                 arrow = arrow(length = unit(0.05, "npc"), angle = 35), color="red", size=1, inherit.aes=FALSE)+
    geom_vline(data=dying_arrows[dying_arrows$type=="report",], aes(xintercept=time), linetype=2, color="red")+
    theme_cowplot() %+replace% theme(legend.position=c(0.9,0.85))+
    scale_y_continuous(expand=c(0.0,0.0), limits = c(0,ylims[2]))+
    scale_x_continuous(expand=c(0.01,0.01),limits=c(0,101))+
    scale_color_manual(values=c("black", "grey"), guide=FALSE) +
    scale_fill_manual(values=c("black", "grey")) +
    labs(x = "Time (days)", y="Prevalence", fill="")



  ggdraw() +
    draw_plot(panel_a, x =  0,y =  0.5,width =  1,height =  0.5) +
    draw_plot(panel_b, x =  0,y =  0,width =  1, height = .5) +
    draw_plot_label(c("A", "B"), c(0, 0), c(1, 0.5), size = 15)
}
