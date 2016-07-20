
#' Plot Fig 1 from Zika risk assessment
#'
#' @return A ggplot object with cowplot theme that contains both figure panels
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


#' Plot Fig 2 from Zika risk assessment
#'
#' @param panels Optional character argument ("a", "b", "c") specifying individual desired panels to return. Accepts uppercase letters as well.
#' @return A ggplot object for specified plot
#' @export
#' @import cowplot
#' @examples
#' plot_fig2()
#' plot_fig2(panels="b")
plot_fig2 <- function(panels=NULL){

  panel_a <- ggplot(ex_outbreak_sims, aes(time, Cum_Detections, group=interaction(.id, risk_level), color=risk_level)) +
    geom_line(alpha=0.15,size=1) +
    scale_color_brewer(palette="Set1", direction = 1)+
    coord_cartesian(xlim=c(0,150), ylim=c(0,50), expand=FALSE) +
    guides(color=guide_legend(override.aes=list(alpha=1))) +
    theme(legend.position=c(0.3,0.8))+
    labs(x = "Time (days)",
         y = "Reported Autochthonous Cases",
         color = expression("R"[0]))

  if(!is.null(panels) & identical(tolower(panels), "a")){
    return(panel_a)
  }

  panel_b <- ggplot(prev_by_reported_data, aes(detected, median, color=risk_level, fill=risk_level, linetype=as.factor(disc_prob), group = interaction(risk_level, disc_prob))) +
    geom_line(size=1)+
    #geom_hline(yintercept=20)+
    geom_ribbon(aes(ymax=max, ymin=min), alpha=0.1, color=NA)+
    scale_y_log10(expand=c(0,0),limits=c(1,50), breaks = c(5,10,25,50))+
    coord_cartesian(xlim = c(0,15))+
    scale_x_continuous(expand=c(0.01,0.01))+
    theme(legend.position = c(0.3,0.79),
          #legend.direction = "horizontal",
          legend.box="horizontal")+
    scale_color_brewer(palette="Set1", direction = 1)+
    scale_fill_brewer(palette="Set1", direction=1)+
    guides(linetype=guide_legend(title.hjust = 0, override.aes=list("fill"=NA), title="Reporting Rate"),
           color=FALSE,fill=FALSE)+
    labs(x = "Reported Autochthonous Cases",
         y = "Autochthonous Cases (log scale)",
         color = expression("R"[0]),
         fill = expression("R"[0]))

  if(!is.null(panels) & identical(tolower(panels), "b")){
    return(panel_b)
  }

  panel_c <- ggplot(epi_prob_by_reported_data, aes(detected, value, linetype=as.factor(disc_prob), color=risk_level)) +
    geom_line(size=1) +
    coord_cartesian(xlim=c(0,15), ylim=c(0,1), expand=FALSE)+
    geom_vline(xintercept=2, size=0.5, linetype=2)+
    scale_color_brewer(palette="Set1", direction = 1) +
    background_grid(major = "xy", minor = "none")+
    theme(legend.position="none",
          legend.box.just="left")+
    labs(x = "Reported Autochthonous Cases",
         y = "Epidemic Probability",
         color = "Trigger Type",
         linetype= "County Risk")

  if(!is.null(panels) & identical(tolower(panels), "c")){
    return(panel_c)
  }

  ggdraw() + draw_plot(panel_a, x = 0, y=0, width=.33, height=1)+
    draw_plot(plot = panel_b, x = 0.33, y=0.0, width=0.33, height=1)+
    draw_plot(plot = panel_c, x = 0.66, y=0.0, width=0.33, height=1)+
    draw_plot_label(c("A", "B", "C"), c(0, 0.33, 0.66), c(1, 1, 1), size = 20)

}
