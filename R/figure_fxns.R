
#' Plot Fig 1 from Zika risk assessment
#'
#' @param panels Optional character argument ("a", "b") specifying individual desired panels to return. Accepts uppercase letters as well.
#' @return A ggplot object with cowplot theme that contains both figure panels
#' @export
#' @import cowplot
#' @examples
#' plot_fig1()
plot_fig1 <- function(panels=NULL){
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

  if(!is.null(panels) & identical(tolower(panels), "b")){
    return(panel_b)
  }

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

  if(!is.null(panels) & identical(tolower(panels), "a")){
    return(panel_a)
  }


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

#' Plot Fig 3 from Zika risk assessment
#'
#' @param panels Optional character argument ("a", "b") specifying individual desired panels to return. Accepts uppercase letters as well.
#' @return A ggplot object for specified plot
#' @export
#' @import cowplot
#' @import maptools
#' @examples
#' plot_fig3()
#' plot_fig3(panels="a")
plot_fig3 <- function(panels=NULL){
  county_sp_data <- ggplot2::fortify(texas_county_shp, region = "ID")
  county_sp_data <- merge(county_sp_data, county_risk_data[, c("id", "Geography", "rnott.expected.round", "importation_probability")], by = "id", all.x = TRUE)

  city_data <- major_city_loc[1:10,]
  panel_a <- ggplot(county_sp_data, aes(x = long, y = lat)) +
    geom_polygon(aes(group = group, fill = log(importation_probability)), color = "grey", size = .1) +
    scale_x_continuous("", breaks=NULL) +
    scale_y_continuous("", breaks=NULL) +
    scale_fill_continuous(name = "Import Probability", low ="white", high = "blue", breaks = log(c(0.002, 0.02, 0.2)), labels = c(0.002,0.02,0.2),
                          na.value = "white") +
    geom_point(data = city_data, aes(x = lon, y = lat), color = "black", size=1, show.legend = FALSE) +
    ggrepel::geom_text_repel(data = city_data, aes(x=lon, y = lat, label = Name), size = 5,  force=0.75, segment.color = "black")+
    theme_cowplot() %+replace% theme(strip.background=element_blank(),
                                     strip.text.x = element_blank(),
                                     legend.position = c(0.2, 0.17),
                                     legend.title.align = 0.5) +
    guides(fill = guide_colorbar(label.position = "bottom", title.position="top",direction = "horizontal", barwidth = 10))

  if(!is.null(panels) & identical(tolower(panels), "a")){
    return(panel_a)
  }

  panel_b <- ggplot(county_sp_data, aes(x=long, y = lat)) +
    geom_polygon(aes(group = group, fill = rnott.expected.round), color = "grey", size = .1) +
    scale_x_continuous("", breaks=NULL) +
    scale_y_continuous("", breaks=NULL) +
    scale_fill_continuous(name = expression("R"[0]), low = "white", high = "darkgreen", na.value = "white") +
    theme_cowplot() %+replace% theme(strip.background=element_blank(),
                                     strip.text.x = element_blank(),
                                     legend.position = c(0.2, 0.17),
                                     legend.title.align = 0.5) +
    guides(fill = guide_colorbar(label.position = "bottom", title.position="top",direction = "horizontal", barwidth = 10))

  if(!is.null(panels) & identical(tolower(panels), "b")){
    return(panel_b)
  }

  plot_grid(panel_a, panel_b, nrow = 1, labels = "AUTO", label_size = 20)
}

#' Plot Fig 4 from Zika risk assessment
#'
#' This can take a long time to run, because the spatial plotting in ggplot can be slow.
#'
#' @param panels Optional character argument ("a", "b", "c") specifying individual desired panels to return. Accepts uppercase letters as well.
#' @return A ggplot object for specified plot
#' @export
#' @import cowplot
#' @import maptools
#' @examples
#' plot_fig4()
#' plot_fig4(panels="a")
#'
#' # It's suggested that you save plot:
#' fig4 <- plot_fig4()
#' save_plot("fig4.pdf", fig4, base_aspect_ratio = 3, base_height=5)
plot_fig4 <- function(panels=NULL){


  #### Match R0/Import Rate with Triggers
  county_risk <- merge(x = county_risk_data, y = exp_triggers_data[,c("r_not", "intro_rate", "epi_trigger")],
                         by.x=c("rnott.expected.round", "importation.projected"), by.y=c("r_not", "intro_rate"), all.x=TRUE, sort=FALSE)


  ## Match R0/import rate with probability of seeing 2 cases and probability ofepidemic given seeing 2 cases
  county_risk <- merge(x = county_risk, y = county_prob_detect_x[which(county_prob_detect_x$detected==2), c("r_not", "intro_rate", "prob_detect")],
                         by.x=c("rnott.expected.round", "importation.projected"), by.y=c("r_not", "intro_rate"), all.x=TRUE, sort=FALSE)

  ## Merge probability of epidemic given you've seen 2 cases
  county_risk <- merge(x = county_risk, y = county_epi_prob_by_d[,c("r_not", "intro_rate", "prob_epidemic")],
                         by.x=c("rnott.expected.round", "importation.projected"), by.y=c("r_not", "intro_rate"), all.x=TRUE, sort=FALSE)


  county_sp_data <- ggplot2::fortify(texas_county_shp, region = "ID")
  county_sp_data <- merge(county_sp_data, county_risk, by = "id", all.x = TRUE)
  county_sp_data <- county_sp_data[order(county_sp_data$id),]



  ind <- which(major_city_loc$Name %in% c("Austin", "Houston", "Dallas", "San Antonio"))

  ## Plot county probability of detecting two cases
  prob_breaks <- c(0, 0.25, 0.5, 0.75, 1)
  panel_a <- ggplot(county_sp_data, aes(x=long, y = lat)) +
    geom_polygon(aes(group = group, fill = prob_detect), color = "grey", size = .1) +
    scale_x_continuous("", breaks=NULL) +
    scale_y_continuous("", breaks=NULL) +
    scale_fill_continuous(name = "Detection Probability", low = "grey95", high = "black", na.value="white", breaks=prob_breaks, limits=c(0,1))+
    # scale_fill_gradientn(name = "Trigger (Reported Cases)", colors=colFunc(10),
    # na.value = "white", breaks = breaks.trigger) +
    geom_point(data = major_city_loc[ind,], aes(x = lon, y = lat), color = "black", size=1, show.legend = FALSE) +
    ggrepel::geom_text_repel(data = major_city_loc[ind,], aes(x=lon, y = lat, label = Name), force=10, nudge_x = ifelse(major_city_loc[ind,]$Name=="San Antonio",-1,0),
                    size = 5, point.padding = unit(0, "lines"), box.padding =unit(1, 'lines'),segment.color = "black")+
    theme_cowplot() %+replace% theme(strip.background=element_blank(),
                                     strip.text.x = element_blank(),
                                     legend.title.align = 0.5,
                                     legend.position = c(0.2, 0.17)) +
    guides(fill = guide_colorbar(label.position = "bottom", title.position="top",direction = "horizontal", barwidth = 10))

  if(!is.null(panels) & identical(tolower(panels), "a")){
    return(panel_a)
  }

  ## Panel B
  ## Plot county probability of an epidemic given you've detected two cases
  panel_b <- ggplot(county_sp_data, aes(x=long, y = lat)) +
    geom_polygon(aes(group = group, fill = prob_epidemic), color = "grey", size = .1) +
    scale_x_continuous("", breaks=NULL) +
    scale_y_continuous("", breaks=NULL) +
    scale_fill_continuous(name = "Epidemic Probability", low = "grey95", high = "red", na.value="white", breaks=prob_breaks, limits=c(0,1))+
    # scale_fill_gradientn(name = "Trigger (Reported Cases)", colors=colFunc(10),
    # na.value = "white", breaks = breaks.trigger) +
    geom_point(data = major_city_loc[ind,], aes(x = lon, y = lat), color = "black", size=1, show.legend = FALSE) +
    ggrepel::geom_text_repel(data = major_city_loc[ind,], aes(x=lon, y = lat, label = Name), force=10, nudge_x = ifelse(major_city_loc[ind,]$Name=="San Antonio",-1,0),
                    size = 5, point.padding = unit(0, "lines"), box.padding =unit(1, 'lines'),segment.color = "black")+
    theme_cowplot() %+replace% theme(strip.background=element_blank(),
                                     strip.text.x = element_blank(),
                                     legend.title.align = 0.5,
                                     legend.position = c(0.2, 0.17)) +
    guides(fill = guide_colorbar(label.position = "bottom", title.position="top",direction = "horizontal", barwidth = 10))
  # print(plot.epirisk)

  if(!is.null(panels) & identical(tolower(panels), "b")){
    return(panel_b)
  }

  ## Panel C
  ## Plot county triggers for epidemics at 70% confidence
  panel_c <- ggplot(county_sp_data, aes(x=long, y = lat)) +
    geom_polygon(aes(group = group, fill = epi_trigger), color = "grey", size = .1) +
    scale_x_continuous("", breaks=NULL) +
    scale_y_continuous("", breaks=NULL) +
    scale_fill_continuous(name = "Trigger (Reported Cases)", low = "red", high = "yellow",
                          # scale_fill_gradientn(name = "Trigger (Reported Cases)", colors=colFunc(10),
                          na.value = "white")+#, breaks = breaks.trigger) +
    geom_point(data = major_city_loc[ind,], aes(x = lon, y = lat), color = "black", size=1, show.legend = FALSE) +
    ggrepel::geom_text_repel(data = major_city_loc[ind,], aes(x=lon, y = lat, label = Name), force=10, nudge_x = ifelse(major_city_loc[ind,]$Name=="Houston",1,-1),
                    nudge_y = ifelse(major_city_loc[ind,]$Name=="Houston",-1.25,0),
                    size = 5, point.padding = unit(0.25, "lines"), box.padding = unit(0.5, "lines"), segment.color = "black")+
    theme_cowplot() %+replace% theme(strip.background=element_blank(),
                                     strip.text.x = element_blank(),
                                     legend.title.align = 0.5,
                                     legend.position=c(0.2, 0.17)) +
    guides(fill = guide_colorbar(label.position = "bottom", title.position="top",direction = "horizontal", barwidth = 10))

  if(!is.null(panels) & identical(tolower(panels), "c")){
    return(panel_c)
  }

  plot_grid(panel_a, panel_b, panel_c, nrow = 1, labels = "AUTO", label_size = 18)
}
