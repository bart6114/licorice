#' Centered licorice plot
#'
#' @param data_transformed the transformed dataset
#' @export
licorice_center_plot<-function(data_transformed){

  results_plot_obj<-
    ggplot(data_transformed$main) +
    geom_rect(aes(xmin = as.numeric(question)-.4, xmax = as.numeric(question)+.4, ymin = y_start_center, ymax = y_end_center, fill=response)) +
    geom_hline(yintercept = 0, lty=2) +
    geom_text(aes(x=as.numeric(question), y=-1.06, label=paste0(round(neg*100), "%")),data=data_transformed$percentages, size=3) +
    geom_text(aes(x=as.numeric(question), y=1.06, label=paste0(round(pos*100), "%")), data=data_transformed$percentages, size=3) +
    coord_flip() +
    scale_y_continuous("", breaks = seq(-1,1,.5), limits = c(-1.1,1.1), labels=percent) +
    xlab("") + ylab("") +
    theme(legend.position="bottom") +
    scale_x_continuous(breaks=1:length(levels(data_transformed$main$question)), labels=levels(data_transformed$main$question))

  if("group" %in% colnames(data_transformed$main)){
    results_plot_obj <- results_plot_obj + facet_wrap(~group, ncol=1)
  }

  results_plot_obj

}


#' Filled licorice plot
#'
#' @param data_transformed the transformed dataset
licorice_fill_plot<-function(data_transformed){
  results_plot_obj<-
    ggplot(data_transformed$main) +
    geom_bar(aes(x=question, fill=response, y=response_relative), position="fill", stat="identity") +
    coord_flip() +
    scale_y_continuous(labels=percent) +
    xlab("") + ylab("") +
    theme(legend.position="bottom")

  if("group" %in% colnames(data_transformed$main)){
    results_plot_obj <- results_plot_obj + facet_wrap(~group, ncol=1)
  }

  results_plot_obj

}


#' Count licorice plot
#'
#' @param data_transformed the transformed dataset
licorice_count_plot<-function(data_transformed){
  stats_plot_obj<-
    ggplot(data_transformed$counts) +
    geom_bar(aes(x=question, fill=type, y=value), stat="identity") +
    coord_flip()+
    xlab("") + ylab("") +
    theme(legend.position="bottom")



  if("group" %in% colnames(data_transformed$main)){
    stats_plot_obj <- stats_plot_obj + facet_wrap(~group, ncol=1)
  }

  stats_plot_obj
}
