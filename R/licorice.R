
#' plot the likert scale results using centered bar plot
#'
#' @param data the dataset
#' @param sort whether to sort results, defaults to TRUE
#' @param short_reversed should sort be reversed?
#' @param middle_pos a number corresponding to the location at which to center the plot
#' @param type the type of graph to create (center, fill, count)
#' @param strwrap_width at what position in the question to place newline characters
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import scales
#' @export
licorice<-function(data, answers_order = levels(as.factor(data$response)), sort = FALSE, sort_reversed = FALSE, middle_pos = 2.5, type=c("center", "fill", "count"), strwrap_width = 25){

  data_transformed<-transform_data(data, answers_order, sort, sort_reversed, middle_pos, strwrap_width)

  if(type[1] == "fill"){
    plot_obj <-
      licorice_fill_plot(data_transformed)

  } else if(type[1] == "count"){
    plot_obj <-
      licorice_count_plot(data_transformed)

  } else {
    plot_obj <-
      licorice_center_plot(data_transformed)
  }

  plot_obj

}

#' Transforms data to be used by licorice function
#'
#' @param data dataset
#' @param answers_order character vector with answers in order
#' @param sort sort data?
#' @param sort_reversed reversed?
#' @param middle_pos middle position of the factor var
transform_data<-function(data, answers_order, sort=TRUE, sort_reversed=FALSE, middle_pos=2.5, strwrap_width=25){
  if("group" %in% colnames(data)){
    group_by_args <- c("group", "question")
  } else {
    group_by_args <- c("question")
  }
  data <-
    data %>%
    ungroup %>%
    mutate(question = strwrap(question, strwrap_width) %>% as.factor)


  results_data_main<-
    data %>%
    mutate(response = factor(response, levels=answers_order)) %>%
    arrange_(.dots = c(group_by_args, "response")) %>%
    group_by_(.dots = group_by_args) %>%
    filter(!is.na(response)) %>%
    mutate(response_relative = count / sum(count),
           y_start = lag(cumsum(response_relative), default=0),
           y_end = y_start + response_relative,
           middle = get_middle_position(middle_pos, as.numeric(response), y_start, y_end)) %>%
    ## set corrected y_start and y_end to center
    mutate(y_start_center = y_start - middle,
           y_end_center = y_end - middle,
           y_max_centered = max(y_end_center)) %>%
    ungroup

  results_data_percentages <-
    data %>%
    group_by_(.dots = group_by_args) %>%
    summarise(missing = length(response[is.na(response)]),
              total = n(),
              completed = total - missing) %>%
    left_join(results_data_main %>%
                group_by_(.dots = group_by_args) %>%
                summarise(neg = abs(min(y_start_center)),
                          pos = max(y_end_center)) %>%
                ungroup, by = group_by_args)


  if(sort == TRUE){
    results_data_main <-
      results_data_main %>%
      mutate(question = reorder(question, y_max_centered))

    results_data_percentages <-
      results_data_percentages %>%
      mutate(question = reorder(question, pos))

  }

  results_data_counts <-
    results_data_percentages %>%
    gather(type, value, completed, missing)

  list(
    percentages = results_data_percentages,
    main = results_data_main %>% filter(!is.na(response)),
    counts = results_data_counts
  )

}

#' find the coordinated to center the responses in a center plot
#'
#' @param middle_pos a number corresponding to the location at which to center the plot
#' @param levels the used response levels
#' @param start_values the y start values
#' @param end_values the y end values
get_middle_position<-function(middle_pos, levels, start_values, end_values){
  # if middle pos is not integer
  if(middle_pos %% 1 != 0){

    # check whether level exists (if middle_pos == 3.5 but level 4 doesn't exist, centerline should be at 3)
    if(ceiling(middle_pos) %in% levels){
      value <- (end_values[levels == ceiling(middle_pos)] - start_values[levels == ceiling(middle_pos)]) * .5 + start_values[levels == ceiling(middle_pos)]
    } else {
      value <- start_values[levels > middle_pos][1]
    }
  } else {
    # middle pos is integer
    # check wether the level after which the center line is set is present
    if(middle_pos %in% levels){
      value <- end_values[levels == middle_pos]
    } else {
      # get first value that follows it
      value <- start_values[levels > middle_pos][1]
    }
  }

  value
}

