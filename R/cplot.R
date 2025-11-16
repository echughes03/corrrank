#' @title correlation plot
#' @description a function that prints bivariate plots of outcome against exposure (from a list of predictors in order)
#' @param outcome numeric outcome variable, vector
#' @param predictors list of numeric predictor(s), vectors
#' @returns list of ggplot2 output(s). List can be indexed to output specific plot(s)
#' @examples
#' exposures <- list(cities$life_expectancy, cities$happiness_level)
#' plot_list <- cplot(exposures, cities$obesity_level)
#' plot_list$`Exposure 1`
#' plot_list$`Exposure 2`
#' @export

cplot <- function(predictors, outcome) {

  plots <- list()

  for (i in seq_along(predictors)) {
    predictor <- predictors[[i]]
    correlation <- cor(x = predictor, y = outcome, use = "complete.obs")

    df <- data.frame(exposure = predictor, response = outcome)

    corr_plot <- ggplot(data = df,
           aes(x = exposure,
               y = response)) +
      geom_point() +
      geom_smooth(method = "lm", se = F) +
      annotate(geom = "text",
               x = min(predictor, na.rm = T),
               y = max(outcome, na.rm = T),
               hjust = 0,
               vjust = 1,
               label = glue("r = {round(correlation,3)}")) +
      labs(title = glue("Correlation Between {deparse(substitute(outcome))} and Exposure {i}"),
           x = glue("Exposure {i}"),
           y = glue("{deparse(substitute(outcome))}"))

    plots[[glue("Exposure {i}")]] <- corr_plot
  }
  return(plots)
}



