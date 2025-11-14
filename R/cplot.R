#' @title correlation plot
#' @description a function that prints bivariate plots of outcome against exposure (from a list of predictors in order)
#' @param outcome outcome variable, vector
#' @param predictors list of predictors, vectors
#' @examples
#' exposures <- list(cities$life_expectancy, cities$happiness_level)
#' cplot(exposures, cities$cities$obesity)
#' @export

cplot <- function(predictors, outcome) {

  for (i in seq_along(predictors)) {
    predictor <- as.numeric(unlist(predictors[i]))
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

    print(corr_plot)
  }
}
