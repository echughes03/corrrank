#' @title correlation ranking
#' @description a function that ranks a list of continuous predictor variables by their correlation with a continuous outcome.
#' @param predictors a list of predictors
#' @param outcome the outcome
#' @returns ranking
#' @examples
#' corrRank(list("BMI", "Blood pressure", "Age"), Cholesterol)
#' @export

corrRank = function(predictors, outcome){
  # message if different dimensions
  ranking = data.frame("Predictor", "Correlation", "Significance")
  stopifnot("Outcome variable must be numeric" = is.numeric(outcome))
  for (predictor in predictors){
    stopifnot("Predictors and outcome must be equal length" = length(predictor)==length(outcome))
    stopifnot("Predictors must be numeric" = is.numeric(predictor))

    rbind(ranking, data.frame(Predictor = predictor,
                              Correlation = cor(predictor, outcome, na.rm=TRUE),
                              Significance = ifelse(cor.test(predictor, outcome)$p.value < 0.05, "Yes", "No")))
  }
  ranking = ranking %>% arrange(abs(Correlation))
  return(ranking)
}
