#' @title correlation ranking
#' @description a function that ranks a list of continuous predictor variables by their correlation with a continuous outcome.
#' @param predictors a list of predictors
#' @param outcome the outcome
#' @returns ranking
#' @examples
#' corrRank(list(BMI, Age, Blood_pressure), Obesity)
#' @export

corrRank = function(predictors, outcome){
  ranking = data.frame(matrix(ncol=3, nrow=0))
  colnames(ranking) = c("Predictor", "Correlation", "Significance")
  stopifnot("Outcome variable must be numeric" = is.numeric(outcome))

  for (i in seq_along(predictors)){
    predictor = predictors[[i]]
    stopifnot("Predictors and outcome must be equal length" = length(predictor)==length(outcome))
    stopifnot("Predictors must be numeric" = is.numeric(predictor))

    ranking = rbind(ranking, data.frame(Predictor = names(predictors)[i],
                                        Correlation = round(cor(predictor, outcome, use = "complete.obs"),2),
                                        Significance = ifelse(cor.test(predictor, outcome)$p.value < 0.05, "Yes", "No"),
                                        "Linear Association" = dplyr::case_when(Correlation == -1 ~ "Perfect negative",
                                                                Correlation == 1 ~ "Perfect positive",
                                                                Correlation == 0 ~ "None",
                                                                Correlation >= 0.8 & Correlation < 1 ~ "Strong positive",
                                                                Correlation >= 0.6 & Correlation < 0.8 ~ "Moderate positive",
                                                                Correlation > 0 & Correlation < 0.6 ~ "Weak positive",
                                                                Correlation < 0 & Correlation > -0.6 ~ "Weak negative",
                                                                Correlation <= -0.6 & Correlation > -0.8 ~ "Moderate negative",
                                                                Correlation > -1 & Correlation <= -0.8 ~ "Strong negative")))
  }
  ranking = ranking %>% dplyr::arrange(desc(abs(Correlation)))
  return(ranking)
}
