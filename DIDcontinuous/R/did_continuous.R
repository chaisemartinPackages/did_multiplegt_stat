#' Main interface for did_continuous
#' @importFrom haven read_dta
#' @md
#' @description This program estimates the three estimators (aoss, waoss, iv-aoss) developped in de Chaisemartin, d'Haultfoeuille, Pasquier and Vazquez‐Bare,Difference-in-Differences Estimators for Treatments Continuously Distributed at Every Period (January 18, 2022). 
#' @param df df
#' @param Y Y
#' @param G G
#' @param T T
#' @param D D
#' @param Z Z
#' @param estimator estimator
#' @param estimation_method estimation_method
#' @param order order
#' @param noextrapolation noestrapolation
#' @param placebo placebo
#' @param weight weight
#' @param switchers switchers
#' @param disaggregate disaggregate
#' @export
did_continuous <- function(
    df,
    Y,
    G,
    T,
    D,
    Z = NULL,
    estimator = NULL,
    estimation_method = NULL,
    order = 1,
    noextrapolation = FALSE,
    placebo = NULL,
    weight = NULL,
    switchers = NULL,
    disaggregate = FALSE
) {
  args <- list()
  for (v in names(formals(did_continuous))) {
    if (v != "df") {
      args[[v]] <- get(v)
    }
  }

  results <- did_continuous_main(df, Y, G, T, D, Z, estimator, estimation_method, order,
  noestrapolation, placebo, weight, switchers, disaggregate)

  did_continuous <- list(args, list(results))
  names(did_continuous) <- c("args", "results")
  class(did_continuous) <- c(class(did_continuous), "did_continuous")
  return(did_continuous)
}
