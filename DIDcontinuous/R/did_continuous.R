#' Main interface for did_continuous
#' @importFrom haven read_dta
#' @md
#' @description This program estimates the three estimators (aoss, waoss, iv-aoss) developped in de Chaisemartin, d'Haultfoeuille, Pasquier and Vazquez‐Bare,Difference-in-Differences Estimators for Treatments Continuously Distributed at Every Period (January 18, 2022). 
#' @param df df
#' @param Y Y
#' @param ID ID
#' @param T T
#' @param D D
#' @param Z Z
#' @param estimator estimator
#' @param estimation_method estimation_method Y
#' @param order order Y
#' @param noextrapolation noestrapolation
#' @param placebo placebo
#' @param weight weight Y
#' @param switchers switchers Y
#' @param disaggregate disaggregate Y
#' @param aoss_vs_waoss aoss_vs_waoss Y
#' @export
did_continuous <- function(
    df,
    Y,
    ID,
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
    disaggregate = FALSE,
    aoss_vs_waoss = FALSE
) {
  args <- list()
  for (v in names(formals(did_continuous))) {
    if (v != "df") {
      args[[v]] <- get(v)
    }
  }

  # General Syntax Check
  if (!is.null(switchers)) {
      if (!(switchers %in% c("up", "down"))) {
        stop("Switchers could be either NULL, up or down")          
      }
  }

  results <- did_continuous_main(df, Y, ID, T, D, Z, estimator, estimation_method, order,
  noextrapolation, placebo, weight, switchers, disaggregate, aoss_vs_waoss)

  did_continuous <- list(args, results)
  names(did_continuous) <- c("args", "results")
  class(did_continuous) <- c(class(did_continuous), "did_continuous")
  return(did_continuous)
}
