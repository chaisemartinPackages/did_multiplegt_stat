#' Internal function for did_continuous
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
#' @noRd
did_continuous_main <- function(
    df,
    Y,
    G,
    T,
    D,
    Z,
    estimator,
    estimation_method,
    order,
    noextrapolation,
    placebo,
    weight,
    switchers,
    disaggregate
) {
    df <- df[c(Y, G, T, D, Z, weight)]
    View(df)
}