#' Customized function for mean
#' @param object object
#' @param df df
#' @param w w
#' @importFrom stats weighted.mean
#' @returns A scalar.
#' @noRd
Mean <- function(
    object,
    df,
    w = "weight_XX"
    ) {
    return(weighted.mean(x = df[[object]], w = df[[w]], na.rm = TRUE))    
}

#' Customized function for sd
#' @param object object
#' @param df df
#' @param w w
#' @importFrom Hmisc wtd.var
#' @returns A scalar.
#' @noRd
Sd <- function(
    object,
    df,
    w = "weight_XX"
    ) {
    return(sqrt(wtd.var(x = df[[object]], weights = df[[w]], na.rm = TRUE)))
}

#' Customized function for sum_w
#' @param df df
#' @param w w
#' @returns A scalar.
#' @noRd
wSum <- function(
    df,
    w = "weight_XX"
    ) {
    return(sum(df[[w]], na.rm = TRUE))
}

#' Customized function for sum
#' @param object object
#' @param df df
#' @param w w
#' @returns A scalar.
#' @noRd
Sum <- function(
    object,
    df,
    w = "weight_XX"
    ) {
    df_nm <- subset(df, !(is.na(df[[object]] | is.na(df[[w]]))))
    return(as.numeric(t(df_nm[[object]]) %*% df_nm[[w]]))
}
