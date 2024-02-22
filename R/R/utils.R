#' Customized function for mean
#' @param object object
#' @param df df
#' @importFrom stats weighted.mean
#' @returns A scalar.
#' @noRd
Mean <- function(
    object,
    df
    ) {
    return(weighted.mean(x = df[[object]], w = df$weight_XX, na.rm = TRUE))    
}

#' Customized function for sd
#' @param object object
#' @param df df
#' @importFrom Hmisc wtd.var
#' @returns A scalar.
#' @noRd
Sd <- function(
    object,
    df
    ) {
    return(sqrt(wtd.var(x = df[[object]], weights = df$weight_XX, na.rm = TRUE)))
}

#' Customized function for sum_w
#' @param df df
#' @returns A scalar.
#' @noRd
wSum <- function(
    df
    ) {
    return(sum(df$weight_XX, na.rm = TRUE))
}

#' Customized function for sum
#' @param object object
#' @param df df
#' @returns A scalar.
#' @noRd
Sum <- function(
    object,
    df
    ) {
    df_nm <- subset(df, !(is.na(df[[object]] | is.na(df$weight_XX))))
    return(as.numeric(t(df_nm[[object]]) %*% df_nm$weight_XX))
}
