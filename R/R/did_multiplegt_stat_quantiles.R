#' Internal function to generate quantile subsets based on delta D or delta Z
#' @param df df
#' @param ID ID
#' @param Time Time
#' @param D D
#' @param Z Z
#' @param quantiles quantiles
#' @importFrom plm pdata.frame make.pbalanced
#' @importFrom stats quantile
#' @returns A list with the df object and the relevant quantiles
#' @noRd
did_multiplegt_stat_quantiles <- function(
    df,
    ID,
    Time,
    D, 
    Z,
    quantiles
    ) {
    
    df <- subset(df, !(is.na(df[[ID]]) | is.na(df[[Time]]) | is.na(df[[D]]) ))
    if (!is.null(Z)) {
        df <- subset(df, !is.na(df[[Z]]))
    }

    ## To be added: case with multiple observations per cell

    ## Balance the panel
    df <- pdata.frame(df, index = c(ID, Time)) 
    df <- make.pbalanced(df, balance.type = "fill")

    if (is.null(Z)) {
        df$delta_pre_XX <- diff(df[[D]])
    } else {
        df$delta_pre_XX <- diff(df[[Z]])
    }

    df$partition_XX <- 0
    df_switchers <- subset(df, df$delta_pre_XX != 0)
    cut_off <- c()
    for (p in 1:length(quantiles)) {
        cut_off <- c(cut_off, quantile(df_switchers$delta_pre_XX, probs = quantiles[p], na.rm = TRUE))
        df$partition_XX <- ifelse(df$delta_pre_XX != 0 & df$delta_pre_XX >= cut_off[length(cut_off)], df$partition_XX + 1, df$partition_XX)
    }
    df$delta_pre_XX <- NULL 
    cut_off <- c(cut_off, max(df_switchers$delta_pre_XX, na.rm = TRUE))
    names(cut_off) <- c()
    class(df) <- "data.frame"
    ret <- list(df = df, val_quantiles = cut_off)
    return(ret)
}