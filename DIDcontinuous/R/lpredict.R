#' Internal function of did_continuous
#' @param df df
#' @param varname varname
#' @param model model
#' @param varlist varlist
#' @param const const
#' @param prob prob
#' @noRd
lpredict <- function(
    df,
    varname,
    model,
    varlist,
    const = TRUE,
    prob = FALSE
) {
    sensitivity <- 10^-8
    df[[varname]] <- 0
    for (v in varlist) {
        if (is.na(model$coefficients[[v]])) {
            next
        } else {
            df[[varname]] <- df[[varname]] + df[[v]] * model$coefficients[[v]]
        }
    }
    if (isTRUE(const)) {
        df[[varname]] <- df[[varname]] + model$coefficients[1]
    }
    if (isTRUE(prob)) {        
        df[[varname]] <- exp(df[[varname]]) / (1 + exp(df[[varname]]))
        df[[varname]] <- ifelse(is.nan(df[[varname]]), 1, df[[varname]])
        df[[varname]] <- ifelse(df[[varname]] < sensitivity, 0, df[[varname]])
    }

    return(df)
}