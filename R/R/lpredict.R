#' Internal function of did_multiplegt_stat that emulates Stata predict function.
#' @param df df
#' @param varname varname
#' @param model model
#' @param varlist varlist
#' @param const const
#' @param prob prob
#' @param factor factor
#' @returns The same input dataframe df with an added column of predicted values.
#' @noRd
lpredict <- function(
    df,
    varname,
    model,
    varlist,
    const = TRUE,
    prob = FALSE,
    factor = FALSE
) {

    sensitivity <- 10^-10
    df[[varname]] <- 0
    if (isTRUE(factor)) {
        singletons <- subset(varlist, sapply(varlist, function(x) length(grep(":", x)) == 0))
        varlist <- names(model$coefficients)[2:length(names(model$coefficients))]

    }
    for (v in varlist) {
        if (is.na(model$coefficients[[v]])) {
            next
        } else if (!is.na(model$coefficients[[v]]) & isTRUE(factor) & grepl("FACT",v,fixed = TRUE)) {
            var_set <- var_extract(str = v, vars = singletons)
            df[[paste0("sel",v)]] <-t(matrix(1,1,length(var_set$var)) %*% (t(as.matrix(df[var_set$var])) == var_set$val)) == length(var_set$var)
            df[[varname]] <- ifelse(df[[paste0("sel",v)]] == 1, 
                df[[varname]] + model$coefficients[[v]], df[[varname]])
            df[[paste0("sel",v)]] <- NULL
        } else if (!is.na(model$coefficients[[v]]) & (isFALSE(factor) |(isTRUE(factor) &  !grepl("FACT",v,fixed = TRUE)))) {
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

var_extract <- function(str, vars) {
    if (length(vars) > 25) {
        stop("Interaction limit (25) exceeded. Reduce number of other treatments.")
    }
    repl <- sapply(1:length(vars), function(x) paste0(intToUtf8(64 + x),"_XX"))
    for (i in 1:length(vars)) {
        str <- gsub(vars[i], repl[i], str)
    }
    num <- as.numeric(str_extract_all(str,"\\d+")[[1]])
    str <- paste(str_extract_all(str,"\\D+")[[1]], collapse = "")
    for (i in 1:length(vars)) {
        str <- gsub(repl[i], vars[i], str)
    }

    return(list(var = strsplit(str,":")[[1]], val = num))
}