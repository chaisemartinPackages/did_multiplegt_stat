#' Internal function for did_continuous
#' @param df df
#' @param Y Y
#' @param G G
#' @param T T
#' @param D D
#' @param Z Z
#' @param estimator estimator
#' @param order order
#' @param noextrapolation noestrapolation
#' @param weight weight
#' @param switchers switchers
#' @param pairwise pairwise
#' @param IDs IDs
#' @param aoss aoss
#' @param waoss waoss
#' @param iwaoss iwaoss
#' @param estimation_method estimation_method
#' @param scalars scalars
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang := 
#' @importFrom rlang .data
#' @importFrom plm pdata.frame make.pbalanced
#' @noRd
did_continuous_pairwise <- function(
    df,
    Y,
    G,
    T,
    D,
    Z,
    estimator,
    order,
    noextrapolation,
    weight,
    switchers,
    pairwise,
    IDs,
    aoss,
    waoss,
    iwaoss,
    estimation_method,
    scalars
) {
    for (v in names(scalars)) {
        assign(v, scalars[[v]])
    }

    ## Start of the program

    gap_XX <- max(df$tsfilled_XX, na.rm = TRUE)
    df <- df %>% group_by(.data$T_XX) %>% mutate(Tbis_XX = cur_group_id()) 
    df$T_XX <- df$Tbis_XX
    df$Tbis_XX <- NULL

    df <- pdata.frame(df, index = c("ID_XX", "T_XX")) 
    df$ID_XX <- as.numeric(as.character(df$ID_XX))
    df$T_XX <- as.numeric(as.character(df$T_XX))
    df$delta_D_XX <- diff(df$D_XX)
    df$delta_Y_XX <- diff(df$Y_XX)

    if (IV_req_XX == 1 & (iwaoss == 1 | is.null(estimator))) {

    }

    df <- df[order(df$ID_XX, df$T_XX), ]

    # Generate deltaD_t = D_t - D_(t-1) and put it at the same level
    df <- df %>% group_by(.data$ID_XX) %>% 
        mutate(delta_D_temp_XX = mean(.data$delta_D_XX, na.rm = TRUE)) %>% ungroup()
    df$delta_D_XX <- df$delta_D_temp_XX
    df$delta_D_temp_XX <- NULL

    # Generate deltaY_t = Y_t - Y_(t-1) and put it at the same level
    df <- df %>% group_by(.data$ID_XX) %>% 
        mutate(delta_Y_temp_XX = mean(.data$delta_Y_XX, na.rm = TRUE)) %>% ungroup()
    df$delta_Y_XX <- df$delta_Y_temp_XX
    df$delta_Y_temp_XX <- NULL

    df[[paste0("used_in_",pairwise,"_XX")]] <- as.numeric(!is.na(df$delta_Y_XX) & !is.na(df$delta_D_XX))
    if (IV_req_XX == 1 & iwaoss == 1) {

    } else {
        df <- subset(df, df[[paste0("used_in_", pairwise, "_XX")]] == 1)
    }

    # Generate Switcher : S = 1 if switcher-up, -1 if switcher-down, 0 if stayer
    df$S_XX <- (df$delta_D_XX > 0) - (df$delta_D_XX < 0)
    if (!is.null(switchers)) {
        df <- subset(df, df$S_g_XX == (switchers == "up") - (switchers == "down"))
    }

    # We have all the variable we need at the first year so we can drop the 'second' year line
    df <- subset(df, df$T_XX == 1)




    ## End of the program

    for (v in names(scalars)) {
        scalars[[v]] <- get(v)
    }
    out_res <- list(scalars)
    names(out_res) <- c("scalars")
    return(out_res)
}