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
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang := 
#' @importFrom rlang .data
#' @importFrom plm pdata.frame make.pbalanced
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

    # Layer 1: keep only variables of interest, as to speed up what follows
    df <- df[c(Y, G, T, D, Z, weight)]
    df <- df %>% group_by(.data[[T]]) %>% mutate(T_temp_XX = cur_group_id())
    coln <- c("Y_XX", "ID_XX", "T_XX", "D_XX", "weight_XX", "tsfilled_XX")
    df$to_drop_XX <- (is.na(df[[Y]]) | is.na(df$T_temp_XX) | is.na(df[[D]]) | is.na(df[[G]]))
    IV_req_XX <- 0
    if (!is.null(Z)) {
        df$to_drop_XX <- (is.na(df[[Z]]) | df$to_drop_XX)
        IV_req_XX <- 1
        coln <- c(coln, "Z_XX")
    }
    df <- subset(df, df$to_drop_XX == 0)

    # Layer 2: balancing the panel and then keeping again only variables of interest
    df$tsfilled_XX <- 0
    df <- pdata.frame(df, index = c(G, T)) 
    df <- make.pbalanced(df, balance.type = "fill")
    df$tsfilled_XX <- is.na(df$tsfilled_XX)
    df <- df %>% group_by(.data[[T]]) %>% mutate(T_XX = cur_group_id())
    df$weight_XX <- 1
    if (!is.null(weight)) {
        df$weight_XX <- df[[weight]]
    }
    df <- df[c(Y, G, "T_XX", D, "weight_XX", "tsfilled_XX", Z)]
    names(df) <- coln

    # Patching the estimator option
    for (v in c("aoss", "waoss", "iwaoss")) {
        if (is.null(estimator)) {
            assign(paste0(v,"_XX"), 1)
        } else {
            assign(paste0(v,"_XX"), as.numeric(v %in% estimator))
        }
    }

    if (IV_req_XX == 0 & (iwaoss_XX == 1 | is.null(estimator))) {
        stop("To compute the iwaoss you must specify the IV variable.")
    }

    # Further useful steps prior to the estimation
    IDs_XX <- as.vector(unique(factor(df$ID_XX)))
    max_T_XX <- max(df$T_XX, na.rm = TRUE)
    scalars <- list(
        PS_sum_XX = 0,
        delta1_1XX = 0,
        E_abs_delta_D_sum_XX = 0,
        delta_2_1_XX = 0,
        N_Switchers_2_1XX = 0,
        N_Stayers_2_1_XX = 0,
        N_Switchers_1_1_XX = 0,
        N_Stayers_1_1_XX = 0,
        IV_req_XX = IV_req_XX
    )

    for (p in 2:max_T_XX) {
        p_df <- subset(df, df$T_XX %in% c(p-1,p))

        est_out <- did_continuous_pairwise(df = p_df, Y = "Y_ID", G = "ID_XX", T = "T_XX", D = "D_XX", Z = "Z_XX", estimator = estimator, order = order, noextrapolation = noextrapolation, weight = "weight_XX", switchers = switchers, pairwise = p, IDs = IDs_XX, aoss = aoss_XX, waoss = waoss_XX, iwaoss = iwaoss_XX, estimation_method = estimation_method, scalars = scalars)

        for (v in names(est_out$scalars)) {
            assign(v, est_out$scalars[[v]])
        }
        est_out <- NULL
    }


}