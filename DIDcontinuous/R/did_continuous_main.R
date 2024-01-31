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
#' @param noextrapolation noextrapolation
#' @param placebo placebo
#' @param weight weight
#' @param switchers switchers
#' @param disaggregate disaggregate
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang := 
#' @importFrom rlang .data
#' @importFrom plm pdata.frame make.pbalanced
#' @importFrom stats sd
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

    # Preallocation of scalars
    aoss_XX <- NULL
    waoss_XX <- NULL
    iwaoss_XX <- NULL

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
    IDs_XX <- as.data.frame(unique(factor(df$ID_XX)))
    names(IDs_XX) <- "ID_XX"

    max_T_XX <- max(df$T_XX, na.rm = TRUE)
    scalars <- list(
        PS_sum_XX = 0,
        delta_1_1_XX = 0,
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

        est_out <- did_continuous_pairwise(df = p_df, Y = "Y_ID", G = "ID_XX", T = "T_XX", D = "D_XX", Z = "Z_XX", estimator = estimator, order = order, noextrapolation = noextrapolation, weight = "weight_XX", switchers = switchers, pairwise = p, aoss = aoss_XX, waoss = waoss_XX, iwaoss = iwaoss_XX, estimation_method = estimation_method, scalars = scalars)

        IDs_XX <- merge(IDs_XX, est_out$to_add, by = "ID_XX", all = TRUE) 
        IDs_XX <- IDs_XX[order(IDs_XX$ID_XX), ]
        scalars <- est_out$scalars;
        est_out <- NULL;

        if (aoss_XX == 1) {
            scalars$delta_1_1_XX <- scalars$delta_1_1_XX + 
                    scalars[[paste0("P_",p,"_XX")]] * scalars[[paste0("delta_1_",p,"_XX")]]
            
            if (scalars[[paste0("N_Stayers_1_",p,"_XX")]] > 1)  {
                scalars$N_Switchers_1_1_XX <- scalars$N_Switchers_1_1_XX + scalars[[paste0("N_Switchers_1_",p,"_XX")]]
            }
            if (scalars[[paste0("N_Switchers_1_",p,"_XX")]] > 0)  {
                scalars$N_Stayers_1_1_XX <- scalars$N_Stayers_1_1_XX + scalars[[paste0("N_Stayers_1_",p,"_XX")]]
            }
        }

        if (waoss_XX == 1) {
            scalars$delta_2_1_XX <- scalars$delta_2_1_XX + 
                    scalars[[paste0("E_abs_delta_D_",p,"_XX")]] * scalars[[paste0("delta_2_",p,"_XX")]]
            
            if (scalars[[paste0("N_Stayers_2_",p,"_XX")]] > 1)  {
                scalars$N_Switchers_2_1_XX <- scalars$N_Switchers_2_1_XX + scalars[[paste0("N_Switchers_2_",p,"_XX")]]
            }
            if (scalars[[paste0("N_Switchers_2_",p,"_XX")]] > 0)  {
                scalars$N_Stayers_2_1_XX <- scalars$N_Stayers_2_1_XX + scalars[[paste0("N_Stayers_2_",p,"_XX")]]
            }
        }
    }

    # Compute the aggregated estimators
    if (aoss_XX == 1) {
        scalars$delta_1_1_XX <- scalars$delta_1_1_XX / scalars$PS_sum_XX
    }
    if (waoss_XX == 1) {
        scalars$delta_1_1_XX <- scalars$delta_2_1_XX / scalars$E_abs_delta_D_sum_XX
    }

	# Compute the influence functions

    IDs_XX$Phi_1_XX <- 0
    counter_XX <- 0
    for (p in 2:max_T_XX) {
        if (scalars[[paste0("non_missing_",p,"_XX")]] == 0) {
            next
        }
        if (aoss_XX == 1) {

            IDs_XX[[paste0("Phi_1_",p,"_XX")]] <- (scalars[[paste0("P_",p,"_XX")]]*IDs_XX[[paste0("Phi_1_",p,"_XX")]] + (scalars[[paste0("delta_1_",p,"_XX")]] - scalars$delta_1_1_XX) * (IDs_XX[[paste0("S_",p,"_XX")]] - scalars[[paste0("P_",p,"_XX")]])) / scalars$PS_sum_XX

            IDs_XX$Phi_1_XX <- ifelse(is.na(IDs_XX[[paste0("Phi_1_",p,"_XX")]]), IDs_XX$Phi_1_XX, IDs_XX$Phi_1_XX + IDs_XX[[paste0("Phi_1_",p,"_XX")]])
            counter_XX <- counter_XX + 1
        }
        if (waoss_XX == 1) {
            IDs_XX[[paste0("Phi_2_",p,"_XX")]] <- (scalars[[paste0("E_abs_delta_D_",p,"_XX")]]*IDs_XX[[paste0("Phi_2_",p,"_XX")]] + (scalars[[paste0("delta_2_",p,"_XX")]] - scalars$delta_2_1_XX) * (IDs_XX[[paste0("abs_delta_D_",p,"_XX")]] - scalars[[paste0("E_abs_delta_D_",p,"_XX")]])) / scalars$E_abs_delta_D_sum_XX
        }
    }
    View(IDs_XX)

    if (aoss_XX == 1) {
        n_obs <- nrow(subset(IDs_XX, !is.na(IDs_XX$Phi_1_XX)))
        scalars$mean_IF1 <- ifelse(counter_XX == 0, NA, mean(IDs_XX$Phi_1_XX, na.rm = TRUE))
        scalars$sd_delta_1_1_XX <- ifelse(counter_XX == 0, NA, sd(IDs_XX$Phi_1_XX, na.rm = TRUE)/ sqrt(n_obs))
        scalars$LB_1_1_XX <-  scalars$delta_1_1_XX - 1.96 *  scalars$sd_delta_1_1_XX
        scalars$UB_1_1_XX <-  scalars$delta_1_1_XX + 1.96 *  scalars$sd_delta_1_1_XX            
    }

    # Returning the results #

    IDs_XX <- NULL
    estims <- c("aoss", "waoss", "iwaoss")

    ret_mat_XX <- matrix(NA, nrow = 3*max_T_XX, ncol = 6)
    rown <- c()
    for (j in 1:length(estims)) {
        for (p in 1:max_T_XX) {
            if (get(paste0(estims[j],"_XX")) == 1) {
                if (((is.na(scalars[[paste0("N_Stayers_",j,"_",p,"_XX")]]) & is.na(scalars[[paste0("N_Switchers_",j,"_",p,"_XX")]])) | scalars[[paste0("N_Stayers_",j,"_",p,"_XX")]] < 2 | scalars[[paste0("N_Switchers_",j,"_",p,"_XX")]] == 0) & p != 1) {
                    scalars[[paste0("delta_",j,"_",p,"_XX")]] <- NA
                }
                ret_mat_XX[(j-1)*max_T_XX + p, 1] <- scalars[[paste0("delta_",j,"_",p,"_XX")]]
                ret_mat_XX[(j-1)*max_T_XX + p, 2] <- scalars[[paste0("sd_delta_",j,"_",p,"_XX")]]
                ret_mat_XX[(j-1)*max_T_XX + p, 3] <- scalars[[paste0("LB_",j,"_",p,"_XX")]]
                ret_mat_XX[(j-1)*max_T_XX + p, 4] <- scalars[[paste0("UB_",j,"_",p,"_XX")]]
                ret_mat_XX[(j-1)*max_T_XX + p, 5] <- scalars[[paste0("N_Switchers_",j,"_",p,"_XX")]]
                ret_mat_XX[(j-1)*max_T_XX + p, 6] <- scalars[[paste0("N_Stayers_",j,"_",p,"_XX")]]   

            }

            if (p == 1) {
                rown <- c(rown, toupper(estims[j]))
            } else {
                rown <- c(rown, paste0(estims[j],"_",p))
            }
        }
    }
    rownames(ret_mat_XX) <- rown
    colnames(ret_mat_XX) <- c("Estimate", "SE", "LB CI", "UB CI", "Switchers", "Stayers")

    out <- list(table = ret_mat_XX, pairs = max_T_XX)
    return(out)
}