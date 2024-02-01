#' Internal function for did_continuous
#' @param df df
#' @param Y Y
#' @param ID ID
#' @param T T
#' @param D D
#' @param Z Z
#' @param estimator estimator
#' @param order order
#' @param noextrapolation noextrapolation
#' @param weight weight
#' @param switchers switchers
#' @param pairwise pairwise
#' @param aoss aoss
#' @param waoss waoss
#' @param iwaoss iwaoss
#' @param estimation_method estimation_method
#' @param scalars scalars
#' @param placebo placebo
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang := 
#' @importFrom rlang .data
#' @importFrom plm pdata.frame make.pbalanced
#' @importFrom stats as.formula lm sd
#' @noRd
did_continuous_pairwise <- function(
    df,
    Y,
    ID,
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
    scalars,
    placebo
) {
    # Preallocation of scalars
    IV_req_XX <- NULL

    for (v in names(scalars)) {
        assign(v, scalars[[v]])
    }

    ## Placebo ##
    if (isTRUE(placebo)) {
        df <- subset(df, df$T_XX %in% c(pairwise-2,pairwise-1,pairwise))
        pl <- "_pl"

    } else {
        df <- subset(df, df$T_XX %in% c(pairwise-1,pairwise))
        pl <- ""
    }


    ## Start of the program

    gap_XX <- max(df$tsfilled_XX, na.rm = TRUE)
    df <- df %>% group_by(.data$T_XX) %>% mutate(Tbis_XX = cur_group_id()) 
    df$T_XX <- df$Tbis_XX
    df$Tbis_XX <- NULL

    df <- pdata.frame(df, index = c("ID_XX", "T_XX")) 
    df$ID_XX <- as.numeric(as.character(df$ID_XX))
    df$T_XX <- as.numeric(as.character(df$T_XX))
    df <- df[order(df$ID_XX, df$T_XX), ]
    df$delta_Y_XX <- diff(df$Y_XX)
    df$delta_D_XX <- diff(df$D_XX)

    if (isTRUE(placebo))  {

    } else {
        # Generate deltaY_t = Y_t - Y_(t-1) and put it at the same level
        df <- df %>% group_by(.data$ID_XX) %>% 
            mutate(delta_Y_temp_XX = mean(.data$delta_Y_XX, na.rm = TRUE)) %>% ungroup()
        df$delta_Y_XX <- df$delta_Y_temp_XX
        df$delta_Y_temp_XX <- NULL
    }

    if (isTRUE(placebo)) {

    } 
    if (nrow(df) == 0) {
        # Write something to exit the program and go to the last section
    }

    # Generate deltaD_t = D_t - D_(t-1) and put it at the same level
    df <- df %>% group_by(.data$ID_XX) %>% 
        mutate(delta_D_temp_XX = mean(.data$delta_D_XX, na.rm = TRUE)) %>% ungroup()
    df$delta_D_XX <- df$delta_D_temp_XX
    df$delta_D_temp_XX <- NULL

    df[[paste0("used_in_",pairwise,"_XX")]] <- as.numeric(!is.na(df$delta_Y_XX) & !is.na(df$delta_D_XX))
    if (iwaoss == 1) {

    } else {
        df <- subset(df, df[[paste0("used_in_", pairwise, "_XX")]] == 1)
    }

    # Generate Switcher : S = 1 if switcher-up, -1 if switcher-down, 0 if stayer
    df$S_XX <- (df$delta_D_XX > 0) - (df$delta_D_XX < 0)
    if (!is.null(switchers)) {
        df <- subset(df, !(df$S_XX == (switchers == "down") - (switchers == "up")))
    }

    # We have all the variable we need at the first year so we can drop the 'second' year line
    df <- subset(df, df$T_XX != max(df$T_XX, na.rm = TRUE))

    df$D1_XX <- df$D_XX; df$D_XX <- NULL;
    if (isTRUE(noextrapolation) & (aoss == 1 | waoss == 1)) {

    }
    W_XX <- sum(df$weight_XX, na.rm = TRUE)
    N_XX <- nrow(df)

    # Panel with gaps (using tsfilled_XX) and cases where we have only switchers or only stayers (using count)
    N_Switchers_XX <- nrow(subset(df, df$S_XX != 0))
    N_Stayers_XX <- nrow(subset(df, df$S_XX == 0))

    if (is.null(estimation_method)) {
        approach <- "reg"
    } else {
        if (estimation_method == "ra") {
            approach <- "reg"
        } else if (estimation_method %in% c("ps", "dr")) {
            approach <- estimation_method
        } else {
            stop("Syntax error in estimation_method option.")
        }
    }

    vars_pol_XX <- c()
    for (pol_level in 1:order) {
        df[[paste0("D1_",pol_level,"_XX")]] <- df$D1_XX^pol_level
        vars_pol_XX <- c(vars_pol_XX, paste0("D1_",pol_level,"_XX"))
    }
    df$S_bis_XX <- df$S_XX != 0

    reg_pol_XX <- ""
    for (level in 1:length(vars_pol_XX)) {
        if (level > 1) {
            reg_pol_XX <- paste0(reg_pol_XX," + ")
        }
        reg_pol_XX <- paste0(reg_pol_XX,paste0("D1_",level,"_XX"))
    } 

    # Start of feasible estimation
    if (gap_XX == 0 & N_Switchers_XX > 0 & N_Stayers_XX > 1) {

        df0 <- subset(df, df$S_XX == 0)
        # \hat{E}(deltaY|D1, S=0)
        model <- lm(as.formula(paste("delta_Y_XX", reg_pol_XX, sep = "~")), 
                data = df0, weights = df0$weight_XX)
        df <- lpredict(df, "mean_pred_XX", model, vars_pol_XX)

        df$inner_sum_delta_1_2_XX <- df$delta_Y_XX -  df$mean_pred_XX

        if (waoss == 1 | aoss == 1) {
            df$S0_XX <- 1 - df$S_bis_XX
            model <- stata_logit(as.formula(paste("S0_XX",reg_pol_XX,sep="~")), df)
            df <- lpredict(df,"PS_0_D_1_XX", model, vars_pol_XX, prob = TRUE)

            PS_0_XX <- mean(df$S0_XX, na.rm = TRUE)
        }

        ####################################################### AOSS ##########

        if (aoss == 1) {
	        # 0) Compute P_t = P(S_t = 1) = E(S_t) for the aggregation afterward
            assign(paste0("P_",pairwise,pl,"_XX"), mean(df$S_bis_XX, na.rm = TRUE))
            assign(paste0("PS_sum",pl,"_XX"), get(paste0("PS_sum",pl,"_XX")) + get(paste0("P_",pairwise,pl,"_XX")))
            ES_XX <- get(paste0("P_",pairwise,pl,"_XX"))

            # 1) Compute \hat{delta}_1
            df$inner_sum_delta_1_XX <- df$inner_sum_delta_1_2_XX / df$delta_D_XX
            df$inner_sum_delta_1_XX <- ifelse(df$delta_D_XX == 0, NA, df$inner_sum_delta_1_XX)
            assign(paste0("delta_1_",pairwise,pl,"_XX"), mean(df$inner_sum_delta_1_XX, na.rm = TRUE))

            # 2) Compute the variance of \hat{delta}_1
            df$S_over_delta_D_XX <- df$S_bis_XX / df$delta_D_XX
            df$S_over_delta_D_XX <- ifelse(df$S_bis_XX == 0, 0, df$S_over_delta_D_XX)

            modelvar <- lm(as.formula(paste("S_over_delta_D_XX", reg_pol_XX, sep = "~")), 
                    data = df, weights = df$weight_XX)
            df <- lpredict(df, "mean_S_over_delta_D_XX", modelvar, vars_pol_XX)

		    # i. estimation of \hat{E}(S/deltaD|D1)
            df[[paste0("Phi_1_",pairwise,pl,"_XX")]] <- (df$S_over_delta_D_XX - df$mean_S_over_delta_D_XX * ((1 - df$S_bis_XX)/(df$PS_0_D_1_XX))) * df$inner_sum_delta_1_2_XX

            df[[paste0("Phi_1_",pairwise,pl,"_XX")]] <- (df[[paste0("Phi_1_",pairwise,pl,"_XX")]] - (get(paste0("delta_1_", pairwise,pl,"_XX")) * df$S_bis_XX)) / ES_XX

            assign(paste0("mean_IF_1_",pairwise,pl),
                    mean(df[[paste0("Phi_1_",pairwise,pl,"_XX")]], na.rm = TRUE))
            assign(paste0("sd_delta_1_",pairwise,pl,"_XX"),
                    sd(df[[paste0("Phi_1_",pairwise,pl,"_XX")]], na.rm = TRUE)/sqrt(N_XX))
            assign(paste0("LB_1_",pairwise,pl,"_XX"),
            get(paste0("delta_1_",pairwise,pl,"_XX")) - 1.96 * get(paste0("sd_delta_1_", pairwise,pl,"_XX")))
            assign(paste0("UB_1_",pairwise,pl,"_XX"),
            get(paste0("delta_1_",pairwise,pl,"_XX")) + 1.96 * get(paste0("sd_delta_1_", pairwise,pl,"_XX")))

            df[[paste0("S_",pairwise,pl,"_XX")]] <- df$S_bis_XX
        }
        
        ###################################################### WAOSS ##########

        if (waoss == 1) {
            df$abs_delta_D_XX <- df$S_XX * df$delta_D_XX
            E_abs_delta_D_XX <- mean(df$abs_delta_D_XX, na.rm = TRUE)
            assign(paste0("E_abs_delta_D_",pairwise,pl,"_XX"), E_abs_delta_D_XX)
            assign(paste0("E_abs_delta_D_sum",pl,"_XX"), get(paste0("E_abs_delta_D_sum",pl,"_XX")) + get(paste0("E_abs_delta_D_",pairwise,pl,"_XX")))

            for (suffix in c("Minus", "Plus")) {
                df$Ster_XX <- NULL
                df$Ster_XX <- df$S_XX == as.numeric((suffix == "Plus") - (suffix == "Minus"))
                
                ## Computing the contribution weights ##
                df$prod_sgn_delta_D_delta_D_XX <- df$S_XX * df$delta_D_XX
                sum_prod_sgn_delta_D_delta_D_XX <- sum(df$prod_sgn_delta_D_delta_D_XX[df$Ster_XX == 1], na.rm = TRUE)
                assign(paste0("w_",suffix,"_",pairwise,pl,"_XX"), sum_prod_sgn_delta_D_delta_D_XX/N_XX)
                assign(paste0("denom_delta_2_",suffix,"_",pairwise,pl,"_XX"), sum(df$delta_D_XX[df$Ster_XX == 1], na.rm = TRUE))

                if (approach == "reg") {
                    if (get(paste0("denom_delta_2_",suffix,"_",pairwise,pl,"_XX")) == 0) {
                        assign(paste0("denom_delta_2_",suffix,"_",pairwise,pl,"_XX"), 1)
                        # in case it is zero set it to 1 to avoid dividing by 0, in that case the numerator is also equal to 0	
                    }
                    assign(paste0("num_delta_2_",suffix,"_",pairwise,pl,"_XX"),
                    sum(df$inner_sum_delta_1_2_XX[df$Ster_XX == 1], na.rm = TRUE))

                    assign(paste0("delta_2_",suffix,"_",pairwise,pl,"_XX"), 
                        get(paste0("num_delta_2_",suffix,"_",pairwise,pl,"_XX")) /
                        get(paste0("denom_delta_2_",suffix,"_",pairwise,pl,"_XX")))
                } 

                assign(paste0("nb_Switchers_",suffix,pl,"_XX"), nrow(subset(df, df$Ster_XX ==1)))
                assign(paste0("PS_",suffix,1,pl,"_XX"), get(paste0("nb_Switchers_",suffix,pl,"_XX"))/N_XX)
                if (get(paste0("PS_",suffix,"1",pl,"_XX")) == 0) {
                    # The regression is performed iff there is at least one switcher up/down.
                    assign(paste0("delta_2_",suffix,"_",pairwise,pl,"_XX"), 0)
                    df[[paste0("PS_1_",suffix,"_D_1_XX")]] <- 0
                } else {
                    model <- stata_logit(as.formula(paste("Ster_XX",reg_pol_XX,sep="~")), df)
                    df <- lpredict(df,paste0("PS_1_",suffix,"_D_1_XX"),model, vars_pol_XX, prob = TRUE)

                    if (approach == "ps") {
                        df[[paste0("delta_Y_P_",suffix,"_XX")]] <- df$delta_Y_XX * (df[[paste0("PS_1_",suffix,"_D_1_XX")]]/df$PS_0_D_1_XX) * (PS_0_XX / get(paste0("PS_",suffix,"1",pl,"_XX")))
                        assign(paste0("mean_delta_Y_P_",suffix,"_XX"), 
                        mean(df[[paste0("delta_Y_P_",suffix,"_XX")]][df$S_XX == 0], na.rm=TRUE))
                        mean_delta_Y_XX <- mean(df$delta_Y_XX[df$Ster_XX == 1], na.rm = TRUE)
                        mean_delta_D_XX <- mean(df$delta_D_XX[df$Ster_XX == 1], na.rm = TRUE)
                        assign(paste0("delta_2_",suffix,"_",pairwise,pl,"_XX"), 
                        (mean_delta_Y_XX - get(paste0("mean_delta_Y_P_",suffix,"_XX"))) / mean_delta_D_XX)
                    }
                }                
            }

            if (approach == "reg" | approach == "ps") {
                ## Computing the final weights ##
                assign(paste0("W_Plus_",pairwise,"_XX"), get(paste0("w_Plus_",pairwise,"_XX")) /
                (get(paste0("w_Plus_",pairwise,"_XX")) + get(paste0("w_Minus_",pairwise,"_XX"))))
            }

            df$dr_delta_Y_XX <- (df$S_XX - ((df$PS_1_Plus_D_1_XX - df$PS_1_Minus_D_1_XX)/df$PS_0_D_1_XX) * (1 - df$S_bis_XX)) * df$inner_sum_delta_1_2_XX
            denom_dr_delta_2_XX <- sum(df$dr_delta_Y_XX, na.rm = TRUE)

            if (approach == "reg" | approach == "ps") {
                assign(paste0("delta_2_",pairwise,pl,"_XX"), 
                get(paste0("W_Plus_",pairwise,"_XX"))*get(paste0("delta_2_Plus_",pairwise,pl,"_XX")) +
                (1-get(paste0("W_Plus_",pairwise,"_XX")))*get(paste0("delta_2_Minus_",pairwise,pl,"_XX")))
            } else if (approach == "dr") {
                sum_abs_delta_D_XX <- sum(df$abs_delta_D_XX, na.rm = TRUE)
                assign(paste0("delta_2_",pairwise,pl,"_XX"), denom_dr_delta_2_XX / sum_abs_delta_D_XX)
            }

            # Computing the variance
            df[[paste0("Phi_2_",pairwise,pl,"_XX")]] <- (df$dr_delta_Y_XX - get(paste0("delta_2_", pairwise,pl,"_XX")) * df$abs_delta_D_XX) / E_abs_delta_D_XX

            assign(paste0("mean_IF_2_",pairwise,pl),
                    mean(df[[paste0("Phi_2_",pairwise,pl,"_XX")]], na.rm = TRUE))
            assign(paste0("sd_delta_2_",pairwise,pl,"_XX"),
                    sd(df[[paste0("Phi_2_",pairwise,pl,"_XX")]], na.rm = TRUE)/sqrt(N_XX))
            assign(paste0("LB_2_",pairwise,pl,"_XX"), get(paste0("delta_2_",pairwise,pl,"_XX")) - 1.96 * get(paste0("sd_delta_2_", pairwise,pl,"_XX")))
            assign(paste0("UB_2_",pairwise,pl,"_XX"), get(paste0("delta_2_",pairwise,pl,"_XX")) + 1.96 * get(paste0("sd_delta_2_", pairwise,pl,"_XX")))

            df[[paste0("abs_delta_D_",pairwise,pl,"_XX")]] <- df$abs_delta_D_XX
        }

        assign(paste0("non_missing_",pairwise,"_XX"), 1)
    } else {
        for (i in 1:3) {
            assign(paste0("delta_",i,"_",pairwise,pl,"_XX"), 0)
            assign(paste0("sd_delta_",i,"_",pairwise,pl,"_XX"), NA)
            assign(paste0("LB_",i,"_",pairwise,pl,"_XX"), NA)
            assign(paste0("UB_",i,"_",pairwise,pl,"_XX"), NA)
            df[[paste0("Phi",i,"_",pairwise,pl,"_XX")]] <- NA

            if (gap_XX != 0) {
                N_Switchers_XX <- NA
                N_Stayers_XX <- NA
            }
            if (N_Stayers_XX < 2) {
                N_Switchers_XX <- nrow(df)
                N_Stayers_XX <- 0
            }
            if (N_Switchers_XX == 0) {
                N_Switchers_XX <- 0
                N_Sayers_XX <- nrow(df)
            }
        }
        df[[paste0("abs_delta_D_",pairwise,"_XX")]] <- NA
        df[[paste0("S_",pairwise,"_XX")]] <- NA
        assign(paste0("E_abs_delta_D_",pairwise,"_XX"), 0)
        assign(paste0("P_",pairwise,"_XX"), 0)
        assign(paste0("non_missing_",pairwise,"_XX"), 0)
    }

    df <- df[order(df$ID_XX), ]
    to_keep <- c("ID_XX", paste0("Phi_1_",pairwise,"_XX"), paste0("Phi_2_",pairwise,"_XX"), paste0("Phi_3_",pairwise,"_XX"), 
        paste0("S_",pairwise,"_XX"), paste0("abs_delta_D_",pairwise,"_XX"), paste0("used_in_",pairwise,"_XX")) 
    df <- df %>% select(dplyr::any_of(to_keep))

    ## End of the program

    for (v in names(scalars)) {
        scalars[[v]] <- get(v)
    }
    if (aoss == 1) {
        scalars[[paste0("P_",pairwise,"_XX")]] <- get(paste0("P_",pairwise,"_XX"))
    }
    if (waoss == 1) {
        scalars[[paste0("E_abs_delta_D_",pairwise,"_XX")]] <- get(paste0("E_abs_delta_D_",pairwise,"_XX"))
    }

    scalars[[paste0("non_missing_",pairwise,"_XX")]] <- get(paste0("non_missing_",pairwise,"_XX"))
    for (v in c("Switchers", "Stayers")) {
        scalars[[paste0("N_",v,"_1_",pairwise,pl,"_XX")]] <- get(paste0("N_",v,"_XX"))
        scalars[[paste0("N_",v,"_2_",pairwise,pl,"_XX")]] <- get(paste0("N_",v,"_XX"))
    }

    estims <- c("aoss", "waoss", "iwaoss")
    indices <- c() 
    for (j in 1:length(estims)) {
        if (get(estims[j]) == 1) {
            indices <- c(indices, j)
        }
    }

    for (i in indices) {
        scalars[[paste0("delta_",i,"_",pairwise,"_XX")]] <- get(paste0("delta_",i,"_",pairwise,"_XX"))
        scalars[[paste0("sd_delta_",i,"_",pairwise,"_XX")]] <- get(paste0("sd_delta_",i,"_",pairwise,"_XX"))
        scalars[[paste0("LB_",i,"_",pairwise,"_XX")]] <- get(paste0("LB_",i,"_",pairwise,"_XX"))
        scalars[[paste0("UB_",i,"_",pairwise,"_XX")]] <- get(paste0("UB_",i,"_",pairwise,"_XX"))
    }

    out_res <- list(scalars, list(df))
    names(out_res) <- c("scalars", "to_add")
    return(out_res)
}