#' Internal function for estimation of pairwise DiD between consecutive time periods.
#' @param df df
#' @param Y Y
#' @param ID ID
#' @param Time Time
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
#' @param exact_match exact_match
#' @param cluster cluster
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang := 
#' @importFrom rlang .data
#' @importFrom plm pdata.frame make.pbalanced
#' @importFrom stats as.formula lm sd
#' @returns A list with two elements. The first element is a sublist of scalars that are updated throughout the run of the command and across each pair of consecutive periods. The second element is a dataframe with the group-specific variables (among which, the influence function) that will be aggregated in the main program.
#' @noRd
did_multiplegt_stat_pairwise <- function(
    df,
    Y,
    ID,
    Time,
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
    placebo,
    exact_match,
    cluster
) {
    # Preallocation of scalars
    IV_req_XX <- NULL
    PS_0_XX <- NULL

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
    df <- df %>% group_by(.data$T_XX) %>% 
            mutate(tsfilled_min_XX = min(.data$tsfilled_XX, na.rm = TRUE)) 
    gap_XX <- max(df$tsfilled_min_XX, na.rm = TRUE)

    df <- df %>% group_by(.data$T_XX) %>% mutate(Tbis_XX = cur_group_id()) 
    df$T_XX <- df$Tbis_XX
    df$Tbis_XX <- NULL

    df <- pdata.frame(df, index = c("ID_XX", "T_XX")) 
    df$ID_XX <- as.numeric(as.character(df$ID_XX))
    df$T_XX <- as.numeric(as.character(df$T_XX))
    df <- df[order(df$ID_XX, df$T_XX), ]
    df$delta_Y_XX <- diff(df$Y_XX)
    df$delta_D_XX <- diff(df$D_XX)

    if (iwaoss == 1) {
        df$delta_Z_XX <- diff(df$Z_XX)        
    }

    if (isTRUE(placebo))  {
        df$delta_temp <- ifelse(df$T_XX == 2, df$delta_Y_XX, NA)
        df <- df %>% group_by(.data$ID_XX) %>% 
                mutate(delta_temp2 = mean(.data$delta_temp, na.rm = TRUE))
        df$delta_Y_XX <- NULL; df$delta_Y_XX <- df$delta_temp2; 
        df$delta_temp <- NULL; df$delta_temp2 <- NULL;
    } else {
        # Generate deltaY_t = Y_t - Y_(t-1) and put it at the same level
        df <- df %>% group_by(.data$ID_XX) %>% 
            mutate(delta_Y_temp_XX = mean(.data$delta_Y_XX, na.rm = TRUE)) %>% ungroup()
        df$delta_Y_XX <- df$delta_Y_temp_XX
        df$delta_Y_temp_XX <- NULL
    }

    if (isTRUE(placebo) & (aoss == 1 | waoss == 1)) {
        # Units s.t. D_{t-2} = D_{t-1}
        df$inSamplePlacebo_temp_XX <- (df$delta_D_XX == 0 & df$T_XX == 2)  
        df <- df %>% group_by(.data$ID_XX) %>%
                mutate(inSamplePlacebo_XX = max(.data$inSamplePlacebo_temp_XX, na.rm = TRUE))
        
	    # Only keep Units such that D_{t-2} = D_{t-1}
        df <- subset(df, df$inSamplePlacebo_XX == 1)
        # we do not need that line since we've already computed y_{t-2} - y_{t-1}, and selected units such that d_{t-2} = d_{t-1} // and evrything that follows is the same as the computation of the effects:)
        df <- subset(df, df$T_XX != 1)
        # We need the DeltaD_t only and we will take the mean after to keep the same value for all the dates
        df$delta_D_XX <- ifelse(df$T_XX != 3, NA,  df$delta_D_XX)
    } 


    if (isTRUE(placebo) & iwaoss == 1) {
        df$inSamplePlacebo_IV_temp_XX <- (df$delta_Z_XX == 0 & df$T_XX == 2)  
        df <- df %>% group_by(.data$ID_XX) %>%
                mutate(inSamplePlacebo_IV_XX = max(.data$inSamplePlacebo_IV_temp_XX, na.rm = TRUE))
        
	    # Only keep Units such that D_{t-2} = D_{t-1}
        df <- subset(df, df$inSamplePlacebo_IV_XX == 1)
        # we do not need that line since we've already computed y_{t-2} - y_{t-1}, and selected units such that z_{t-2} = z_{t-1} // and evrything that follows is the same as the computation of the effects:)
        df <- subset(df, df$T_XX != 1)
        # We need the Delta_Z_t only and we will take the mean after to keep the same value for all the dates
        df$delta_Z_XX <- ifelse(df$T_XX != 3, NA,  df$delta_Z_XX)
    }

    if (nrow(df) == 0) {
        # Since there are no obs, we exit the program
        for (v in names(scalars)) {
            scalars[[v]] <- get(v)
        }

        if (aoss == 1) {
            scalars[[paste0("P_",pairwise,pl,"_XX")]] <- 0
        }
        if (waoss == 1) {
            scalars[[paste0("E_abs_delta_D_",pairwise,pl,"_XX")]] <- 0
        }

        if (iwaoss == 1) {
            scalars[[paste0("denom_delta_IV_",pairwise,pl,"_XX")]] <- 0
        }

        scalars[[paste0("non_missing_",pairwise,pl,"_XX")]] <- 0
        for (v in c("Switchers", "Stayers")) {
            for (n in 1:3) {
                scalars[[paste0("N_",v,"_",n,"_",pairwise,pl,"_XX")]] <- 0
            }
        }

        estims <- c("aoss", "waoss", "iwaoss")
        indices <- c() 
        for (j in 1:length(estims)) {
            if (get(estims[j]) == 1) {
                indices <- c(indices, j)
            }
        }

        for (i in indices) {
            scalars[[paste0("delta_",i,"_",pairwise,pl,"_XX")]] <- 0
            scalars[[paste0("sd_delta_",i,"_",pairwise,pl,"_XX")]] <- NA
            scalars[[paste0("LB_",i,"_",pairwise,pl,"_XX")]] <- NA
            scalars[[paste0("UB_",i,"_",pairwise,pl,"_XX")]] <- NA
        }

        out_res <- list(scalars = scalars, to_add = NULL)
        names(out_res) <- c("scalars", "to_add")
        return(out_res)
    }

    # Generate deltaD_t = D_t - D_(t-1) and put it at the same level
    df <- df %>% group_by(.data$ID_XX) %>% 
        mutate(delta_D_temp_XX = mean(.data$delta_D_XX, na.rm = TRUE)) %>% ungroup()
    df$delta_D_XX <- df$delta_D_temp_XX
    df$delta_D_temp_XX <- NULL


    if (iwaoss == 1) {
        df <- df %>% group_by(.data$ID_XX) %>% 
            mutate(delta_Z_temp_XX = mean(.data$delta_Z_XX, na.rm = TRUE)) %>% ungroup()
        df$delta_Z_XX <- df$delta_Z_temp_XX
        df$delta_Z_temp_XX <- NULL

        df$SI_XX <- (df$delta_Z_XX > 0) - (df$delta_Z_XX < 0) 
        # This is equivalent to sgn(delta_X)
        df$Z1_XX <- NULL; df$Z1_XX <- df$Z_XX; df$Z_XX <- NULL;
    }

    df[[paste0("used_in_",pairwise,"_XX")]] <- as.numeric(!is.na(df$delta_Y_XX) & !is.na(df$delta_D_XX))
    if (iwaoss == 1) {
        df[[paste0("used_in_IV_",pairwise,"_XX")]] <- as.numeric(df[[paste0("used_in_",pairwise,"_XX")]] == 1 & !is.na(df$delta_Z_XX))
        df <- subset(df, df[[paste0("used_in_IV_", pairwise, "_XX")]] == 1)

    } else {
        df <- subset(df, df[[paste0("used_in_", pairwise, "_XX")]] == 1)
    }

    # Generate Switcher : S = 1 if switcher-up, -1 if switcher-down, 0 if stayer
    df$S_XX <- (df$delta_D_XX > 0) - (df$delta_D_XX < 0)

    if (waoss == 1 | aoss == 1) {
        if (!is.null(switchers)) {
            df <- subset(df, !(df$S_XX == (switchers == "down") - (switchers == "up")))
        }
    }
    if (iwaoss == 1) {
        if (!is.null(switchers)) {
            df <- subset(df, !(df$SI_XX == (switchers == "down") - (switchers == "up")))
        }
        df$abs_delta_Z_XX <- df$SI_XX * df$delta_Z_XX
    }
    df$abs_delta_D_XX <- df$S_XX * df$delta_D_XX

    # We have all the variable we need at the first year so we can drop the 'second' year line
    df <- subset(df, df$T_XX != max(df$T_XX, na.rm = TRUE))

    df$D1_XX <- df$D_XX; df$D_XX <- NULL;
    if (isTRUE(noextrapolation)) {
        if (aoss == 1 | waoss == 1) {
            assign(paste0("max_D1",pl,"_XX"), max(df$D1_XX[df$S_XX == 0], na.rm = TRUE))
            assign(paste0("min_D1",pl,"_XX"), min(df$D1_XX[df$S_XX == 0], na.rm = TRUE))
            df$outofBounds_XX <- (df$D1_XX < get(paste0("min_D1",pl,"_XX")) |
                    df$D1_XX > get(paste0("max_D1",pl,"_XX"))) 
            assign(paste0("N_drop_",pairwise,pl,"_XX"), sum(df$outofBounds_XX, na.rm = TRUE))
            df <- subset(df, df$outofBounds_XX != 1)

            if (get(paste0("N_drop_",pairwise,pl,"_XX")) > 0 & isFALSE(placebo) & gap_XX == 0) {
                N_drop_total_XX <- N_drop_total_XX + get(paste0("N_drop_",pairwise,pl,"_XX"))

            }           
        }
        if (iwaoss == 1) {
            assign(paste0("max_Z1",pl,"_XX"), max(df$Z1_XX[df$SI_XX == 0], na.rm = TRUE))
            assign(paste0("min_Z1",pl,"_XX"), min(df$Z1_XX[df$SI_XX == 0], na.rm = TRUE))
            df$outofBoundsIV_XX <- (df$Z1_XX < get(paste0("min_Z1",pl,"_XX")) |
                    df$Z1_XX > get(paste0("max_Z1",pl,"_XX"))) 
            assign(paste0("N_IVdrop_",pairwise,pl,"_XX"), sum(df$outofBoundsIV_XX, na.rm = TRUE))
            df <- subset(df, df$outofBoundsIV_XX != 1)
          
            if (get(paste0("N_IVdrop_",pairwise,pl,"_XX")) > 0 & isFALSE(placebo) & gap_XX == 0) {
                N_drop_total_XX <- N_drop_total_XX + get(paste0("N_IVdrop_",pairwise,pl,"_XX"))

            }           
        }
    }
    if (isTRUE(exact_match)) {
        if (aoss == 1 | waoss == 1) {
            df <- df %>% group_by(.data$D1_XX) %>% 
                    mutate(has_match_temp_XX = min(.data$abs_delta_D_XX, na.rm = TRUE)) %>% ungroup()
            df$has_match_XX <- as.numeric(df$has_match_temp_XX == 0)
            df$has_match_XX <- ifelse(df$S_XX == 0, NA, df$has_match_XX)
            df$has_match_temp_XX <- NULL
        } else if (iwaoss == 1) {
            df <- df %>% group_by(.data$Z1_XX) %>% 
                    mutate(has_match_temp_XX = min(.data$abs_delta_Z_XX, na.rm = TRUE)) %>% ungroup()
            df$has_match_XX <- as.numeric(df$has_match_temp_XX == 0)
            df$has_match_XX <- ifelse(df$SI_XX == 0, NA, df$has_match_XX)
            df$has_match_temp_XX <- NULL
        }
        assign(paste0("N_drop_",pairwise,pl,"_XX"), nrow(subset(df, df$has_match_XX == 0)))
        df <- subset(df, (!is.na(df$has_match_XX) & df$has_match_XX != 0) | is.na(df$has_match_XX))
        if (get(paste0("N_drop_",pairwise,pl,"_XX")) > 0 & gap_XX == 0) {
            N_drop_total_XX <- N_drop_total_XX + get(paste0("N_drop_",pairwise,pl,"_XX")) 
        }

        order <- length(unique(df$D1_XX))
    }
    assign(paste0("W",pl,"_XX"), sum(df$weight_XX, na.rm = TRUE))
    assign(paste0("N",pl,"_XX"), nrow(df))

    # Panel with gaps (using tsfilled_XX) and cases where we have only switchers or only stayers (using count)
    if (waoss == 1 | aoss == 1) {
        assign(paste0("N_Switchers",pl,"_XX"), nrow(subset(df, df$S_XX != 0)))
        assign(paste0("N_Stayers",pl,"_XX"),nrow(subset(df, df$S_XX == 0)))
    }
    if (iwaoss == 1) {
        assign(paste0("N_Switchers_IV",pl,"_XX"), nrow(subset(df, df$SI_XX != 0)))
        assign(paste0("N_Stayers_IV",pl,"_XX"),nrow(subset(df, df$SI_XX == 0)))
    }

    vars_pol_XX <- c()
    for (pol_level in 1:order) {
        df[[paste0("D1_",pol_level,"_XX")]] <- df$D1_XX^pol_level
        vars_pol_XX <- c(vars_pol_XX, paste0("D1_",pol_level,"_XX"))
    }
    reg_pol_XX <- ""
    for (level in 1:length(vars_pol_XX)) {
        if (level > 1) {
            reg_pol_XX <- paste0(reg_pol_XX," + ")
        }
        reg_pol_XX <- paste0(reg_pol_XX,paste0("D1_",level,"_XX"))
    } 

    if (iwaoss == 1) {
        IV_vars_pol_XX <- c()
        for (pol_level in 1:order) {
            df[[paste0("Z1_",pol_level,"_XX")]] <- df$Z1_XX^pol_level
            IV_vars_pol_XX <- c(IV_vars_pol_XX, paste0("Z1_",pol_level,"_XX"))
        }
        IV_reg_pol_XX <- ""
        for (level in 1:length(IV_vars_pol_XX)) {
            if (level > 1) {
                IV_reg_pol_XX <- paste0(IV_reg_pol_XX," + ")
            }
            IV_reg_pol_XX <- paste0(IV_reg_pol_XX,paste0("Z1_",level,"_XX"))
        } 
    }

    df$S_bis_XX <- df$S_XX != 0 & !is.na(df$S_XX)

    # Feasibility conditions:
    feasible_est <- FALSE
    if (aoss == 1 | waoss == 1) {
        feasible_est <- (gap_XX == 0 & get(paste0("N_Switchers",pl,"_XX")) > 0 & get(paste0("N_Stayers",pl,"_XX")) > 1)
    } else if (iwaoss == 1) {
        feasible_est <- (gap_XX == 0 & get(paste0("N_Switchers_IV",pl,"_XX")) > 0 & get(paste0("N_Stayers_IV",pl,"_XX")) > 1)
    }

    # Start of feasible estimation
    if (feasible_est) {       

        if (waoss == 1 | aoss == 1) {

            df0 <- subset(df, df$S_XX == 0)
            # \hat{E}(deltaY|D1, S=0)
            model <- lm(as.formula(paste("delta_Y_XX", reg_pol_XX, sep = "~")), 
                    data = df0, weights = df0$weight_XX)
            df <- lpredict(df, "mean_pred_XX", model, vars_pol_XX)

            df$inner_sum_delta_1_2_XX <- df$delta_Y_XX -  df$mean_pred_XX

            df$S0_XX <- 1 - df$S_bis_XX

            if (isFALSE(exact_match)) {
                ## 1. Estimation of P(S = 0|D_1) 
                model <- stata_logit(as.formula(paste("S0_XX",reg_pol_XX,sep="~")), df)
                df <- lpredict(df,"PS_0_D_1_XX", model, vars_pol_XX, prob = TRUE)
            } else {
		        ## Estimation of 1-E(S|D1) = P(S = 0|D_1) in exact_match case
                model <- lm(as.formula(paste("S_bis_XX",reg_pol_XX,sep="~")),data=df,weights= df$weight_XX)
                df <- lpredict(df, "ES_bis_XX_D_1", model, vars_pol_XX)

		        ## Estimation of \hat{E}(S+-S-|D1) for both \Phi_2:
                model <- lm(as.formula(paste("S_XX",reg_pol_XX,sep="~")),data=df,weights= df$weight_XX)
                df <- lpredict(df, "ES_XX_D_1", model, vars_pol_XX)
            }
            assign(paste0("PS_0",pl,"_XX"), Mean("S0_XX", df))
        }

        ####################################################### AOSS ##########

        if (aoss == 1) {
	        # 0) Compute P_t = P(S_t = 1) = E(S_t) for the aggregation afterward
            assign(paste0("P_",pairwise,pl,"_XX"), Mean("S_bis_XX", df))
            assign(paste0("PS_sum",pl,"_XX"), get(paste0("PS_sum",pl,"_XX")) + get(paste0("P_",pairwise,pl,"_XX")))
            assign(paste0("ES",pl,"_XX"), get(paste0("P_",pairwise,pl,"_XX")))

            # 1) Compute \hat{delta}_1
            df$inner_sum_delta_1_XX <- df$inner_sum_delta_1_2_XX / df$delta_D_XX
            df$inner_sum_delta_1_XX <- ifelse(df$delta_D_XX == 0, NA, df$inner_sum_delta_1_XX)
            assign(paste0("delta_1_",pairwise,pl,"_XX"), Mean("inner_sum_delta_1_XX",df))

            # 2) Compute the variance of \hat{delta}_1
            df$S_over_delta_D_XX <- df$S_bis_XX / df$delta_D_XX
            df$S_over_delta_D_XX <- ifelse(df$S_bis_XX == 0, 0, df$S_over_delta_D_XX)

            modelvar <- lm(as.formula(paste("S_over_delta_D_XX", reg_pol_XX, sep = "~")), data = df, weights = df$weight_XX)
            df <- lpredict(df, "mean_S_over_delta_D_XX", modelvar, vars_pol_XX)

		    # i. estimation of \hat{E}(S/deltaD|D1)
            if (isFALSE(exact_match)) {
               df[[paste0("Phi_1_",pairwise,pl,"_XX")]] <- (df$S_over_delta_D_XX - df$mean_S_over_delta_D_XX * ((1 - df$S_bis_XX)/(df$PS_0_D_1_XX))) * df$inner_sum_delta_1_2_XX
            } else {
               df[[paste0("Phi_1_",pairwise,pl,"_XX")]] <- (df$S_over_delta_D_XX - df$mean_S_over_delta_D_XX * ((1 - df$S_bis_XX)/(df$ES_bis_XX_D_1))) * df$inner_sum_delta_1_2_XX
            }

            df[[paste0("Phi_1_",pairwise,pl,"_XX")]] <- (df[[paste0("Phi_1_",pairwise,pl,"_XX")]] - (get(paste0("delta_1_", pairwise,pl,"_XX")) * df$S_bis_XX)) / get(paste0("ES",pl,"_XX"))

            assign(paste0("mean_IF_1_",pairwise,pl),
                    Mean(paste0("Phi_1_",pairwise,pl,"_XX"), df))

            if (!is.null(cluster)) {
                df <- df %>% group_by(.data$cluster_XX) %>% 
                        mutate(!!paste0("Phi_1_",pairwise,pl,"_c_XX") := sum(.data[[paste0("Phi_1_", pairwise,pl,"_XX")]], na.rm = TRUE)) %>%
                        mutate(first_obs_by_clus = row_number() == 1) %>% ungroup()
                df <- df[order(df$ID_XX), ]
                df[[paste0("Phi_1_",pairwise,pl,"_c_XX")]] <- ifelse(df$first_obs_by_clus == 1, df[[paste0("Phi_1_",pairwise,pl,"_c_XX")]], NA)
                nobs_c_XX <- wSum(subset(df, !is.na(df[[paste0("Phi_1_",pairwise,pl,"_c_XX")]])))
                assign(paste0("sd_delta_1_",pairwise,pl,"_XX"),
                        Sd(paste0("Phi_1_",pairwise,pl,"_c_XX"), df)/sqrt(nobs_c_XX))
                nobs_c_XX <- NULL
                df$first_obs_by_clus <- NULL
            } else {
                assign(paste0("sd_delta_1_",pairwise,pl,"_XX"),
                        Sd(paste0("Phi_1_",pairwise,pl,"_XX"), df)/sqrt(wSum(df)))
            }
            assign(paste0("LB_1_",pairwise,pl,"_XX"),
            get(paste0("delta_1_",pairwise,pl,"_XX")) - 1.96 * get(paste0("sd_delta_1_", pairwise,pl,"_XX")))
            assign(paste0("UB_1_",pairwise,pl,"_XX"),
            get(paste0("delta_1_",pairwise,pl,"_XX")) + 1.96 * get(paste0("sd_delta_1_", pairwise,pl,"_XX")))

            df[[paste0("S_",pairwise,pl,"_XX")]] <- df$S_bis_XX
        }
        
        ###################################################### WAOSS ##########

        if (waoss == 1) {
            assign(paste0("E_abs_delta_D",pl,"_XX"), Mean("abs_delta_D_XX", df))
            assign(paste0("E_abs_delta_D_",pairwise,pl,"_XX"), get(paste0("E_abs_delta_D",pl,"_XX")))
            assign(paste0("E_abs_delta_D_sum",pl,"_XX"), get(paste0("E_abs_delta_D_sum",pl,"_XX")) + get(paste0("E_abs_delta_D_",pairwise,pl,"_XX")))

            for (suffix in c("Minus", "Plus")) {
                df$Ster_XX <- NULL
                df$Ster_XX <- df$S_XX == as.numeric((suffix == "Plus") - (suffix == "Minus"))
                
                ## Computing the contribution weights ##
                df$prod_sgn_delta_D_delta_D_XX <- df$S_XX * df$delta_D_XX
                sum_prod_sgn_delta_D_delta_D_XX <- Sum("prod_sgn_delta_D_delta_D_XX", subset(df,df$Ster_XX == 1))
                assign(paste0("w_",suffix,"_",pairwise,pl,"_XX"), sum_prod_sgn_delta_D_delta_D_XX/get(paste0("N",pl,"_XX")))
                assign(paste0("denom_delta_2_",suffix,"_",pairwise,pl,"_XX"), Sum("delta_D_XX", subset(df, df$Ster_XX == 1)))


                if (estimation_method == "ra") {
                    if (get(paste0("denom_delta_2_",suffix,"_",pairwise,pl,"_XX")) == 0) {
                        assign(paste0("denom_delta_2_",suffix,"_",pairwise,pl,"_XX"), 1)
                        # in case it is zero set it to 1 to avoid dividing by 0, in that case the numerator is also equal to 0	
                    }
                    assign(paste0("num_delta_2_",suffix,"_",pairwise,pl,"_XX"),
                    Sum("inner_sum_delta_1_2_XX", subset(df, df$Ster_XX == 1)))

                    assign(paste0("delta_2_",suffix,"_",pairwise,pl,"_XX"), 
                        get(paste0("num_delta_2_",suffix,"_",pairwise,pl,"_XX")) /
                        get(paste0("denom_delta_2_",suffix,"_",pairwise,pl,"_XX")))
                } 

                assign(paste0("nb_Switchers_",suffix,pl,"_XX"), nrow(subset(df, df$Ster_XX ==1)))

                assign(paste0("PS_",suffix,"1",pl,"_XX"), get(paste0("nb_Switchers_",suffix,pl,"_XX"))/get(paste0("N",pl,"_XX")))

                if (isFALSE(exact_match)) {
                    if (get(paste0("PS_",suffix,"1",pl,"_XX")) == 0) {
                        # The regression is performed iff there is at least one switcher up/down.
                        assign(paste0("delta_2_",suffix,"_",pairwise,pl,"_XX"), 0)
                        df[[paste0("PS_1_",suffix,"_D_1_XX")]] <- 0
                    } else {
                        model <- stata_logit(as.formula(paste("Ster_XX",reg_pol_XX,sep="~")), df)
                        df <- lpredict(df,paste0("PS_1_",suffix,"_D_1_XX"),model, vars_pol_XX, prob = TRUE)


                        if (estimation_method == "ps") {
                            df[[paste0("delta_Y_P_",suffix,"_XX")]] <- df$delta_Y_XX * (df[[paste0("PS_1_",suffix,"_D_1_XX")]]/df$PS_0_D_1_XX) * (get(paste0("PS_0",pl,"_XX")) / get(paste0("PS_",suffix,"1",pl,"_XX")))

                            assign(paste0("mean_delta_Y_P_",suffix,pl,"_XX"), 
                            Mean(paste0("delta_Y_P_",suffix,"_XX"),subset(df,df$S_XX == 0)))
                            assign(paste0("mean_delta_Y",pl,"_XX"), Mean("delta_Y_XX", subset(df, df$Ster_XX == 1)))
                            assign(paste0("mean_delta_D",pl,"_XX"), Mean("delta_D_XX", subset(df, df$Ster_XX == 1)))
                            assign(paste0("delta_2_",suffix,"_",pairwise,pl,"_XX"), 
                            (get(paste0("mean_delta_Y",pl,"_XX")) - get(paste0("mean_delta_Y_P_",suffix,pl,"_XX"))) / get(paste0("mean_delta_D",pl,"_XX")))
                        }
                    }                
                }
            }

            if (estimation_method == "ra" | estimation_method == "ps") {

                ## Computing the final weights ##
                assign(paste0("W_Plus_",pairwise,pl,"_XX"), get(paste0("w_Plus_",pairwise,pl,"_XX")) /
                (get(paste0("w_Plus_",pairwise,pl,"_XX")) + get(paste0("w_Minus_",pairwise,pl,"_XX"))))
            }
            
            if (isFALSE(exact_match)) {
                df$dr_delta_Y_XX <- (df$S_XX - ((df$PS_1_Plus_D_1_XX - df$PS_1_Minus_D_1_XX)/df$PS_0_D_1_XX) * (1 - df$S_bis_XX)) * df$inner_sum_delta_1_2_XX
                assign(paste0("denom_dr_delta_2",pl,"_XX"), Sum("dr_delta_Y_XX", df))
            }

            if (estimation_method == "ra" | estimation_method == "ps") {
                assign(paste0("delta_2_",pairwise,pl,"_XX"), 
                get(paste0("W_Plus_",pairwise,pl,"_XX"))*get(paste0("delta_2_Plus_",pairwise,pl,"_XX")) + (1-get(paste0("W_Plus_",pairwise,pl,"_XX")))*get(paste0("delta_2_Minus_",pairwise,pl,"_XX")))
            } else if (estimation_method == "dr") {
                sum_abs_delta_D_XX <- Sum("abs_delta_D_XX",df)
                assign(paste0("delta_2_",pairwise,pl,"_XX"), get(paste0("denom_dr_delta_2",pl,"_XX")) / sum_abs_delta_D_XX)
            }

            # Computing the variance
            if (isFALSE(exact_match)) {
                df[[paste0("Phi_2_",pairwise,pl,"_XX")]] <- (df$dr_delta_Y_XX - get(paste0("delta_2_", pairwise,pl,"_XX")) * df$abs_delta_D_XX) / get(paste0("E_abs_delta_D",pl,"_XX"))
            } else {
                df[[paste0("Phi_2_",pairwise,pl,"_XX")]] <- ((df$S_XX - df$ES_XX_D_1 * ((1 - df$S_bis_XX)/(1 - df$ES_bis_XX_D_1))) * df$inner_sum_delta_1_2_XX - get(paste0("delta_2_", pairwise,pl,"_XX")) * df$abs_delta_D_XX) / get(paste0("E_abs_delta_D",pl,"_XX"))
            }

            assign(paste0("mean_IF_2_",pairwise,pl),
                    Mean(paste0("Phi_2_",pairwise,pl,"_XX"), df))

            if (!is.null(cluster)) {
                df <- df %>% group_by(.data$cluster_XX) %>% 
                        mutate(!!paste0("Phi_2_",pairwise,pl,"_c_XX") := sum(.data[[paste0("Phi_2_", pairwise,pl,"_XX")]], na.rm = TRUE)) %>%
                        mutate(first_obs_by_clus = row_number() == 1) %>% ungroup()
                df[[paste0("Phi_2_",pairwise,pl,"_c_XX")]] <- ifelse(df$first_obs_by_clus == 1, df[[paste0("Phi_2_",pairwise,pl,"_c_XX")]], NA)
                nobs_c_XX <- nrow(subset(df, !is.na(df[[paste0("Phi_2_",pairwise,pl,"_c_XX")]])))
                assign(paste0("sd_delta_2_",pairwise,pl,"_XX"),
                        sd(df[[paste0("Phi_2_",pairwise,pl,"_c_XX")]], na.rm = TRUE)/sqrt(nobs_c_XX))
                nobs_c_XX <- NULL
                df$first_obs_by_clus <- NULL
            } else {
                assign(paste0("sd_delta_2_",pairwise,pl,"_XX"),
                        Sd(paste0("Phi_2_",pairwise,pl,"_XX"), df)/sqrt(wSum(df)))
            }
            assign(paste0("LB_2_",pairwise,pl,"_XX"), get(paste0("delta_2_",pairwise,pl,"_XX")) - 1.96 * get(paste0("sd_delta_2_", pairwise,pl,"_XX")))
            assign(paste0("UB_2_",pairwise,pl,"_XX"), get(paste0("delta_2_",pairwise,pl,"_XX")) + 1.96 * get(paste0("sd_delta_2_", pairwise,pl,"_XX")))

            df[[paste0("abs_delta_D_",pairwise,pl,"_XX")]] <- df$abs_delta_D_XX
        }

        ##################################################### IWAOSS ##########
        if (iwaoss == 1) {
            assign(paste0("E_abs_delta_Z",pl,"_XX"), Mean("abs_delta_Z_XX", df))

            df$SI_bis_XX <- (df$SI_XX != 0 & !is.na(df$SI_XX))
            df$SI_Plus_XX <- (df$SI_XX == 1)
            df$SI_Minus_XX <- (df$SI_XX == -1)

            # Preliminaries: logit regression
            df$S_IV_0_XX <- 1 - df$SI_bis_XX
            if (isFALSE(exact_match)) {
                model <- stata_logit(as.formula(paste("S_IV_0_XX",IV_reg_pol_XX,sep="~")), df)
                df <- lpredict(df,"PS_IV_0_Z_1_XX",model, IV_vars_pol_XX, prob = TRUE)
            } else {
                model <- lm(as.formula(paste("SI_bis_XX",IV_reg_pol_XX,sep="~")), data = df, weights = df$weight_XX)
                df <- lpredict(df, "ES_I_bis_XX_Z_1", model, IV_vars_pol_XX)
                model <- lm(as.formula(paste("SI_XX",IV_reg_pol_XX,sep="~")), data = df, weights = df$weight_XX)
                df <- lpredict(df, "ES_I_XX_Z_1", model, IV_vars_pol_XX)
            }
            assign(paste0("PS_IV_0",pl,"_XX"), Mean("S_IV_0_XX", df))

            for (suffix in c("Minus", "Plus")) {
                assign(paste0("nb_Switchers_I_",suffix,pl,"_XX"), 
                        nrow(subset(df, df[[paste0("SI_",suffix,"_XX")]] == 1)))
                assign(paste0("PS_I_",suffix,"_1",pl,"_XX"), get(paste0("nb_Switchers_I_",suffix,pl,"_XX")) / get(paste0("N",pl,"_XX")))

                if (get(paste0("PS_I_",suffix,"_1",pl,"_XX")) == 0) {
                    df[[paste0("PS_I_",suffix,"_1_Z_1_XX")]] <- 0
                } else {
                    if (isFALSE(exact_match)) {
                        model <- stata_logit(as.formula(paste(paste0("SI_",suffix,"_XX"),IV_reg_pol_XX,sep="~")), df)
                        df <- lpredict(df,paste0("PS_I_",suffix,"_1_Z_1_XX"),model, IV_vars_pol_XX, prob = TRUE)
                    }
                }
            }

 	        # i. Estimation of  \hat{E}(deltaY|Z1, SI=0)
            df_temp <- subset(df, df$SI_XX == 0)
            model <- lm(as.formula(paste("delta_Y_XX",IV_reg_pol_XX, sep = "~")), 
                    data = df_temp, weights = df_temp$weight_XX)
            df <- lpredict(df,"mean_delta_Y_pred_IV_XX", model, IV_vars_pol_XX)   
            df$inner_sum_IV_num_XX <- df$delta_Y_XX - df$mean_delta_Y_pred_IV_XX

 	        # i. Estimation of  \hat{E}(deltaD|Z1, SI=0)
            model <- lm(as.formula(paste("delta_D_XX",IV_reg_pol_XX, sep = "~")), 
                    data = df_temp, weights = df_temp$weight_XX)
            df <- lpredict(df,"mean_delta_D_pred_IV_XX", model, IV_vars_pol_XX)   
            df$inner_sum_IV_denom_XX <- df$delta_D_XX - df$mean_delta_D_pred_IV_XX
            df_temp <- NULL

            if (estimation_method == "ra") {
                for (v in c("num","denom")) {
                    df[[paste0("inner_sum_IV_",v,"_XX")]] <- df[[paste0("inner_sum_IV_",v,"_XX")]] * df$SI_XX
                    assign(paste0(v,"_delta_IV_",pairwise,pl,"_XX"), 
                        Mean(paste0("inner_sum_IV_",v,"_XX"), df))
                }
            }

            if (estimation_method == "ps") {
                # Numerator
                df$delta_Y_P_IV_XX <- df$delta_Y_XX * ((df$PS_I_Plus_1_Z_1_XX - df$PS_I_Minus_1_Z_1_XX)/df$PS_IV_0_Z_1_XX) * get(paste0("PS_IV_0",pl,"_XX"))
                assign(paste0("mean_delta_Y_P_IV",pl,"_XX"), Mean("delta_Y_P_IV_XX", subset(df, df$SI_bis_XX == 0)))
                df$prod_sgn_delta_Z_delta_Y_XX <- df$SI_XX * df$delta_Y_XX
                assign(paste0("mean_sgn_delta_Z_delta_Y",pl,"_XX"), Mean("prod_sgn_delta_Z_delta_Y_XX", df))
                assign(paste0("num_delta_IV_",pairwise,pl,"_XX"), 
                get(paste0("mean_sgn_delta_Z_delta_Y",pl,"_XX")) - get(paste0("mean_delta_Y_P_IV",pl,"_XX")))

                # Denominator
                df$delta_D_P_IV_XX <- df$delta_D_XX * ((df$PS_I_Plus_1_Z_1_XX - df$PS_I_Minus_1_Z_1_XX)/df$PS_IV_0_Z_1_XX) * get(paste0("PS_IV_0",pl,"_XX"))
                assign(paste0("mean_delta_D_P_IV",pl,"_XX"), Mean("delta_D_P_IV_XX", subset(df, df$SI_bis_XX == 0)))
                df$prod_sgn_delta_Z_delta_D_XX <- df$SI_XX * df$delta_D_XX
                assign(paste0("mean_sgn_delta_Z_delta_D",pl,"_XX"), Mean("prod_sgn_delta_Z_delta_D_XX", df))
                assign(paste0("denom_delta_IV_",pairwise,pl,"_XX"), 
                get(paste0("mean_sgn_delta_Z_delta_D",pl,"_XX")) - get(paste0("mean_delta_D_P_IV",pl,"_XX")))
            }

            if (estimation_method == "dr") {
                df$dr_IV_delta_Y_XX <- (df$SI_XX - ((df$PS_I_Plus_1_Z_1_XX - df$PS_I_Minus_1_Z_1_XX) / df$PS_IV_0_Z_1_XX) * (1 - df$SI_bis_XX)) * df$inner_sum_IV_num_XX
                assign(paste0("num_delta_IV_",pairwise,pl,"_XX"), Mean("dr_IV_delta_Y_XX", df))

                df$dr_IV_delta_D_XX <- (df$SI_XX - ((df$PS_I_Plus_1_Z_1_XX - df$PS_I_Minus_1_Z_1_XX) / df$PS_IV_0_Z_1_XX) * (1 - df$SI_bis_XX)) * df$inner_sum_IV_denom_XX
                assign(paste0("denom_delta_IV_",pairwise,pl,"_XX"), Mean("dr_IV_delta_D_XX", df))
            }

            assign(paste0("delta_3_",pairwise,pl,"_XX"), 
            get(paste0("num_delta_IV_",pairwise,pl,"_XX"))/get(paste0("denom_delta_IV_",pairwise,pl,"_XX")))

            assign(paste0("denom_delta_IV_sum",pl,"_XX"), get(paste0("denom_delta_IV_sum",pl,"_XX")) + get(paste0("denom_delta_IV_",pairwise,pl,"_XX")))

            assign(paste0("delta_Y",pl,"_XX"), Mean("inner_sum_IV_num_XX", df)) 
            df_temp <- subset(df, df$SI_XX == 0)
            model <- lm(as.formula(paste("delta_Y_XX", reg_pol_XX, sep = "~")), 
                    data = df_temp, weights = df_temp$weight_XX)
            df <- lpredict(df,"mean_pred_Y_IV_XX", model, vars_pol_XX)   
            if (isFALSE(exact_match)) {
                df$Phi_Y_XX <- ((df$SI_XX - (df$PS_I_Plus_1_Z_1_XX - df$PS_I_Minus_1_Z_1_XX) * (1 - df$SI_bis_XX) / df$PS_IV_0_Z_1_XX) * (df$delta_Y_XX - df$mean_pred_Y_IV_XX) - get(paste0("delta_Y",pl,"_XX")) * df$abs_delta_Z_XX) / get(paste0("E_abs_delta_Z",pl,"_XX"))
            } else {
                df$Phi_Y_XX <- ((df$SI_XX - (df$ES_I_XX_Z_1) * ((1 - df$SI_bis_XX) / (1- df$ES_I_bis_XX_Z_1))) * (df$delta_Y_XX - df$mean_pred_Y_IV_XX) - get(paste0("delta_Y",pl,"_XX")) * df$abs_delta_Z_XX) / get(paste0("E_abs_delta_Z",pl,"_XX"))
            }
            
            assign(paste0("delta_D",pl,"_XX"), Mean("inner_sum_IV_denom_XX", df)) 
            df_temp <- subset(df, df$SI_XX == 0)
            model <- lm(as.formula(paste("delta_D_XX", reg_pol_XX, sep = "~")), 
                    data = df_temp, weights = df_temp$weight_XX)
            df <- lpredict(df,"mean_pred_D_IV_XX", model, vars_pol_XX)   

            if (isFALSE(exact_match)) {
                df$Phi_D_XX <- ((df$SI_XX - (df$PS_I_Plus_1_Z_1_XX - df$PS_I_Minus_1_Z_1_XX) * (1 - df$SI_bis_XX) / df$PS_IV_0_Z_1_XX) * (df$delta_D_XX - df$mean_pred_D_IV_XX) - get(paste0("delta_D",pl,"_XX")) * df$abs_delta_Z_XX) / get(paste0("E_abs_delta_Z",pl,"_XX"))
            } else {
                df$Phi_D_XX <- ((df$SI_XX - (df$ES_I_XX_Z_1) * ((1 - df$SI_bis_XX) / (1- df$ES_I_bis_XX_Z_1))) * (df$delta_D_XX - df$mean_pred_D_IV_XX) - get(paste0("delta_D",pl,"_XX")) * df$abs_delta_Z_XX) / get(paste0("E_abs_delta_Z",pl,"_XX"))
            }

            df[[paste0("Phi_3_",pairwise,pl,"_XX")]] <- (df$Phi_Y_XX - get(paste0("delta_3_",pairwise,pl,"_XX")) * df$Phi_D_XX) / get(paste0("delta_D",pl,"_XX"))
            mean_IF3 <- Mean(paste0("Phi_3_",pairwise,pl,"_XX"), df)

            if (!is.null(cluster)) {
                df <- df %>% group_by(.data$cluster_XX) %>% 
                        mutate(!!paste0("Phi_3_",pairwise,pl,"_c_XX") := sum(.data[[paste0("Phi_3_", pairwise,pl,"_XX")]], na.rm = TRUE)) %>%
                        mutate(first_obs_by_clus = row_number() == 1) %>% ungroup()
                df[[paste0("Phi_3_",pairwise,pl,"_c_XX")]] <- ifelse(df$first_obs_by_clus == 1, df[[paste0("Phi_3_",pairwise,pl,"_c_XX")]], NA)
                nobs_c_XX <- nrow(subset(df, !is.na(df[[paste0("Phi_3_",pairwise,pl,"_c_XX")]])))
                assign(paste0("sd_delta_3_",pairwise,pl,"_XX"),
                        sd(df[[paste0("Phi_3_",pairwise,pl,"_c_XX")]], na.rm = TRUE)/sqrt(nobs_c_XX))
                nobs_c_XX <- NULL
                df$first_obs_by_clus <- NULL
            } else {
                assign(paste0("sd_delta_3_",pairwise,pl,"_XX"),
                        Sd(paste0("Phi_3_",pairwise,pl,"_XX"), df)/sqrt(wSum(df)))
            }

            assign(paste0("LB_3_",pairwise,pl,"_XX"), get(paste0("delta_3_",pairwise,pl,"_XX")) - 1.96*get(paste0("sd_delta_3_",pairwise,pl,"_XX")))
            assign(paste0("UB_3_",pairwise,pl,"_XX"), get(paste0("delta_3_",pairwise,pl,"_XX")) + 1.96*get(paste0("sd_delta_3_",pairwise,pl,"_XX")))

            df[[paste0("inner_sum_IV_denom_",pairwise,pl,"_XX")]] <- df$inner_sum_IV_denom_XX
        }

        assign(paste0("non_missing_",pairwise,pl,"_XX"), 1)
    } else {
        for (i in 1:3) {
            assign(paste0("delta_",i,"_",pairwise,pl,"_XX"), 0)
            assign(paste0("sd_delta_",i,"_",pairwise,pl,"_XX"), NA)
            assign(paste0("LB_",i,"_",pairwise,pl,"_XX"), NA)
            assign(paste0("UB_",i,"_",pairwise,pl,"_XX"), NA)
            df[[paste0("Phi",i,"_",pairwise,pl,"_XX")]] <- NA
        }

        if (aoss == 1 | waoss == 1) {
            IVt <- ""
        } else if (iwaoss == 1) {
            IVt <- "_IV"
        }

        if (gap_XX != 0) {
            assign(paste0("N_Switchers",IVt,pl,"_XX"), NA)
            assign(paste0("N_Stayers",IVt,pl,"_XX"), NA)
        }
        if (!is.na(get(paste0("N_Stayers",IVt,pl,"_XX"))) & get(paste0("N_Stayers",IVt,pl,"_XX")) < 2) {
            assign(paste0("N_Switchers",IVt,pl,"_XX"), nrow(df))
            assign(paste0("N_Stayers",IVt,pl,"_XX"), 0)
        }
        if (!is.na(get(paste0("N_Switchers",IVt,pl,"_XX"))) & get(paste0("N_Switchers",IVt,pl,"_XX")) == 0) {
            assign(paste0("N_Switchers",IVt,pl,"_XX"), 0)
            assign(paste0("N_Stayers",IVt,pl,"_XX"), nrow(df))
        }
        df[[paste0("abs_delta_D_",pairwise,pl,"_XX")]] <- NA
        df[[paste0("S_",pairwise,pl,"_XX")]] <- NA
        if (aoss == 1) {
            assign(paste0("P_",pairwise,pl,"_XX"), 0)
        }
        if (waoss == 1) {
            assign(paste0("E_abs_delta_D_",pairwise,pl,"_XX"), 0)
        }
        if (iwaoss == 1) { 
            assign(paste0("denom_delta_IV_",pairwise,pl,"_XX"), 0)
        }

        assign(paste0("non_missing_",pairwise,pl,"_XX"), 0)
    }

    df <- df[order(df$ID_XX), ]
    to_keep <- c("ID_XX", paste0("Phi_1_",pairwise,pl,"_XX"), paste0("Phi_2_",pairwise,pl,"_XX"), paste0("Phi_3_",pairwise,pl,"_XX"), paste0("S_",pairwise,pl,"_XX"), paste0("abs_delta_D_",pairwise,pl,"_XX"), paste0("used_in_",pairwise,pl,"_XX"), paste0("inner_sum_IV_denom_",pairwise,pl,"_XX")) 

    df <- df %>% select(dplyr::any_of(to_keep))

    ## End of the program

    for (v in names(scalars)) {
        scalars[[v]] <- get(v)
    }
    if (aoss == 1) {
        scalars[[paste0("P_",pairwise,pl,"_XX")]] <- get(paste0("P_",pairwise,pl,"_XX"))
    }
    if (waoss == 1) {
        scalars[[paste0("E_abs_delta_D_",pairwise,pl,"_XX")]] <- get(paste0("E_abs_delta_D_",pairwise,pl,"_XX"))
    }

    if (iwaoss == 1) {
        scalars[[paste0("denom_delta_IV_",pairwise,pl,"_XX")]] <- get(paste0("denom_delta_IV_",pairwise,pl,"_XX"))
    }

    scalars[[paste0("non_missing_",pairwise,pl,"_XX")]] <- get(paste0("non_missing_",pairwise,pl,"_XX"))
    for (v in c("Switchers", "Stayers")) {
        if (waoss == 1 | aoss == 1) {
            scalars[[paste0("N_",v,"_1_",pairwise,pl,"_XX")]] <- get(paste0("N_",v,pl,"_XX"))
            scalars[[paste0("N_",v,"_2_",pairwise,pl,"_XX")]] <- get(paste0("N_",v,pl,"_XX"))
        } else if (iwaoss == 1) {
            scalars[[paste0("N_",v,"_3_",pairwise,pl,"_XX")]] <- get(paste0("N_",v,"_IV",pl,"_XX"))
        }
    }

    estims <- c("aoss", "waoss", "iwaoss")
    indices <- c() 
    for (j in 1:length(estims)) {
        if (get(estims[j]) == 1) {
            indices <- c(indices, j)
        }
    }

    for (i in indices) {
        scalars[[paste0("delta_",i,"_",pairwise,pl,"_XX")]] <- get(paste0("delta_",i,"_",pairwise,pl,"_XX"))
        scalars[[paste0("sd_delta_",i,"_",pairwise,pl,"_XX")]] <- get(paste0("sd_delta_",i,"_",pairwise,pl,"_XX"))
        scalars[[paste0("LB_",i,"_",pairwise,pl,"_XX")]] <- get(paste0("LB_",i,"_",pairwise,pl,"_XX"))
        scalars[[paste0("UB_",i,"_",pairwise,pl,"_XX")]] <- get(paste0("UB_",i,"_",pairwise,pl,"_XX"))
    }

    out_res <- list(scalars, list(df))
    names(out_res) <- c("scalars", "to_add")
    return(out_res)
}