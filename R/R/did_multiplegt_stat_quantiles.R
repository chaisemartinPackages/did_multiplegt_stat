#' Internal function to generate quantile subsets based on delta D or delta Z
#' @param df df
#' @param ID ID
#' @param Time Time
#' @param D D
#' @param Z Z
#' @param by_opt by_opt
#' @param quantiles quantiles
#' @importFrom plm pdata.frame make.pbalanced
#' @importFrom stats quantile median
#' @import ggplot2
#' @returns A list with the df object and the relevant quantiles
#' @noRd
did_multiplegt_stat_quantiles <- function(
    df,
    ID,
    Time,
    D,  
    Z,
    by_opt,
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
        df$delta_pre_XX <- abs(diff(df[[D]]))
        gr_title <- paste0("\U0394", "D")
    } else {
        df$delta_pre_XX <- abs(diff(df[[Z]]))
        gr_title <- paste0("\U0394", "Z")
    }

    df$switchers_dummy_XX <- df$delta_pre_XX != 0
    df <- df %>% group_by(.data[[Time]]) %>%
        mutate(switchers_N_XX = sum(.data$switchers_dummy_XX, na.rm = TRUE)) %>% 
        mutate(stayers_N_XX = sum(1-.data$switchers_dummy_XX, na.rm = TRUE)) %>% 
        ungroup()
    df$in_aggregation_XX <- df$switchers_N_XX > 0 & df$stayers_N_XX > 1
    df$switchers_dummy_XX <- df$switchers_N_XX <- df$stayers_N_XX <- NULL

    df_switch <- subset(df, !is.na(df$delta_pre_XX) & df$delta_pre_XX != 0 & df$in_aggregation_XX == 1)
    N_switchers_plot <- nrow(df_switch)
    df_switch$unit_XX <- 1
    df_switch <- df_switch %>% group_by(.data$delta_pre_XX) %>% 
        mutate(tot_delta_XX = sum(.data$unit_XX, na.rm = TRUE))
    df_switch$unit_XX <- NULL; 
    df_switch <- df_switch[c("delta_pre_XX", "tot_delta_XX")]
    df_switch <- df_switch %>% group_by(.data$delta_pre_XX) %>% summarise(tot_delta_XX = mean(.data$tot_delta_XX, na.rm = TRUE))
    df_switch$tot_delta_XX <- df_switch$tot_delta_XX/sum(df_switch$tot_delta_XX, na.rm = TRUE)
    df_switch <- df_switch[order(df_switch$delta_pre_XX), ]
    df_switch$cdf <- cumsum(df_switch$tot_delta_XX)
    df_switch$partition_XX <- by_opt
    cut_off <- c()
    quantiles_temp <- c(0)
    for (j in 2:length(quantiles)) {
        df_switch$partition_XX <- ifelse(df_switch$cdf >= quantiles[j-1] & df_switch$cdf < quantiles[j], j - 1, df_switch$partition_XX)
        if (nrow(subset(df_switch, df_switch$partition_XX == j-1)) > 0) {
            cut_off <- c(cut_off, min(subset(df_switch, df_switch$partition_XX == j-1)$delta_pre_XX,na.rm = TRUE))
            quantiles_temp <- c(quantiles_temp, max(subset(df_switch, df_switch$partition_XX == j-1)$cdf,na.rm = TRUE))
        }
    }
    cut_off <- c(cut_off, max(df_switch$delta_pre_XX, na.rm = TRUE))

    quantiles_plot <- ggplot(data = df_switch, aes(x = .data$delta_pre_XX, y = .data$cdf)) + geom_line(size = 0.5) + scale_x_continuous(breaks= cut_off, labels = sprintf("%.2f", cut_off)) + theme(plot.title = element_text(hjust = 0.5), panel.grid.minor = element_blank()) + ylab("CDF") + xlab(gr_title) + ggtitle(sprintf("Empirical distribution of %s", gr_title)) + labs(caption = sprintf("N = %.0f. Quantiles bins cutoffs reported as x axis ticks.", N_switchers_plot))
    quantiles <- quantiles_temp
    df_switch <- quantiles_temp <- NULL


    df$switchers_XX <- df$delta_pre_XX != 0 & !is.na(df$delta_pre_XX) & df$in_aggregation_XX == 1
    df$partition_XX <- ifelse(df$switchers_XX, 1, 0)
    for (p in 2:length(cut_off)) {
        df$partition_XX <- ifelse(df$switchers_XX & (df$delta_pre_XX > cut_off[p-1] & df$delta_pre_XX <= cut_off[p]), p-1, df$partition_XX)
    }
    df$partition_XX <- as.numeric(df$partition_XX)
    df$partition_XX <- ifelse(df$in_aggregation_XX, df$partition_XX, NA)
    names(cut_off) <- c()
    class(df) <- "data.frame"
    df$it_XX <- 1
    switch_df <- df %>% filter(.data$partition_XX != 0) %>% 
            group_by(.data$partition_XX) %>%
            summarise(N_partition_XX = sum(.data$it_XX, na.rm = TRUE), Med_delta_pre_XX = median(.data$delta_pre_XX, na.rm = TRUE)) %>% ungroup()
    df$it_XX <- switch_df$it_XX <- NULL
    ret <- list(df = df, val_quantiles = cut_off, quantiles = quantiles, switch_df = switch_df, quantiles_plot = quantiles_plot)
    df$delta_pre_XX <- df$switchers_XX <-  NULL     
    return(ret)
}