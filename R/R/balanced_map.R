#' Internal function of did_multiplegt_stat to handle unbalancedness
#' @param df df
#' @returns A matrix
#' @importFrom stats reshape
#' @noRd
balanced_map <- function(df) {
    df <- df[c("ID_XX", "T_XX", "tsfilled_XX")]
    df$H_t_XX <- as.numeric(1 - df$tsfilled_XX)
    df$tsfilled_XX <- NULL
    df <- df[order(df$ID_XX, df$T_XX), ]
    df <- df %>% group_by(.data$ID_XX) %>%
            mutate(H_t_m_1_XX = lag(.data$H_t_XX)) %>% ungroup()
    df <- subset(df, !is.na(df$H_t_m_1_XX))
    df$H_t <- as.numeric(df$H_t_XX == 1 & df$H_t_m_1_XX == 1)
    df$H_t_XX <- df$H_t_m_1_XX <- NULL
    times <- levels(factor(df$T_XX))
    df <- stats::reshape(data = as.data.frame(df), idvar = "ID_XX", timevar = "T_XX", 
            direction = "wide", v.names = "H_t") 
    colnames(df) <- c("ID_XX", sapply(times, function(x) return(paste0("H_",x))))
    rownames(df) <- 1:nrow(df)
    return(df)
}