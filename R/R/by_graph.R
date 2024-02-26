#' Internal function for by option graph
#' @param obj obj
#' @import ggplot2
#' @returns The did_multiplegt_stat object + a ggplot object
#' @noRd
by_graph <- function(obj) {
    pe_set <- as.data.frame(matrix(NA, ncol = 2, nrow = 0))
    names(pe_set) <- c("model", "pe")
    models <- c("aoss", "waoss", "iwaoss")
    for (i in 1:3) {
        pe_set_temp <- as.data.frame(matrix(NA, ncol = 2, nrow = length(obj$by_levels)))
        for (j in 1:length(obj$by_levels)) {
            subobj <- obj[[paste0("results_by_",j)]]
            pe_set_temp[j,2] <- subobj$table[subobj$pairs * (i-1) + 1, 1]
        }
        names(pe_set_temp) <- c("model", "pe")
        pe_set_temp$model <- models[i]
        pe_set_temp$lbin <- obj$val_quantiles[1:length(obj$val_quantiles) - 1]
        pe_set_temp$ubin <- obj$val_quantiles[2:length(obj$val_quantiles)]
        pe_set_temp$id <- 1:length(obj$by_levels)
        pe_set_temp$colname <- sprintf("[%.3f,%.3f]\n(%.0f%%-%.0f%%)", pe_set_temp$lbin, pe_set_temp$ubin, (pe_set_temp$id-1)*obj$args$by, pe_set_temp$id*obj$args$by)
        
        pe_set <- rbind(pe_set, pe_set_temp)
    }
    var_gr <- ifelse("iwaoss" %in% obj$args$estimator, "Z", "D")
    pe_set <- subset(pe_set, !is.na(pe_set$pe))
    by_graph <- ggplot(data = pe_set, aes(x = .data$colname, y = .data$pe, fill = .data$model)) + 
        geom_bar(stat = "identity", position = position_dodge()) +
        xlab(sprintf("Quantile bins of \U0394%s", var_gr)) +
        ylab("") + ggtitle(sprintf("did_multiplegt_stat results by bins of \U0394%s", var_gr)) +
        theme(plot.title = element_text(hjust = 0.5))
    return(by_graph)
}