#' Internal function for by_fd option graph
#' @param obj obj
#' @import ggplot2
#' @importFrom cowplot plot_grid
#' @returns The did_multiplegt_stat object + a ggplot object
#' @noRd
by_fd_graph <- function(obj) {
    suppressWarnings({
    pe_set <- as.data.frame(matrix(NA, ncol = 6, nrow = 0))
    names(pe_set) <- c("model", "pe", "lb", "ub", "nswitchers")
    models <- c("aoss", "waoss", "ivwaoss")
    for (i in 1:3) {
        if (models[i] %in% obj$args$estimator) {
            pe_set_temp <- as.data.frame(matrix(NA, ncol = 6, nrow = length(obj$by_levels)))
            for (j in 1:length(obj$by_levels)) {
                subobj <- obj[[paste0("results_by_",j)]]
                pe_set_temp[j,2] <- subobj$table[subobj$pairs * (i-1) + 1, 1]
                pe_set_temp[j,3] <- subobj$table[subobj$pairs * (i-1) + 1, 3]
                pe_set_temp[j,4] <- subobj$table[subobj$pairs * (i-1) + 1, 4]
                pe_set_temp[j,5] <- obj$switchers_df$N_partition_XX[j]
                pe_set_temp[j,6] <- obj$switchers_df$Med_delta_pre_XX[j]
            }
            names(pe_set_temp) <- c("model", "pe", "lb", "ub", "nswitchers", "median")
            pe_set_temp$model <- models[i]
            pe_set_temp$lbin <- obj$quantiles[2, 1:ncol(obj$quantiles) - 1]
            pe_set_temp$ubin <- obj$quantiles[2, 2:ncol(obj$quantiles)]
            pe_set_temp$lbin_cdf <- obj$quantiles[1, 1:ncol(obj$quantiles) - 1] * 100
            pe_set_temp$ubin_cdf <- obj$quantiles[1, 2:ncol(obj$quantiles)] * 100
            pe_set_temp$id <- 1:length(obj$by_levels)
            pe_set_temp$include <- ifelse(pe_set_temp$id == 1, "[", "(")
            
            pe_set <- rbind(pe_set, pe_set_temp)
        }
    }
    for (c in 1:ncol(pe_set)) {
        pe_set[,c] <- ifelse(is.nan(pe_set[,c]), NA, pe_set[,c])
    }
    var_gr <- ifelse("ivwaoss" %in% pe_set$model, "Z", "D")
    pe_set$colname <- sprintf("%.2f\n(%.0f%%-%.0f%%)\n%s%.2f,%.2f]\nN=%.0f\n", pe_set$median, pe_set$lbin_cdf, pe_set$ubin_cdf,pe_set_temp$include, pe_set_temp$lbin, pe_set_temp$ubin, pe_set$nswitchers)

    ticks <- c()
    labels <- c()
    ticks_size <- c()
    for (j in 1:(nrow(pe_set)/length(levels(as.factor(pe_set$model))))) {
        ticks <- c(ticks, pe_set$median[j])
        labels <- c(labels, pe_set$colname[j])
    }
    ticks <- c(ticks, pe_set$ubin[nrow(pe_set)])
    labels <- c(labels, "")
    ticks_size <- c(ticks_size, 1)

    by_graph_tot <- NULL
    by_graph_1_XX <- NULL
    by_graph_2_XX <- NULL
    font <- 18/length(obj$args$estimator)
    tot_lim <- c(0,0)
    for (j in 1:length(obj$args$estimator))  {
        pe_set_temp <- subset(pe_set, pe_set$model == obj$args$estimator[j])

        by_graph <- ggplot(data = pe_set_temp, aes(x = .data$median, y = .data$pe)) + 
        geom_point(size = 4) + geom_line(size = 0.2, linetype = "dashed") +
        geom_errorbar(aes(ymin = .data$lb, ymax = .data$ub, fill = .data$model), width = 0) +
        xlab(sprintf("|\U0394%s| - %s", var_gr, toupper(obj$args$estimator[j]))) + ylab("") +
        scale_x_continuous(breaks= ticks, labels = labels) +
        theme(plot.title = element_text(hjust = 0.5, size = 2*font), axis.ticks.x = element_line(), axis.line.x = element_line(color="black", size = 0.5), axis.ticks.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line(color = "black", size = 0.2), axis.text = element_text(size = font), axis.title = element_text(size = font)) + geom_hline(yintercept = 0, color = "black", size = 0.5)

        tot_lim[1] <- ifelse(layer_scales(by_graph)$y$range$range[1] > tot_lim[1], tot_lim[1], layer_scales(by_graph)$y$range$range[1])
        tot_lim[2] <- ifelse(layer_scales(by_graph)$y$range$range[2] > tot_lim[2], layer_scales(by_graph)$y$range$range[2], tot_lim[2])
        assign(paste0("by_graph_",j,"_XX"), by_graph)
        pe_set_temp <- by_graph <- NULL
    }
    for (j in 1:length(obj$args$estimator))  {
        assign(paste0("by_graph_",j,"_XX"), get(paste0("by_graph_",j,"_XX")) + ylim(tot_lim))
    }
    if (length(obj$args$estimator) == 1) {
        by_graph_tot <- by_graph_1_XX
    } else {
        by_graph_tot <- plot_grid(by_graph_1_XX, by_graph_2_XX, nrow = 1)
    }
    })
    return(by_graph_tot)
}