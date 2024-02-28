#' Internal function for by option graph
#' @param obj obj
#' @import ggplot2
#' @importFrom gtools invalid
#' @returns The did_multiplegt_stat object + a ggplot object
#' @noRd
by_graph <- function(obj) {
    suppressWarnings({
    pe_set <- as.data.frame(matrix(NA, ncol = 5, nrow = 0))
    names(pe_set) <- c("model", "pe", "lb", "ub", "nswitchers")
    models <- c("aoss", "waoss", "iwaoss")
    for (i in 1:3) {
        if (models[i] %in% obj$args$estimator) {
            pe_set_temp <- as.data.frame(matrix(NA, ncol = 5, nrow = length(obj$by_levels)))
            for (j in 1:length(obj$by_levels)) {
                subobj <- obj[[paste0("results_by_",j)]]
                pe_set_temp[j,2] <- subobj$table[subobj$pairs * (i-1) + 1, 1]
                pe_set_temp[j,3] <- subobj$table[subobj$pairs * (i-1) + 1, 3]
                pe_set_temp[j,4] <- subobj$table[subobj$pairs * (i-1) + 1, 4]
                pe_set_temp[j,5] <- subobj$table[subobj$pairs * (i-1) + 1, 5]
            }
            names(pe_set_temp) <- c("model", "pe", "lb", "ub", "nswitchers")
            pe_set_temp$model <- models[i]
            pe_set_temp$lbin <- obj$quantiles[2, 1:ncol(obj$quantiles) - 1]
            pe_set_temp$ubin <- obj$quantiles[2, 2:ncol(obj$quantiles)]
            pe_set_temp$lbin_cdf <- obj$quantiles[1, 1:ncol(obj$quantiles) - 1] * 100
            pe_set_temp$ubin_cdf <- obj$quantiles[1, 2:ncol(obj$quantiles)] * 100
            pe_set_temp$id <- 1:length(obj$by_levels)
            pe_set_temp$include <- ifelse(pe_set_temp$id == length(obj$by_levels), "]", ")")
            pe_set_temp$colname <- sprintf("(%.0f%%-%.0f%%)\n[%.3f,%.3f%s\n%.0f switchers", pe_set_temp$lbin_cdf, pe_set_temp$ubin_cdf,pe_set_temp$lbin, pe_set_temp$ubin,  pe_set_temp$include, pe_set_temp$nswitchers)
            
            pe_set <- rbind(pe_set, pe_set_temp)
        }
    }
    pe_set <- subset(pe_set, !is.na(pe_set$pe) & !is.nan(pe_set$pe))
    pe_set$width <- (pe_set$ubin_cdf - pe_set$lbin_cdf) * (obj$args$by_fd/100)

    pe_set$d_w <- 0
    for (j in 2:nrow(pe_set)) {
        pe_set$d_w[j] <- ifelse(pe_set$model[j] == pe_set$model[j-1], pe_set$width[j-1], 0)
    }
    pe_set$x_var <- pe_set$width + pe_set$d_w
    pe_set <- pe_set %>% group_by(.data$model) %>% mutate(x_pos = cumsum(.data$x_var))
    offset <- 0.005 * max(pe_set$x_pos, na.rm = TRUE)
    pe_set$width <- 2 * pe_set$width - offset
    var_gr <- ifelse("iwaoss" %in% pe_set$model, "Z", "D")

    ticks <- c(0)
    labels <- c("")
    ticks_size <- c(1)
    ticks_len <- c(-2)
    for (j in 1:(nrow(pe_set)/length(levels(as.factor(pe_set$model))))) {
        ticks <- c(ticks, ticks[length(ticks)] + pe_set$width[j]/2 + offset)
        ticks <- c(ticks, ticks[length(ticks)] + pe_set$width[j]/2 + offset)
        ticks_len <- c(ticks_len, c(0,-2))
        ticks_size <- c(ticks_size, c(0,1))
        labels <- c(labels, c(pe_set$colname[j], ""))
    }

    by_graph <- ggplot(data = pe_set, aes(x = .data$x_pos, y = .data$pe, fill = .data$model)) + 
        geom_col(width = pe_set$width, position = position_dodge2(padding = 0, preserve = "single"), color = "black") +
        xlab(sprintf("Quantile bins of \U0394%s", var_gr)) +
        ylab("") + ggtitle(sprintf("did_multiplegt_stat results by bins of \U0394%s", var_gr)) +
        scale_x_continuous(breaks= ticks, labels = labels) +
        theme(plot.title = element_text(hjust = 0.5), 
        axis.ticks.x = element_line(size = ticks_size), axis.ticks.length = unit(ticks_len, "mm"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line(color = "black", size = 0.2)) + geom_hline(yintercept = 0, color = "black", size = 0.2)
    })
    return(by_graph)
}