#' Internal function for by option graph
#' @param obj obj
#' @import ggplot2
#' @returns The did_multiplegt_stat object + a ggplot object
#' @noRd
by_graph <- function(
    obj
    ){

    by_str <- obj$args$by[1]
    if (length(obj$args$by) > 1) {
    for (j in 2:length(obj$args$by)) {
        by_str <- paste0(by_str,",", obj$args$by[j])
    }      
    }
        
    pe_set <- as.data.frame(matrix(NA, ncol = 5, nrow = 0))
    names(pe_set) <- c("model", "pe", "lb", "ub")
    models <- c("aoss", "waoss", "ivwaoss")
    x_count <- 1
    n_ci <- length(obj$by_levels)
    v_off <- 0.6
    offset <- -(v_off/2) + (v_off/(n_ci -1)) * (1:n_ci -1)

    gr_col <- c()
    for (i in 1:3) {
        if (models[i] %in% obj$args$estimator) {
            pe_set_temp <- as.data.frame(matrix(NA, ncol = 5, nrow = length(obj$by_levels)))
            for (j in 1:length(obj$by_levels)) {
                subobj <- obj[[paste0("results_by_",j)]]
                pe_set_temp[j,2] <- subobj$table[subobj$pairs * (i-1) + 1, 1]
                pe_set_temp[j,3] <- subobj$table[subobj$pairs * (i-1) + 1, 3]
                pe_set_temp[j,4] <- subobj$table[subobj$pairs * (i-1) + 1, 4]
                pe_set_temp[j,5] <- sprintf("(%s)", obj$by_levels[j])
            }
            names(pe_set_temp) <- c("model", "pe", "lb", "ub", "id")
            pe_set_temp$model <- models[i]
            pe_set_temp$x_pos <- x_count - offset
            x_count <- x_count + 1            
            pe_set <- rbind(pe_set, pe_set_temp)
        }
    }
    pe_set <- subset(pe_set, !is.na(pe_set$pe) & !is.nan(pe_set$pe))
    by_graph <- ggplot(data = pe_set, aes(x = .data$x_pos, y = .data$pe, group = 1)) + 
        geom_point(size = 2, aes(color = factor(.data$model))) + 
        geom_errorbar(aes(ymin = .data$lb, ymax = .data$ub, color = factor(.data$model)), width = 0.1) +
        scale_x_continuous(breaks= pe_set$x_pos, labels = pe_set$id) + ggtitle(sprintf("did_multiplegt_stat results by %s", by_str)) + ylab(" ") + xlab(" ") +
        theme(plot.title = element_text(hjust = 0.5), axis.ticks.y = element_blank(), legend.position = "right",
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line(color = "black", size = 0.2)) + geom_hline(yintercept = 0, color = "black", size = 0.2) + labs(color = "Estimators")
    return(by_graph)
}