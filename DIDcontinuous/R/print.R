#' @title summary method for did_continuous
#' @name summary.did_continuous
#' @description A customized printed display for did_continuous output
#' @param object A did_continuous object
#' @param ... Undocumented
#' @export
summary.did_continuous <- function(object, ...) {
    estims <- list(0, 1, 2)
    names(estims) <- c("aoss", "waoss", "iwaoss") 

    if (is.null(object$args$estimator) & is.null(object$args$Z)) {
        estim_list <- c("aoss","waoss")
    } else if (is.null(object$args$estimator) & !is.null(object$args$Z)) {
        estim_list <- "iwaoss"
    } else {
        estim_list <- object$args$estimator
    }

    for (t in names(estims)){
        if (t %in% estim_list) {
            cat("\n");
            cat(noquote(strrep("-", 70)));cat("\n");
            cat(strrep(" ", 20));cat(sprintf("Estimation of %s(s)", toupper(t)));cat("\n");
            cat(noquote(strrep("-", 70)));cat("\n");

            l_bound <- 1 + estims[[t]] * object$results$pairs 
            u_bound <- l_bound + isTRUE(object$args$disaggregate) * (object$results$pairs - 1)
            mat_sel <- object$results$table[l_bound:u_bound, ]
            mat_print(mat_sel, t)
            cat("\n");

            if (isTRUE(object$args$placebo)) {
                cat("\n");
                cat(noquote(strrep("-", 70)));cat("\n");
                cat(strrep(" ", 15));cat(sprintf("Estimation of %s(s) - Placebo", toupper(t)));cat("\n");
                cat(noquote(strrep("-", 70)));cat("\n");

                l_bound <- 1 + estims[[t]] * object$results$pairs 
                u_bound <- l_bound + isTRUE(object$args$disaggregate) * (object$results$pairs - 1)
                mat_sel_placebo <- object$results$table_placebo[l_bound:u_bound, ]
                mat_print(mat_sel_placebo, t)
                cat("\n");
            }
        }
    }

    if (isTRUE(object$args$aoss_vs_waoss)) {
            cat("\n");
            cat(noquote(strrep("-", 70)));cat("\n");
            cat(strrep(" ", 15));cat("Difference test: AOSS and WAOSS");cat("\n");
            cat(noquote(strrep("-", 70)));cat("\n");
            cat("H0: AOSS = WAOSS\n");
            tab_print(object$results$aoss_vs_waoss)
    }
}

#' @title print method for did_continuous
#' @name print.did_continuous
#' @description A customized printed display for did_continous output
#' @param x A did_continuous object
#' @param ... Undocumented
#' @export
print.did_continuous <- function(x, ...) {
    summary(x)
}

mat_print <- function(mat, name) {
    if (inherits(mat,"matrix")) {
        dis <- matrix(data = 0, nrow = nrow(mat), ncol = ncol(mat))
        dis[,1:4] <- sprintf("%s", format(round(mat[,1:4], 5), big.mark=",", scientific=FALSE, trim=TRUE))
        dis[,5:ncol(dis)] <- 
            sprintf("%s", format(round(mat[,5:ncol(dis)], 0), big.mark=",", scientific=FALSE, trim=TRUE))
        rownames(dis) <- rownames(mat)
        colnames(dis) <- colnames(mat)
        print(noquote(dis[, , drop = FALSE]))
    } else {
        new_mat <- t(as.matrix(mat))
        rownames(new_mat) <- toupper(name)
        mat_print(new_mat) 
    }
}

tab_print <- function(mat) {
    if (inherits(mat,"matrix")) {
        dis <- matrix(data = 0, nrow = nrow(mat), ncol = ncol(mat))
        dis[,1:ncol(dis)] <- sprintf("%s", format(round(mat[,1:ncol(mat)], 5), big.mark=",", scientific=FALSE, trim=TRUE))
        rownames(dis) <- rownames(mat)
        colnames(dis) <- colnames(mat)
        print(noquote(dis[, , drop = FALSE]))
    }
}