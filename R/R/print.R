#' @title summary method for did_multiplegt_stat
#' @name summary.did_multiplegt_stat
#' @description A customized printed display for did_multiplegt_stat output
#' @param object A did_multiplegt_stat object
#' @param ... Undocumented
#' @returns No return, just a custom summary method for did_multiplegt_stat output.
#' @export
summary.did_multiplegt_stat <- function(object, ...) {
    estims <- list(0, 1, 2)
    names(estims) <- c("aoss", "waoss", "ivwaoss") 

    if (is.null(object$args$estimator) & is.null(object$args$Z)) {
        estim_list <- c("aoss","waoss")
    } else if (is.null(object$args$estimator) & !is.null(object$args$Z)) {
        estim_list <- "ivwaoss"
    } else {
        estim_list <- object$args$estimator
    }

    if (is.null(object$args$by)) {
        by_levs <- c("_no_by")
        by_obj <- c("results")
    } else {
        by_levs <- object$by_levels
        by_obj <- c()
        for (res in 1:length(by_levs)) {
            by_obj <- c(by_obj, paste0("results_by_", res))
        }

        if (!is.null(object$args$by_fd)) {
            by_name <- "quantiles"
        } else if (!is.null(object$args[["by"]])) {
            by_name <- object$args$by
        }
        by_totl <- length(by_levs)

        cat("\n");
        cat(noquote(strrep("#", 70)));cat("\n");
        cat(sprintf("## did_multipegt_stat by %s (%.0f levels)", by_name, by_totl));cat("\n");
        cat(noquote(strrep("#", 70)));cat("\n");
    }

    for (temp in 1:length(by_obj)) {
        print_obj <- object[[by_obj[temp]]]

        if (by_levs[temp] != "_no_by") {
            msg <- paste0(" By level: ", by_levs[temp])
            cat(noquote(strrep("#", 70 - nchar(msg))));cat(msg);cat("\n");
        }

        cat("\n");
        cat(noquote(strrep("-", 35)));cat("\n");
        if ("ivwaoss" %in% estim_list) {
            strdisplay("N",print_obj$table[2*print_obj$pairs+1,5] + print_obj$table[2*print_obj$pairs+1,6])
        } else {
            if ("waoss" %in% estim_list) {
                strdisplay("N",print_obj$table[print_obj$pairs+1,5] + print_obj$table[print_obj$pairs+1,6])
            } else {
                strdisplay("N",print_obj$table[1,5] + print_obj$table[1,6])
            }
        }
        methods <- list(ra = "Reg. Adjustment", dr = "Doubly Robust", ps = "Propensity Score")
        method <- ifelse(is.null(object$args$estimation_method), "dr", object$args$estimation_method)
        method <- ifelse(isTRUE(object$args$exact_match), "ra", method)
        for (m in c("waoss", "ivwaoss")) {
            if (m %in% estim_list) { 
                strdisplay(paste0(toupper(m), " Method"), methods[[method]])            
            }
        }
        if (isFALSE(object$args$exact_match)) {
            strdisplay("Polynomial Order",object$args$order)
        }
        support <- c("Exact Matching", "No Extrapolation")
        index <- 1
        for (m in c("exact_match", "noextrapolation")) {
            if (isTRUE(object$args[[m]])) {
                strdisplay("Common Support",support[index])
            }
            index <- index + 1
        }
        if (!is.null(object$args$switchers)) {
            strdisplay("Switchers", object$args$switchers)
        }
        cat(noquote(strrep("-", 35)));cat("\n");
        if (!is.null(object$args$cluster)) {
            if (object$args$cluster != object$args$ID) {
                cat(sprintf("(Std. errors adjusted for %.0f clusters in %s)\n", 
                        print_obj$n_clusters[[1]], object$args$cluster))
            }
        }

        for (t in names(estims)){
            if (t %in% estim_list) {

                cat("\n");
                cat(noquote(strrep("-", 70)));cat("\n");
                cat(strrep(" ", 20));cat(sprintf("Estimation of %s(s)", toupper(t)));cat("\n");
                cat(noquote(strrep("-", 70)));cat("\n");

                l_bound <- 1 + estims[[t]] * print_obj$pairs 
                u_bound <- l_bound + isTRUE(object$args$disaggregate) * (print_obj$pairs - 1)
                mat_sel <- print_obj$table[l_bound:u_bound, ]
                mat_print(mat_sel, t)
                cat("\n");

                if (isTRUE(object$args$placebo)) {

                    cat("\n");
                    cat(noquote(strrep("-", 70)));cat("\n");
                    cat(strrep(" ", 15));cat(sprintf("Estimation of %s(s) - Placebo", toupper(t)));cat("\n");
                    cat(noquote(strrep("-", 70)));cat("\n");

                    l_bound <- 1 + estims[[t]] * print_obj$pairs 
                    u_bound <- l_bound + isTRUE(object$args$disaggregate) * (print_obj$pairs - 1)
                    mat_sel_placebo <- print_obj$table_placebo[l_bound:u_bound, ]

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
        
                tab_print(print_obj$aoss_vs_waoss)
        }
    }
}

#' @title print method for did_multiplegt_stat
#' @name print.did_multiplegt_stat
#' @description A customized printed display for did_continous output
#' @param x A did_multiplegt_stat object
#' @param ... Undocumented
#' @returns No return, just a custom summary print for did_multiplegt_stat output.
#' @export
print.did_multiplegt_stat <- function(x, ...) {
    summary(x)
}

#' Ancillary function for print/summary methods
#' @param mat mat
#' @param name name
#' @returns No return, just printing output.
#' @noRd
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

#' Ancillary function for print/summary methods
#' @param mat mat
#' @returns No return, just printing output.
#' @noRd
tab_print <- function(mat) {
    if (inherits(mat,"matrix")) {
        dis <- matrix(data = 0, nrow = nrow(mat), ncol = ncol(mat))
        dis[,1:ncol(dis)] <- sprintf("%s", format(round(mat[,1:ncol(mat)], 5), big.mark=",", scientific=FALSE, trim=TRUE))
        rownames(dis) <- rownames(mat)
        colnames(dis) <- colnames(mat)
        print(noquote(dis[, , drop = FALSE]))
    }
}

#' Ancillary function for print/summary methods
#' @param objs string object
#' @param objn numeric object
#' @returns No return, just printing output.
#' @noRd
strdisplay <- function(objs, objn) {
    ltot1 <- 16; ltot2 <- 16;
    out1 <- ifelse(nchar(objs) <= ltot1, paste0(objs,strrep(" ",ltot1 - nchar(objs))), substr(objs, 1, ltot1))
    if (inherits(objn, "character")) {
        out2 <- ifelse(nchar(objn) <= ltot2, paste0(strrep(" ",ltot2 - nchar(objn)), objn), substr(objn, 2, ltot2))
        cat(paste0(out1," = ",out2),"\n")
    } else {
        objns <- sprintf("%.0f", objn)
        strdisplay(objs, objns)
    }
}