#' @title A print method for did_multiplegt_dyn
#' @name print.did_continuous
#' @description A customized printed display for did_continous output
#' @param x A did_continuous object
#' @param ... Undocumented
#' @export
print.did_continuous <- function(x, ...) {
    estims <- list(0, 1, 2)
    names(estims) <- c("aoss", "waoss", "iwaoss") 

    for (t in names(estims)){
        if (t %in% x$args$estimator) {
            cat("\n");
            cat(noquote(strrep("-", 70)));cat("\n");
            cat(strrep(" ", 20));cat(sprintf("Estimation of %s(s)", toupper(t)));cat("\n");
            cat(noquote(strrep("-", 70)));cat("\n");

            l_bound <- 1 + estims[[t]] * x$results$pairs 
            u_bound <- l_bound + isTRUE(x$args$disaggregate) * (x$results$pairs - 1)
            mat_sel <- x$results$table[l_bound:u_bound, ]
            mat_print(mat_sel, t)
            cat("\n");
        }
    }
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