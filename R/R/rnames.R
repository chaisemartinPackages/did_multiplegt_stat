#' @title rnames method for did_multiplegt_stat
#' @name rnames.did_multiplegt_stat
#' @description A customized rnames method for did_multiplegt_stat output
#' @param obj A did_multiplegt_stat object
#' @param ... Undocumented
#' @import rnames
#' @returns The same output as rnames.
#' @export
rnames.did_multiplegt_stat <- function(obj, ...) {
    class(obj) <- "list"
    return(rnames(obj = obj, ignore = c("by_fd_graph", "by_graph", "cdf_plot", "args")))
}