#' @title rnames method for did_multiplegt_stat
#' @name rnames.did_multiplegt_stat
#' @description A customized rnames method for did_multiplegt_stat output
#' @param object A did_multiplegt_stat object
#' @param ignore Ignored sublists
#' @param ... Undocumented
#' @returns The same output as rnames.
#' @export
rnames.did_multiplegt_stat <- function(object, ignore = c("by_fd_graph", "by_graph", "args"), ...) {
    rnames(object = object, ignore = ignore)    
}