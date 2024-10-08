#' Internal function of did_multiplegt_stat that emulates Stata logit function.
#' @param formula formula
#' @param df df
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats binomial glm predict
#' @returns A glm object.
#' @noRd
stata_logit <- function(
    formula,
    df
) {
    suppressWarnings({
    model <- glm(formula, data = df, weights = df$weight_XX, family = binomial(link = 'logit'), maxit = 300, epsilon = 10^-8)
    })
    return(model)
}