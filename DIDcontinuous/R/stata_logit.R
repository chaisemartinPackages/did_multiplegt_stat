#' Internal function of did_continuous
#' @noRd
#' @param formula formula
#' @param df df
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats binomial glm predict
stata_logit <- function(
    formula,
    df
) {
    suppressWarnings({
    sensitivity <- 10^-8
    while (TRUE) {
        model <- glm(formula, data = df, weights = df$weights, family = binomial(link = 'logit'))
        df$pred <- NULL; df$pred <- predict(model, newdata = df, type="response");
        df_temp <- subset(df, df$pred > sensitivity & df$pred < 1 - sensitivity)
        if (nrow(df_temp) < nrow(df) & nrow(df_temp) > 0) {
            df <- NULL; df <- df_temp; df_temp <- NULL;
        } else {
            break
        }
    }
    })
    return(model)
}