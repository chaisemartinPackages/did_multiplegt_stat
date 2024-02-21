#' Main interface for did_continuous
#' @importFrom haven read_dta
#' @md
#' @description Estimation of Difference-in-Difference (DID) Estimators for Treatments and Instruments Continuously Distributed at Every Period with Stayers.
#' @param df (data.frame) A dataframe object.
#' @param Y (char) Outcome variable.
#' @param ID (char) Identifier of the unit of analysis.
#' @param Time (char) Time variable. The command assumes that the time variable is evenly spaced (e.g.: the panel is at the yearly level, and no year is missing for all groups). When it is not (e.g.: the panel is at the yearly level, but three consecutive years are missing for all groups), the command can still be used. For example, if the year n is missing, the command does not comuptes the DID estimators of the pairs of years (n-1,n),(n,n+1), and (n-1,n+1).
#' @param D (char) Treatment variable.
#' @param Z (char) Instrumental variable. This option is only required when the IV-related estimator (the so-called iwaoss) is requested.
#' @param estimator (char vector) Estimator(s) to be computed. The allowed arguments are: (1) "aoss", i.e the Average Of Switchers’ Slopes which is the average, across switchers, of the effect on their period-(t) outcome of moving their treatment from its period-(t-1) to its period-(t) value, scaled by the difference between these two values. (2) "waoss" which corresponds to a weighted version of "aoss" where slopes receive a weight proportional to switchers’ absolute treatment change from period-(t-1) to period-(t). (3) "iwaoss" which generalizes "waoss" to the instrumental-variable case, and is equal to the reduced-form "waoss" effect of the instrument on the outcome, divided by the first-stage "waoss" effect of the instrument on the treatment. If this option is not specified: by default, the command estimates both "aoss" and "waoss" if the instrumental-variable Z is not specified, or only iwaoss otherwise. 
#' @param estimation_method (char) This option allows to specify which estimation method to use when estimating the waoss or the iwaoss, as described in de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Vazquez‐Bare, G (2022). It takes as argument "ra" (regression adjustment-based approach), or "ps" (propensity-based approach), or "dr" (double robust-based approach).
#' @param order (int) This option takes as argument the order of the polynomial series used to estimate the counterfactual of the variation of the outcome from period t-1 to period t  for the switchers, namely $E(Y_t - Y_t-1 |D_{t-1}, S_t = 0)$ or $E(Y_t - Y_t-1 |Z_{t-1}, SI_t = 0)$. 
#' @param switchers (char) The allowed inputs for this option are "up" and "down". If the argument "up" is specified, the command estimates the effects on switchers-up, i.e, units whose treatments (or instruments) increase from period t-1 to period t. If the argument "down" is given, the command estimates the effects on switchers-down, i.e., units whose treaments (or instruments) decrease from period t-1 to period t.
#' @param disaggregate (logical) If this potion is specified, the command displays the estimands of the effects for each two consecutive periods as well as the aggregated estimands. Otherwise, the command only outputs the aggregated results.
#' @param placebo (logical) This option allows to estimate the placebos versions of the estimators requested in the estimator option. If this option is combined with the option disaggregate, the command also displays the placebo version of each two consecutive time-periods.
#' @param weight (char) TBD.
#' @param noextrapolation (logical) This option forces the command to use only switchers whose period-(t-1) treatments (or instruments) are between the minimum and the maximum values of the period-(t-1) treatments (or instruments) of the stayers. This a less restrictive common support assumption.
#' @param aoss_vs_waoss (logical) As highlighted in de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Vazquez‐Bare, G (2022), the aoss and the waoss are equal if and only if switchers’ slopes are uncorrelated with $|D_t - D_{t-1}|$. When this option is specified, the command performs and displays the test of the equality between the aoss and  the waoss. Note that the use of this option requires specifying in the estimator option both aoss and waoss.
#' @param exact_match exact_match
#' @param cluster cluster
#' @section Overview:
#' did_continuous estimates difference-in-differences estimators for continuous treatments with heterogeneous effects, assuming that between consecutive periods, the treatment of some units, the switchers, changes, while the treatment of other units does not change. It computes the three estimators (including an IV-related estimator) introduced in [de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Vazquez‐Bare, G (2022)](https://ssrn.com/abstract=4011782). The estimators computed by the command assume static effects and rely on a parallel trends assumptions.
#' 
#' The command can be used with more than two periods. If the number of periods is greater than two, the command estimates, for each pair of two successive periods, the requested DID estimators (aoss, waoss or iwaoss) as well as their aggregated versions defined as a weighted average of the estimators of the different pairs of periods. The command can also be used when the panel data is unbalaced or presents gaps.
#' 
#' The command also computes, when the number of periods is larger than two, the placebos versions of the different estimators  for each two successive time periods, and the aggregated versions. Thus, allowing to test for parallel trends assumptions under which the proposed estimators computed by did_continuous are unbiased.
#' 
#' This command can also be used when the treatment is discrete. In particular, when the treatment is discrete and takes a large number of values and the number of periods is equal to two, did_continuous can be used as an alternative to the did_multiplegt_dyn command, which may not be applicable in such a design since it requires finding switchers and controls with the same period-one treatment. When the number of periods is larger than two, the two commands estimate two different models (static effects for did_continuous, and dynamic effects for did_multiplegt_dyn).
#' @section FAQ:
#' TBD
#' @section Comparison with Stata command:
#' Stata "logit" and R "glm" functions handle binary prediction with slightly different conventions. These discrepancies are usually negligible, but they may add up to detectable (yet small) differences in the final estimates. The next code blocks showcase an instance where the logit and glm predictions differ. We estimate a logistic regression of a binary variable D on an order 2 polynomial of a continuous variable X. The binary variable D takes value 1 only for D = 38.4. Due to these sample features, the logit command fails to converge. Both commands yield non missing predictions from their respective regression outputs. However, the predicted values at rows 8 and 9 are strictly different, with Stata reporting way larger predictions than R.
#' ## Stata
#' 
#' global rep "https://raw.githubusercontent.com/chaisemartinPackages"
#' 
#' use "$rep/ApplicationData/main/Tests/logit_tests.dta", clear
#' 
#' cap logit D X X_sq, asis
#' 
#' predict D_hat, pr asif
#' 
#' browse
#' 
#' ## R
#' 
#' library(haven)
#' 
#' library(stats)
#' 
#' rep <- "https://raw.githubusercontent.com/chaisemartinPackages"
#' 
#' data <- haven::read_dta(paste0(rep,"/ApplicationData/main/Tests/logit_tests.dta"))
#' 
#' model <- glm(D ~ X + X_sq, data = data, family = binomial(link = "logit"))
#' 
#' data$D_hat <- predict(model, newdata = data, type="response")
#' 
#' View(data)
#' 
#' @section References:
#' de Chaisemartin, C, D'Haultfoeuille, X, Pasquier, F, Vazquez‐Bare, G (2022). [Difference-in-Differences for Continuous Treatments and Instruments with Stayers](https://ssrn.com/abstract=4011782)
#' @examples
#' # In the following example, we use data from Li et al. (2014). 
#' # The dataset can be downloaded from GitHub:
#' repo <- "chaisemartinPackages/ApplicationData/main" 
#' file <- "data_gazoline.dta"
#' url <- paste("https://raw.githubusercontent.com", repo, file, sep = "/")
#' gazoline <-  haven::read_dta(url)

#' # Estimating the effect of gasoline taxes on gasoline consumption and prices
#' summary(did_continuous(
#'     df = gazoline,
#'     Y = "lngca",
#'     ID = "id",
#'     Time = "year",
#'     D = "tau",
#'     order = 1,
#'     estimator = "waoss",
#'     estimation_method = "dr",
#'     noextrapolation = TRUE
#' ))
#' @returns A list with two sublists. The first sublist includes the arguments used in the command. The second sublist includes the results from the estimation. Regardless of the options, the output in object$results$table will be a (3 x object$results$pairs) x 6 matrix, where only the requested output (that is, the rows corresponding to the estimators requested) will be non-missing. The list is given a did_continuous class to trigger custom method dispatches by the print and summary functions. 
#' @export
did_continuous <- function(
    df,
    Y,
    ID,
    Time,
    D,
    Z = NULL,
    estimator = NULL,
    estimation_method = NULL,
    order = 1,
    noextrapolation = FALSE,
    placebo = NULL,
    weight = NULL,
    switchers = NULL,
    disaggregate = FALSE,
    aoss_vs_waoss = FALSE,
    exact_match = FALSE,
    cluster = NULL
) {
  args <- list()
  for (v in names(formals(did_continuous))) {
    if (!is.null(get(v))) {
      if (v == "df" & !inherits(get(v), "data.frame")) {
        stop(sprintf("Syntax error in %s option. Dataframe required.",v))
      } else if (v %in% c("Y", "ID", "Time", "D", "Z", "estimation_method", "switchers", "cluster")) {
        if (!(inherits(get(v), "character") & length(get(v)) == 1)) {
          stop(sprintf("Syntax error in %s option. The option requires a single string.",v))
        }
      } else if (v == "estimator") {
        if (!inherits(get(v), "character")) {
          stop(sprintf("Syntax error in %s option. Character vector required.",v))
        }
      } else if (v %in% c("noextrapolation", "placebo", "disaggregate", "aoss_vs_waoss", "exact_match")) {
        if (!inherits(get(v), "logical")) {
          stop(sprintf("Syntax error in %s option. Logical required.",v))
        }
      } else if (v == "order") {
          if (!(inherits(get(v), "numeric") & get(v) %% 1 == 0)) {
          stop(sprintf("Syntax error in %s option. Integer required.",v))
          }
      }
    }
    if (v != "df") {
      args[[v]] <- get(v)
    }
  }

  if (is.null(estimator) & is.null(Z)) {
      estimator <-  c("aoss", "waoss")
  } else if (is.null(estimator) & !is.null(Z) ) {
      estimator <- "iwaoss"
  }

  if (!is.null(estimator) & length(intersect(estimator, c("aoss","waoss","iwaoss"))) != length(estimator)) {
    stop("Syntax error in estimator option: only aoss, waoss and iwaoss allowed.")
  }

  if (!is.null(switchers)) {
      if (!(switchers %in% c("up", "down"))) {
        stop("Switchers could be either NULL, up or down")          
      }
  }

  if (is.null(estimation_method)) {
    estimation_method <- "ra"
  }

  if (isTRUE(exact_match)) {
    if (estimation_method != "ra") {
      stop("The exact_match option is only compatible with regression adjustment estimation method")
    }
    if (isTRUE(noextrapolation)) {
      message("When exact_match and noextrapolation are both specified, the command will only consider the option exact_match.")
      noextrapolation <- FALSE
    }
    if (order != 1) {
      stop("The order option is not compatible with exact_match.")
    } else {
      order <- 1
    }
  }

  if (!(estimation_method %in% c("ra", "dr", "ps"))) {
      stop("Syntax error in estimation_method option.")
  }
  if (length(estimator) == 1) {
    if (estimation_method %in% c("dr","ps")  & estimator == "aoss") {
      stop("The propensity score-based approach is only available for the waoss and the iwaoss.")
    }
  }

  if ("iwaoss" %in% estimator & sum(c("aoss", "waoss") %in% estimator)) {
    stop("The estimation of AOSS or WAOSS cannot be combined with the estimation of IV-WAOSS (see helpfile).")
  }

  if (isTRUE(aoss_vs_waoss) & sum(c("aoss","waoss") %in% estimator) != 2) {
	  stop("To test the equility between AOSS and WAOSS you must specify aoss and waoss in the estimator option.")
  }

  if ("iwaoss" %in% estimator & is.null(Z)) {
    stop("To compute the iwaoss you must specify the IV variable.")
  }

  results <- did_continuous_main(df = df, Y = Y, ID = ID, Time = Time, D = D, Z = Z, estimator = estimator, estimation_method = estimation_method, order = order, noextrapolation = noextrapolation, placebo = placebo, weight = weight, switchers = switchers, disaggregate = disaggregate, aoss_vs_waoss = aoss_vs_waoss, exact_match = exact_match, cluster = cluster)

  did_continuous <- list(args, results)
  names(did_continuous) <- c("args", "results")
  class(did_continuous) <- c(class(did_continuous), "did_continuous")
  return(did_continuous)
}
