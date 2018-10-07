#' Create estimates, CIs and P-values for a linear combination of two coefficients
#'
#'
#' @param model The fit of a generalized linear model (lm or glm)
#' @param lc Character vector for the linear combination(s) set equal to null value(s) (usually zero) to test.  Written in terms of variable names.
#' @param robust Logical expression indicating whether robust standard errors are to be used
#' @param expo Logical expression indicating whether exponentiated linear combination of coefficients is requiested
#' @param digits Number of digits to round to in output
#' @param type Character string giving type of robust standard error from sandwich package
#' @param level Numeric confidence coefficient for CIs (as proportion)
#' @param df Degrees of freedom for reference t distribution (Inf means Normal distribution)
#' @return A table of linear combination values or exponentiated linear combination values with CI(s)s (robust or not) and P-value(s).
#' @examples
#' x1 <- rbinom(100, 2, .5)
#' x2 <- rbinom(100, 2, .5)
#' expit <- function(x) exp(x)/(1 + exp(x))
#' y <- rbinom(100, 1, p = expit(x1 + x2 + .1))
#' model <- glm(y~x1*x2, family = "binomial")
#' variable.names(model)
#' lincom(model, lc = "x1 + x1:x2 == 0")
lincom <- function(model, lc, robust = FALSE, expo = TRUE, digits = 3, type = "HC1", level = .95, df = Inf){
  require(multcomp)
  require(sandwich)
  if(robust) {result <- glht(model, linfct = lc, vcov = vcovHC, type = type)
  ciname <- c("Robust CI")
  }
  else {result <- glht(model, linfct = lc)
  ciname <- c("CI")
  }
  if(expo) {theta <- exp(coef(result))
  ci <- exp(confint.default(result, df = df, level = level))
  name <- "exp(beta)"
  }
  else {theta <- coef(result)
  ci <- confint.default(result, df = df, level = level)
  name <- "beta"
  }
  tvals <- summary(result)$test$tstat
  pvals <- 2*pt(-abs(tvals), df = df)
  fullresult <- data.frame(theta,  ci[,1], ci[,2], pvals)
  names(fullresult) <- c(name, paste(colnames(ci)), "pval")
  cat("\nEstimate, ", round(100*level),"% ", ciname, ", and P-value:\n\n", sep = "")
  round(fullresult, digits)
}
