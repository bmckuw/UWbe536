#' Computes a coefficient summary table for glms with robust standard errors and robust CIs
#'
#' @param model The fit of a generalized linear model (lm or glm)
#' @param type Character string giving type of robust standard error from sandwich package
#' @param df Degrees of freedom for reference t distribution (Inf means Normal distribution)
#' @param expo Logical expression indicating whether exponentiated linear combination of coefficients is requiested
#' @param level Numeric confidence coefficient for CIs (as proportion)
#' @param digits Number of digits to round to in output
#' @import sandwich
#' @import lmtest
#' @export robust

#' @return A table with coefficient estimates (exponentiated or not), CIs and P-values based on robust standard errors
#' @examples
#' x <- rbinom(100, 2, .5)
#' expit <- function(x) exp(x)/(1 + exp(x))
#' y <- rbinom(100, 1, p = expit(x + .1))
#' model <- glm(y~x, family = "binomial")
#' robust(model)
robust <- function(model, type = c("HC1"), df = Inf, expo = TRUE, level = .95, digits = 3){
  requireNamespace("multcomp", quietly = TRUE)
  result <-  coeftest(model, vcov. = vcovHC, type = type, df = df)
  ci <-  coefci(model, vcov. = vcovHC, type = type, df = df, level = level)
  result <- cbind(result[,1:3],  ci, result[,4])
  colnames(result)[6] <- "Pval"
  colnames(result)[2] <- "Robust SE"
  result <- round(result, digits)
  if(!expo){return(result)}
  else{
  expresult <- cbind(exp(coef(model)), exp(ci))
  colnames(expresult) <- c("exp(beta)", colnames(ci))
  expresult <- list(coef = result, expcoef = round(expresult,digits))
  #fullresult <- ifelse(expo, list(coef = result, expcoef = expresult))
  return(expresult)
          }
}


