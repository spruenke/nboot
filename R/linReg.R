#' Compute bootstrapped linear regression
#'
#' @param formula Formula of the linear model
#' @param data Data to use for the formula
#' @param subset An optional vector specifying a subset of observations to be used in the fitting process
#' @param y Numeric Vector of dependent variables (if formula is not provided)
#' @param X Numeric Matrix or data.frame of dim n x k, where n is the number of observations and k the number of independent variables (if formula is not provided)
#' @param nboot Number of bootstrap iterations (default is 100)
#' @param intercept Logical, specifying whether an intercept should be used (default is TRUE, only relevant if formula is not provided)
#' @param boot.type Character string specifying which bootstrap type should be used. Must be either "np" (Non-parametric, default) or "wild" (Wild Rademacher bootstrap)
#' @return A list of class "lm"
lmBoot <- function(formula, y = NULL, X = NULL, nboot = 100, intercept = T, boot.type = "np", subset = NULL){
  if(!(boot.type %in% c("np", "wild"))) stop("boot.type has to be either 'np' or 'wild'")
    if(missing(formula)){
        if(is.null(y) || is.null(X)) stop("Either formula and data, or y and X have to be provided")
        if(!is.numeric(y)) stop("Response must be numeric.")
        if(!is.numeric(X)) stop("Predictors have to be numeric. Code Factors as dummies.")
        if(!is.logical(intercept)) stop("Intercept has to be logical")
        if(intercept == T){
            data <- cbind(y, cbind(rep(1, nrow(X)),as.matrix(X)))
        } else if(intercept == F){
            data <- cbind(y, as.matrix(X))
        }
    } else {
          cl <- match.call()
          mf <- match.call(expand.dots = FALSE)
          m  <- match(c("formula", "data", "subset"),
                     names(mf), 0L)
          mf <- mf[c(1L, m)]

          mf$drop.unused.levels <- TRUE

          mf[[1L]] <- quote(stats::model.frame)
          mf       <- eval(mf, parent.frame())
          mt       <- attr(mf, "terms")
          y        <- model.response(mf, "numeric")
          mlm      <- is.matrix(y)
          ny       <- if (mlm) nrow(y) else length(y)
          x        <- model.matrix(mt, mf)
          data     <- cbind(y, x)
    }
    switch(boot.type,
           np = {
              beta.boot <- .betaBootNp(data[,1], data[,-1], nboot)
           },
           wild = {
              beta.boot <- .betaBootWild(data[,1], data[,-1], nboot)
           }
           )
    beta.hat <- rowMeans(beta.boot)
    names(beta.hat) <- colnames(data[,-1])
    fitted.values <- data[,-1]%*%beta.hat
    resid <- data[,1] - fitted.values
    names(resid) <- names(fitted.values) <- as.character(c(1:ny))
    df <- ny - (ncol(data) - 1)
    ret <- list() # coefficients, residuals, rank, fitted.values, df.residual, call, terms, model
    ret$coefficients <- beta.hat
    ret$residuals <- resid
    ret$rank <- ncol(data) - 1
    ret$fitted.values <- fitted.values
    ret$df.residual <- df
    ret$x <- data[,-1]
    ret$y <- data[,1]
    ret$call <- cl
    ret$terms <- mt
    ret$model <- mf
    class(ret) = c("lm")
    return(ret)
}
