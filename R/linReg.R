#' Compute bootstrapped linear regression
#'
#' @param y Numeric Vector of dependent variables
#' @param X Numeric Matrix or data.frame of dim n x k, where n is the number of observations and k the number of independent variables
#' @param nboot Number of bootstrap iterations (default is 100)
#' @param intercept Logical, specifying whether an intercept should be used (default is TRUE)
#' @param boot.type Character string specifying which bootstrap type should be used. Must be either "np" (Non-parametric, default) or "wild" (Wild Rademacher bootstrap)
#' @return Vector of sample skewness for all rows
lmBoot <- function(y, X, nboot = 100, intercept = T, boot.type = "np"){
    if(intercept == T){
        data <- cbind(y, cbind(rep(1, nrow(X)),as.matrix(X)))
    } else if(intercept == F){
        data <- cbind(y, as.matrix(X))
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
    return(beta.hat)
}
