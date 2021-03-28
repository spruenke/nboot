#' Computes row- or columnwise sample variances.
#'
#' @param x Numeric Matrix or data.frame.
#' @param margin Scalar giving the subscripts which the function will be applied over, such as in apply. 1 indicates rows, 2 indicates columns.
#' @return Vector of sample variances of each row/column.
#'
matVar <- function(x, margin = 1){
  if(is.data.frame(x)){
    y <- as.matrix(x)
  } else if (is.matrix(x)){
    y <- x
  } else {
    stop("x is neither matrix nor data.frame")
  }

  if(!(margin %in% c(1,2))) stop("margin must be either 1 (rows) or 2 (columns)")
  if(any(!is.numeric(y))) stop("x must be completely numeric")

  if(margin == 1){
    n <- ncol(y)
    xSq <- rowSums(y^2)
    xBar <- rowMeans(y)
    Var <- (xSq - n * xBar^2) / (n-1)
  } else if(margin == 2){
    n <- nrow(y)
    xSq <- colSums(y^2)
    xBar <- colMeans(y)
    Var <- (xSq - n * xBar^2) / (n-1)
  }
  return(Var)
}
