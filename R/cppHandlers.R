#' Computes row- or columnwise sample skewness.
#'
#' @param x Numeric Matrix or data.frame.
#' @param margin Scalar giving the subscripts which the function will be applied over, such as in apply. 1 indicates rows (default), 2 indicates columns.
#' @return Vector of sample skewness of each row/column.
#'
matSkewness <- function(x, margin = 1){
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
    res <- .rowSkewCpp(y)
  } else if(margin == 2){
    res <- .colSkewCpp(y)
  }
  return(res)
}

#' Computes row- or columnwise sample kurtosis.
#'
#' @param x Numeric Matrix or data.frame.
#' @param margin Scalar giving the subscripts which the function will be applied over, such as in apply. 1 indicates rows (default), 2 indicates columns.
#' @return Vector of sample kurtosis of each row/column.
#'
matKurtosis <- function(x, margin = 1){
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
    res <- .rowKurtCpp(y)
  } else if(margin == 2){
    res <- .colKurtCpp(y)
  }
  return(res)
}

#' Computes row- or columnwise Minimum or Maximum.
#'
#' @param x Numeric Matrix or data.frame.
#' @param margin Scalar giving the subscripts which the function will be applied over, such as in apply. 1 indicates rows (default), 2 indicates columns.
#' @param min Logical, whether the Minimum should be computed (default is TRUE), otherwise Maximum is computed (FALSE).
#' @return Vector of Min/Max of each row/column.
#'
matMinMax <- function(x, margin = 1, min = T){
  if(is.data.frame(x)){
    y <- as.matrix(x)
  } else if (is.matrix(x)){
    y <- x
  } else {
    stop("x is neither matrix nor data.frame")
  }

  if(!(margin %in% c(1,2))) stop("margin must be either 1 (rows) or 2 (columns)")
  if(any(!is.numeric(y))) stop("x must be completely numeric")
  if(!is.logical(min)) stop("min must be logical")
  if(margin == 1){
    if(min == T){
      res <- .rowMinCpp(y)
    } else {
      res <- .rowMaxCpp(y)
    }
  } else if(margin == 2){
    if(min == T){
      res <- .colMinCpp(y)
    } else {
      res <- .colMaxCpp(y)
    }
  }
  return(res)
}

#' Computes row- or columnwise quantiles. The computation mimics the default computation of stat's quantile function (type 7).
#'
#' @param x Numeric Matrix or data.frame.
#' @param margin Scalar giving the subscripts which the function will be applied over, such as in apply. 1 indicates rows (default), 2 indicates columns.
#' @param probs Vector of quantiles to compute, default is 0.5.
#' @return Vector of row-/columnwise quantiles specified in probs.
#'
matQuantile <- function(x, margin = 1, probs = 0.5){
  if(is.data.frame(x)){
    y <- as.matrix(x)
  } else if (is.matrix(x)){
    y <- x
  } else {
    stop("x is neither matrix nor data.frame")
  }

  if(!(margin %in% c(1,2))) stop("margin must be either 1 (rows) or 2 (columns)")
  if(any(!is.numeric(y))) stop("x must be completely numeric")
  if(!is.numeric(probs)) stop("Probabilities must be numeric")
  if(margin == 1){
    res <- .rowQuantCpp(y, probs)
  } else if(margin == 2){
    res <- .colQuantCpp(y, probs)
  }
  if(length(probs) == 1){
    res <- as.numeric(res)
  } else if(length(probs) > 1){
    colnames(res) <- paste0(probs * 100, "%")
  }
  return(res)
}


#' Computes row- or columnwise ranks. Ties are adjusted with mean-values of given ranks (statistical tie handling).
#'
#' @param x Numeric Matrix or data.frame.
#' @param margin Scalar giving the subscripts which the function will be applied over, such as in apply. 1 indicates rows (default), 2 indicates columns.
#' @return Vector of row-/columnwise ranks.
#'
matRank <- function(x, margin = 1){
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
    res <- .RowRankAvg(y)
  } else if(margin == 2){
    res <- .ColRankAvg(y)
  }
  return(res)
}
