#' Computes sample skewness for each row of a matrix
#'
#' @param x Numeric Matrix
#' @return Vector of sample skewness for all rows
rowSkew <- function(x){
  res <- .rowSkewCpp(x)
  return(res)
}

#' Computes sample skewness for each column of a matrix
#'
#' @param x Numeric Matrix
#' @return Vector of sample skewness for all columns
colSkew <- function(x){
  res <- .colSkewCpp(x)
  return(res)
}

#' Computes sample excess kurtosis for each row of a matrix
#'
#' @param x Numeric Matrix
#' @return Vector of sample excess kurtosis for all rows
rowKurt <- function(x){
  res <- .rowKurtCpp(x)
  return(res)
}

#' Computes sample excess kurtosis for each column of a matrix
#'
#' @param x Numeric Matrix
#' @return Vector of sample excess kurtosis for all columns
colKurt <- function(x){
  res <- .colKurtCpp(x)
  return(res)
}

#' Computes rowwise minimum of a numeric matrix
#'
#' @param x Numeric Matrix
#' @return Vector of minimum value of each row
rowMin <- function(x){
  res <- .rowMinCpp(x)
  return(res)
}


#' Computes rowwise maximum of a numeric matrix
#'
#' @param x Numeric Matrix
#' @return Vector of maximum value of each row
rowMax <- function(x){
  res <- .rowMaxCpp(x)
  return(res)
}


#' Computes columnwise minimum of a numeric matrix
#'
#' @param x Numeric Matrix
#' @return Vector of minimum value of each column
colMin <- function(x){
  res <- .colMinCpp(x)
  return(res)
}

#' Computes columnwise maximum of a numeric matrix
#'
#' @param x Numeric Matrix
#' @return Vector of maximum value of each column
colMax <- function(x){
  res <- .colMaxCpp(x)
  return(res)
}
#' Computes columnwise quantiles of a numeric matrix
#'
#' @param x Numeric Matrix
#' @param probs Vector of quantiles to compute
#' @return Vector/Matrix of columnwise quantiles
colQuant <- function(x, probs = 0.5){
  res <- .colQuantCpp(x, probs)
  if(length(probs) == 1){
        res <- as.numeric(res)
  } else if(length(probs) > 1){
      colnames(res) <- paste0(probs*100, "%")
  }
  return(res)
}


#' Computes rowwise quantiles of a numeric matrix
#'
#' @param x Numeric Matrix
#' @param probs Vector of quantiles to compute
#' @return Vector/Matrix of rowwise quantiles
rowQuant <- function(x, probs = 0.5){
  res <- .rowQuantCpp(x, probs)
  if(length(probs) == 1){
      res <- as.numeric(res)
  } else if(length(probs) > 1){
      colnames(res) <- paste0(probs * 100, "%")
  }
  return(res)
}
