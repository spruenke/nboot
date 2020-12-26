#' Computes rowwise sample variances
#'
#' @param x Numeric Matrix
#' @return Vector of sample variances of each row
#' 

rowVar = function(x){
    n = ncol(x)
    xSq = rowSums(x^2)
    xBar = rowMeans(x)
    rVar = (xSq - n * xBar^2) / (n-1)
    return(rVar)
}


#' Computes rowwise sample variances
#'
#' @param x Numeric Matrix
#' @return Vector of sample variances of each row
#' 
colVar = function(x){
    n = nrow(x)
    xSq = colSums(x^2)
    xBar = colMeans(x)
    cVar = (xSq - n * xBar^2) / (n-1)
    return(cVar)
}
  