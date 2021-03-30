.comp_fun <- function(a){
  x <- a[1]
  y <- a[2]
  if(x == y){
    return(0.5)
  } else if(x > y) {
    return(1)
  } else {
    return(0)
  }
}

.u_exact <- function(x, y){
  u.true <- 0
  n.1    <- length(x)
  n.2    <- length(y)
  for(i in 1:n.1){
    for(j in 1:n.2){
      u.true <- u.true + .comp_fun(c(x[i], y[j]))
    }
  }
  return(u.true)

}

#' Resampling version of the student t-Test for one or two samples
#'
#' @param x,y Sample Data (Non-empty numeric vectors)
#' @param alpha Significance level (default is 0.05).
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @param nboot Integer specifying the number of bootstrap iterations (default is 100)
#' @param boot.type A character string specifying the bootstrap type, must be one of "perm" (Permutation, default) or "np" (Non-parametric bootstrap).
#' @return A list of class "htest"
#'
mann.whitney.testBoot <- function(x, y, alpha = 0.05, alternative = "two.sided", nboot = 100, boot.type = "perm"){
  # error handlers ----------------------------------------------------------
    if(!is.numeric(x) || !is.numeric(y)) stop("Data must be numeric")
    if(!is.numeric(alpha)) stop("Signifiance level must be numeric")
    if(!is.numeric(nboot)) stop("Number of Bootstrap Iterations must be numeric")
    if(!(alternative %in% c("two.sided", "less", "greater"))) stop("Alternative hypothesis must be one of 'two.sided', 'less' or 'greater'")
    if(!(boot.type %in% c("np", "perm"))) stop("Specified resampling type not known")

# Compute Statistics ------------------------------------------------------
    u.true <- .u_exact(x, y)
    n = length(x) + length(y)
    switch(boot.type,
             perm = {
               dist   <- .uExactBootPerm(x, y, nboot)
               est    <- "Permutation"
             },
             np = {
               dist   <- .uExactBootNp(x, y, nboot)
               est    <- "Nonparametric"
             }
           )
    crits <- quantile(dist, probs = c(alpha/2, alpha, 1-alpha, 1-alpha/2))
    switch(alternative,
           "two.sided" = {

             ret <- u.true < crits[1] || u.true > crits[4]
             p.v <- min(2 * mean(dist > u.true), 2 - 2 * mean(dist > u.true))

           },
           "greater" = {
             ret <- u.true > crits[3]
             p.v <- mean(dist > u.true)
           },
           "less" = {
             ret <- u.true < crits[2]
             p.v <- mean(dist < u.true)
           }

           )



# Prepare Results ---------------------------------------------------------

    dat.nam         <- c(deparse(substitute(x)), deparse(substitute(y)))
    dat.nam.2       <- paste(dat.nam[1], "and", dat.nam[2])
    h.0             <- 0
    names(h.0)      <- "location shift"
    pop.stat        <- u.true
    names(pop.stat) <- "U"

    t.list        <- list("reject" = ret ,"null.value" = h.0, "alternative" = alternative, "method" = "Mann-Whitney-U-Test", "data.name" = dat.nam.2, "statistic" = pop.stat, "p.value" = p.v, "estimation.method" = est, "sample.size" = n)
    class(t.list) <- c("htest", class(t.list))
    return(t.list)

}
