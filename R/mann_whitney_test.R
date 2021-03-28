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

mann.whitney.testBoot <- function(x, y, alpha = 0.05, nboot = 100, boot.type = "perm"){
    u.true <- .u_exact(x, y)
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
    crits = quantile(dist, probs = c(1-alpha))
    ret = u.true > crits
    return(ret)
}
