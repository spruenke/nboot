#' Bootstrap version of the student t-Test for one or two samples
#'
#' @param x Sample Data (Non-empty numeric vector)
#' @param y Optional Non-empty numeric vector to compare against (two-sample-test).
#' @param mu.0 Expected value under the null hypothesis (one-sample-test), default is 0.
#' @param alpha Significance level (default is 0.05).
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @param nboot Integer specifying the number of bootstrap iterations (default is 100)
#' @param boot.type A character string specifying the bootstrap type, must be one of "np" (Non-parametric bootstrap, default), "wild" (Rademacher wild bootstrap) or "npg" (groupwise non-parametric, two-sample only).
#' @return A list of class "htest"

t.testBoot <- function(x, y = NULL, mu.0 = 0, alpha = 0.05, alternative = "two.sided", nboot = 100, boot.type = "np"){
  if(is.null(y)){
      T.x    <- numeric(nboot)
      n      <- length(x)
      t.true <- (mean(x) - mu.0) / sqrt(var(x)) * sqrt(n)

      switch(boot.type, # switch between nonparametric and wild bootstrap
             np = {
               x.boot <- matrix(sample(x, size = n*nboot, replace = T), ncol = nboot)
               x.mean <- colMeans(x.boot)
               x.var  <- (colSums(x.boot^2) - n*x.mean^2)/(n-1)
               T.x    <- sqrt(n) * (x.mean - mean(x)) / sqrt(x.var)
               est    <- "Nonparametric Bootstrap"
             },
             wild = {
               z      <- x - mean(x)
               w.boot <- matrix(sample(c(-1,1), size = n*nboot, replace = T), ncol = nboot) # Rademacher Weights
               x.boot <- w.boot * z
               x.mean <- colMeans(x.boot)
               x.var  <- (colSums(x.boot^2) - n*x.mean^2)/(n-1)
               T.x    <- sqrt(n) * x.mean / sqrt(x.var)
               est    <- "Wild Bootstrap"
             }
      )

      crit   <- quantile(T.x, probs = c(alpha/2, alpha, 1-alpha, 1-alpha/2)) # compute critical values for one- and twosided
      switch(alternative, # switch between alternative hypotheses and choose whether to reject or not reject
             two.sided = {
               ret    <- (t.true < crit[1] || t.true > crit[4])

             },
             greater   = {
               ret  <- t.true > crit[3]

             },
             less      = {
               ret  <- t.true < crit[2]

             })

      # compute p-value
      p.v  <- mean(T.x > t.true)
      p.v  <- min(2 * p.v, 2 - 2 * p.v)

      # Prepare Results #######################################
      names(mu.0)     <- "mean"
      pop.var         <- mean(x.mean)
      names(pop.var)  <- "Mean"
      pop.stat        <- t.true
      names(pop.stat) <- "t"
      pop.par         <- length(x) - 1
      names(pop.par)  <- "df"
      conf.int        <- quantile(x.mean, probs = c(alpha/2, 1-alpha/2))
      attr(conf.int, "conf.level") <- 1 - alpha
      ##########################################################

      t.list        <- list("reject" = ret, "null.value" = mu.0, "alternative" = alternative, "method" = "t.test", "estimate" = pop.var, "data.name" = deparse(substitute(x)), "statistic" = pop.stat, "parameters" = pop.par, "p.value" = p.v, "estimation.method" = est, "sample.size" = length(x), "conf.int" = conf.int)
      class(t.list) <- c("htest", class(t.list))
      return(t.list)
    }
    if(!is.null(y)){
      T.x <- numeric(nboot)
      n.1 <- length(x)
      n.2 <- length(y)
      n   <- n.1 + n.2

      t.true <- (mean(x) - mean(y)) / (sqrt((var(x)/n.1) + (var(y)/n.2))) # see Toutenburg p. 145

      switch(boot.type, # switch between (groupwise) nonparametric and wild bootstrap
             npg = { # groupwise nonparametric bootstrap
               x.1.boot <- matrix(sample(x, size = n.1*nboot, replace = T), ncol = nboot)
               x.2.boot <- matrix(sample(y, size = n.2*nboot, replace = T), ncol = nboot)
               x.1.mean <- colMeans(x.1.boot)
               x.2.mean <- colMeans(x.2.boot)
               x.1.var  <- (colSums(x.1.boot^2) - n.1*x.1.mean^2)/(n.1-1)
               x.2.var  <- (colSums(x.2.boot^2) - n.2*x.2.mean^2)/(n.2-1)
               T.x      <- (x.1.mean - x.2.mean - (mean(x) - mean(y))) / (sqrt((x.1.var / n.1) + (x.2.var / n.2)))
               est      <- "Groupwise Nonparametric Bootstrap"
             },
             np  = { # Nonparametric Bootstrap
               x.boot   <- matrix(sample(c(x, y), size = n*nboot, replace = T), ncol = nboot)
               x.1.boot <- x.boot[1:n.1, ]
               x.2.boot <- x.boot[(n.1+1):n, ]
               x.1.mean <- colMeans(x.1.boot)
               x.2.mean <- colMeans(x.2.boot)
               x.1.var  <- (colSums(x.1.boot^2) - n.1*x.1.mean^2)/(n.1-1)
               x.2.var  <- (colSums(x.2.boot^2) - n.2*x.2.mean^2)/(n.2-1)
               T.x      <- (x.1.mean - x.2.mean ) / (sqrt((x.1.var / n.1) + (x.2.var / n.2)))
               est      <- "Nonparametric Bootstrap"

             },
             wild = { # Wild (Rademacher) Bootstrap
               z.1      <- x - mean(x)
               z.2      <- y - mean(y)
               w.boot   <- matrix(sample(c(-1,1), size = n*nboot, replace = T), ncol = nboot)
               x.1.boot <- z.1 * w.boot[1:n.1, ]
               x.2.boot <- z.2 * w.boot[(n.1+1):n, ]
               x.1.mean <- colMeans(x.1.boot)
               x.2.mean <- colMeans(x.2.boot)
               x.1.var  <- (colSums(x.1.boot^2) - n.1*x.1.mean^2)/(n.1-1)
               x.2.var  <- (colSums(x.2.boot^2) - n.2*x.2.mean^2)/(n.2-1)
               T.x      <- (x.1.mean - x.2.mean) / (sqrt((x.1.var / n.1) + (x.2.var / n.2)))
               est      <- "Wild Bootstrap"
             },
             perm = {
               x.boot   <- replicate(nboot, sample(c(x,y)))
               x.1.boot <- x.boot[1:n.1, ]
               x.2.boot <- x.boot[(n.1+1):n, ]
               x.1.mean <- colMeans(x.1.boot)
               x.2.mean <- colMeans(x.2.boot)
               x.1.var  <- (colSums(x.1.boot^2) - n.1*x.1.mean^2)/(n.1-1)
               x.2.var  <- (colSums(x.2.boot^2) - n.2*x.2.mean^2)/(n.2-1)
               T.x      <- (x.1.mean - x.2.mean ) / (sqrt((x.1.var / n.1) + (x.2.var / n.2)))
               est      <- "Permutation"
             }
      )


      crit   <- quantile(T.x, probs = c(alpha/2, alpha, 1-alpha, 1-alpha/2)) # compute critical values for one- and twosided
      switch(alternative, # switch between alternative hypotheses and choose whether to reject or not reject
             two.sided = {
               ret    <- (t.true < crit[1] || t.true > crit[4])

             },
             greater   = {
               ret  <- t.true > crit[3]

             },
             less      = {
               ret  <- t.true < crit[2]

             })

      # compute p-value
      p.v  <- mean(T.x > t.true)
      p.v  <- min(p.v, 1 - p.v)

      # Prepare Results #######################################
      dat.nam         <- c(deparse(substitute(x.1)), deparse(substitute(x.2)))
      dat.nam.2       <- paste(dat.nam[1], "and", dat.nam[2])
      h.0             <- 0
      names(h.0)      <- "difference in means"
      pop.var         <- c(mean(x.1.mean), mean(x.2.mean))
      names(pop.var)  <- paste("Mean of", dat.nam)
      pop.stat        <- t.true
      names(pop.stat) <- "t"
      pop.par         <- n - 2
      names(pop.par)  <- "df"
      conf.int        <- quantile((x.1.mean - x.2.mean), probs = c(alpha/2, 1-alpha/2))

      attr(conf.int, "conf.level") <- 1 - alpha
      ##########################################################

      t.list        <- list("reject" = ret ,"null.value" = h.0, "alternative" = alternative, "method" = "Welch Two Sample t-Test", "estimate" = pop.var, "data.name" = dat.nam.2, "statistic" = pop.stat, "parameters" = pop.par, "p.value" = p.v, "estimation.method" = est, "sample.size" = n, "conf.int" = conf.int)
      class(t.list) <- c("htest", class(t.list))
      return(t.list)
    }
}
