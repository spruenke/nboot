## Welcome to the nboot Package

This R-package was designed for researchers and practioners using statistical methods on small samples. Often, bootstrap statistics can provide convenient solutions to small sample problems, however, in some cases they are either hard to set up or computationally complex (at least regarding computation time). Furthermore, one may not spend time on reading how to actually implement a bootstrap version for a specific method. 

nboot offers a range of methods in a bootstrapped versions, each offering different bootstrap methods to choose from. Also, the number of bootstrap iterations can be chosen. Computations are mostly done in C++ using the Rcpp integration for R, granting high speed and precision. 

The current version of the package is 0.3.0.

## Installing the Package

Currently, there is no version available on CRAN. To download and install the Package, make sure you have the latest versions of R, Rtools and devtools. If you are using a windows machine, please make sure Rtools is installed and up-to-date: [Rtools on CRAN](https://cran.r-project.org/bin/windows/Rtools/history.html)

Install the package by using `devtools::install_github("spruenke/nboot")`.

## 

## Issues

If you have any issue with the package or it's functions, please refer to me directly at twitter (@erin_hub)
