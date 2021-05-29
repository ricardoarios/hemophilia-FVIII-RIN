## This code will find the Pareto front of a matrix.
##
### Adapted from the R package called MCO - https://github.com/cran/mco/blob/master/R/mco.R

pareto_front <- function(x, ...) {
  d <- ncol(x)
  n <- nrow(x)
  is.optimal <- rep(TRUE, n)
  
  for(i in 1:(n-1)) {
    for (j in i:n) {
      if (i != j && (is.optimal[i] || is.optimal[j])) {
        xi <- x[i,]
        xj <- x[j,]
        
        if (all(xi >= xj) && any(xi > xj)) { ## i dominates j
          is.optimal[j] <- FALSE
        } else if (all(xj >= xi) && any(xj > xi)) { ## j dominates i
          is.optimal[i] <- FALSE
        }
      }
    }
  }
  ## Always return a matrix!
  return(x[is.optimal,,drop=FALSE])
}
