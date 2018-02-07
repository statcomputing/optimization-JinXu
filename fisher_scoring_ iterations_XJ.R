# Define fisher-scoring iterations
fisher_scoring <- function(theta0, gprime, fisher, eps =.00001, nlim = 1000) {i <- 0
repeat {
  i <- i+1
  if(i > nlim) {
    x1 <- NA
    break
  }
  x1 <- theta0 + gprime(theta0)/fisher
  if(abs(theta0 - x1) < eps||abs(gprime(x1))<1.0e-12)
    break
  theta0 <- x1
}
return(x1)
}
