# pred y1 < y0 | Data #
# d = y0,y1,n0,n1
pred_y0.gt.y1 <- function(d, N0, N1, theta=0.95, delta=0, prior=c(0.5,0.5)){
  
  samp.Trial <- function(p, n){ return( rbinom( 1, n, p) ) }
  
  SAMP <- 5000
  p0 <- rbeta(SAMP, prior[1] + d[1], prior[2] + d[3] - d[1])
  p1 <- rbeta(SAMP, prior[1] + d[2], prior[2] + d[4] - d[2])
  
  if( d[3] < N0 ){ Y0 <- d[1] + sapply(p0, FUN=samp.Trial, n=N0-d[3])  
  } else{ Y0 <- rep(d[1], SAMP); N0 <- d[3] }
  if( d[4] < N1 ){ Y1 <- d[2] + sapply(p1, FUN=samp.Trial, n=N1-d[4])
  } else{ Y1 <- rep(d[2], SAMP); N1 <- d[4] }
  
  y <- cbind(Y0,Y1)
  
  post <- apply( y, MARGIN=1, FUN=posterior_y0.gt.y1, n0=N0, n1=N1, delta=0, prior=c(0.5,0.5) )
  return( sum( as.numeric( post > theta  ) )/SAMP )
}