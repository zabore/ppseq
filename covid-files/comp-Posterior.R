
#-----------------------------------------------------
#  Beta distribution comparison 
# Prob beta(c,d) < beta(a,b)
#-----------------------------------------------------

#### Posterior Design #####

 # prob y1 < y0
 posterior_y0.gt.y1 <- function(n0, n1, y0, y1, delta=0, prior=c(0.5,0.5)){ ## y0 standard     y1 experimental
  SAMP <- 10000
  out <- sum( rbeta(SAMP, prior[1] + y0, prior[2] + n0 - y0) > ( rbeta(SAMP, prior[1] + y1, prior[2] + n1 - y1) + delta ) ) / SAMP
  return(out)
 }

find.Boundary <- function(j, theta, Tab, delta=0, prior=c(0.5,0.5) ){

 Y=Tab$n1[j]; STOP <- FALSE
 temp <- posterior_y0.gt.y1( n0=Tab$n0[j], n1=Tab$n1[j], y0=Tab$y0[j], y1=Y, delta=delta, prior=prior )
 if( is.na(temp) ){ temp <- 0; print(paste0("NA - ",Tab[j,])) }
 if( temp > theta ){ STOP <- TRUE }
 while( !STOP ){
  Y <- Y - 1
  temp <- posterior_y0.gt.y1( n0=Tab$n0[j], n1=Tab$n1[j], y0=Tab$y0[j], y1=Y, delta=delta, prior=prior )
  if( is.na(temp) ){ temp <- 0 }
  if( Y == 0 ){ STOP <- TRUE }
  if( temp > theta ){ STOP <- TRUE }
 }

 if( temp > theta ){ out <- Y 
 } else{ out <- NA }

 return( c(j, out) )
}

library(parallel)
library(foreach)
library(doParallel)

###### Paralllel Computing Setup #########################
 no_cores <- min(detectCores() - 1, 90)

 if( grepl("Windows", sessionInfo()[4]$running) ){ 
  cl <- makeCluster(no_cores); registerDoParallel(cl) 
 } else{
  library(doMC)
  registerDoMC(no_cores)
 }


###########
Max <- 2000
N.tab <- expand.grid( list( seq(100,1500,100), seq(100,1500,100) ) )
N.tab <- N.tab[-which( rowSums(N.tab) > Max ), ]

Tab <- as.data.frame( list(n0=NA, n1=NA, y0=NA) ) 
for(i in 1:nrow(N.tab)){

 s <- 0:N.tab[i,1]
 temp <- as.data.frame( list(n0=rep(N.tab[i,1],length(s)),
                             n1=rep(N.tab[i,2],length(s)),
                             y0=s) )
 Tab <- rbind(Tab, temp)
}
Tab <- Tab[-1,]
 

 prior <- c(0.1,0.9)
 delta <- 0
 theta <- 0.9516

 bounds <- foreach::foreach( j = 1:nrow(Tab),
                           .multicombine = TRUE, .combine="rbind", .packages = c("stats"),
                           .export = c("Tab","find.Boundary","posterior_y0.gt.y1","theta","delta","prior") 
                  )  %dopar% find.Boundary( j, theta=theta, Tab, delta=delta, prior=prior )

 #saveRDS(bounds,file.path("bounds.rds"))

 ### Merge Boundaries ###
 Tab$y1.stop <- rep(NA, nrow(Tab) )
 Tab$y1.stop[ bounds[, 1] ] <- bounds[, 2]

 #saveRDS(Tab,file.path("Monitor-Table.rds"))


#####
 check.Boundary <- function(n0,n1,y0,MTab){
  out <- NULL
  u <- which( ( as.numeric(MTab$n0==n0) + as.numeric(MTab$n1==n1) + as.numeric(MTab$y0==y0) )==3 )
  if( length(u)>0 ){
   out <- MTab$y1.stop[u]
  } else{ print("n0, n1, y0 combination not found") }

  return( out )
 }


 MTab <- readRDS(file.path("Monitor-Table.rds"))
 
 MTab[1:10,]

 check.Boundary(n0=300, n1=300, y0=30, MTab)









