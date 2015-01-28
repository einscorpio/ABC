#'Calculate Pi
#'@param N number of trials
#'@return Estimate of Pi
#'@export

calPI <- function(N){
  
  countIn <- 0
  
  coord <- list()
  #Monte-Carlo simulation
  for (i in 1:N){
    x <- runif(1,0,1)
    y <- runif(1,0,1)
    z <- squareSum(x, y)
    if (z < 1){
      countIn <- countIn + 1
    }
    coord[[i]] <- c(x, y) 
  }
  
  area <- 4*countIn/N
  
  list(xycoord = coord, PI = area)
}
