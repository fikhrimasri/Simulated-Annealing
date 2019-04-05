fungsi <- function(x1,x2){
  temp = -((sin(x1) * cos(x2)) + ((4/5)*exp(1-sqrt((x1^2)+(x2^2)))))
  return(temp)
}

foundx <- function(){
  return(runif(1, -10, 10))
}

checkprob <- function(dE,suhu){
  P <- exp(-(dE) / suhu)
  R <- runif(1)
  if (R < P){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

simulatedAnnealing <- function(x1, x2, x1baru, x2baru, Eb, E, suhu , stopT, alpha){
  Eb <- E
  x1baru <- x1
  x2baru <- x2
  
  suhu <- 10000
  stopT <- 0.0001
  alpha <- 0.001
  
  while (suhu > stopT ) {
    #generate solusi baru
    x1n <- foundx()
    x2n <- foundx()
    
    #calculate cost baru
    En <- fungsi(x1n, x2n)
    deltaE <- En - E
    
    #jika solusi baru lebih besar dari solusi sebelumnya
    if(deltaE < 0){
      x1 <- x1n
      x2 <- x2n
      E <- En
      
      # check wether the new solution is the current best solution
      if(En < Eb){
        x1baru <- x1n
        x2baru <- x2n
        Eb <- En
        
        # if new solution is worse than current solution
      }else{
        if(checkprob){
          x1 <- x1n
          x2 <- x2n
          E <- Eb
        }
      }
      
    }
    suhu <- suhu - alpha
    
  }
  print(sprintf("X1: %f",x1baru))
  print(sprintf("X2: %f",x2baru))
  print(sprintf("Energy: %f",Eb))
  
}

# run simulated annealing
simulatedAnnealing(x1, x2, x1baru, x2baru, Eb, E, suhu , stopT, alpha)
