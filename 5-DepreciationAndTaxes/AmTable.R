#Helper

PMT <- function(P, i, N) {
  P * (i*(1+i)^N)/((1+i)^N - 1)  
}

#PMT(10000, .1, 2)

AmTable <- function(P, i, N){
  pmt <- PMT(P,i,N)
  #Table init.
  ret <- data.frame(Time = 0, Payment = 0, Interest = 0, Principal = 0 , Balance = P)
  
  for(period in 2:(N+1)){
    ret[period,]$Time <- period-1
    ret[period,]$Payment <- pmt
    ret[period,]$Interest <- ret[period -1,]$Balance*i
    ret[period,]$Principal <- ret[period,]$Payment - ret[period,]$Interest
    ret[period,]$Balance <- ret[period-1,]$Balance - ret[period,]$Principal  
  }
  
  round(ret,2)
}

#AmTable(1000, .1, 100)
