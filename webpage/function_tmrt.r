# functions ========================================================================
#
#calc tmrt
#
calc_tmrt <- function(tglobe,tfast,speed) { #----------------------------------------------------------
  diam <- 150.0
  emis <- 1.0
  a <- (tglobe + 273.15)^4
  b <- 1.1*10^8 * speed^0.6 / (emis*diam^0.4) * (tglobe-tfast)
  tmrt <- (a + b)^0.25 - 273.15
  return(tmrt)
}
