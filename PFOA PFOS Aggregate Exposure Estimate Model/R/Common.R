# Common Functions and PK functions for Lorber-Egeghy Model
# AE, ORAU, 2020.

# WSD Function from package 'radiant'
# https://rdrr.io/cran/radiant.data/src/R/transform.R

WM <- function(x,w){
  
  ans <-sum(x*w)/sum(w)
  return(ans)
}

# x = mean
# w = weight


WSD <- function(x, wt, na.rm = TRUE) {
  if (na.rm) {
    ind <- is.na(x) | is.na(wt)
    x <- x[!ind]
    wt <- wt[!ind]
  }
  wt <- wt / sum(wt)
  wm <- weighted.mean(x, wt)
  sqrt(sum(wt * (x - wm) ^ 2))
}
# x = mean
# wm = weighted mean
# M = number of non zero weights


Simple.Serum.PK <- function(DP,kP,Vd){
  
  CP <- DP/(kP * Vd)
  
  return(CP)
}


Simple.Dose.PK <- function(CP,kP,Vd){
  
  DP <- CP*kP*Vd
  
  return(DP)
}

# DP = dose (ng/kg bw/day) # notice its per kg BODYWEIGHT
# CP = serum conc. ng/mL
# Vd = volume distribution (mL/kg bw)
# kP = elimination rate (day -1)


get.people <- function(){
  
  exposurefactors <- read_excel(input, sheet = 'Exposure Factors')
  individuals<- split(exposurefactors, rownames(exposurefactors))
  names(individuals)<-exposurefactors$Individual
  
  return(individuals)
}













