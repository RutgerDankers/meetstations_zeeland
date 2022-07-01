# functions ========================================================================
#
#calc tg
calc_tglobe <- function(tair,rh,pair,speed,solar,fdir,cza) { #--------------------------------
  stefanb <- 5.6696e-8
  emis_globe <- 0.95
  alb_globe <- 0.05
  dglobe <- 0.0508
  emis_sfc <- 0.999
  alb_sfc <- 0.45
  convergence <- 0.02
  max_iter <- 50
  tsfc <- tair
  tglobe_prev <- tair
  converged <- FALSE
  iter <- 0
  while (!converged & iter<max_iter)
    {
    iter <- iter + 1
    tref <- 0.5*( tglobe_prev + tair ) #evaluate properties at the average temperature
    h <- h_sphere_in_air( dglobe, tref, pair, speed )
    x <- emis_atm(tair,rh)
    tglobe <- ( 0.5*( emis_atm(tair,rh) * tair**4 + emis_sfc * tsfc**4 ) - h/(emis_globe*stefanb)*(tglobe_prev - tair) + solar/(2*emis_globe*stefanb)*(1.-alb_globe)*(fdir*(1./(2.*cza)-1.)+1.+alb_sfc) )**0.25
    if (abs(tglobe-tglobe_prev) < convergence) converged <- TRUE
    tglobe_prev <- (0.9*tglobe_prev + 0.1*tglobe)
    }# end of while loop
  if (converged) tglobe <- tglobe - 273.15
  else tglobe <- NA
  return(tglobe)
}
#
#calc tnwb
calc_twb <- function(tair,rh,pair,speed,solar,fdir,cza,irad) { #------------------------------
  cp <- 1003.5
  mair <- 28.97
  mh2o <- 18.015
  ratio <- cp*mair/mh2o
  rgas <- 8314.34
  rair <- rgas/mair
  pr <- cp / ( cp + 1.25*rair)
  gravity <- 9.807
  pi <- 3.141592654
  min_speed <- 0.13  #threshold of wind speed anemometers
  stefanb <- 5.6696e-8
  emis_wick <- 0.95
  alb_wick <- 0.4
  dwick <- 0.007
  lwick <- 0.0254
  emis_globe <- 0.95
  alb_globe <- 0.05
  emis_sfc <- 0.999
  alb_sfc <- 0.45
  convergence <- 0.02
  max_iter <- 50
  tsfc <- tair
  sza <- acos(cza)
  eair <- rh * esat(tair,0)
  tdew <- dew_point(eair,0)
  twb_prev <- tdew #first guess is the dew point temperature
  converged <- FALSE
  iter <- 0
  a <- 0.56
  b <- 0.281
  c <- 0.4
  while (!converged & iter<max_iter) {
    iter <- iter + 1
    tref <- 0.5*( twb_prev + tair ) #evaluate properties at the average temperature
    h <- h_cylinder_in_air( dwick, lwick, tref, pair, speed )
    fatm <- stefanb*emis_wick*( 0.5*(emis_atm(tair,rh)*tair**4 + emis_sfc*tsfc**4) - twb_prev**4 ) + (1.-alb_wick) * solar * ( (1.-fdir)*(1.+0.25*dwick/lwick) + ((tan(sza)/pi)+0.25*dwick/lwick)*fdir + alb_sfc )
    ewick <- esat(twb_prev,0)
    density <- pair * 100. / ( rair * tref )
    sc <- viscosity(tref)/(density*diffusivity(tref,pair))
    twb <- tair - evap(tref)/ratio*(ewick-eair)/(pair-ewick)*(pr/sc)**a + fatm/h * irad
    twb <- max(twb,tdew)
    twb_prev <- 0.9*twb_prev + 0.1*twb
    if (abs(twb-twb_prev) < convergence) converged <- TRUE
  }# end of while loop
  if (converged) {
    twb <- twb - 273.15
  } else
    twb <- NA
  return(twb)
}
#
calc_solar_parameters <- function(year, day, hour, lat, lon, solar) { #------------------
  deg2rad <- 3.141592654/180.
  solar_const <- 1367.
  normsolar_max <- 0.85
  cza_min <- 0.00873
  x <- sunae(year, day, hour, lat, lon)
  el <- x[1]
  soldst <- x[2]
  cza <- cos( (90.-el)*deg2rad )
  toasolar <- solar_const * max(0.,cza) / soldst**2
  if (cza < cza_min) toasolar <- 0. # If the sun is not fully above the horizon, set the maximum solar to zero.
  if (toasolar > 0) {
    normsolar <- min( solar/toasolar, normsolar_max ) # Account for calibration errors.
    solar <- normsolar * toasolar
#   Calculate the fraction of the solar irradiance due to the direct beam
    if (normsolar > 0) {
      fdir <- exp( 3. - 1.34 * normsolar - 1.65 / normsolar )
      fdir <- max( min( fdir, 0.9 ), 0. )
    } else {
      fdir <- 0.
    }
  } else {
    fdir <- 0.
  }
  return(c(cza,fdir,toasolar*normsolar_max))
}
#
sunae <- function(year,day,hour,lat,lon) { #---------------------------------------------
# work with real variables and define some constants, including one to change between degs and radians
  twopi <- 6.2831853
  pi <- 3.1415927
  rad <- 0.017453293
#
# get the current julian date (actually add 2,400,000 for jd)
  delta <- year-1949.
  leap <- as.integer(delta/4.)
  jd <- 32916.5+delta*365.+leap+day+hour/24.
# 1st no. is mid. 0 jan 1949 minus 2.4e6; leap=leap days since 1949, the last year of century is not leap year unless divisible by 400
  if (year%%100==0 & year%%400!=0) jd <- jd-1.
#
# calculate ecliptic coordinates
  time <- jd-51545.0
# 51545.0 + 2.4e6  #=noon 1 jan 2000
#
# force mean longitude between 0 and 360 degs
  mnlong <- 280.460+0.9856474*time
  mnlong <- mnlong%%360.
  if (mnlong<0.) mnlong <- mnlong+360.
#
# mean anomaly in radians between 0 and 2*pi
  mnanom <- 357.528+0.9856003*time
  mnanom <- mnanom%%360.
  if (mnanom<0.) mnanom <- mnanom+360.
  mnanom <- mnanom*rad
#
# compute the ecliptic longitude and obliquity of ecliptic in radians
  eclong <- mnlong+1.915*sin(mnanom)+0.020*sin(2.*mnanom)
  eclong <- eclong%%360.
  if (eclong<0.) eclong <- eclong+360.
  oblqec <- 23.439-0.0000004*time
  eclong <- eclong*rad
  oblqec <- oblqec*rad
#
# calculate right ascension and declination
  num <- cos(oblqec)*sin(eclong)
  den <- cos(eclong)
  ra <- atan(num/den)
# force ra between 0 and 2*pi
  if (den<0) {
    ra <- ra+pi
  } else if (num<0) {
    ra <- ra+twopi
  }
#
# dec in radians
  dec <- asin(sin(oblqec)*sin(eclong))
#
# calculate Greenwich mean sidereal time in hours
  gmst <- 6.697375+0.0657098242*time+hour 
# hour not changed to sidereal time since 'time' includes the fractional day 
  gmst <- gmst%%24.
  if (gmst<0.) gmst <- gmst+24.
#
# calculate local mean sidereal time in radians 
  lmst <- gmst+lon/15.
  lmst <- lmst%%24.
  if (lmst<0.) lmst <- lmst+24.
  lmst <- lmst*15.*rad
#
# calculate hour angle in radians between -pi and pi
  ha <- lmst-ra
  if(ha<(-1*pi)) ha <- ha+twopi
  if(ha>pi) ha <- ha-twopi
#
# change latitude to radians
  lat <- lat*rad
#
# calculate azimuth and elevation
  el <- asin(sin(dec)*sin(lat)+cos(dec)*cos(lat)*cos(ha))
  az <- asin(-cos(dec)*sin(ha)/cos(el))
#
# this puts azimuth between 0 and 2*pi radians
  if (sin(dec)-sin(el)*sin(lat)>=0.) {
    if (sin(az)<0.) az <- az+twopi
  } else {
    az <- pi-az
  }
#
# calculate refraction correction for US stan. atmosphere, need to have el in degs before calculating correction
  el <- el/rad
#
  if(el>=19.225) {
    refrac <- 0.00452*3.51823/tan(el*rad)
  } else if (el>-0.766 & el<19.225) {
    refrac <- 3.51823*(0.1594+0.0196*el+0.00002*el**2)/(1.+0.505*el+0.0845*el**2)
  } else if (el<=-0.766) {
    refrac <- 0.0
  }
#
# note that 3.51823=1013.25 mb/288 C
  el <- el+refrac
# elevation in degs
#
# calculate distance to sun in A.U. & diameter in degs
  soldst=1.00014-0.01671*cos(mnanom)-0.00014*cos(2.*mnanom)
  soldia=0.5332/soldst
#
# convert az and lat to degs before returning
  az <- az/rad
  lat <- lat/rad
#
  return (c(el,soldst))
}
#
diffusivity <- function(tair,pair) { #---------------------------------------------------
  pcrit_air <- 36.4
  pcrit_h2o <- 218.
  tcrit_air <- 132.
  tcrit_h2o <- 647.3
  mair <- 28.97
  mh2o <- 18.015
  a <- 3.640E-4
  b <- 2.334
  pcrit13  <- ( pcrit_air * pcrit_h2o )**(1./3.) 
  tcrit512 <- ( tcrit_air * tcrit_h2o )**(5./12.) 
  tcrit12 <-  ( tcrit_air * tcrit_h2o )**0.5 
  mmix <- ( 1./mair + 1./mh2o )**0.5 
  patm <- pair / 1013.25
  diffusivity <- a * ( tair / tcrit12 )**b * pcrit13 * tcrit512 * mmix / patm * 1E-4
  return(diffusivity)
}
#
evap <-function(tair) {
  evap = (313.15 - tair)/30. * (-71100.) + 2.4073E6
  return(evap)
}
#
h_sphere_in_air <- function(diameter,tair,pair,speed) { #--------------------------------
  mair <- 28.97
  rgas <- 8314.34
  rair <- rgas/mair
  cp <- 1003.5
  pr <- cp / ( cp + 1.25*rair)
  min_speed <- 0.13  #threshold of wind speed anemometers
  density <- pair * 100. / ( rair * tair )	# kg/m3
  re <- max(speed,min_speed) * density * diameter / viscosity(tair)
  nu <- 2.0 + 0.6 * re**0.5 * pr**0.3333
  h_sphere_in_air <- nu * thermal_cond(tair) / diameter # w/(m2 k)
  return(h_sphere_in_air)
}
#
viscosity <- function(tair) { #----------------------------------------------------------
  mair <- 28.97
  epskappa <- 97.0
  sigma <- 3.617
  sigma2 <- sigma**2
  tr <- tair / epskappa
  omega <- ( tr-2.9 )/0.4 * (-0.034) + 1.048
  viscosity <- 2.6693E-6 * sqrt( mair*tair ) / ( sigma2 * omega )
  return(viscosity)
}
#
thermal_cond <-function(tair) { #--------------------------------------------------------
  mair <- 28.97
  rgas <- 8314.34
  cp <- 1003.5
  rair <- rgas/mair
  thermal_cond <- ( cp + 1.25 * rair ) * viscosity(tair)
  return(thermal_cond)
}
#
esat <- function(tk,iphase) { #----------------------------------------------------------
# over liquid water
  if (iphase == 0) {
    y <- (tk - 273.15)/(tk - 32.18)
    es <- 6.1121 * exp( 17.502 * y )
# over ice
  } else {
    y <- (tk - 273.15)/(tk - 0.6)
    es <- 6.1115 * exp( 22.452 * y )
  }
  es <- 1.004 * es  # correction for moist air, if pressure is not available; for pressure > 800 mb
  esat <- es
  return(esat)
}
#
dew_point <- function(e,iphase) { #------------------------------------------------------
  if (iphase==0) {  # dew point
    z <- log( e / (6.1121*1.004) )
    dew_point <- 273.15 + 240.97*z/(17.502-z)
    } else {	                  # frost point
    z <- log( e / (6.1115*1.004) )
    dew_point <- 273.15 + 272.55*z/(22.452-z)
    }
  return(dew_point)
}
#
h_cylinder_in_air <- function(diameter,length,tair,pair,speed) { #-----------------------
  cp <- 1003.5
  mair <- 28.97
  mh2o <- 18.015
  ratio <- cp*mair/mh2o
  rgas <- 8314.34
  rair <- rgas/mair
  pr <- cp / ( cp + 1.25*rair)
  min_speed <- 0.13  #threshold of wind speed anemometers
  a <- 0.56
  b <- 0.281
  c <- 0.4  # from Bedingfield and Drew
  density <- pair * 100. / ( rair * tair )
  re <- max(speed,min_speed) * density * diameter / viscosity(tair)
  nu <- b * re**(1.-c) * pr**(1.-a)
  h_cylinder_in_air <- nu * thermal_cond(tair) / diameter # W/(m2 K)
  return(h_cylinder_in_air)
}
#
emis_atm <- function(t,rh) { #-----------------------------------------------------------
  e <- rh * esat(t,0)
  emis_atm <- 0.575* e ** 0.143
  return(emis_atm)
}
# end functions =========================================================================
#
