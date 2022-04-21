#' Atmospheric Conductance
#'
#' computes atmospheric conductance
#' @param  v_m wind speed (cm/s)
#' @param  z_m height at which windspeed is measured (cm); usually 200 cm
#' @param  k_0 default = 0.1
#' @param  k_d default = 0.7
#' @param  h vegetation height (cm)
#' @author Allie Cole, Yutian Fang, Steven Cognac
#' @references 
#' @return
#' atmospheric conductance (a measure of how easily vapor diffuses from vegetation surfaces) 
#'
crop_water_use = function(v_m, z_m, k_0=0.1, k_d=0.7, h) {
  
  z_m <- 200 + h
  # creating zd and z0
  zd <- k_d * h
  z0 <- k_0 * h
  # calculating atmospheric conductance
  conductance <- v_m / (6.25 * log((z_m - zd) / z0)^2)
  
  return(list(conductance=conductance))
}





