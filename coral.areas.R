## THESE FUNCTIONS TAKE MEASUREMENTS OF LENGTH, WIDTH AND HEIGHT OF A CORAL 
## COLONY AND RETURN THE AREA ACCORDING TO THE GEOMETRICAL SHAPE OF THE 
## GROWTH MORPHOLOGY.

## JUNE 2023, BY IRENE A TOVAR

ellipsoid <- function(length,width,height) {

  height <- 2*height
  dimen <- c(length,width,height)
  o_dim <- dimen[order(dimen,decreasing = TRUE, na.last = TRUE)]  ## Ordering dimensions so a >= b >= c

  a <- 1/2*o_dim[1]     ## Setting parameters 
  b <- 1/2*o_dim[2]
  c <- 1/2*o_dim[3]

  ##cat("\nParameters are a =", a, "cm, b =", b, "cm, and c =", c,"cm;")

  ##vol <- fraction*(4/3*pi*a*b*c)  ## Calculating the volume of an ellipsoid
  ##cat("\nVolume is", vol,"cm3;")			

  phi <- acos(c/a)                ## Calculating the area of an ellipsoid
  k <- sqrt((a^2*(b^2-c^2))/(b^2*(a^2-c^2)))

  f_integrand <- function(t) {1/sqrt(1-k^2*sin(t)^2)} ## Partial elliptic integrals of first and second type
  Fint <- integrate(f_integrand, 0, phi)
  Fnum <- Fint[[1]]
  e_integrand <- function(t) {sqrt(1-k^2*sin(t)^2)}
  Eint <- integrate(e_integrand, 0, phi)
  Enum <- Eint[[1]]

  area_x <- (2*pi*c^2)+((2*pi*a*b)/sin(phi))*(Enum*sin(phi)^2+Fnum*cos(phi)^2)
  area <- 0.5*area_x
  area
  ## cat("\nThe surface area is", area, "cm2;")
  ## live_tissue <- area*((100-pm)/100)
  ## cat("\nConsidering", pm, "% partial mortality,", live_tissue, "cm2 is live tissue.\n")
}
  
ellipse <- function(length,width,height = 0) {
  ## cat("\nThe geometric shape is encrusting / flat and elliptical")
  area <- pi*(length/2)*(width/2)
  area
  ## cat("\nSurface area is", area, "cm2;")
  ## live_tissue <- area*((100-pm)/100)
  ## cat("\nConsidering", pm, "% partial mortality,", live_tissue, "cm2 is live tissue.")
}

cylinder.ell <- function(length,width,height) {
  ## cat("\nThe geometric shape is a cylinder")
  area.b <- ellipse(length,width)
  
  a <- length/2
  b <- width/2
  lambda <- (a-b)/(a+b)
  perimeter <- pi*(a+b)*(1+(3*lambda^2)/(10+sqrt(4-3*lambda^2)))
  area.l <- perimeter*height
  
  area <- area.l+area.b   ## Calculating the area of a cylinder (except bottom circle)
  area
  ##cat("\nSurface area is", area, "cm2;")
  ##live_tissue <- area*((100-pm)/100)
  ##cat("\nConsidering", pm, "% partial mortality,", live_tissue, "cm2 is live tissue.\n")
}
