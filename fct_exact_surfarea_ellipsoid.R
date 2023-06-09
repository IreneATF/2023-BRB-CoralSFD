a_ell_x <- function(a,b,c,pm = 0) {
  
  phi <- acos(c/a)
  k <- sqrt((a^2*(b^2-c^2))/(b^2*(a^2-c^2)))
  
  f_integrand <- function(t) {1/sqrt(1-k^2*sin(t)^2)}
  Fint <- integrate(f_integrand, 0, phi)
  print(Fint)
  
  e_integrand <- function(t) {sqrt(1-k^2*sin(t)^2)}
  Eint <- integrate(e_integrand, 0, phi)
  print(Eint)
  
  Enum <- Eint[1]
  Enum
  Fnum <- Fint[1]
  Fnum
  
  # area_x <- (2*pi*c^2)+((2*pi*a*b)/sin(phi))*(Eint*sin(phi)^2+Fint*cos(phi)^2)
  # area_x_f <- fraction*area_x
}

a_ell_x(60,50,20)
