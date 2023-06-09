prop <- function(l,w,h,shape = 0,fraction = 1,pm = 0) {
	if (shape == 1) {
		cat("\n\nThe geometric shape is", fraction, "ellipsoid.")

		dimen <- c(l,w,h)
		o_dim <- dimen[order(dimen,decreasing = TRUE, na.last = TRUE)]  ## Ordering dimensions so a >= b >= c
		 
	  a <- 1/2*o_dim[1]                    ## Setting parameters 
		b <- 1/2*o_dim[2]
		c <- 1/2*o_dim[3]
    cat("\nParameters are a =", a, "cm, b =", b, "cm, and c =", c,"cm;")
		
    v_ell <- function(a,b,c) {    ## Calculating the volume of an ellipsoid
			vol <- fraction*(4/3*pi*a*b*c)
			cat("\nVolume is", vol,"cm3;")			
		}
		v_ell(a,b,c)
		
		a_ell_x <- function(a,b,c) {
		  
		  phi <- acos(c/a)
		  k <- sqrt((a^2*(b^2-c^2))/(b^2*(a^2-c^2)))
		  
		  f_integrand <- function(t) {1/sqrt(1-k^2*sin(t)^2)}
		  Fint <- integrate(f_integrand, 0, phi)
		  # print(Fint)
		  Fnum <- Fint[[1]]
		  
		  e_integrand <- function(t) {sqrt(1-k^2*sin(t)^2)}
		  Eint <- integrate(e_integrand, 0, phi)
		  # print(Eint)
		  Enum <- Eint[[1]]
		  
		  area_x <- (2*pi*c^2)+((2*pi*a*b)/sin(phi))*(Enum*sin(phi)^2+Fnum*cos(phi)^2)
		  area_x_f <- fraction*area_x
		  cat("\nThe surface area is", area_x_f, "cm2;")
		
		  live_tissue <- area_x_f*((100-pm)/100)
		  cat("\nConsidering", pm, "% partial mortality,", live_tissue, "cm2 is live tissue.")
		}
		a_ell_x(a,b,c)
		
	} else if (shape == 2) {
	  cat("\n\nThe geometric shape is", fraction, "plate / flat.")
	  
	  v_plt <- function(l,w,h) {    ## Calculating the volume of a plate (prism)
	    vol <- fraction*l*w*h
	    cat("\nVolume is", vol,"cm3;")			
	  }
	  v_plt(l,w,h)
	  
	  a_plt <- function(l,w,h) {    ## Calculating the area of a plate (top rectangle)
	    area <- l*w
	    cat("\nSurface area is", area, "cm3;")
	    live_tissue <- area*((100-pm)/100)
	    cat("\nConsidering", pm, "% partial mortality,", live_tissue, "cm2 is live tissue.")
	  }
	  a_plt(l,w,h)
	    
	} else if (shape == 3) {
	  cat("\n\nThe geometric shape is", fraction, "cylinder")
	  
	  dimen <- c(l,w)
	  r <- max(dimen, na.rm = FALSE)/2
	  
	  v_cyl <- function(r,h) {    ## Calculating the volume of a plate (prism)
	    vol <- pi*r^2*h
	    cat("\nVolume is", vol,"cm3;")			
	  }
	  v_cyl(r,h)
	  
	  a_cyl <- function(r,h) {    ## Calculating the area of a cylinder (except bottom circle)
	    area <- 2*pi*r*h + pi*r^2 
	    cat("\nSurface area is", area, "cm3;")
	    live_tissue <- area*((100-pm)/100)
	    cat("\nConsidering", pm, "% partial mortality,", live_tissue, "cm2 is live tissue.")
	  }
	  a_cyl(r,h)
	  
	} else if (shape == 4) {
		cat("\n\nThe geometric shape is complex, we must decompose into multiple geometric shapes.")

	} else {
		cat("Specify shape")
	}
}

