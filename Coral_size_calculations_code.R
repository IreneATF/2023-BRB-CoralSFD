## This code reads sampling data sheet for coral SFD and returns area and volume
data <- read.csv("SCTL_sampling_sheet_test1.csv")
#rows <- nrow(data)
#col <- ncol(data)
#n <- data(last)

for (x in 1:11){                 ## Figure out how to make it go until no more data on its own
  obs_l <- data.frame(data[x,])
  species <- obs_l[[7]]
  dth <-  obs_l[[8]]
  shape <- obs_l[[9]]
  fraction <- obs_l[[10]]
  l <- obs_l[[11]]
  w <- obs_l[[12]]
  h <- obs_l[[13]]
  pm <- obs_l[[14]]
  rm <- obs_l[[15]]

  cat("\n\nObservation", obs_l[[1]], "; Species", species,"; Depth", dth,"m.")

prop <- function(l,w,h = 0,shape = 0,fraction = 1,pm = 0) {
	if (shape == 1) {
		cat("\nThe geometric shape is", fraction, "ellipsoid.")
    
	  if (fraction == 0.5) {h <- 2*h}
		
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
	  cat("\nThe geometric shape is encrusting / flat and elliptical")
	  
	  a_enc <- function(l,w) {
	    area <- pi*(l/2)*(w/2)
	    cat("\nSurface area is", area, "cm2;")
	    live_tissue <- area*((100-pm)/100)
	    cat("\nConsidering", pm, "% partial mortality,", live_tissue, "cm2 is live tissue.")
	  }
	  a_enc(l,w)
	  
	  } else if (shape == 3) {
	  cat("\nThe geometric shape is prismic / rectangular")
	  
	  v_plt <- function(l,w,h) {    ## Calculating the volume of a plate (prism)
	    vol <- l*w*h
	    cat("\nVolume is", vol,"cm3;")			
	  }
	  v_plt(l,w,h)
	  
	  a_plt <- function(l,w,h) {    ## Calculating the area of a plate (top rectangle)
	    area <- l*w
	    cat("\nSurface area is", area, "cm2;")
	    live_tissue <- area*((100-pm)/100)
	    cat("\nConsidering", pm, "% partial mortality,", live_tissue, "cm2 is live tissue.")
	  }
	  a_plt(l,w,h)
	    
	} else if (shape == 4) {
	  cat("\nThe geometric shape is", fraction, "cylinder")
	  
	  dimen <- c(l,w)
	  r <- max(dimen, na.rm = FALSE)/2
	  
	  v_cyl <- function(r,h) {    ## Calculating the volume of a plate (prism)
	    vol <- pi*r^2*h
	    cat("\nVolume is", vol,"cm3;")			
	  }
	  v_cyl(r,h)
	  
	  a_cyl <- function(r,h) {    ## Calculating the area of a cylinder (except bottom circle)
	    area <- 2*pi*r*h + pi*r^2 
	    cat("\nSurface area is", area, "cm2;")
	    live_tissue <- area*((100-pm)/100)
	    cat("\nConsidering", pm, "% partial mortality,", live_tissue, "cm2 is live tissue.")
	  }
	  a_cyl(r,h)
	  
	} else if (shape == 5) {
		cat("\nThe geometric shape is complex, we must decompose into multiple geometric shapes.")

	} else {
		cat("Specify shape")
	}
}
prop(l,w,h,shape,fraction,pm)
}

