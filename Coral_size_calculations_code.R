prop <- function(l,w,h,shape = 0,pm = 0,fraction = 1) {
	if (shape == 1) {
		cat("\nThe geometric shape is", fraction, "ellipsoid.")

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
		
		a_ell <- function(a,b,c) {    ## Calculating the surface area of an ellipsoid
			p <- 1.6075
			area <- fraction*(4*pi*(((a^p*b^p + a^p*c^p + b^p*c*p)/3)^(1/p))) ## best approx with error 1%, for precise formula need numerical computation of elliptic integrals 
			live_tissue <- area*((100-pm)/100)
			cat("\nSurface area is", area,"cm2;")
			cat("\nConsidering", pm, "% partial mortality,", live_tissue, "cm2 is live tissue.")
		}
		a_ell(a,b,c)
		
	} else if (shape == 2) {
		print("plate")

	} else if (shape == 3) {
		print(cylinder)

	} else if (shape == 4) {
		print("complex")

	} else {
		print("Specify shape")
	}
}