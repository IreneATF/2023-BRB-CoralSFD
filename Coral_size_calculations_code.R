prop <- function(l,w,h,shape = 0,pm = 0,fraction = 1) {
	if (shape == 1) {
		cat("\nThe geometric shape is", fraction, "ellipsoid.")

		a <- 1/2*l                    ## Setting parameters 
		b <- 1/2*h
		c <- 1/2*w

		v_sph <- function(a,b,c) {    ## Calculating the volume of an ellipsoid
			vol <- fraction*(4/3*pi*a*b*c)
			cat("\nVolume is", vol,"cm3;")			
		}
		v_sph(a,b,c)
		
		a_sph <- function(a,b,c,pm) {    ## Calculating the surface area of an ellipsoid
			p <- 1.6075
			area <- fraction*(4*pi*(((a^p*b^p + a^p*c^p + b^p*c*p)/3)^(1/p))) ## best approx with error 1%, for precise formula need numerical computation of elliptic integrals 
			live_tissue <- area*((100-pm)/100)
			cat("\nSurface area is", area,"cm2;")
			cat("\nConsidering", pm, "% partial mortality,", live_tissue, "cm2 is live tissue.")
		}
		a_sph(a,b,c,pm)
		
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