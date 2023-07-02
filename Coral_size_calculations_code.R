## This code reads sampling data sheet for coral SFD and returns area

cat("\014")

coral.prop <- function() {

setwd("C:/Users/irene/Documents/QES Program/CODE/Coral_properties")
data <- as.data.frame(read.csv("SCTL_sampling_sheet.csv"), ncol = 16)
rows <- nrow(data)
cols <- ncol(data)

prop <- data.frame(Transect = numeric(),Quadrat = numeric(),Species.CODE = character(),
                   Depth.m = numeric(),Surface.Area.cm = numeric(),
                   Partial.Mortality = numeric(),Recent.Mortality = numeric(),
                   SCTLD.Presence = logical(),Bleached.Other.Disease = logical(), Comments = character())

source("coral.areas.R")

for (x in 1:rows){            
  obs_l <- data.frame(data[x,])
  species <- as.character(obs_l$Species.CODE)
  shape <- as.numeric(obs_l$Geometric.Shape.s)
  length <- as.numeric(obs_l$Length.cm)
  width <- as.numeric(obs_l$Width.cm)
  height <- as.numeric(obs_l$Height.cm)
  partial.mortality <- as.numeric(obs_l$Partial.mortality)
  recent.mortality <- as.numeric(obs_l$Recent.mort)
  SCTLD <- as.logical(obs_l$SCTLD.Presence)
  
  ##cat("\n\nObservation", x, "; Species", species,"; Depth", depth,"m.")

	if (shape == 1) {
    area <- round(ellipsoid(length,width,height))
	  
	}else if (shape == 2) {
	  area <- round(ellipse(length,width,height))
	  
	}else if (shape == 3) {
	  area <- round(cylinder.ell(length,width,height))
	  
	} else if (is.na(species) == TRUE) {
		## The geometric shape is complex, we must decompose into multiple geometric shapes
	  if (shape == 1) {
	    area.sub <- ellipsoid(length,width,height)
	    
	  }else if (shape == 2) {
	    area.sub <- ellipse(length,width,height)
	    
	  }else if (shape == 3) {
	    area.sub <- cylinder.ell(length,width,height)
	    
	  }else {
	    cat("Specify shape\n")}
	  
	  area <- round(area.sub + prop$Surface.Area[[x-1]])

	}else {
		cat("Specify shape\n")
	}
  
  prop[x,] <- c(obs_l$Transect,obs_l$Quadrat,species,obs_l$Depth.m,
                area,round(obs_l$Partial.mortality,2),round(obs_l$Recent.mort,2),
                obs_l$SCTLD.Presence, obs_l$Bleached.Other.disease, obs_l$Comments)
  
}

prop <- prop[order(prop$Transect, prop$Quadrat, prop$Species.CODE, prop$Surface.Area.cm)]
prop

library(readr)
write_csv(prop, "C:/Users/irene/Documents/QES Program/CODE/Coral_properties/Coral_properties.csv")
}

coral.prop()
