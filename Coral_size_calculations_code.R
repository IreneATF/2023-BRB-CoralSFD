## This code reads sampling data sheet for coral SFD and returns area

cat("\014")

coral.prop <- function() {

setwd("C:/Users/irene/Documents/QES Program/CODE/Coral_properties")
data <- as.data.frame(read.csv("SCTL_sampling_sheet.csv"), ncol = 17)
rows <- nrow(data)

prop <- data.frame(Observation = numeric(), Transect = numeric(),Quadrat = numeric(), 
                   N.DMS = numeric(), W.DMS = numeric(), Species.CODE = character(),
                   Depth.m = numeric(), Surface.Area.cm2 = numeric(),
                   Partial.Mortality = numeric(), SCTLD.Mortality = numeric(),
                   SCTLD.Presence = logical(), Bleached.Other.Disease = character(), 
                   Comments = character())

source("coral.areas.R")
library(stringr)

for (x in 1:rows){  
  
  ## Setting variables
  obs_h <- data.frame(data[x,])
  obs <- as.numeric(obs_h$Observation)
  species <- as.character(obs_h$Species.CODE)
  shape <- as.numeric(obs_h$Geometric.Shape.s)
  length <- as.numeric(obs_h$Length.cm)
  width <- as.numeric(obs_h$Width.cm)
  height <- as.numeric(obs_h$Height.cm)
  partial.mortality <- as.numeric(obs_h$Partial.mortality)
  SCTLD.mortality <- as.numeric(obs_h$SCTLD.mortality)
  SCTLD <- as.logical(obs_h$SCTLD.Presence)
  BOD <- as.character(obs_h$Bleached.Other.disease)
  Comments <- as.character(obs_h$Comments)
  
  ## Calculating area
	if (shape == 1) {
    area <- round(ellipsoid(length,width,height))
	  
	}else if (shape == 2) {
	  area <- round(ellipse(length,width,height))
	  
	}else if (shape == 3) {
	  area <- round(cylinder.ell(length,width,height))
	  
	}else {cat("Specify shape\n")}
  
  properties <- c(obs,obs_h$Transect, obs_h$Quadrat, obs_h$N.DMS, obs_h$W.DMS, 
                  species, obs_h$Depth.m, area, partial.mortality, 
                  SCTLD.mortality, SCTLD, BOD, obs_h$Comments)
  
  ## For complex shapes
  bad <- is.na(prop$Species)
  prop <- data.frame(prop[!bad,])
  
  obs_v <- as.numeric(c(prop$Observation))
  complex <- obs == obs_v
  
  if (any(complex) == TRUE) {
    
    ## Area 
    number_same.colony <- which(complex)
    obs_same.colony <- data.frame(prop[number_same.colony,])
    area_sub1 <- area
    area_sub2 <- as.numeric(obs_same.colony$Surface.Area.cm2)
    area <- area_sub1 + area_sub2
    
    ## Partial Mortality
    pm_sub1 <- partial.mortality
    pm_sub2 <- as.numeric(obs_same.colony$Partial.Mortality)
    
    partial.mortality <- (area_sub1*pm_sub1+area_sub2*pm_sub2)/(area_sub1+area_sub2)
    partial.mortality <- round(partial.mortality, digits = 2)
    
    ## SCTLD Mortality
    SCTLDm_sub1 <- SCTLD.mortality
    SCTLDm_sub2 <- as.numeric(obs_same.colony$SCTLD.Mortality)
    
    SCTLD.mortality <- (area_sub1*SCTLDm_sub1+area_sub2*SCTLDm_sub2)/(area_sub1+area_sub2)
    
    if (obs_same.colony$SCTLD.Presence == TRUE) {
      SCTLD <- TRUE
    }
    
    ## Bleached or other diseases
    BOD_sub1 <- BOD
    BOD_sub2 <- as.character(obs_same.colony$Bleached.Other.Disease)
    BOD_c <- c(BOD_sub1,BOD_sub2)
    BOD <- str_flatten(BOD_c)
    
    ## Comments
    
    Comm_sub1 <- Comments
    Comm_sub2 <- as.character(obs_same.colony$Comments)
    Comm_c <- c(Comm_sub1,Comm_sub2)
    Comments <- str_flatten(Comm_c)
    
    ## New properties 
    properties <- c(obs,obs_h$Transect, obs_h$Quadrat, obs_h$N.DMS, obs_h$W.DMS, 
                    species, obs_h$Depth.m, area, partial.mortality, 
                    SCTLD.mortality, SCTLD, BOD, Comments)
    prop[number_same.colony,] <- c(NA, NA, NA, NA, NA, NA, NA, NA, 
                                   NA, NA, NA, NA, NA)
  }
  
  ## Creating data frame output
  prop[x,] <- properties
}

prop <- transform(prop, Observation = as.numeric(Observation), Transect = as.numeric(Transect),
                  Quadrat = as.numeric(Quadrat), N.DMS = as.numeric(N.DMS), 
                  W.DMS = as.numeric(W.DMS), Depth.m = as.numeric(Depth.m), 
                  Surface.Area.cm2 = as.numeric(Surface.Area.cm2), Partial.Mortality = as.numeric(Partial.Mortality),
                  SCTLD.Mortality = as.numeric(SCTLD.Mortality), SCTLD.Presence = as.logical(SCTLD.Presence))

prop <- prop[order(prop$Transect, prop$Quadrat, prop$Species.CODE, prop$Surface.Area.cm2)]
prop

library(readr)
write_csv(prop, "C:/Users/irene/Documents/QES Program/CODE/Coral_properties/Coral_properties.csv")
}

source("Coral_size_calculations_code.R")
coral.prop()
