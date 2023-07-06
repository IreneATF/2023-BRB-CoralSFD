## This code splits by species and pools data into size categories

##### CODE ##### 

by.species.classes <- function() {
  
  cat("\014")
  
  setwd("C:/Users/irene/Documents/QES Program/CODE/Coral_properties")
  data <- as.data.frame(read.csv("Coral_properties.csv"), ncol = 13)
  
  by.species <- split(data,data$Species.CODE)
  n.species <- nrow(summary(by.species))
  
  byspecies.prop <- data.frame(Observation = numeric(), Transect = numeric(),Quadrat = numeric(), 
                               N.DMS = numeric(), W.DMS = numeric(), Species.CODE = character(),
                               Depth.m = numeric(), Surface.Area.cm2 = numeric(), 
                               Surface.Area.Class.cm2 = numeric(), Partial.Mortality = numeric(), 
                               SCTLD.Mortality = numeric(), SCTLD.Presence = logical(),
                               Bleached.Other.Disease = character(), Comments = character())
  
  library(stringr)
  
  for (x in 1:n.species) { 
    species <- as.character(names(by.species[x]))
    species.data <- data.frame(by.species[species])
    colnames(species.data) <- c("Observation", "Transect", "Quadrat", "N.DMS", 
                                 "W.DMS", "Species.CODE", "Depth.m", 
                                 "Surface.Area.cm2", "Partial.Mortality", 
                                 "SCTLD.Mortality", "SCTLD.Presence", 
                                 "Bleached.Other.Disease", "Comments")
    species.row <- nrow(species.data)
    for (y in 1:species.row) {
      
      obs_h <- species.data[y,]
      area <- as.numeric(obs_h$Surface.Area.cm2)
      if (area < 0.37) {area.class <- 0
      }else if (area >= 0.37 && area < 1) {area.class <- (0.37+1)/2
      }else if (area >= 0.1 && area < 2.7) {area.class <- (1+2.7)/2
      }else if (area >= 2.7 && area < 7.4) {area.class <- (2.7+7.4)/2
      }else if (area >= 7.4 && area < 20.1) {area.class <- (7.4+20.1)/2
      }else if (area >= 20.1 && area < 54.6) {area.class <- (20.1+54.6)/2
      }else if (area >= 54.6 && area < 148.4) {area.class <- (54.6+148.4)/2
      }else if (area >= 148.4 && area < 403.4) {area.class <- (148.4 + 403.4)/2
      }else if (area >= 403.4 && area < 1096) {area.class <- (403.4+1096)/2
      }else if (area >= 1096 && area < 2980) {area.class <- (1096+2980)/2
      }else if (area >= 2980 && area < 8103) {area.class <- (2980+8103)/2
      }else if (area > 8103) {area.class <- 8103
      }else {stop("invalid area")}
      
      properties <- c(obs_h$Observation,obs_h$Transect, obs_h$Quadrat, obs_h$N.DMS, obs_h$W.DMS, 
                      species, obs_h$Depth.m, area, area.class, obs_h$Partial.Mortality, 
                      obs_h$SCTLD.Mortality, obs_h$SCTLD.Presence, obs_h$Bleached.Other.Disease, 
                      obs_h$Comments)
      byspecies.prop[y,] <- properties
    }
    ## write code for each species
    file.sub1 <- getwd()
    file.sub2 <- "/BySpecies_properties_"
    file.sub3 <- ".csv"
    file.v <- c(file.sub1,file.sub2,species,file.sub3)
    file.path <- str_flatten(file.v)
    library(readr)
    write_csv(byspecies.prop,file.path)
  }
}

source("By_species_classes.R")
by.species.classes()
  