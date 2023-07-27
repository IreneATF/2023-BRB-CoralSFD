## This code splits by species and pools data into size categories

##### CODE ##### 

by.species.classes <- function() {
  
  cat("\014")
  
  setwd("C:/Users/irene/Documents/QES Program/CODE/Coral_properties")
  data <- as.data.frame(read.csv("Coral_properties.csv"), ncol = 14)
  
  by.species <- split(data,data$Species.CODE)
  n.species <- nrow(summary(by.species))
  
  
  library(stringr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(dplyr)
  library(plyr)
  library(moments)
  library(EnvStats)
  
  species.list <- c("A. agaricites", "A. humilis", "C. natans", 
                    "D. labyrinthiformis", "D. stokesii", "M. cavernosa", "Meandrina spp.",
                    "O. annularis", "O. faveolata", "O. franksi", 
                    "P. astreoides", "P. clivosa", "P. strigosa",
                    "S. bournoni", "S. intersepta", "S. siderea")
  color.vector <- c("red", "coral", "darkorange4", "orange", "gold", "yellow", "darkolivegreen2",
                    "green", "seagreen", "cyan","blue", "dodgerblue", "slategray1", 
                    "purple3", "magenta", "hotpink1")
  
  byspecies.stat <- data.frame(Species.CODE = character(), Species.Name = character(), N.Observations = integer(), 
                                 Mean = numeric(), Median = numeric(), Geometric.mean = numeric(), Q95 = numeric(), 
                                 Standard.Deviation = numeric(), CV = numeric(),
                                 Skewness = numeric(), Kurtosis = numeric())
                       
  for (x in 1:n.species) { 
    # byspecies.prop <- data.frame(Observation = numeric(), Transect = numeric(),Quadrat = numeric(), 
                               #N.DMS = numeric(), W.DMS = numeric(), Species.CODE = character(),
                               #Depth.m = numeric(), Surface.Area.cm2 = numeric(), 
                               #Surface.Area.Log = numeric(), Partial.Mortality = numeric(), 
                               #SCTLD.Mortality = numeric(), SCTLD.Presence = logical(), 
                               #Bleached.Other.Disease = character(), Comments = character())
    species <- as.character(names(by.species[x]))
    species.data <- data.frame(by.species[species])
    colnames(species.data) <- c("Observation", "Transect", "Quadrat", "N.DMS", 
                                 "W.DMS", "Species.CODE", "Depth.m", 
                                 "Surface.Area.cm2", "Surface.Area.Log", "Partial.Mortality", 
                                 "SCTLD.Mortality", "SCTLD.Presence", 
                                 "Bleached.Other.Disease", "Comments")
    species.row <- nrow(species.data)
    #for (y in 1:species.row) {
      
      ## Writing output table for each species
      #obs_h <- species.data[y,]
      #area <- as.numeric(obs_h$Surface.Area.cm2)
      #area_log <- as.numeric(obs_h$Surface.Area.Log)  ## Log transformation
      #properties <- c(obs_h$Observation,obs_h$Transect, obs_h$Quadrat, obs_h$N.DMS, obs_h$W.DMS, 
                      #species, obs_h$Depth.m, area, area_log, obs_h$Partial.Mortality, 
                      #obs_h$SCTLD.Mortality, obs_h$SCTLD.Presence, obs_h$Bleached.Other.Disease, 
                      #obs_h$Comments)
      #byspecies.prop[y,] <- properties
    #}
    
    ## Create .csv file for each species
    file.sub1 <- getwd()
    file.sub2 <- "/BySpecies_properties_"
    file.sub3 <- ".csv"
    file.v <- c(file.sub1,file.sub2,species,file.sub3)
    file.path <- str_flatten(file.v)
    write_csv(species.data,file.path)
  
    ## Getting area and log data 
    species.area.data <- species.data$Surface.Area.cm2
    species.log.data <- species.data$Surface.Area.Log
    
    ## Creating histogram for each species in ggplot2
        ## ref. https://rb.gy/mg6vu 
    fill.color <- color.vector[[x]]
    
    hist.sub1 <- "Size-frequency distribution for "
    hist.sub2 <- species.list[[x]]
    hist.title <- str_flatten(c(hist.sub1, hist.sub2))
    specie.histogram <-  ggplot(species.data, aes(x = species.log.data)) +
                              geom_histogram(bins = 20, color = "black", fill = fill.color, 
                                             aes(y = after_stat(count/sum(count)))) + 
                              scale_x_continuous(breaks = c(0,1,2,3,4,5), limits = c(0,5)) +
                              scale_y_continuous(labels = percent) + 
                              geom_vline(aes(xintercept=mean(species.log.data)),
                                 color="gray25", linetype="dashed", linewidth=0.5) +
                              labs(title = hist.title, x = "log10(Surface area cm2)", y = "Frequency") +
                              theme(plot.title = element_text(size=12), axis.text=element_text(size=10))
    specie.histogram
    
    ## Saving histogram as .pdf 
    hist2.sub1 <- "SFD_"
    hist2.sub2 <- ".pdf"
    hist2.title <- str_flatten(c(hist2.sub1, species, hist2.sub2))
    ggsave(specie.histogram, file=hist2.title, width = 6, height = 4, units = "in")
    
    ## Calculating frequency parameters 
    n.obs <- nrow(species.data)        ## Count
    
    mean.val <- mean(species.log.data)     ## Mean of log transf data
    median.val <- median(species.log.data)
    geo.mean.val <- geoMean(species.log.data, na.rm = FALSE) ## geometric mean, antilog of log transformed data, 
                                                             ## maximum likelihood estimator of the median of distribution
                                                             ## https://search.r-project.org/CRAN/refmans/EnvStats/html/geoMean.html 
    Q95 <- quantile(species.log.data, 0.95) ## 95th percentile
    Q95 <- Q95[[1]]
    
    std.deviation.val <- sd(species.log.data)  ## Standard deviation and coefficient of variation          
    CV.val <- cv(species.log.data)             ## of log transf data
    
    skew.val <- skewness(species.log.data)    ## Calculating skewness on log transf data
    kurt.val <- kurtosis(species.log.data)    ## Calculating kurtosis on log transf data
    
    statistics <- data.frame(species, species.list[[x]], n.obs, mean.val, median.val, 
                    geo.mean.val, Q95, std.deviation.val, CV.val, skew.val, kurt.val)
    byspecies.stat[x,] <- statistics
    }
  
  ## Exporting .csv file with freq parameters for all species observed
  file2.sub1 <- getwd()
  file2.sub2 <- "/BySpecies_freq_parameters"
  file2.sub3 <- ".csv"
  file2.v <- c(file2.sub1,file2.sub2,file2.sub3)
  file2.path <- str_flatten(file2.v)
  write_csv(byspecies.stat,file2.path)
  
  }

source("By_species_classes.R")
by.species.classes()
  
