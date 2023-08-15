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
  library(DescTools)
  
  species.list <- c("A. agaricites", "A. humilis", "C. natans", "D. cylindrus",
                    "D. labyrinthiformis", "D. stokesii", "M. cavernosa", "Meandrina spp.",
                    "O. annularis", "O. faveolata", "O. franksi", 
                    "P. astreoides", "P. clivosa", "P. strigosa",
                    "S. bournoni", "S. intersepta", "S. siderea")
  color.vector <- c("red", "coral", "darkorange4", "orange", "gold", "yellow", "darkolivegreen2",
                    "green", "seagreen", "aquamarine", "cyan","blue", "dodgerblue", "slategray1", 
                    "purple3", "magenta", "hotpink1")
  
  byspecies.stat <- data.frame(Species.CODE = character(), Species.Name = character(), 
                               Geometric.mean = numeric(), Skewness = numeric(), 
                               Kurtosis = numeric(), Q95 = numeric(), Standard.Deviation = numeric(), 
                               CV = numeric(), Pnorm = numeric(), N.Observations = integer())
  ## byspecies.count <- data.frame(Species.CODE = character(), Species.Name = character(),
                                ## Bin.1 = numeric(), Bin.2 = numeric(), Bin.3 = numeric(), 
                                ## Bin.4 = numeric(), Bin.5 = numeric(), Bin.6 = numeric(), 
                                ## Bin.7 = numeric(), Bin.8 = numeric(), Bin.9 = numeric(), 
                                ## Bin.10 = numeric(), Bin.11 = numeric(), Bin.12 = numeric(),
                                ## Bin.13 = numeric(), Bin.14 = numeric(), Bin.15 = numeric(),
                                ## Bin.16 = numeric(), Bin.17 = numeric(), Bin.18 = numeric(),
                                ## Bin.19 = numeric(), Bin.20 = numeric())
                       
  for (x in 1:n.species) { 
    species <- as.character(names(by.species[x]))
    species.data <- data.frame(by.species[species])
    colnames(species.data) <- c("Observation", "Transect", "Quadrat", "N.DMS", 
                                 "W.DMS", "Species.CODE", "Depth.m", 
                                 "Surface.Area.cm2", "Surface.Area.Log", "Partial.Mortality", 
                                 "SCTLD.Mortality", "SCTLD.Presence", 
                                 "Bleached.Other.Disease", "Comments")
    species.row <- nrow(species.data)
    
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
    
    ## Saving per bin frequency
    ## specie.histogram.count <- (ggplot_build(specie.histogram)$data[[1]]$count)/species.row                        
    ## specie.hist.count.v <- c(species, species.list[[x]], c(specie.histogram.count))
    
    ## byspecies.count[x,] <- specie.hist.count.v
    
    ## Saving histogram as .pdf 
    hist2.sub1 <- "SFD_"
    hist2.sub2 <- ".pdf"
    hist2.title <- str_flatten(c(hist2.sub1, species, hist2.sub2))
    ggsave(specie.histogram, file=hist2.title, width = 6, height = 4, units = "in")
    
    ## Calculating frequency parameters 
    n.obs <- nrow(species.data)        ## Count
    
    ## mean.val <- mean(species.log.data)     ## Mean of log transf data
    ## median.val <- median(species.log.data)
    geo.mean.val <- geoMean(species.area.data, na.rm = FALSE) ## geometric mean, antilog of log transformed data, 
                                                             ## maximum likelihood estimator of the median of distribution
                                                             ## https://search.r-project.org/CRAN/refmans/EnvStats/html/geoMean.html 
    Q95 <- quantile(species.log.data, 0.95) ## 95th percentile
    Q95 <- 10^Q95[[1]]
    
    std.deviation.val <- sd(species.log.data)  ## Standard deviation and coefficient of variation          
    CV.val <- cv(species.log.data)             ## of log transf data
    
    skew.val <- skewness(species.log.data)    ## Calculating skewness on log transf data
    kurt.val <- kurtosis(species.log.data)    ## Calculating kurtosis on log transf data
    
    if (n.obs < 5) {pnorm <- NA
    } else { 
    pnorm <- LillieTest(species.log.data)[[2]]  ## Lilliefors (Kolmogorov-Smirnov) test to determine probability 
    }                                           ## that data are from normal distribution, p value 
    
    statistics <- data.frame(species, species.list[[x]], geo.mean.val, skew.val, 
                             kurt.val, Q95, std.deviation.val, CV.val, pnorm, n.obs)
    
    byspecies.stat[x,] <- statistics
    }
  
  ## Transforming count table and exporting 
  ## byspecies.count <- transform(byspecies.count, Species.CODE = as.character(Species.CODE), 
                               ## Species.Name = as.character(Species.Name),
                               ## Bin.1 = as.numeric(Bin.1), Bin.2 = as.numeric(Bin.2), 
                               ## Bin.3 = as.numeric(Bin.3), Bin.4 = as.numeric(Bin.4), 
                               ## Bin.5 = as.numeric(Bin.5), Bin.6 = as.numeric(Bin.6), 
                               ## Bin.7 = as.numeric(Bin.7), Bin.8 = as.numeric(Bin.8), 
                               ## Bin.9 = as.numeric(Bin.9), Bin.10 = as.numeric(Bin.10), 
                               ## Bin.11 = as.numeric(Bin.11), Bin.12 = as.numeric(Bin.12),
                               ## Bin.13 = as.numeric(Bin.13), Bin.14 = as.numeric(Bin.14), 
                               ## Bin.15 = as.numeric(Bin.15), Bin.16 = as.numeric(Bin.16), 
                               ## Bin.17 = as.numeric(Bin.17), Bin.18 = as.numeric(Bin.18),
                               ## Bin.19 = as.numeric(Bin.19), Bin.20 = as.numeric(Bin.20))
  ## file3.sub1 <- getwd()
  ## file3.sub2 <- "/BySpecies_freq_count"
  ## file3.sub3 <- ".csv"
  ## file3.v <- c(file3.sub1,file3.sub2,file3.sub3)
  ## file3.path <- str_flatten(file3.v)
  ## write_csv(byspecies.count,file3.path)
  

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
  
