## This code splits by species, creates histograms and calculates frequency parameters

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
  library(gridExtra)
  library(cowplot)
  library(magick)
  library(pdftools)
  
  species.list <- c("A. agaricites", "A. humilis", "C. natans", "D. cylindrus",
                    "D. labyrinthiformis", "D. stokesii", "M. cavernosa", "Meandrinadae spp.",
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
    
    scale.factor <- 5/20*1
    ## ybreaks <- seq(0,100,10)
    specie.histogram <-  ggplot(species.data, aes(x = species.log.data)) +
                              
                              geom_histogram(bins = 20, color = "black", fill = fill.color, 
                                             aes(y =after_stat(count/sum(count)))) + 
                              stat_function(
                                fun = function(x, mean, sd, n) {
                                  n * dnorm(x = x, mean = mean, sd = sd)
                                },
                                args = with(species.data, c(mean = mean(species.log.data), sd = sd(species.log.data), n = scale.factor))) +
                              scale_x_continuous("log10(Surface area cm2)", breaks = c(0,1,2,3,4,5), limits = c(0,5)) +
                              scale_y_continuous("Frequency", labels = percent) +
                              labs(title = hist.title) +
                              theme(plot.title = element_text(size=12), axis.text=element_text(size=10))
                            
    specie.histogram
    
    ## Saving histogram as .pdf 
    hist2.sub1 <- "SFD_"
    hist2.sub2 <- ".pdf"
    hist2.title <- str_flatten(c(hist2.sub1, species, hist2.sub2))
    ggsave(specie.histogram, file=hist2.title, width = 4.25, height = 4, units = "in")
    
    
    ## Calculating frequency parameters 
    n.obs <- nrow(species.data)        ## Count
    
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
  

  ## Exporting .csv file with freq parameters for all species observed
  file2.sub1 <- getwd()
  file2.sub2 <- "/BySpecies_freq_parameters"
  file2.sub3 <- ".csv"
  file2.v <- c(file2.sub1,file2.sub2,file2.sub3)
  file2.path <- str_flatten(file2.v)
  write_csv(byspecies.stat,file2.path)
  
  ## Creating grid of all histograms 
  
  panel.11 <- image_read_pdf("SFD_AAGA.pdf")
  panel.12 <- image_read_pdf("SFD_AHUM.pdf")
  panel.13 <- image_read_pdf("SFD_CNAT.pdf")
  panel.21 <- image_read_pdf("SFD_DCYL.pdf")
  panel.22 <- image_read_pdf("SFD_DLAB.pdf")
  panel.23 <- image_read_pdf("SFD_DSTO.pdf")
  panel.31 <- image_read_pdf("SFD_MCAV.pdf")
  panel.32 <- image_read_pdf("SFD_MMEA.pdf")
  panel.33 <- image_read_pdf("SFD_OANN.pdf")
  panel.41 <- image_read_pdf("SFD_OFAV.pdf")
  panel.42 <- image_read_pdf("SFD_OFRA.pdf")
  panel.43 <- image_read_pdf("SFD_PAST.pdf")
  panel.51 <- image_read_pdf("SFD_PCLI.pdf")
  panel.52 <- image_read_pdf("SFD_PSTR.pdf")
  panel.53 <- image_read_pdf("SFD_SBOU.pdf")
  panel.61 <- image_read_pdf("SFD_SINT.pdf")
  panel.62 <- image_read_pdf("SFD_SSID.pdf")
  
  whole1 <- c(panel.11, panel.12, panel.13) %>% 
    image_append()
  whole2 <- c(panel.21, panel.22, panel.23) %>% 
    image_append() 
  whole3 <- c(panel.31, panel.32, panel.33) %>% 
    image_append()
  whole4 <- c(panel.41, panel.42, panel.43) %>% 
    image_append()
  whole5 <- c(panel.51, panel.52, panel.53) %>% 
    image_append()
  whole6 <- c(panel.61, panel.62) %>% 
    image_append()
  
  whole <- c(whole1, whole2, whole3, whole4, whole5, whole6) %>%
    image_append(stack = TRUE)
  whole
  
  image_write(whole, path = "All_species_hist.png", format = "png")
  
  }

source("By_species_classes.R")
by.species.classes()
  
