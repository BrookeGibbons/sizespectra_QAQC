#load all functions ----
devtools::document()
devtools::load_all() ### load packages and functions in R folder

# Set the folder path
folder_path <- "~/Desktop/Git/sizespectra_QAQC/R"

# Get a list of all R files in the folder
r_files <- list.files(path = folder_path, pattern = "\\.R$", full.names = TRUE)

# Source each file (load the functions)
sapply(r_files, source)

library(readxl)
library(ggplot2)

# explore bruvs data

########### PELAGIC bruvs-----
# load pelagic data-- 
load("1_read_clean_pelagic.RData")
fl_pelagic <- tidyr::drop_na(fl_pelagic, weight_kg)

########### BENTHIC bruvs----
# load benthic data---
load("1_read_clean_benthic.RData")
fl_benthic <- tidyr::drop_na(fl_benthic, weight_kg)

### load pelagic benthic meta and merged fork lengths data

load("1_read_clean_pelagic_benthic.RData")
fl_pelagic_benthic_meta <- tidyr::drop_na(fl_pelagic_benthic_meta, weight_kg)
data = fl_pelagic_benthic_meta[fl_pelagic_benthic_meta$weight_kg > 0.001, ]# select predatory fish - individuals larger than minsize kg

### load pelagic benthic meta summary
pelagic_benthic_sum <- read_data_pelagic_benthic_sum() # this does not work

################# Fig 1 data bruvs screen grab ################### ----
draw_screengrab_combo()

################# Fig 2 sampling effort  ###################----
#map shapefiles
WorldData <- ggplot2::map_data("world") #%>% filter(region != "Antarctica") %>% fortify
#dsn_mar_layer <- "/Users/tomletessier/Documents/R stuff/Rcourse_graphics_Nov2011/"
#mar <- rgdal::readOGR(dsn = dsn_mar_layer, layer = "World_Maritime_Boundaries_v8") #maritime boundary

#map with expedition location with eez
#fig_map_sum <- globalmap_sum_eez(world = WorldData, mar = mar, meta_pb = pelagic_benthic_sum)

#map with expedition location with eez
fig_map_sum <- globalmap_sum(world = WorldData, meta_pb = pelagic_benthic_sum)


##### species rank order weights with marginal violin  MAIN BODY

# when I run original plotting function
# however they have filtered out the too big data!
fig_sp_rank <- fl_species_ord_marg(data = fl_pelagic_benthic_meta,
                                   lower.line=0.001, 
                                   mid.line = 0.1, 
                                   upper.line=10, 
                                   minsize =0.001)# define quantiles for lines

# Run the function below first
fig_sp_rank_without_limits <- fl_species_ord_marg_without_limits(data = fl_pelagic_benthic_meta,
                                                                 lower.line=0.001, 
                                                                 mid.line = 0.1, 
                                                                 upper.line=10, 
                                                                 minsize =0.001)# define quantiles for lines


fl_species_ord_marg_without_limits <- function(data, lower.line, mid.line, upper.line, minsize){
  
  data = data[data$weight_kg > minsize, ]# select predatory fish - individuals larger than minsize kg
  
  
  fl_speciesrank_without_limits <-  ggplot2::ggplot()+
    #geom_jitter(data= data, aes(x=reorder(Binomial, weight_kg, na.rm = TRUE), y= weight_kg,  colour = Type, alpha= Type), size = 0.5, width = 1.5)|> blend("lighten")+
    geom_jitter(data= data, aes(x=reorder(Binomial, weight_kg, na.rm = TRUE), y=weight_kg,  colour = Type, alpha= Type), size = 0.5)+
    #scale_y_log10(name  = "Bigger individuals (kg, n = 880,242)", breaks= c(0.001, 1, 100), labels= c("0.001", "1", "100"))+
    scale_y_log10(name  = "Body size (kg)", limits =c(0.001,200000),  breaks= c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000),labels= c("0.001","0.01", "0.1","1", "10", "100", "1000", "10000"))+
    
    labs(x="Species identified on BRUVS (n = 1,460)")+
    theme(legend.position = "none", axis.title.y = element_text(size=20, angle = 90),
          legend.text = element_text(size =16),axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16), axis.title.x = element_text(size=20),
          axis.ticks.x = element_blank(), 
          axis.line = element_line(arrow = arrow(type='closed', length = unit(10,'pt')))) +
    scale_colour_manual(values = c("Midwater" = '#077DAA', 'Seabed' = 'orange'))+ 
    scale_x_discrete(label = NULL)+
    scale_alpha_discrete(range = c(0.80, 0.05))
  
  #benthic mean of medians
  #(0.050+0.073+0.090+0.089+0.120+0.027)/6 = 0.74
  #benthic mean of 0.95 quantiles
  #(0.355+2.052+2.112+2.189+2.935+1.414)/6 = 1.84
  
  #benthic quantiles
  #geom_hline(yintercept=0.74, na.rm =TRUE, linetype="solid", colour = "darkorange")+
  #geom_hline(yintercept=1.84, na.rm =TRUE, linetype="dashed",colour = "darkorange")+ 
  
  #pelagic mean of medians
  #(0.014+0.134+0.004+0.006+0.009+0.015)/6 =0.030
  #pelagic mean of 0.95 quantiles
  #(0.355+83.342+10.057+32.821+15.303+12.97)/6 = 25.808
  
  #pelagic quantiles
  #geom_hline(yintercept=0.030, na.rm =TRUE, linetype="solid", colour = "#077DAA")+ 
  #geom_hline(yintercept=25, na.rm =TRUE, linetype="dashed", colour = "#077DAA")
  
  
  fl_species <- ggExtra::ggMarginal(fl_speciesrank_without_limits, groupFill= TRUE, groupColour = TRUE, type = "violin", alpha = .6, size = 2, margins = "y")
  
  
  
  print(fl_species)
  ggsave(fl_species, filename = here::here("outputs", "species_ord_marg_without_limits.png"), width = 20, height = 12, units = "in", dpi =300)
  
  #ggsave(fl_species, filename = here::here("outputs", "species_ord_marg.png"), width = 20, height = 12, units = "in", dpi =300)
  
  invisible(fl_species)
  
}





