#load all functions
# NOTE: loading this way no longer works on newer versions of R due to rgdal being deprecatied at the end of 2023.
# devtools::load_all()
# devtools::document()

# Set the folder path
folder_path <- "~/Desktop/Git/sizespectra_QAQC/R"

# Get a list of all R files in the folder
r_files <- list.files(path = folder_path, pattern = "\\.R$", full.names = TRUE)

# Source each file (load the functions)
sapply(r_files, source)

library(dplyr)
library(patchwork)

# Install CheckEM to check biomass and length
library('remotes')
options(timeout=9999999)

remotes::install_github("GlobalArchiveManual/CheckEM")
library(CheckEM)

pelagic_fl <- read_fl_pelagic() %>% 
  dplyr::select(-c(Exped.)) %>%
  CheckEM::clean_names() %>%
  dplyr::mutate(rms_mm = as.numeric(rms_mm)) %>%
  dplyr::mutate(direction_deg = as.numeric(direction_deg)) %>%
  dplyr::mutate(horz_dir_deg = as.numeric(horz_dir_deg)) %>%
  dplyr::mutate(vert_dir_deg = as.numeric(vert_dir_deg)) %>%
  dplyr::mutate(mid_x_mm = as.numeric(mid_x_mm)) %>%
  dplyr::mutate(mid_y_mm = as.numeric(mid_y_mm)) %>%
  dplyr::mutate(mid_z_mm = as.numeric(mid_z_mm)) %>%
  dplyr::mutate(frame = as.numeric(frame)) %>%
  dplyr::mutate(period_time_mins = as.numeric(period_time_mins)) %>%
  #dplyr::select(new_opcode, location, family, binomial, length_mm, precision_mm, rms_mm, range_mm, number) %>%
  glimpse()

names(pelagic_fl) %>% sort()

benthic_fl <- read_fl_benthic() %>% 
  CheckEM::clean_names() %>%
  dplyr::mutate(rms_mm = as.numeric(rms_mm)) %>%
  dplyr::mutate(direction_deg = as.numeric(direction_deg)) %>%
  dplyr::mutate(horz_dir_deg = as.numeric(horz_dir_deg)) %>%
  dplyr::mutate(vert_dir_deg = as.numeric(vert_dir_deg)) %>%
  dplyr::mutate(mid_x_mm = as.numeric(mid_x_mm)) %>%
  dplyr::mutate(mid_y_mm = as.numeric(mid_y_mm)) %>%
  dplyr::mutate(mid_z_mm = as.numeric(mid_z_mm)) %>%
  dplyr::mutate(frame = as.numeric(frame)) %>%
  dplyr::mutate(period_time_mins = as.numeric(period_time_mins)) %>%
  #dplyr::select(sample_id, family, binomial, length_mm, precision_mm, rms_mm, range_mm) %>%
  glimpse()

names(benthic_fl)

fish_families_pelagic <- c("Nomeidae", 
                   "Istiophoridae", 
                   "Fistulariidae", 
                   "Coryphaenidae", 
                   "Scombridae", 
                   "Carangidae", 
                   "Myliobatidae", 
                   "Echeneidae", 
                   "Gerreidae", "
                   Carcharhinidae", 
                   "Sphyrnidae", 
                   "Exocoetidae", 
                   "Monacanthidae", 
                   "Lamnidae", 
                   "Balistidae", 
                   "Molidae", 
                   "Caproidae", 
                   "Centriscidae", 
                   "Centrolophidae", 
                   "Dasyatidae", 
                   "Clupeidae", 
                   "Blenniidae", 
                   "Mullidae", 
                   "Priacanthidae", 
                   "Sphyraenidae", 
                   "Apogonidae", 
                   "Tetraodontidae", 
                   "Pomacentridae", 
                   "Lutjanidae", 
                   "Labridae", 
                   "Syngnathidae", 
                   "Sparidae", 
                   "Kyphosidae", 
                   "Rachycentridae", 
                   "Belonidae", 
                   "Acanthuridae", "
                   Caesionidae", 
                   "Aulostomidae", 
                   "Platax", 
                   "Engraulidae", 
                   "Chanidae", 
                   "Nematistiidae", 
                   "Lobotidae", 
                   "Macroramphosidae", 
                   "Ephippidae", 
                   "Rhincodontidae")

fish_families_benthic <- c("Glaucosomatidae", "Labridae", "Lethrinidae", "Pomacanthidae", "Scaridae", "Serranidae", "Sparidae", "Carangidae",   
                   "Chaetodontidae", "Haemulidae", "Kyphosidae", "Mullidae", "Pomacentridae", "Dasyatidae", "Acanthuridae", "Muraenidae",   
                   "Siganidae","Pseudochromidae", "Monacanthidae", "Tetraodontidae","Heterodontidae", "Lutjanidae", "Aulopidae", "Orectolobidae", "Clupeidae",    
                   "Nemipteridae",  "Enoplosidae", "Myliobatidae",  "Scorpaenidae", "Scorpididae",   "Rhinidae", "Carcharhinidae","Sphyrnidae",   
                   "Arripidae","Caesionidae",  "Scombridae",    "Gobiidae", "Pinguipedidae", "Cheilodactylidae", "Aracanidae",    "Microcanthidae",
                   "Platycephalidae", "Rhinobatidae", "Urolophidae",   "Ephippidae",   "Synodontidae",  "Blenniidae",   "Caesioscorpididae", "Apogonidae",   "Plesiopidae",   
                   "Echeneidae",   "Cirrhitidae",   "Girellidae",   "Neosebastidae", "Scyliorhinidae", "Lamnidae", "Hemigaleidae",  "Sillaginidae",  "Grammistidae", 
                   "Sphyraenidae",  "Pempheridae",  "Gerreidae","Alopiidae",  "Odacidae", "Sebastidae",   "Pleuronectidae", "Parascylliidae", "Rachycentridae","Zanclidae",    
                   "Antennariidae", "Veliferidae",  "Triakidae","Terapontidae", "Berycidae","Dinolestidae", "Diodontidae",   "Oplegnathidae", "Pristiophoridae",                                  
                   "Chironemidae",  "Gempylidae",    "Mobulidae",    "Balistidae",    "Cheloniidae",  "Holocentridae",  "Aulostomidae",  "Ginglymostomatidae",                              
                   "Ostraciidae",   "Malacanthidae", "Clinidae", "Microdesmidae", "Stegostomatidae", "Fistulariidae", "Belonidae",  "Albulidae","Priacanthidae",
                   "Hemiscylliidae","Tripterygiidae", "Odontaspididae","Plotosidae",   "Elopidae", "Paralichthyidae", "Bothidae", "Ariidae", "Latidae",  "Oneirodidae",  
                   "Syngnathidae",  "Callionymidae", "Opistognathidae", "Centriscidae", "Trichonotidae", "Pristidae",   "Congridae","Chanidae", "Samaridae",
                   "Aplodactylidae", "Zeidae",   "Melanostomiidae", "Monodactylidae","Leiognathidae", "Atherinidae",   "Hemiramphidae", "Pentacerotidae",                                 
                   "Triglidae", "Rhinopteridae", "Istiophoridae", "Soleidae", "Pimelodidae",  "Chirocentridae","Chimaeridae",  "Chlorophthalmidae",
                   "Squalidae",    "Polymixiidae",  "Centrophoridae", "Macrouridae",   "Centrolophidae", "Polyprionidae", "Zeniontidae", "Hexanchidae",   
                   "Euclichthyidae",  "Torpedinidae",  "Macroramphosidae", "Rajidae",  "Trichiuridae", "Myxinidae","Trachichthyidae", "Etmopteridae",  "Ophidiidae",   
                   "Moridae",  "Molidae",  "Acropomatidae", "Myctophidae",  "Tetrarogidae",  "Salangidae",   "Scatophagidae", "Acanthoclinidae" , "Ophichthidae",  
                   "Gobiesocidae", "Carapidae","Peristediidae", "labridae", "carangidae",  "balistidae",    "cirrhitidae",   "Polynemidae",   "Mugilidae",    
                   "Eleotridae",    "Tetrabrachiidae" , "caesionidae",   "serranidae",   "Trygonorrhinidae", "Sciaenidae",   "Pomatomidae",   "Stomiidae",    
                   "Rhombosoleidae","Monocentridae", "Rhyncobatus",  "Glaucostegidae", "ACANTHURIDAE",  "BALISTIDAE", "BLENNIDEA","LABRIDAE", "MULLIDAE", 
                   "PINGUIPEDIDAE", "POMACENTRIDAE", "SERRANIDAE",   "CARANGIDAE",    "CHAETODONTIDAE", "LETHRINIDAE",   "MOBULIDAE", "NEMIPTERIDAE",  
                   "POMACANTHIDAE", "SCARIDAE", "SIGANIDAE",  "TETRAODONTIDAE","CIRRHITIDAE",  "DASYATIDAE",    "HOLOCENTRIDAE", "LUTJANIDAE",  "ANTHIINAE", 
                   "CAESIONIDAE",   "GOBIIDAE", "PSEUDOCHROMIDAE", "MURAENIDAE", "HAEMULIDAE",  "EPHIPPIDAE", "MICRODESMIDAE", "SPHYRAENIDAE", "MONACANTHIDAE", 
                   "MALACANTHIDAE", "DIODONTIDAE",   "ECHENEIDIDAE", "SPARIDAE", "OSTRACIIDAE",   "KYPHOSIDAE",    "SYNODONTIDAE",  "BLENNIDAE",  
                   "CHARCHARHINIDAE", "PEMPHERIDAE",  "APLOACTINIDAE", "CONGRIDAE", "PRIACANTHIDAE", "HEMIRAMPHIDAE", "SCORPAENIDAE",  "APOGONIDAE",   
                   "FISTULARIIDAE", "TORPEDINIDAE",  "Gadidae", "Lotidae", "Cottidae",   "Trachinidae",  "Anarhichadidae")  


filtered_pelagic_fl <- pelagic_fl %>%
  dplyr::rename(Family = family,
                Binomial = binomial) %>%
  select_fish_families_pelagic(., fish_families_pelagic) %>%
  clean_names()

filtered_benthic_fl <- benthic_fl %>%
  dplyr::rename(Family = family,
                Binomial = binomial) %>%
  select_fish_families_benthic(., fish_families_benthic) %>%
  clean_names()

all_forklengths <- bind_rows(filtered_pelagic_fl, filtered_benthic_fl) 

over_20rms <- all_forklengths %>%
  dplyr::filter(rms_mm > 20)

over_range <- all_forklengths %>%
  dplyr::filter(range_mm > 7000)

percent_length_over_rms <- nrow(over_20rms)/nrow(all_forklengths)
percent_length_over_rms * 100 # 18.8% 

percent_length_over_range <- nrow(over_range)/nrow(all_forklengths)
percent_length_over_range * 100 # 1.1% 

plot(all_forklengths$range_mm, all_forklengths$length_mm)
plot(all_forklengths$length_mm, all_forklengths$rms_mm)

nrow(all_forklengths)

gg_range <- ggplot(all_forklengths, aes(x = range_mm, y = length_mm)) +
  geom_point(shape = 21, col = "black", fill = "blue", alpha = 0.3) +
  theme_minimal() +
  labs(title = "Relationship of Range (mm) and Length (mm)", 
       x = "Range (mm)", 
       y = "Length of fish (mm)") +
  geom_vline(xintercept = 7000) +
  annotate("text",
           x = 7000,
           y = 3500,
           vjust = -0.5,
           angle = 90,
           label = "7 m range cut off",
           size = unit(4, "pt")) +
  theme(axis.line = element_line(colour = "black")) 

gg_range

gg_range_hist <- ggplot(all_forklengths, aes(x = range_mm)) +
  geom_histogram(binwidth = 100, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Range", 
       x = "Range (mm)", 
       y = "Frequency") +
  geom_vline(xintercept = 7000) +
  annotate("text",
           x = 7000,
           y = 7500,
           vjust = -0.5,
           angle = 90,
           label = "7 m range cut off" ,
           size = unit(4, "pt")) +
  theme_minimal()

gg_range_hist

gg_rms <- ggplot(all_forklengths, aes(x = length_mm, y = rms_mm)) +
  geom_point(shape = 21, col = "black", fill = "blue", alpha = 0.3) +
  theme_minimal() +
  labs(title = "Relationship of RMS (mm) and Length (mm)", 
       x = "Length of fish (mm)", 
       y = "RMS (mm)") +
  geom_hline(yintercept = 20)+
  annotate("text",
           x = 3800,
           y = 20,
           vjust = -0.5,
           label = "20 mm RMS",
           size = unit(4, "pt")) +
  theme(axis.line = element_line(colour = "black"))

gg_rms

gg_rms_hist <- ggplot(all_forklengths, aes(x = rms_mm)) +
  geom_histogram(binwidth = 20, fill = "steelblue", color = "black") +
  labs(title = "Histogram of RMS", 
       x = "RMS (mm)", 
       y = "Frequency") +
  geom_vline(xintercept = 20) +
  annotate("text",
           x = 20,
           y = 75000,
           vjust = 1.5,
           angle = 90,
           label = "20 mm RMS cut off",
           size = unit(4, "pt")) +
  theme_minimal()

gg_rms_hist



plots_range_rms <- (gg_range + gg_range_hist) / (gg_rms + gg_rms_hist) + plot_annotation(tag_levels = "A")
plots_range_rms

ggsave(plots_range_rms,
       filename = "qaqc-plots/range_rms_plot.png",
       height = 9,
       width = 9,
       dpi = 500)


# Check lengths over maximum size ----
life_history <- CheckEM::global_life_history %>%
  dplyr::select(family, genus, species, fb_length_max, fb_length_max_type) %>%
  dplyr::rename(fb_length_max_cm = fb_length_max) %>%
  dplyr::mutate(binomial = paste(genus, species)) %>%
  dplyr::mutate(fb_length_max_cm = if_else(binomial %in% "Rhynchobatus australiae", 300, fb_length_max_cm)) %>%
  glimpse()

names(all_forklengths)

# Use the authors cleaning functions on the fork length data first ----

pelagic_fl_cleaned <- clean_fl_pelagic(read_fl_pelagic()) %>%
  select_fish_families_pelagic(., fish_families_pelagic) %>%
  clean_names()

benthic_fl_cleaned <- clean_fl_benthic(read_fl_benthic()) %>%
  select_fish_families_benthic(., fish_families_benthic) %>%
  clean_names()

all_fl_cleaned <- bind_rows(pelagic_fl_cleaned, benthic_fl_cleaned) 

# Find species that aren't in life_history
length(unique(all_fl_cleaned$binomial)) # 1495 unique species

missing_in_lh <- anti_join(all_fl_cleaned, life_history, by = "binomial") %>%
  distinct(family, binomial) %>%
  separate(binomial, into = c("genus", "species"), extra = "merge") %>%
  dplyr::filter(!species %in% c("sp", "sp1", "sp2", "sp3", "sp."))


nrow(missing_in_lh)/length(unique(all_fl_cleaned$binomial)) * 100 # 4.7% of species don't match fishbase

# find length measurements that are over maximum
joined_data <- left_join(all_fl_cleaned, life_history, by = "binomial") %>%
  filter(!is.na(fb_length_max_cm))

rays <- joined_data %>%
  dplyr::filter(fb_length_max_type %in% "WD")

unique(joined_data$fb_length_max_type)

fork_or_total <- joined_data %>%
  dplyr::filter(fb_length_max_type %in% c("FL", "TL"))

over_size <- fork_or_total %>%
  dplyr::filter(lengthcm > fb_length_max_cm) %>%
  dplyr::mutate(difference = lengthcm - fb_length_max_cm) %>%
  dplyr::mutate(percent_difference = lengthcm/fb_length_max_cm*100)

nrow(over_size)/nrow(fork_or_total) * 100 # 1.4% are over their maximum length on fishbase
