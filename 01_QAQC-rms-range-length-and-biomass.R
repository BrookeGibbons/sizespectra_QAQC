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

all_forklengths <- bind_rows(pelagic_fl, benthic_fl)

over_20rms <- all_forklengths %>%
  dplyr::filter(rms_mm > 20)

over_range <- all_forklengths %>%
  dplyr::filter(range_mm > 7000)

percent_length_over_rms <- nrow(over_20rms)/nrow(all_forklengths)
percent_length_over_rms * 100 # 19.2% 

percent_length_over_range <- nrow(over_range)/nrow(all_forklengths)
percent_length_over_range * 100 # 1.2% 

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
           y = 7500,
           vjust = -0.5,
           angle = 90,
           label = "7 m range cut off") +
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
           y = 10000,
           vjust = -0.5,
           angle = 90,
           label = "7 m range cut off") +
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
           x = 7500,
           y = 20,
           vjust = -0.5,
           label = "20 mm RMS") +
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
           y = 100000,
           vjust = 1.5,
           angle = 90,
           label = "20 mm RMS cut off") +
  theme_minimal()

gg_rms_hist



plots_range_rms <- (gg_range + gg_range_hist) / (gg_rms + gg_rms_hist) + plot_annotation(tag_levels = "A")
plots_range_rms

ggsave(plots_range_rms,
       filename = "qaqc-plots/range_rms_plot.png",
       dpi = 300)

