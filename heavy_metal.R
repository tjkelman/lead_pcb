
# my_packages <- c("tidyverse", "googlesheets", "readxl", "janitor", "stringr",
#                  "ggthemes", "here", "hrbrthemes", "gghighlight")
# install.packages(my_packages)

library(tidyverse)
library(googlesheets)
library(readxl)
library(janitor)
library(stringr)
library(ggthemes)
library(here)
library(hrbrthemes)
library(gghighlight)

# sheet_meta <- gs_title("Pb/PBDE/PCB Egg Submissions")

# open Pb/PBDE/PCB Egg Submissions using the direct key since googlesheets package can't
# use gs_title (or gs_ls()) for Team Drives:

sheet_meta <- gs_key("16iR75UiNn_PPle-ORK9sVbWFUJPlVIuHBhQlEJ9Ayuk", lookup = FALSE, visibility = "private")
sheet_data <- gs_read(sheet_meta, ws = "HVM Results", col_names = TRUE, skip = 0, verbose = TRUE, literal = FALSE)
premise_directory <- read_excel("Data/Premise_Directory 4-25-19 geocoded.xlsx")
# premise_directory <- gs_read(sheet_meta, ws = "Premise Directory", col_names = TRUE, skip = 0, verbose = TRUE, literal = FALSE)

# metals <- c("Cr ppm", "Fe ppm", "Zn ppm", "As ppm", "Ni ppm", "Cu ppm", "Cd ppm", "Hg ppm", "Pb ppm")

cn_sheet_data <- clean_names(sheet_data)
cn_premise_directory <- clean_names(premise_directory)

# extract the metals of interest
metal_subset <- cn_sheet_data %>% 
  select(bp_id, sample_id, zip, county, matches("57g")) %>%
  mutate(zip = str_sub(as.character(zip), 1, 5)) %>%      # convert from zip+5 to just zip
  mutate(county = str_to_upper(county)) %>%
  mutate(premise_id = str_extract(sample_id, "[^-]+"))

# what percentage of samples had Pb exceed repro daily threshold?
# mean(metal_subset$Pb_ug > 0.5)
# what percentage of samples had Cd exceed toxic daily threshold?
# mean(metal_subset$Cd_ug > 4.1)

cv_label <- function(x, ...) {
  args = list(...)
  return(data.frame(y = max(x) + args$nudge, 
                    label = paste0(round(sd(x)/mean(x)*100, 1), "%")))
}

premise_metal_subset <- metal_subset %>% 
  add_count(premise_id)

# variances in Pb for >=5 eggs:
premise_metal_subset %>% 
  filter(n>5) %>% 
  filter(!str_detect(tolower(premise_id), pattern = "c")) %>% 
  mutate(premise_id = as.integer(premise_id)) %>% 
  arrange(premise_id) %>% 
  ggplot(mapping = aes(x = factor(premise_id), y = pb_ug_57g)) +
    geom_boxplot() +
    geom_dotplot(binaxis='y', 
                stackdir='center', 
                dotsize = 5, 
                binwidth = .02,
                fill="red") +
    stat_summary(fun.data = "cv_label", geom = "text", colour = "blue", size = 2, 
                 fun.args = list(nudge = 0.5)) +
    stat_summary(fun.y = mean, geom = "point", colour = "purple", size = 1.5) +
    labs(x = "Premise ID", y = "ug Pb per 57g egg", 
         title = "Lead values for noncommercial premises with at least 5 eggs submitted (blue = CV%, purple = mean)") +
    theme(title = element_text(size=8,face="bold"), 
          plot.title = element_text(hjust = 0.5))
    

# variances in Pb for 3-4 eggs:
premise_metal_subset %>% 
  filter(n>2, n<5) %>% 
  filter(!str_detect(tolower(premise_id), pattern = "c")) %>% 
  mutate(premise_id = as.integer(premise_id)) %>% 
  arrange(premise_id) %>% 
  ggplot(mapping = aes(x = factor(premise_id), y = pb_ug_57g)) +
  geom_boxplot() +
  geom_dotplot(binaxis='y',
               stackdir='center',
               dotsize = 20,
               binwidth = .02,
               fill="red") +
  stat_summary(fun.data = "cv_label", geom = "text", colour = "blue", size = 2,
               fun.args = list(nudge = 1.5)) +
  stat_summary(fun.y = mean, geom = "point", colour = "purple", size = 1.5) +
  labs(x = "Premise ID", y = "ug Pb per 57g egg", 
       title = "Lead values for noncommercial premises with 3-4 eggs submitted (blue = CV%, purple = mean)") +
  theme(title = element_text(size=8,face="bold"), 
        plot.title = element_text(hjust = 0.5)) 
         
# variances in Pb for 2 eggs:
premise_metal_subset %>% 
  filter(n==2) %>% 
  filter(!str_detect(tolower(premise_id), pattern = "c")) %>% 
  mutate(premise_id = as.integer(premise_id)) %>% 
  arrange(premise_id) %>% 
  ggplot(mapping = aes(x = factor(premise_id), y = pb_ug_57g)) +
  geom_boxplot() +
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = 5, 
               binwidth = .02,
               fill="red") +
  stat_summary(fun.data = "cv_label", geom = "text", colour = "blue", size = 2,
               fun.args = list(nudge = 0.5)) +
  stat_summary(fun.y = mean, geom = "point", colour = "purple", size = 1.5) +
  labs(x = "Premise ID", y = "ug Pb per 57g egg", 
       title = "Lead values for noncommercial premises with 2 eggs submitted (blue = CV%, purple = mean)") +
  theme(title = element_text(size=8,face="bold"), 
        plot.title = element_text(hjust = 0.5)) 


# what percentage of noncommerical premises had max Pb exceed FDA child recommendation?
noncom_premise_summary <- premise_metal_subset %>% 
  filter(!str_detect(tolower(premise_id), pattern = "c")) %>% 
  mutate(premise_id = as.integer(premise_id)) %>% 
  group_by(premise_id, county, n) %>% 
  summarize_at(vars(contains("ug")), funs(mean, max)) %>% 
  mutate_at(vars(-premise_id, -county), list(~round(., digits = 2))) 

# join the grouped-by-premise data with the premise director (which is geocoded) to have lat/long
# available for mapping:
# geocoded_metal_subset <- left_join(cn_premise_metal_subset, premise_directory, by = "premise_id")
noncom_premise_join <- left_join(noncom_premise_summary, cn_premise_directory, by = "premise_id")
com_premise_join <- left_join(com_premise_summary, cn_premise_directory, by = "premise_id")


# summarize mean and max of all the metals grouped by county
metal_summary_county <- noncom_premise_join  %>%
  ungroup() %>% 
  drop_na(arc_subreg) %>%
  add_count(arc_subreg) %>% 
  group_by(arc_subreg, n) %>%
  summarize(pct_above_fda_pb = mean(pb_ug_57g_mean > 3))

metal_summary_county %>% 
  ungroup() %>% 
  filter(n > 9, pct_above_fda_pb > 0) %>% 
  mutate(arc_subreg = fct_reorder(factor(str_to_title(arc_subreg)), desc(pct_above_fda_pb))) %>% 
  ggplot(aes(x = arc_subreg, y = pct_above_fda_pb)) +
  geom_col(fill = "#cc6600", size = 0) +
  theme_ipsum(base_size = 11, grid = "Y") +
  theme(axis.text.x = element_text(angle = 60, hjust=1)) +
  labs(y = "Percent of premises", 
       x = "County",
       title = "Percent of premises per county exceeding FDA lead threshold", 
       subtitle = "total number of premises from each county in parenthesis (at least 10 to qualify)")  +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = paste0("(",n,")")), color = "black", y = 0.01, size = 3) +
  coord_cartesian(clip="off")
               
ggsave("Plots/county_column_plot.png")  


# number of properties in Sonoma:
noncom_premise_summary_sonoma <- noncom_premise_join %>% 
  filter(arc_subreg == "Sonoma") 
nrow(noncom_premise_summary_sonoma)
sum(noncom_premise_summary_sonoma$pb_ug_57g_mean > 3)
mean(noncom_premise_summary_sonoma$pb_ug_57g_mean > 3)

nrow(noncom_premise_summary)
sum(noncom_premise_summary$pb_ug_57g_mean > 3)
mean(noncom_premise_summary$pb_ug_57g_mean > 3)

noncom_premise_join %>% 
  ggplot(aes(x = pb_ug_57g_mean)) +
  geom_histogram(bins = 100, size = 0) +
  geom_vline(aes(xintercept = 3, color = "FDA Child Threshold")) +
  geom_vline(aes(xintercept = 12.5, color = "FDA Adult Threshold")) +
  labs(x = "Amount of lead (micrograms) in an average sized egg", y = "Number of premises",
       title = "Mean lead values per premise", 
       subtitle = "non-commercial residences only") +
  scale_color_manual(name = "",
                     values = c('FDA Child Threshold' = "red",
                                'FDA Adult Threshold' = "blue")) +
  # annotate("text",
  #          label = str_wrap(paste0("Percentage of premises exceeding FDA Child Threshold: ",
  #                         100 * round(mean(noncom_premise_summary$pb_ug_57g_mean >= 3, na.rm = TRUE), 2), "%"), 22),
  #          x = 21, y = 110, size = 3, colour = "red", hjust = 0) +
  # annotate("text",
  #        label = str_wrap(paste0("Percentage of premises exceeding FDA Adult Threshold: ",
  #                                100 * round(mean(noncom_premise_summary$pb_ug_57g_mean >= 12.5, na.rm = TRUE), 2), "%"), 22),
  #        x = 21, y = 90, size = 3, colour = "blue", hjust = 0) +
  annotate("rect", xmin = 21, xmax = 28, ymin = 51, ymax = 74, fill = "white", color = "white") +
  theme_ipsum(base_size = 12) +
  theme(legend.position = c(0.80, 0.51),
        axis.title.x = element_text(size = 12, vjust = -2),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12))

ggsave("Plots/lead_histogram.png")

noncom_premise_join %>% 
  ggplot(aes(x = pb_ug_57g_mean)) +
    geom_histogram(bins = 100, size = 0, fill = "orange") +
    gghighlight(arc_subreg == "Sonoma") +
    geom_vline(aes(xintercept = 3, color = "FDA Child Threshold")) +
    geom_vline(aes(xintercept = 12.5, color = "FDA Adult Threshold")) +
    labs(x = "Amount of lead (micrograms) in an average sized egg", y = "Number of premises",
         title = "Mean lead values per premise", 
         subtitle = "non-commercial residences only (Sonoma county highlighted)") +
    scale_color_manual(name = "",
                       values = c('FDA Child Threshold' = "red",
                                  'FDA Adult Threshold' = "blue")) +
    annotate("rect", xmin = 21, xmax = 28, ymin = 51, ymax = 74, fill = "white", color = "white") +
    theme_ipsum(base_size = 12) +
    theme(legend.position = c(0.80, 0.51),
          axis.title.x = element_text(size = 12, vjust = -2),
          axis.title.y = element_text(size = 12),
          legend.text = element_text(size = 12))

ggsave("Plots/lead_histogram_sonoma.png")

noncom_premise_join %>% 
  ggplot(aes(x = pb_ug_57g_mean)) +
  geom_histogram(bins = 100, size = 0, fill = "orange") +
  gghighlight(arc_subreg == "Butte") +
  geom_vline(aes(xintercept = 3, color = "FDA Child Threshold")) +
  geom_vline(aes(xintercept = 12.5, color = "FDA Adult Threshold")) +
  labs(x = "Amount of lead (micrograms) in an average sized egg", y = "Number of premises",
       title = "Mean lead values per premise", 
       subtitle = "non-commercial residences only (Butte county highlighted)") +
  scale_color_manual(name = "",
                     values = c('FDA Child Threshold' = "red",
                                'FDA Adult Threshold' = "blue")) +
  annotate("rect", xmin = 21, xmax = 28, ymin = 51, ymax = 74, fill = "white", color = "white") +
  theme_ipsum(base_size = 12) +
  theme(legend.position = c(0.80, 0.51),
        axis.title.x = element_text(size = 12, vjust = -2),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12))

ggsave("Plots/lead_histogram_butte.png")

noncom_premise_join %>% 
  ggplot(aes(x = pb_ug_57g_mean)) +
  geom_histogram(bins = 100, size = 0, fill = "orange") +
  gghighlight(arc_subreg == "Napa") +
  geom_vline(aes(xintercept = 3, color = "FDA Child Threshold")) +
  geom_vline(aes(xintercept = 12.5, color = "FDA Adult Threshold")) +
  labs(x = "Amount of lead (micrograms) in an average sized egg", y = "Number of premises",
       title = "Mean lead values per premise", 
       subtitle = "non-commercial residences only (Napa county highlighted)") +
  scale_color_manual(name = "",
                     values = c('FDA Child Threshold' = "red",
                                'FDA Adult Threshold' = "blue")) +
  annotate("rect", xmin = 21, xmax = 28, ymin = 51, ymax = 74, fill = "white", color = "white") +
  theme_ipsum(base_size = 12) +
  theme(legend.position = c(0.80, 0.51),
        axis.title.x = element_text(size = 12, vjust = -2),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 12))

ggsave("Plots/lead_histogram_napa.png")

hg_rel <- noncom_premise_summary %>%
  summarise(hg_rel = ceiling(11.2/ hg_ug_57g_mean)) %>% 
  arrange(hg_rel)

cd_madl <- noncom_premise_summary %>%
  summarise(cd_madl = ceiling(4.1/ cd_ug_57g_mean)) %>% 
  arrange(cd_madl)

sum(noncom_premise_summary$pb_ug_57g_mean >= 3, na.rm = TRUE)
sum(noncom_premise_summary$pb_ug_57g_mean >= 12, na.rm = TRUE)
mean(noncom_premise_summary$pb_ug_57g_mean, na.rm = TRUE)
median(noncom_premise_summary$pb_ug_57g_mean, na.rm = TRUE)


com_premise_summary <- premise_metal_subset %>% 
  filter(str_detect(tolower(premise_id), pattern = "c")) %>% 
  group_by(premise_id, n) %>% 
  summarize_at(vars(contains("ug")), funs(mean, max))

com_premise_summary %>% 
  ggplot(aes(x = pb_ug_57g_mean)) +
  geom_histogram(bins = 100) +
  geom_vline(aes(xintercept = 3, color = "FDA Child Threshold")) +
  geom_vline(aes(xintercept = 12, color = "FDA Adult Threshold")) +
  labs(x = "Mean Pb value per 57g egg", y = "Number of Premises",
       title = "Histogram of mean lead values per premise (commerical)") +
  scale_color_manual(name = "", 
                     values = c('FDA Child Threshold' = "red", 
                                'FDA Adult Threshold' = "blue")) +
  annotate("rect", xmin = 15, xmax = 25, ymin = 60, ymax = 100, alpha = 0.2) +
  annotate("text", 
           label = str_wrap(paste0("Percentage of premises exceeding FDA Child Threshold: ",
                                   100 * round(mean(com_premise_summary$pb_ug_57g_mean >= 3, na.rm = TRUE), 2), "%"), 22),
           x = 16, y = 90, size = 3, colour = "red", hjust = 0) +
  annotate("text", 
           label = str_wrap(paste0("Percentage of premises exceeding FDA Adult Threshold: ",
                                   100 * round(mean(com_premise_summary$pb_ug_57g_mean >= 12, na.rm = TRUE), 2), "%"), 22),
           x = 16, y = 70, size = 3, colour = "blue", hjust = 0) 


mean(com_premise_summary$pb_ug_57g_mean, na.rm = TRUE)
median(com_premise_summary$pb_ug_57g_mean, na.rm = TRUE)

mean(noncom_premise_summary$hg_ug_57g_mean, na.rm = TRUE)
mean(com_premise_summary$hg_ug_57g_mean, na.rm = TRUE)
median(noncom_premise_summary$hg_ug_57g_mean, na.rm = TRUE)
median(com_premise_summary$hg_ug_57g_mean, na.rm = TRUE)

mean(noncom_premise_summary$cd_ug_57g_mean, na.rm = TRUE)
mean(com_premise_summary$cd_ug_57g_mean, na.rm = TRUE)
median(noncom_premise_summary$cd_ug_57g_mean, na.rm = TRUE)
median(com_premise_summary$cd_ug_57g_mean, na.rm = TRUE)

noncom_premise_summary %>% 
  filter(!is.na(cd_ug_57g_mean)) %>% 
  ggplot(aes(x = cd_ug_57g_mean)) +
    geom_histogram(bins = 50) +
    geom_vline(aes(xintercept = 4.1, color = "MADL Repro Threshold")) +
    labs(x = "Mean Cd value per 57g egg", y = "Number of Premises",
         title = "Histogram of mean cadmium values per premise") +
    scale_color_manual(name = "", 
                       values = c('MADL Repro Threshold' = "red"))
  
# what percentage of premises had Cd exceed repro daily threshold?
mean(noncom_premise_summary$cd_ug_57g_max >= 4.1, na.rm = TRUE)



sum(as.numeric(noncom_premise_join$number_of_eggs_submitted), na.rm = TRUE)
sum(as.numeric(com_premise_join$number_of_eggs_submitted), na.rm = TRUE)

summary(premise_metal_subset$Pb_ug_max)
quantile(premise_metal_subset$Pb_ug_max, prob = seq(0, 1, length = 11), type = 5)

# what percentage of premises had Cd exceed toxic daily threshold?
mean(premise_metal_subset$Cd_ug_max > 0.05)

## Write out the data
write_csv(metal_summary_county, "Data/metal_summary_county.csv")
write_csv(metal_summary_zip, "Data/metal_summary_zip.csv")
write_csv(geocoded_metal_subset, "Data/geocoded_metal_data.csv")
write_csv(noncom_premise_join, "Data/noncom_metal_data.csv")
