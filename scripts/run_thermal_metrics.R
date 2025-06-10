library(tidyverse)
library(readxl)
library(rLakeAnalyzer)
source('./scripts/functions/calculate_many_metrics.R')

lake_name <- 'Okaro'
profile_filepath <- './data/profiler_data/okaro/Okaro_202104-202504_profiles.csv'
met_filepath <- './data/profiler_data/okaro/Okaro_202103-202504_meteorology.csv'

df <- read.csv(profile_filepath)

# select just one year and rename columns
df <- df %>% 
  filter(DateTime > '2024-01-01' & DateTime < '2025-01-01') %>% 
  rename(datetime = DateTime, 
         depth = DptSns,
         temperature = TmpWtr)

# calculate daily temps for calculating thermal metrics
df_daily <- df %>% 
  mutate(date = as.Date(datetime)) %>% 
  group_by(date, depth) %>% 
  summarise(temperature = mean(temperature, na.rm = TRUE))  

# read in bathy estimates
bty <- read_excel('./data//Rotlakes_bathymetry.xls', skip = 1) 
colnames(bty) <- c('lake', 'depth_m', 'vol_to_bottom_m3', 'vol_at_countour_m3', 
                   'planar_sa_m2', 'model_sd_m2')

bty$depth_m <- abs(bty$depth_m)
bty <- bty %>% 
  filter(lake==lake_name)

# area and depth of bathymetry
bthA <- bty$model_sd_m2
bthD <- bty$depth_m
volume <- bty$vol_to_bottom_m3[1]

# read in met data 
met <- read.csv(met_filepath)
met_daily <- met %>% 
  mutate(date = as.Date(DateTime)) %>% 
  group_by(date) %>% 
  summarise(wind = mean(WndSpd, na.rm = TRUE))

ggplot(met_daily, aes(x = as.Date(date), y = wind)) +
  geom_point()

df_daily <- left_join(df_daily, met_daily, by = 'date')

df_daily <- df_daily %>% 
  filter(!is.nan(temperature))

# calculate a load of thermal metrics
df_therm <- run_thermal_metrics(df_daily, 
                                bty_area = bthA, 
                                bty_depth = bthD,
                                volume = volume)

therm_long <- df_therm %>% 
  pivot_longer(thermo_depth:norm_ss_max, names_to = 'stratification_var',
               values_to = 'value')

ggplot(therm_long, aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~stratification_var, scales= 'free_y') +
  theme_bw() +
  ggtitle(paste0('Stratification metrics ', lake_name))

write.csv(therm_long, paste0('./data/', lake_name, '_thermal_metrics.csv'), row.names = FALSE)
