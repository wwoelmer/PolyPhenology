# plot rerewhakaaitu temperature data and calculate some mixing metrics

library(tidyverse)
library(rLakeAnalyzer)
library(readxl)
library(ggpubr)

# read in profiler data
okaro <- read.csv('./data/okaro/Okaro_202104-202504_profiles.csv')

# read in bathy estimates
bty <- read_excel('./data//Rotlakes_bathymetry.xls', skip = 1) 
colnames(bty) <- c('lake', 'depth_m', 'vol_to_bottom_m3', 'vol_at_countour_m3', 
                   'planar_sa_m2', 'model_sd_m2')

bty$depth_m <- abs(bty$depth_m)
bty <- bty %>% 
  filter(lake=='Okaro')

# area and depth of bathymetry
bthA <- bty$model_sd_m2
bthD <- bty$depth_m

bty_interp <- data.frame(lake = bty$lake[1],
                         depth_m = seq(1, max(bty$depth_m), 1),
                         vol_at_contour_m3  = approx(bty$depth_m, bty$vol_at_countour_m3 , seq(0, max(bty$depth_m) - 1, 1))$y)

# select just one year
okaro <- okaro %>% 
  filter(DateTime > '2024-01-01' & DateTime < '2025-01-01')

# rename columns
okaro <- okaro %>% 
  rename(datetime = DateTime,
         depth = DptSns,
         temperature = TmpWtr)

source('./scripts/functions/interpolate_plot_profiler_data.R')

okaro_interp <- interpolate_depths(df = okaro, lake_name = 'Okaro')
p1 <- create_heatmap(df = okaro_interp, lake_name = 'Okaro')
p1

# calculate daily temps
okaro_daily <- okaro_interp %>% 
  mutate(date = as.Date(datetime)) %>% 
  group_by(date, depth) %>% 
  summarise(temperature = mean(temperature, na.rm = TRUE))  

# calculate thermocline depth
okaro_thermo <- okaro_daily %>% 
  group_by(date) %>% 
  summarise(thermocline_0.1 = thermo.depth(temperature, depth, Smin = 0.1),
            thermocline_0.2 = thermo.depth(temperature, depth, Smin = 0.2),
            thermocline_0.3 = thermo.depth(temperature, depth, Smin = 0.3),
            schmidt_stability = schmidt.stability(temperature, 
                                                  depth, 
                                                  bthA = bty$model_sd_m2, 
                                                  bthD = bty$depth_m),
            epi_temp = epi.temperature(wtr = temperature, depths = depth, bthA = bthA, bthD = bthD),
            hypo_temp = hypo.temperature(wtr = temperature, depths = depth, bthA = bthA, bthD = bthD))

okaro_thermo$date <- as.POSIXct(okaro_thermo$date)

okaro_long <- okaro_thermo %>% 
  pivot_longer(thermocline_0.1:thermocline_0.3, names_to = 'density_min',
               values_to = 'thermocline')

p1 +
  geom_line(data = okaro_long, aes(x = date, y = thermocline, color = density_min), 
            size = 1, inherit.aes = FALSE) +
  scale_color_manual(values = c('black', 'grey', 'brown'))


p2 <- ggplot(okaro_thermo, aes(x = date, y = schmidt_stability)) +
  geom_line(size = 1) +
  theme_bw()

cv <- sd(okaro_thermo$schmidt_stability)/mean(okaro_thermo$schmidt_stability)
cv

ggarrange(p1, p2, nrow = 2)

## calculate the number of mixings
# definition: stratified is when there is no thermocline depth
okaro_thermo <- okaro_thermo %>% 
  mutate(thermocline_0.1 = ifelse(is.na(thermocline_0.1), 0, thermocline_0.1)) %>% # if thermocline is NA, set it to 0
  mutate(strat = ifelse(thermocline_0.1 > 0, 1, 0)) # if thermoclien is >0, then it is stratified

p1 + 
  geom_step(data = okaro_thermo, aes(x = date, y = strat*max_depth),
            color = "black", size = 1, inherit.aes = FALSE) +
  scale_y_reverse(
    name = "Depth (m)",
    sec.axis = sec_axis(
      trans = ~ . / max_depth,
      breaks = c(0, 1) * max_depth,
      labels = c("Mixed", "Stratified"),
      name = "Stratification"
    ))

okaro_long <- okaro_thermo %>% 
  select(-thermocline_0.2, -thermocline_0.3) %>% 
  pivot_longer(thermocline_0.1:hypo_temp, names_to = 'stratification_var',
               values_to = 'value')

ggplot(okaro_long, aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~stratification_var, scales= 'free') +
  theme_bw() +
  ggtitle('Stratification metrics Okaro')
