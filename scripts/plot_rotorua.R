# plot rotorua temperature data and calculate some mixing metrics

library(tidyverse)
library(rLakeAnalyzer)
library(readxl)
library(ggpubr)

# read in profiler data
rotorua <- read.csv('./data/rotorua/Rotorua_202202-202503_profiles.csv')

# read in bathy estimates
bty <- read_excel('./data//Rotlakes_bathymetry.xls', skip = 1) 
colnames(bty) <- c('lake', 'depth_m', 'vol_to_bottom_m3', 'vol_at_countour_m3', 
                   'planar_sa_m2', 'model_sd_m2')

bty$depth_m <- abs(bty$depth_m)
bty <- bty %>% 
  filter(lake=='Rotorua')

# area and depth of bathymetry
bthA <- bty$model_sd_m2
bthD <- bty$depth_m
area <- bty$model_sd_m2[1]
volume <- bty$vol_to_bottom_m3[1]

bty_interp <- data.frame(lake = bty$lake[1],
                         depth_m = seq(1, max(bty$depth_m), 1),
                         vol_at_contour_m3  = approx(bty$depth_m, bty$vol_at_countour_m3 , seq(0, max(bty$depth_m) - 1, 1))$y)

# select just one year
rotorua <- rotorua %>% 
  filter(DateTime > '2024-01-01' & DateTime < '2025-01-01')

# rename columns
rotorua <- rotorua %>% 
  rename(datetime = DateTime,
         depth = DptSns,
         temperature = TmpWtr)

source('./scripts/functions/interpolate_plot_profiler_data.R')

rotorua_interp <- interpolate_depths(df = rotorua, lake_name = 'Rotorua')
p1 <- create_heatmap(df = rotorua_interp, lake_name = 'Rotorua')
p1

# calculate daily temps for calculating thermal metrics
rotorua_daily <- rotorua_interp %>% 
  mutate(date = as.Date(datetime)) %>% 
  group_by(date, depth) %>% 
  summarise(temperature = mean(temperature, na.rm = TRUE))  

# read in met data 
met <- read.csv('./data/rotorua/Rotorua_202202-202503_meteorology.csv')
met_daily <- met %>% 
  mutate(date = as.Date(DateTime)) %>% 
  group_by(date) %>% 
  summarise(wind = mean(WndSpd, na.rm = TRUE))

rotorua_daily <- left_join(rotorua_daily, met_daily, by = 'date')

# calculate thermocline depth
rotorua_thermo <- rotorua_daily %>% 
  group_by(date) %>% 
  summarise(thermo_depth = thermo.depth(temperature, depth, seasonal = TRUE),
            thermo_depth = ifelse(is.na(thermo_depth), 0, thermo_depth),
            thermocline_0.1 = thermo.depth(temperature, depth, Smin = 0.1),
            thermocline_0.2 = thermo.depth(temperature, depth, Smin = 0.2),
            thermocline_0.3 = thermo.depth(temperature, depth, Smin = 0.3),
            schmidt_stability = schmidt.stability(temperature, depth, bthA = bty$model_sd_m2, bthD = bty$depth_m),
            epi_temp = epi.temperature(temperature, depth, bthA, bthD),
            epi_dens = water.density(epi_temp),
            hypo_temp = hypo.temperature(temperature, depth, bthA, bthD),
            hypo_dens = water.density(hypo_temp),
            meta_top = meta.depths(temperature, depth)[1],
            meta_bot = meta.depths(temperature, depth)[2],
            uStar = uStar(wndSpeed = wind, wndHeight = 10, averageEpiDense = epi_dens),
            lake_num = lake.number(bthA = bthA, bthD = bthD, uStar = uStar, St = schmidt_stability,
                                   metaT = meta_top, metaB = meta_bot, averageHypoDense = hypo_dens),
            strat = ifelse(thermo_depth > 0, 1, 0),
            wedderburn = wedderburn.number(hypo_dens - epi_dens, 
                                           metaT = meta_bot - meta_top,
                                           uSt = uStar,
                                           Ao = area,
                                           AvHyp_rho = mean(hypo_dens)))
  

# calculate a noramlized schmidt stability
rotorua_thermo <- rotorua_thermo %>% 
  group_by(date) %>% 
  mutate(norm_ss = (schmidt_stability*area)/volume)

rotorua_thermo$date <- as.POSIXct(rotorua_thermo$date)

rotorua_long <- rotorua_thermo %>% 
  pivot_longer(thermocline_0.1:thermocline_0.3, names_to = 'density_min',
               values_to = 'thermocline')

p1 +
  geom_line(data = rotorua_long, aes(x = date, y = thermocline, color = density_min), 
            size = 1, inherit.aes = FALSE) +
  scale_color_manual(values = c('black', 'grey', 'brown'))


p2 <- ggplot(rotorua_thermo, aes(x = date, y = schmidt_stability)) +
  geom_line(size = 1) +
  theme_bw()
p2
cv <- sd(rotorua_thermo$schmidt_stability)/mean(rotorua_thermo$schmidt_stability)
cv

ggarrange(p1, p2, nrow = 2)

## calculate the number of mixings
# definition: stratified is when there is no thermocline depth
rotorua_thermo <- rotorua_thermo %>% 
  mutate(thermocline_0.1 = ifelse(is.na(thermocline_0.1), 0, thermocline_0.1))  # if thermocline is NA, set it to 0

max_depth <- max(rotorua_daily$depth)

p3 <- p1 + 
  geom_step(data = rotorua_thermo, aes(x = date, y = strat*max_depth),
            color = "black", size = 1, inherit.aes = FALSE) +
  scale_y_reverse(
    name = "Depth (m)",
    sec.axis = sec_axis(
      trans = ~ . / max_depth,
      breaks = c(0, 1) * max_depth,
      labels = c("Mixed", "Stratified"),
      name = "Stratification"
    ))

ggarrange(p3, p2, nrow = 2)

rotorua_long <- rotorua_thermo %>% 
  select(-thermocline_0.1, -thermocline_0.2, -thermocline_0.3) %>% 
  pivot_longer(thermo_depth:norm_ss, names_to = 'stratification_var',
               values_to = 'value')

ggplot(rotorua_long, aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~stratification_var, scales= 'free_y') +
  theme_bw() +
  ggtitle('Stratification metrics rotorua')

