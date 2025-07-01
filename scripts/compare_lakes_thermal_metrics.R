

rere <- read.csv('./data/Rerewhakaaitu_thermal_metrics.csv') %>% 
  mutate(lake = 'Rerewhakaaitu')

rotorua <- read.csv('./data/Rotorua_thermal_metrics.csv') %>% 
  mutate(lake = 'Rotorua') 

rotoehu <- read.csv('./data/Rotoehu_thermal_metrics.csv') %>% 
  mutate(lake = 'Rotoehu') 

okaro <- read.csv('./data/Okaro_thermal_metrics.csv') %>% 
  mutate(lake = 'Okaro') 

lakes <- full_join(rere, rotorua)
lakes <- full_join(lakes, rotoehu)
lakes <- full_join(lakes, okaro)

ss <- lakes %>% 
  filter(stratification_var %in% c('norm_ss_max',
                                   'schmidt_stability',
                                   'norm_ss_size',
                                   'norm_ss_depth'))

ss %>% 
  filter(lake!='Okaro') %>% 
  ggplot(aes(x = stratification_var, y = value, fill = lake)) +
  geom_boxplot() +
  facet_wrap(~stratification_var, scales = 'free') +
  theme_bw()

ggplot(ss, aes(x = as.Date(date), y = value, color = lake)) +
  geom_point() +
  facet_wrap(~stratification_var, scales= 'free') +
  theme_bw() +
  xlab('Date')

ss %>% 
  filter(lake!='Okaro') %>% 
ggplot(aes(x = as.Date(date), y = value, color = lake)) +
  geom_point() +
  facet_wrap(~stratification_var, scales= 'free') +
  theme_bw() +
  xlab('Date')


# subset to variable which are comparable across lakes
lakes %>% 
  filter(stratification_var %in% c('norm_ss_max', 
                                   'schmidt_stability',
                                   'norm_ss_size',
                                   'norm_ss_depth',
                                   'wedderburn',
                                   'uStar')) %>% 
  ggplot(aes(x = as.Date(date), y = value, color = lake)) +
  geom_point() +
  facet_wrap(~stratification_var, scales = 'free') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
