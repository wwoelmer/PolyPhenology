
run_thermal_metrics <- function(df_daily, # dataframe with date, temperature, depth, wind
                                bty_area, # column of df with bathymetric areas
                                bty_depth, # column of df with  bathymetric depths which correspond to bty_area
                                volume, # volume of the lake
                                Smin = 0.1 # default Smin for thermocline depth calculations
                                ){
  
  area <- bty_area[1]

  df_out <- df_daily %>% 
    group_by(date) %>% 
    summarise(thermo_depth = thermo.depth(temperature, depth, Smin = Smin, seasonal = TRUE),
              thermo_depth = ifelse(is.na(thermo_depth), 0, thermo_depth),
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
              norm_ss_size = (schmidt_stability*area)/volume,
              wedderburn = wedderburn.number(hypo_dens - epi_dens, 
                                             metaT = meta_bot - meta_top,
                                             uSt = uStar,
                                             Ao = area,
                                             AvHyp_rho = mean(hypo_dens)))
  
  # calculate normalized schmidt stability based on max observed schmidt stability
  df_out <- df_out %>% # first set any negatives to 0
    ungroup() %>% 
    mutate(schmidt_stability = ifelse(schmidt_stability < 0, 0, schmidt_stability),
           max_ss = max(schmidt_stability, na.rm = TRUE)) %>% 
    group_by(date) %>% 
    mutate(norm_ss_max = (schmidt_stability - 1)/max_ss) %>% 
    select(-max_ss)
  
  return(out = df_out)
  
}

