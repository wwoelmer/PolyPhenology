
# helper functions for formatting, interpolating, plotting profiler data as heatmap

# create regular depth and time intervals
interpolate_depths <- function(df, lake_name){
  library(akima)
  
  # set as posixct
  df$datetime <- as.POSIXct(df$datetime)
  
  df <- df %>% 
    mutate(time_hours = as.numeric(difftime(datetime, min(datetime), units = "hours")))
  
  # remove na's in temp
  df <- df %>% 
    filter(!is.na(temperature))
  
  # round times and depths to even intervals
  time_grid <- seq(min(df$time_hours), max(df$time_hours), by = 1)
  depth_grid <- seq(floor(min(df$depth)), ceiling(max(df$depth)), by = 0.25)
  
  # interpolate over the time/depth grid
  interp <- with(df, akima::interp(
    x = time_hours,
    y = depth,
    z = temperature,
    xo = time_grid,
    yo = depth_grid,
    duplicate = "mean",
    linear = TRUE
  ))
  
  interp_df <- expand.grid(
    time_hours = interp$x,
    depth = interp$y)
  
  interp_df$temperature <- as.vector(interp$z)
  
  # Convert time back to POSIXct
  interp_df$datetime <- min(df$datetime) + hours(interp_df$time_hours)
  
  interp_df <- interp_df %>% 
    filter(!is.na(temperature)) %>% 
    select(datetime, depth, temperature) %>% 
    arrange(datetime)
  
  return(df = interp_df)
}

create_heatmap <- function(df, lake_name){

  
  p1 <- ggplot(df, aes(x = datetime, y = depth, fill = temperature)) +
    geom_raster(interpolate = TRUE) +
    scale_y_reverse() +
    scale_fill_viridis_c(option = "plasma", na.value = "transparent") +
    labs(title = paste0("Temperature Profile ", lake_name),
         x = "Date",
         y = "Depth (m)",
         fill = "°C") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(plot = p1)
}
