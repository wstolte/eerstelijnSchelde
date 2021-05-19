

# functions to be used in the eerstelijnsrapportage

statTable <- function(df, parname, rounding, meanorder = "decreasing") {
  stats <- df %>% sf::st_drop_geometry() %>%
    filter(parametername == parname) %>%
    mutate(year = year(datetime), month = month(datetime)) %>%
    group_by(stationname, year, month) %>% 
    summarize(monthlymean = mean(value)) %>%
    group_by(stationname, year) %>% 
    summarize(yearlymean = mean(monthlymean)) %>%
    group_by(stationname) %>%
    do(broom::tidy(lm(yearlymean ~ year, data = .))) %>% 
    filter(term == "year")
  
  df %>% sf::st_drop_geometry() %>%
    filter(parametername == parname) %>% 
    mutate(year = year(datetime), month = month(datetime)) %>%
    group_by(stationname) %>% summarize(mean = mean(value, na.rm = T), `10-perc` = quantile(value, 0.1, na.rm = T), `90-perc` = quantile(value, 0.9, na.rm = T)) %>%
    left_join(stats) %>%
    mutate(across(where(is.numeric), round, 3)) %>%
    mutate(across(where(is.numeric), signif, rounding)) %>%
    select(Station = stationname,
           Gemiddelde = mean,
           `90-perc`,
           `10-perc`,
           Trend = estimate,
           p = p.value) #%>%
  # arrange(-Gemiddelde)
}

plotTrends <- function(df, parname) {
  df %>% sf::st_drop_geometry() %>%
    filter(parametername == parname) %>%
    mutate(year = year(datetime), month = month(datetime)) %>%
    group_by(stationname, year) %>% 
    summarize(mean = mean(value, na.rm = T), `10-perc` = quantile(value, 0.1, na.rm = T), `90-perc` = quantile(value, 0.9, na.rm = T)) %>%
    select(Station = stationname,
           Jaar = year,
           Gemiddelde = mean,
           `90-perc`,
           `10-perc`) %>%
    arrange(-Gemiddelde) %>%
    ggplot(aes(Jaar, Gemiddelde)) +
    geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`), fill = "lightgrey") +
    geom_line() + geom_point(fill = "white", shape = 21) +
    geom_smooth(method = "lm", fill = "blue", alpha = 0.2) +
    facet_wrap(~Station) +
    theme_minimal() +
    ylab(parname)
}

plotMeanMap <- function(df, parname) {
  values = df %>% st_drop_geometry() %>%
    filter(parametername == parname) %>%
    group_by(stationname) %>% summarize(mean = mean(value, na.rm = T)) %>%
    select(mean) %>% unlist() %>% unname()
  
  pal <- colorNumeric(palette = "Blues",
                      domain = values
  )
  
  df %>% st_drop_geometry() %>%
    filter(parametername == parname) %>%
    group_by(stationname) %>% 
    summarize(mean = mean(value, na.rm = T), latitude = mean(latitude), longitude = mean(longitude)) %>%
    select(Station = stationname,
           Gemiddelde = mean,
           latitude,
           longitude
    ) %>% 
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers(fillColor = ~pal(Gemiddelde), fillOpacity = 1, stroke = F) %>%
    leaflet::addLegend("topright", pal, values, opacity = 1)
}