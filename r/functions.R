# functions to be used in the eerstelijnsrapportage
require(tidyverse)

# nog te maken functies
# filternutriendata
#   - if value < detection limiet, dan value <- 0.* detectielimiet
#   - Completteer data met alle combinaties van tijdstip, locatie en parameter
#   - Bereken schatting van ontbrekende data per seizoen
#   - 



# Make table with statistics
statTable <- function(df, parname, rounding, meanorder = "decreasing") {
  
  stats <- df %>% #sf::st_drop_geometry() %>%
    filter(parametername == parname) %>%
    mutate(year = year(datetime), month = month(datetime)) %>%
    # group_by(stationname, year, month) %>% 
    # summarize(monthlymean = mean(value)) %>%
    group_by(stationname, year) %>% 
    summarize(yearlymedian = median(value)) %>%
    group_by(stationname) %>%
    do(broom::tidy(lm(yearlymedian ~ year, data = .))) %>% 
    filter(term == "year")
  
  df %>% #sf::st_drop_geometry() %>%
    filter(parametername == parname) %>% 
    mutate(year = year(datetime), month = month(datetime)) %>%
    group_by(stationname) %>% summarize(median = median(value, na.rm = T), `10-perc` = quantile(value, 0.1, na.rm = T), `90-perc` = quantile(value, 0.9, na.rm = T)) %>%
    left_join(stats) %>%
    mutate(across(where(is.numeric), round, 3)) %>%
    mutate(across(where(is.numeric), signif, rounding)) %>%
    select(Station = stationname,
           Mediaan = median,
           `90-perc`,
           `10-perc`,
           Trend = estimate,
           p = p.value) #%>%
  # arrange(-Gemiddelde)
}

# Plot trends of nutrients
plotTrends <- function(df, parname, sf = T) {
if(sf) df <- df %>% #st_drop_geometry()
  df %>%
    filter(parametername == parname) %>%
    mutate(year = year(datetime), month = month(datetime)) %>%
    group_by(stationname, year) %>% 
    summarize(median = median(value, na.rm = T), `10-perc` = quantile(value, 0.1, na.rm = T), `90-perc` = quantile(value, 0.9, na.rm = T)) %>%
    select(Station = stationname,
           Jaar = year,
           Mediaan = median,
           `90-perc`,
           `10-perc`) %>%
    arrange(-Mediaan) %>%
    ggplot(aes(Jaar, Mediaan)) +
    geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`), fill = "lightgrey") +
    geom_line() + geom_point(fill = "white", shape = 21) +
    geom_smooth(method = "lm", fill = "blue", alpha = 0.2) +
    facet_wrap(~Station) +
    theme_minimal() +
    ylab(parname) +
    coord_cartesian(ylim = c(0,NA))
}

plotTrendsSeizoen <- function(df, parname, sf = T) {
  if(sf) df <- sf %>% #st_drop_geometry()
  df %>%
    filter(parametername == parname) %>%
    mutate(year = year(datetime), month = month(datetime)) %>%
    mutate(seizoen = case_when(
      month %in% c(4:9) ~ "zomer",
      !month %in% c(4:9) ~ "winter"
    )) %>%
    mutate(seizoen = factor(seizoen, levels = c("zomer", "winter"))) %>%
    group_by(stationname, year, seizoen) %>% 
    summarize(median = median(value, na.rm = T), `10-perc` = quantile(value, 0.1, na.rm = T), `90-perc` = quantile(value, 0.9, na.rm = T)) %>%
    select(Station = stationname,
           Jaar = year,
           Mediaan = median,
           `90-perc`,
           `10-perc`,
           seizoen) %>%
    arrange(-Mediaan) %>%
    ggplot(aes(Jaar, Mediaan)) +
    geom_ribbon(aes(ymin = `10-perc`, ymax = `90-perc`, fill = seizoen), alpha = 0.4) +
    geom_line(aes(color = seizoen)) + geom_point(aes(color = seizoen), fill = "white", shape = 21) +
    # geom_smooth(method = "lm", fill = "blue", alpha = 0.2) +
    facet_wrap(~Station) +
    theme_minimal() +
    ylab(parname) +
    coord_cartesian(ylim = c(0,NA))
}


plotMeanMap <- function(df, parname) {
  values = df %>% #st_drop_geometry() %>%
    filter(parametername == parname) %>%
    group_by(stationname) %>% summarize(mean = mean(value, na.rm = T)) %>%
    select(mean) %>% unlist() %>% unname()
  
  pal <- colorNumeric(palette = "Blues",
                      domain = values
  )
  
  df %>% #st_drop_geometry() %>%
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
