graphs
================
Michael Harper

# Load Data

``` r
df_all_extra <- read_csv(here("data/global/covid_data_global.csv"),
                         col_types = cols())
world_map <- sf::read_sf(here("data/global/covid_data_global_boundaries.geojson"))

df_all_spatial <- 
  df_all_extra %>%
  left_join(world_map)
```

    ## Joining, by = "region"

# Exploring contagion rates

Media attention has been focussing on several countries, primarly,
China, Italy, Iran, Spain. As I live in the UK, this will be compared.

``` r
# ---- Plot 1: cases with time
dataSelect <- 
  df_all_spatial %>% 
  filter(type == "cases") %>%
  filter(region %in% c("China", "Italy", "Spain", "UK", "Iran", "France")) %>%
  filter(value != 0)

dataLabels <- dataSelect %>%
  filter(date == max(date))

p <- ggplot(dataSelect, aes(x = date, y = casespermillion)) +
  geom_line(aes(colour = region)) +
  labs(x = "Date",
       y = "Cases / million",
       caption = "Data source: 2020 Johns Hopkins University\nGraph by Michael Harper 2020",
       title = "Cases of COVID-19 per million of population") +
  ggrepel::geom_text_repel(data = dataLabels, aes(label = region, y = casespermillion, colour = region), x = max(dataLabels$date), hjust = -.1, direction = "y", vjust = 0.5) +
  theme(aspect.ratio = 0.5,
        legend.position = "none") +
  scale_x_datetime(limits = c(min(dataSelect$date), max(dataSelect$date) + lubridate::ddays(6)), 
                   expand = c(0,0), breaks = "2 week", date_labels = " %d %B") +
  theme(plot.margin = unit(c(1,5,1,1), "lines")) 
p

gt <- ggplotGrob(p)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid::grid.draw(gt)
```

![](01_Graphs_files/figure-gfm/casesWithTime-1.png)<!-- -->

# Growth Rates

``` r
growthRates <- 
  df_all_spatial %>% 
  filter(type == "cases") %>%
  filter(region %in% c("China", "Italy", "Spain", "UK", "Iran")) %>%
  filter(daysSince200Outbreak >= 0)

growthLines <- 
  growthRates %>%
  group_by(region) %>%
  arrange(daysSince200Outbreak) %>%
  summarise(day = last(daysSince200Outbreak),
            value = last(value))


p <- ggplot(growthRates, aes(x = daysSince200Outbreak, y = value, colour = region)) +
  geom_line() +
  geom_segment(data = growthLines, aes(x = day, xend = Inf, y = value, yend = value),
               lineend = "round", linetype = "dashed", size = 0.4, alpha = 0.6) +
  geom_text(data = growthLines, aes(label = region, y = value, colour = region),
            x = max(growthRates$daysSince200Outbreak) + 3,
            hjust = -.1) +
  theme(plot.margin = unit(c(1,5,1,1), "lines"),
        legend.position = "none") +
  labs(x = "Days",
       y = "Number of Cases",
       title = "Growth in cases since outbreak",
       caption = "Day 0 counted as first day with 200 cases")
p

# We are playing around with the grob to allow the labels to be printed off the graph
gt <- ggplotGrob(p)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid::grid.draw(gt)
```

![Rates of growth in cases of COVID-19, measured in days after the 200
case was recorded](01_Graphs_files/figure-gfm/growthRates-1.png)

# Ranking countries

``` r
outbreaks <- 
  df_all_spatial %>%
  filter(type == "cases") %>%
  group_by(region) %>%
  top_n(n = 1, wt = casespermillion)

outbreaks_top <- 
  outbreaks %>%
  ungroup() %>%
  arrange(-casespermillion) %>%
  filter(!is.na(continent)) %>%
  top_n(n = 20, wt = casespermillion) %>%
  mutate(morethan200cases = value > 200)

p1 <- 
  outbreaks_top %>%
  filter(morethan200cases == TRUE) %>%
  ggplot(aes(x = reorder(region, casespermillion), y = casespermillion)) +
  geom_col(aes(fill = continent)) +
  coord_flip() +
  labs(subtitle = "Cases / million",
       y = "Cases Per Million",
       x = "Country",
       fill = "Continent") +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "top")


p2 <- 
  outbreaks_top %>%
  filter(morethan200cases == TRUE) %>%
  ggplot(aes(x = reorder(region, casespermillion), y = value)) +
  geom_col(aes(fill = continent)) +
  coord_flip() +
  labs(subtitle = "Total Cases",
       y = "Cases",
       x = "Country",
       fill = "Continent") +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "top",
        axis.text.y = element_blank())

# Plots combined using patchwork package
combined <- p1 + p2 & theme(legend.position = "right")
combined + plot_layout(guides = "collect") + patchwork::plot_annotation(title = "Comparing normalised rates vs total cases", caption = "Note: Only displaying countries with more than 200 reported cases", subtitle = "Although China still accounts for the vast majority of cases, the overall rates are lower \nthan many European countries")
```

![A comparison of the normalised ranking of countries against total
cases](01_Graphs_files/figure-gfm/outbreaks-1.png)