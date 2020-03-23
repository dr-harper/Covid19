Visualising Covid-19 With Animated, Static and Interactive Maps
================
Michael Harper

# About

I have reflecting on the unprecedented week in the UK. Even this time
last week, the country seemed fairly laid back as government advice was
largely to continue with business as usual. It is all very different to
now, with schools set to close on Monday and most people either working
from home or not working at all.

This blog post provides details of anaylsis and data viusalisation to
assess the spread of virus, giving three example maps which I have
produced. Like usual, it is aimed at a technical audience of data
visualisation people who want to understand how to make R graphs\!
Hopefully it provides an example of how you may be able to use the
**gganimate** \[@R-gganimate\] package to produce data visualisations.

# Setup

The following packages are used. Note, we are using the **ggthemr**
\[@R-ggthemr\] which is not avaialable on CRAN, so you will need to
install that if you haven’t already got it.

``` r
# Load packages
library(readr)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✓ ggplot2 3.3.0     ✓ purrr   0.3.3
    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.5
    ## ✓ tidyr   0.8.3     ✓ stringr 1.4.0
    ## ✓ ggplot2 3.3.0     ✓ forcats 0.4.0

    ## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(sf)
```

    ## Linking to GEOS 3.7.2, GDAL 2.4.2, PROJ 5.2.0

``` r
library(maps)
```

    ## 
    ## Attaching package: 'maps'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     map

``` r
library(gganimate)
library(ggthemr)
library(magick) # For combining gifs
```

    ## Linking to ImageMagick 6.9.9.39
    ## Enabled features: cairo, fontconfig, freetype, lcms, pango, rsvg, webp
    ## Disabled features: fftw, ghostscript, x11

``` r
library(patchwork)
library(here)
```

    ## here() starts at /Users/michaelharper/Documents/GitHub/Covid19analysis

``` r
# devtools::install_github('cttobin/ggthemr')
ggthemr("light")
```

# Data Loading and Preparation

As in my [previous analysis](https://mikeyharper.uk/Covid-19-march-17/),
I’ll be using the John hopkins dataset.As a reminder, the data can be
accessed through the GitHub [repository
here](https://github.com/CSSEGISandData/Covid-19), and they also offer a
great dashboard for visualising the data
[here](https://coronavirus.jhu.edu/map.html).

# Producing the plots

There are two components to the plot:

1.  The map itself
2.  A timeline bar which shows the progress along the bottom

Using the **gganimate** package, you need to produce these two
separately then merge the results. First we will prep the data for the
plot and then produce the base ggplot which we will animate afterwards.
Note, there are a few workarounds in the following cocde. First I am
finding the earlier date for each region that there was a a reported
case:

``` r
# Extracts the first cases
df_firstcase <- 
  df_all_spatial %>%
  filter(type == "cases") %>%
  group_by(region) %>%
  filter(value > 0) %>%
  filter(date == min(date, na.rm = T)) %>%
  filter(!is.na(continent)) %>%
  ungroup()
```

Next we convert the spatial data from a spatial feature into a slightly
old-fashioned format for doing spatial data in ggplot using the
`fortify` function. I would typically use the `geom_sf` option but this
is not yet fully supported by gganimate.

``` r
# Convert geometry to format to allow to work with gganimate
# gganiamte currently doesn't support sf
df_firstcase_sp <- 
  df_firstcase %>%
  as("Spatial")

d <- merge(fortify(df_firstcase_sp), as.data.frame(df_firstcase_sp), by.x="id", by.y=0)
```

    ## Regions defined for each Polygons

I want the graph to display a frame for every date, even if there is no
change in the countries which are reporting infection. I therefore
created a placeholder dataframe which had the same format as the spatial
data above, but only gave a date value. This will trick **gganimate**
into thinking there is data to display for every slide.

``` r
# Create a placeholder dataframe which has every date in a sequence
# This is used to force gganimate to display every single day even if there is no change
# in the data
allDays <-seq(from = min(d$date), to = max(d$date), by = lubridate::ddays(1))

allDaysDf <- data.frame("id" = NA,
                        "long" = NA,
                        "lat" = NA,
                        "order" = NA,
                        "hole" = NA,
                        "piece"  = NA,
                        "group" = "extra",
                        "region" = NA,
                        "continent" = NA,
                        "date" = allDays,
                        "value" = NA,
                        "type" = NA,
                        "pop" = NA,
                        "casespermillion" = NA)

f <- bind_rows(d, allDaysDf)
```

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector, coercing
    ## into character vector
    
    ## Warning in bind_rows_(x, .id): binding character and factor vector, coercing
    ## into character vector

Finally I will build the ggplot. First I specify just the raw
components, with nothing different for the animation yet:

``` r
# Make the plot
plot_map <- 
  ggplot(d) +
  geom_sf(data = world_map, fill = "grey90", colour = "#dadada") +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "#3a6589") +
  labs(title = "Countries with reported Covid-19 Cases") +
  theme(plot.caption = element_text(size = rel(0.5)),
        plot.background = element_rect(fill = '#f6f1eb', colour = NA),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.background = element_blank(),
        title = element_text(colour = '#706f6f', size = rel(2), hjust = 0.5),
        plot.title = element_text(colour = '#706f6f', size = rel(1.5), hjust = 0.5),
        legend.position = "none")
```

If you are new to gganimate, it offers a fairly user-friendly way of
animating plots. We can take our base ggplot object above and add
`transition_states` which will progressively go through the dates and
show data for each date. I had initially wanted to use
`transition_reveal`, but I couldn’t find a way to make these smoothly
animate data as they were added to the graph as it doesn’t seem to
accept any of the transition aes arguments. As a slight work around to
make `transition_states` work, I used the `shadow_mark` to make the data
remain on the map:

``` r
nframesAnim <- length(allDays) * 2
durationAnim <- 15

plot_anim <- plot_map + 
  transition_states(date, wrap = F) +
  enter_recolor(fill = "#f0f5f9") +
  shadow_mark(past = T, alpha = 1, fill = "#3a6589")

# Render the map
gif1 <- animate(plot = plot_anim,
                height = 768,
                width =1800,
                nframes = nframesAnim,
                duration = durationAnim,
                end_pause = 10)
```

We will also produce the timeline below the plot:

``` r
# Select first and last date to interpolate between
# For some reason this is smoother than letting gganimate do every single point
g <- allDaysDf[c(1,nrow(allDaysDf)),]

plot_timeline <- 
  ggplot(g, aes(x = date, y = 1)) + 
  geom_point(aes(group = 1L), size = 5) + 
  scale_x_datetime(date_labels = "%b %d", date_breaks = "1 week") +
  labs(caption = "Data used from  https://github.com/CSSEGISandData/Covid-19 2020 Johns Hopkins University \nData Visulation by Michael Harper 2020 www.mikeyharper.uk") +
  theme(aspect.ratio = 0.025,
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = rel(3)), 
        axis.title = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(size = 2, linetype = "solid"),
        plot.background = element_rect(fill = '#f6f1eb', colour = "#f6f1eb"),
        plot.margin = margin(1, 2, 0, 2, "cm"),
        panel.background = element_rect(fill = '#e3dfda', colour = NA))

gif2 <- animate(
  plot_timeline +
    transition_states(date,
                      transition_length = 1,
                      state_length = 0,
                      wrap = F),
  width =1800, height = 130, nframes = nframesAnim, duration = durationAnim, end_pause = 10, rewind = F
)
gif2
```

![](02_Map_files/figure-gfm/unnamed-chunk-6-1.gif)<!-- -->

**gganimate** does not natively support plotting multiple plots
together, however it is very easy to achieve this using the **magick**
package. A demo is given in the [wiki
here](https://github.com/thomasp85/gganimate/wiki/Animation-Composition).

``` r
a_mgif <- image_read(gif1)
b_mgif <- image_read(gif2)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = T)
for(i in 2:nframesAnim){
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = T)
  new_gif <- c(new_gif, combined)
}
```

We can visualise the final gif below\!

![Finalised plot showing the change in infection
rates](02_Map_files/figure-gfm/unnamed-chunk-8-1.gif)

# Today Cases

As a second data exploration, I just wanted to produce a static map of
the current death totals per country, and then to highlight the top 10
countries. The following code creates Figure @ref(fig:deathsToday). In
these plots, I am using the **patchwork** \[@R-patchwork\] package to
combine the two ggplot objects into a single plot:

``` r
# Select the current day
deathsToday <- 
  df_all_spatial %>%
  filter(type == "deaths") %>%
  group_by(region) %>%
  filter(date == max(date, na.rm = T)) %>%
  ungroup() %>%
  filter(value > 0)

# Make colour scale
labels <- c("0 - 10", "10 - 100", "100-500", "500-1000", "1000+")
deathsToday$cut <- cut(deathsToday$value, c(0,10,100,500,1000,Inf), labels = labels)

# Globabl
p1 <- deathsToday %>%
  ggplot() +
  geom_sf(data = world_map, fill = "grey90", colour = "white", size = 0.1) +
  geom_sf(aes(fill = cut), colour = "black", size = 0.1) +
  scale_fill_manual(values = c("#F2DFDC", "#E5C0B9", "#D8A197", "#CB8174", "#BE6251", "#B2432F")) +
  labs(fill = "Deaths") +
  theme(plot.caption = element_text(size = rel(0.5)),
        panel.background = element_rect(fill = "#f6f1eb"),
        plot.background = element_rect(colour = "#f6f1eb"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.1, 0.2),
        legend.background = element_blank(),
        panel.grid = element_blank())

p2 <- 
  deathsToday %>%
  top_n(n = 10, wt = value) %>%
  ggplot(aes(x = reorder(region, value), y = value, fill = cut)) +
  geom_col(colour = "#706f6f") +
  scale_fill_manual(values = c("#F2DFDC", "#E5C0B9", "#D8A197", "#CB8174", "#BE6251", "#B2432F")) +
  coord_flip() +
  labs(y = "Deaths",
       x = "Country",
       fill = "Continent",
       caption = "Data used from  https://github.com/CSSEGISandData/Covid-19 2020 Johns Hopkins University") +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        plot.background = element_rect(colour = "#f6f1eb"),
        aspect.ratio = 1.5)


p1 + p2 + plot_layout(widths = c(3.5, 1)) +
  plot_annotation(title = glue::glue("Covid-19 Deaths as of {date}", date = format(max(deathsToday$date), "%d %B")),
                  subtitle = glue::glue("Total deaths recorded: {deaths}", deaths = sum(deathsToday$value, na.rm = T))) & theme(plot.background = element_rect(fill = "#f6f1eb"))
```

![A summary of deaths per
country](02_Map_files/figure-gfm/deathsToday-1.png)
