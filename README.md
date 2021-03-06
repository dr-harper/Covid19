
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Covid-19 Analysis and Website

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
<img src="https://img.shields.io/github/last-commit/dr-harper/Covid19.svg" />
<img src="https://img.shields.io/github/stars/dr-harper/Covid19.svg" />

![](figures/animatedMap.gif)

This repository contains code used to clean data, analyse and visualise
the Covid-19 outbreak. This data is updated daily and served to [the
website](https://dr-harper.github.io/Covid19/). This GitHub repository
is available for people to browse the code and use to their own
interest.

**View all the data at the website:**
<https://dr-harper.github.io/Covid19/>

# Why I made this

There are a lot of dashboards out there, but I ultimately feel that
these can make it hard to interpret the data. Without context, it can be
difficult to understand whether a graph which is still going upwards is
good or bad news. This repository tries to pair the graphics with some
context, and make it easy for people to get the graphs they need to
understand the data.

# Repository Structure

For those interested in seeing more:

  - `data`: contains the cleaned data, along with the scripts used to
    load the data
  - `reports`: template files which are used to run the analysis through
  - `scripts`: files used to automatically update files daily

The cleaned data outputs are available in the
[data/global](https://github.com/dr-harper/Covid19/tree/master/data/global)
subdirectory. This is from the [John
Hopkins](https://github.com/CSSEGISandData/COVID-19) Covid-19 database.

# Plots

If you are interested in any of the plots, the code to make them are
available in the subdirectory `graphs`:

``` r
list.files("graphs/", pattern = ".Rmd")
```

    ##  [1] "comparisonLogLinear.Rmd"    "dailyChange.Rmd"           
    ##  [3] "dailyDeaths.Rmd"            "deathRateFacet.Rmd"        
    ##  [5] "linearGrowthGraphs.Rmd"     "logarithmicGrowthGraph.Rmd"
    ##  [7] "mapAnimatedCoverage.Rmd"    "mapGlobalCases.Rmd"        
    ##  [9] "rateofchange.Rmd"           "totalDeaths.Rmd"
