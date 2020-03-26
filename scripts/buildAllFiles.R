# Build Files
library(here)

rootDir <- here()

# List all the files
files <- list.files(rootDir, pattern = "*.Rmd", recursive = T) %>% rev()

# Updates all the files
lapply(files, rmarkdown::render)
