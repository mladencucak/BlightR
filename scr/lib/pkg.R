list.of.packages <-
  c(
    "tidyverse",
    "readxl",
    "broom", 
    "data.table",
    "minpack.lm",
    "knitr",
    "zoo",
    "egg",
    "imputeTS",
    "ggthemes",
    "rcompanion",
    "mgsub",
    "R.utils",
    "here",
    "stringr",
    "pander",
    "leaflet",
    "ggridges",
    "lubridate",
    "RColorBrewer",
    "plotly",
    "foreach",
    "pbapply",
    "parallel",
    "unikn",
    "pracma",
    "Metrics"
  )

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

#Download packages that are not already present in the library
if (length(new.packages))
  install.packages(new.packages)

if (length(new.packages))
  install.packages(new.packages, repos = c(CRAN="https://cran.r-project.org/"))


if ("reconPlots" %in% installed.packages() == FALSE) devtools::install_github("andrewheiss/reconPlots")

packages_load <-
  lapply(list.of.packages, require, character.only = TRUE)

#Print warning if there is a problem with installing/loading some of packages
if (any(as.numeric(packages_load) == 0)) {
  warning(paste("Package/s", paste(list.of.packages[packages_load != TRUE]), "not loaded!"))
} else {
  print("All packages were successfully loaded.")
}

rm(packages_load, list.of.packages, new.packages)
                                                                      
