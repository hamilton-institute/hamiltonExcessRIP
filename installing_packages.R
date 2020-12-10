install.packages(
  c("dplyr", "lubridate", "tidyverse", "reshape2", "ggplot2", "shiny",
    "shinyBS", "shinyWidgets", "shinythemes", "plotly", "shinycssloaders",
    "rgdal", "leaflet", "leaflet.extras", "tigris", "spdplyr",
    "ggdendro", "scales", "shinyalert", "shinybusy", "RcppRoll",
    "zoo", "readxl", "Jmisc", "cowplot", "survival", "reticulate", "devtools",
    "usethis"
  ),
  repos = "https://packagemanager.rstudio.com/all/__linux__/bionic/latest"
)

install.packages(c("sf", "rsconnect"))

devtools::install_github('Displayr/flipTime')
