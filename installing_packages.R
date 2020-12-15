install.packages(
  c("dplyr", "lubridate", "tidyverse", "reshape2", "ggplot2", "shiny",
    "shinyBS", "shinyWidgets", "shinythemes", "plotly", "shinycssloaders",
    "rgdal", "leaflet", "leaflet.extras", "tigris", "spdplyr",
    "ggdendro", "scales", "shinyalert", "shinybusy", "RcppRoll",
    "zoo", "readxl", "Jmisc", "cowplot", "survival", "reticulate", "devtools",
    "bs4Dash"
  ),
  repos = "https://packagemanager.rstudio.com/all/__linux__/bionic/latest"
)

install.packages('https://cran.r-project.org/src/contrib/Archive/usethis/usethis_1.6.0.tar.gz', repos=NULL, type='source')
install.packages(c("sf", "rsconnect"))

devtools::install_github('Displayr/flipTime')
devtools::install_github('hamilton-institute/hamiltonThemes')
