args <- commandArgs(trailingOnly = TRUE)

usethis::ui_todo("Loading packages...")

suppressPackageStartupMessages({
  library(RcppRoll)
  library(lubridate)
  library(zoo)
  library(readxl)
  library(dplyr)
  library(lubridate)
  library(Jmisc)
  library(tidyverse)
  library(reshape2)
  library(ggplot2)
  library(cowplot)
  library(scales)
  library(shiny)
  library(shinyWidgets)
  library(plotly)
  library("survival")
  library(shinycssloaders)
  library(sf)
  library(reticulate)
  library(stringr)
})


usethis::ui_todo("Scraping data...")

suppressMessages({
  py_run_file("read_RIP_daily_py.py")
})

usethis::ui_todo("Parsing files...")

suppressMessages({
  # 1. load new RIP data from Pádraig
  data_path <- file.path(getwd(),"")  # path to the data
  files <- dir(pattern = "*.tsv") # get file names

  all_rip_data <- files %>%
    map(~ read_delim(file.path(data_path, .),"\t", escape_double = FALSE, trim_ws = TRUE)) %>%
    reduce(rbind) %>%
    select(-all_addresses) %>%
    rename(Date = "%date",Town = town, County = county) %>%
    filter(! County %in% c("Fermanagh","Armagh","Tyrone","Derry","Antrim","Down")) %>%
    separate(Date,c("Year","Month","Day"),sep="-") %>%
    mutate(Year = as.numeric(Year),
           Month = as.numeric(Month),
           Day = as.numeric(Day),
           Date = as.Date(paste0(Year,"/",Month,"/",Day))) %>%
    filter(!is.na(Date))


  strt = min(all_rip_data$Date)
  endd = max(all_rip_data$Date)

  load("rk_groupings.Rdata")

  RIP_Towns <- all_rip_data %>%
    filter(!is.na(Town)) %>%
    group_by(Town,Date) %>%
    tally(name = "Notices_Posted") %>%
    complete(Date = seq.Date(strt, endd, by="day"),fill = list(Notices_Posted = 0)) %>%
    mutate(Month = month(Date),
           DOY = yday(Date),
           Year = year(Date),
           Monthly_Notices = rollsumr(Notices_Posted, 28, fill=NA))

  locations <- read_csv("rip_town_counties_google_geocoded.csv") %>%
    filter(!is.na(lon)) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs=29903)

  town_in_rk <- st_join(locations, rk_grouped, join = st_within)

  town_in_rk_count <- RIP_Towns %>%
    ungroup() %>%
    left_join(town_in_rk, by = c("Town"="town")) %>%
    ungroup() %>%
    group_by(Group,Date) %>%
    summarize(Notices_Posted=sum(Notices_Posted)) %>%
    mutate(Month = month(Date),
           DOY = yday(Date),
           Year = year(Date),
           Monthly_Notices = rollsumr(Notices_Posted, 28, fill=NA),
           Monthly_Proportion = Monthly_Notices/mean(Monthly_Notices,na.rm = TRUE)) %>%
    ungroup()
})


usethis::ui_todo("Saving new data...")
# merged_rk_data = RIP_rk_aggregated_data_merged_7Sept.RData
merged_rk_data <- town_in_rk_count
save(merged_rk_data,file = "RIP_rk_aggregated_data_merged_12Nov.RData")


# Deploying the app

usethis::ui_todo("Deploying the app to shinyapps.io...")

rsconnect::setAccountInfo(
  name = 'apmuhamilton',
  token = args[1],
  secret= args[2]
)

files <- list.files('.')
files <- files[!str_detect(files, ".tsv$")]

rsconnect::deployApp(
  appFiles = files,
  appName = 'hamiltonExcessRIP',
  forceUpdate = TRUE,
  account = 'apmuhamilton',
  logLevel = "quiet"
)

