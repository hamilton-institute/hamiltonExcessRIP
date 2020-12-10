library(dplyr)
library(lubridate)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(flipTime)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(shinycssloaders)
library(sf)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(tigris)
library(spdplyr)
library("ggdendro")
library("reshape2")
library(scales)
library(shinyalert)
library(shinybusy)
rm(list = ls())


#Reading RIP dataset + Eircodes shapefiles:

load("RIP_rk_aggregated_data_merged_12Nov.RData")
eircodes = readOGR(dsn="eircodes", layer="eircode_polygons")

#Data preparation:
ref_level <- merged_rk_data %>%
  filter(Year < 2020 & Year >=2015 ) %>%
  ungroup() %>%
  group_by(Group,Date) %>%
  summarize(Monthly_Notices = sum(Monthly_Notices)) %>%
  mutate(DOY=yday(Date)) %>%
  group_by(Group,DOY) %>%
  mutate(Ref_Level = mean(Monthly_Notices),
         Prev_Max = max(Monthly_Notices))

df2020 <- merged_rk_data %>% filter(Year == 2020)
df_ref <- ref_level %>% ungroup() %>% filter(year(Date) == 2019) %>% select(Group,DOY, Ref_Level)

merged_df <- left_join(df2020, df_ref)
merged_df <- merged_df %>% mutate(value = round(100*(Monthly_Notices - Ref_Level)/Ref_Level)) #Mortality rate change

total <- merged_df %>% group_by(Date) %>%
  summarise(Monthly_Notices = sum(Monthly_Notices, na.rm = T), Ref_Level = sum(Ref_Level)) %>%
  mutate(value = round(100*(Monthly_Notices - Ref_Level)/Ref_Level))

get_about_text <- function() {
  HTML(paste0("<div style = 'color: black;'><p> This app is developed for tracking excess mortality in the ",
              "Republic of Ireland. The excess mortality (p value) calculation is done ",
              "following the method explained <a href='https://ourworldindata.org/excess-",
              "mortality-covid'> here</a>. <p> The data used by this app is scrapped from ",
              "the RIP.ie on a daily basis enabling the app to provide near real-time infor",
              "mation on excess mortality. However, please note that the data is not officia",
              "lly confirmed by authorities. <p> The code presented here has been written ",
              "by academics and not by professional coders. It may contain bugs or other ",
              "mistakes which we have not discovered yet. All the code for this app is ",
              "available in our <a href = 'https://github.com/hamilton-institute/covid19ir",
              "eland'>GitHub</a> repository which we encourage you to look at and improve.</p>" ))
}

ui <-     bs4Dash::bs4DashPage(
  sidebar_collapsed = TRUE,
  sidebar_mini = TRUE,
  body = bs4Dash::bs4DashBody(
    hamiltonThemes::use_bs4Dash_distill_theme(),
    br(),
    fluidRow(
      bs4Dash::bs4TabCard(
        width = 12,
        title = " ",
        id = "tabcard",
        closable = FALSE,
        collapsible = FALSE,
        bs4Dash::tabPanel(
          tabName = "Map",
          leaflet::leafletOutput("view") %>% hamiltonThemes::distill_load_spinner(),
          hr(),
          radioButtons("yscale", p("Select a region from the map and choose the comparison method to show each region's excess mortality time-series."),
                       choices = list("Percentage" = "percent","Absolute" = "exact"),inline=TRUE),
        ),
        bs4Dash::bs4TabPanel(
          tabName = "National Excess Mortality",
          plotly::plotlyOutput("plot3") %>% hamiltonThemes::distill_load_spinner(),
        ),
        bs4Dash::bs4TabPanel(
          tabName = "Heatmap by region",
          plotly::plotlyOutput("plot4", height = "720px") %>% hamiltonThemes::distill_load_spinner()
        ),
        bs4Dash::bs4TabPanel(
          tabName = "About",
          get_about_text()
        )
      )
    )
  ),
  footer = hamiltonThemes:::bs4dash_distill_footer()

) #End of UI


server <- function(input, output) {

  output$view <- renderLeaflet({
    df2 <- merged_df %>% group_by(Group) %>% filter(Date == max(Date))

    df2 <- geo_join(eircodes, df2,"Group", "Group")

    df2 <- df2 %>% dplyr::mutate(pop1 = case_when(as.character(df2$Group) != as.character(df2$Descriptor) ~ paste0("Excess: ",df2$value,"% at ", df2$Group, " including ",df2$Descriptor),
                                                 TRUE ~ paste0("Excess: ",df2$value,"% at ", df2$Group)),
                                 pop2 = case_when(as.character(df2$Group) != as.character(df2$Descriptor) ~ paste0(Monthly_Notices," at ", df2$Group, " including ",df2$Descriptor),
                                                  TRUE ~ paste0(Monthly_Notices," at ", df2$Group)))

    pal <- colorNumeric(palette = c("white","gray","darkred"), domain = df2$value)

    popup_sb <- df2$pop1

     leaflet() %>%
      addTiles() %>% setView(-7.5959, 53.5, zoom = 6) %>%
      addPolygons(data = df2, fillColor = ~pal(df2$value), layerId= ~Descriptor,
                  fillOpacity = 0.8,
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=popup_sb,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = df2$value, title = "Excess mortality %", opacity = 0.7,
                labFormat = labelFormat(suffix = " %")) %>%
      leaflet.extras::addResetMapButton() %>%
      leaflet.extras::addSearchOSM(options = searchOptions(collapsed = T,zoom = 9,hideMarkerOnCollapse = T, moveToLocation = FALSE,
                                                           autoCollapse =T))
  })

  observeEvent(input$view_shape_click,{#Plotting excess mortality plots for each region after clicking on the map

   output$plot2 <- renderPlotly({
      m = subset(eircodes, Descriptor == input$view_shape_click$id)

      df3 <- merged_rk_data %>% filter(Group == m$Group)

     # calculate reference levels:
       ref_level <- df3 %>% filter(Year < 2020 & Year >=2015 ) %>%
         ungroup() %>%
         group_by(Group,Date) %>%
         summarize(Monthly_Notices = sum(Monthly_Notices)) %>%
         mutate(DOY=yday(Date)) %>%
         group_by(Group,DOY) %>%
         mutate(Ref_Level = mean(Monthly_Notices),
                Prev_Max = max(Monthly_Notices))


        x <- eircodes$RoutingKey[which(eircodes$Group == m$Group)]
        x <- knitr::combine_words(x)

        plt1 <- ggplot()+
          geom_line(data = df3 ,
                    aes(x=Date,y=Monthly_Notices, linetype="2020"), color = "red")+
          geom_line(data = ref_level ,
                    aes(x=as.Date(DOY,origin="2020-01-01"),y=Prev_Max, linetype="Previous years max"), color ="blue") +
          geom_line(data = ref_level,
                    aes(x=as.Date(DOY,origin="2020-01-01"),y=Ref_Level, linetype="Previous years mean"), color ="darkblue") +
          facet_wrap(facets = vars(Group)) +
          ggtitle(paste0("Notices Posted in 2020 - Eircode: ", x)) +
          labs(x="",y="Monthly Notices") +
          theme(axis.text.x = element_text(angle = 90), legend.position = c(0.89, 0.85))  +
          scale_x_date(date_breaks = "1 month", date_labels = "%b",limits=c(as.Date("2020-01-01"),as.Date("2020-12-01"))) +
          labs(linetype = "") +
          scale_linetype_manual(values=c("solid", "dotted", "dashed")) + theme_bw()


       # Merging 2020 and one of the previous years (doesn't matter which one they have identical ref col)

       df2020 <- df3 %>% filter(Year == 2020)
       df_ref <- ref_level %>% ungroup() %>% filter(year(Date) == 2019) %>% select(DOY, Ref_Level)

       merged_df <- left_join(df2020, df_ref, "DOY")
       merged_df <- merged_df %>% mutate(value = round(100*(Monthly_Notices - Ref_Level)/Ref_Level)) #Mortality rate change

       p <- merged_df %>% ggplot(aes(Date, value)) + geom_line(color = "red") + ylab("Excess postings %") +
              geom_hline(yintercept = 0, linetype="dotted") + ggtitle(paste0("Excess postings at ", m$Group, " relative to 2015-2019 mean")) +theme_bw()

       plt2 <- ggplotly(p)

       if(input$yscale == "percent"){
         final_plot <- plt2
       } else {
         final_plot <- plt1
       }
       final_plot
     })
  }) #End of Observation

  observeEvent(input$view_shape_click,{

  showModal(modalDialog(
    title = "",
    size = "l",
    footer = actionButton("close", "Close"),
    plotlyOutput("plot2") %>% withSpinner(color="#FF4500") ))
  })

  observeEvent(input$close, { #Removing modal and erasing previous plot
    output$plot2 <- NULL
    removeModal()
  })


      #National excess morality plot:
       output$plot3 <- renderPlotly({

         national_plot <- total %>% ggplot(aes(Date, value)) + geom_line(color = "red") + ylab("Excess postings %") +
           geom_hline(yintercept = 0, linetype="dotted") + ggtitle("Excess mortality in Ireland")  + theme_bw()

         ggplotly(national_plot)

       })

        #Creating heatmap:
        output$plot4 <- renderPlotly({

          heat <- merged_df %>% select(Group, Month, Monthly_Notices, Ref_Level) %>% na.omit()
          heat_grouped <- heat %>% group_by(Group, Month) %>% summarise(Monthly_Notices = round(mean(Monthly_Notices)),
                                                                        Ref_Level = round(mean(Ref_Level))) %>%
            mutate(exc = round(100*(Monthly_Notices - Ref_Level)/Ref_Level))

          heat_wide <- dcast(heat_grouped, Month ~ Group, value.var = "exc")

          heat.scaled <- heat_wide


          #Run clustering
          heat.matrix <- as.matrix(heat.scaled[, -1])
          #rownames(heat.matrix) <- heat.scaled$variable
          heat.dendro <- as.dendrogram(hclust(d = dist(x = t(heat.matrix))))
          heat.order <- order.dendrogram(heat.dendro)

          heatmatrix<- as.data.frame(heat.matrix)
          heatmatrix <- heatmatrix[,heat.order]
          heatmatrix <- heatmatrix %>% mutate(Month = heat_wide$Month)
          heat.long <- melt(heatmatrix, id = "Month")
          heat.long$Month <- paste0(month.abb[heat.long$Month]," 2020")
          heat.long$Month <- as.POSIXct(AsDate(heat.long$Month))
          names(heat.long) <- c("Month", "Region", "Exc")
          heat.long$Excess <- paste0(heat.long$Exc, " %")

          heatmap.plot <- ggplot(data = heat.long, aes(x = Month, y = Region, Excess = Excess)) +
            geom_tile(aes(fill = Exc)) +
            labs(y = NULL, fill = "Excess %") +
            scale_x_datetime(labels = date_format("%b"), breaks = "1 month") +
            scale_fill_gradient2(limits=c(-100, 100), high = "firebrick3", low = "dodgerblue4", oob=squish) +
            theme(axis.text.y = element_text(size = 6), legend.position = "Top") + theme_bw()

          ggplotly(heatmap.plot, tooltip = c("Region","Excess"))
        })
}

# Run the application
shinyApp(ui = ui, server = server)

