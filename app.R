
# setup -------------------------------------------------------------------

library(DT)
library(rnaturalearth)
library(shiny)
library(wbstats)
library(ggrepel)
library(plotly)
library(mapview)
library(leaflet)
library(sf)
library(tidyverse)



# indicators --------------------------------------------------------------

indicators = c(# Electric power consumption (kWh per capita)
  "Electricity Consumption" = "EG.USE.ELEC.KH.PC",
  
  # Urban population (% of total population)
  "Urban Population" = "SP.URB.TOTL.IN.ZS",
  
  # Mobile cellular subscriptions (per 100 people)
  "Mobile Connectivity" = "IT.CEL.SETS.P2",
  
  # Individuals using the Internet (% of population)
  "Internet Connectivity" = "IT.NET.USER.ZS",
  
  # CO2 emissions (metric tons per capita)
  "CO2 Emmissions" = "EN.ATM.CO2E.PC",
  
  # Population, total
  "Population" = "SP.POP.TOTL",
  
  # # GDP (current US$)
  # "GDP" = "NY.GDP.MKTP.CD",
  
  # GDP per capita, PPP
  "GDP per capita PPP" = "NY.GDP.PCAP.PP.KD")


# Accessing data and adding shortnames


## Import all data based on selected indicators using wbstats
## Add useful indicator names 

# master_data <- 
#   wb_data(indicators, 
#           country = "all",
#           start_date = 2000, 
#           end_date = 2022, 
#           return_wide = FALSE) %>% 
#   left_join(., 
#             data.frame(short_name = names(indicators),
#                        indicator_id = unname(indicators)), 
#             by = join_by(indicator_id))


## Import data for countries based on selected indicators using wbstats
## Add useful indicator names 

country_data <- 
  wb_data(indicators, 
          country = "countries_only",
          start_date = 2000, 
          end_date = 2022, 
          return_wide = FALSE) %>% 
  left_join(., 
            data.frame(short_name = names(indicators),
                       indicator_id = unname(indicators)), 
            by = join_by(indicator_id))


### Set the factor levels of country names to order them alphabetically on charts

country_data$country = forcats::fct_rev(factor(country_data$country))



wb_geo <- 
  wb_cachelist$countries %>% 
  select(iso3c, longitude, latitude, region, income_level)


# Import shapes of countries from rnatrualearth

worldmapshapes <- 
  rnaturalearth::ne_countries(scale = 110, returnclass = "sf") %>% 
  filter(iso_a3_eh != "ATA") %>% 
  select(iso_a3_eh, geometry)


# ui ----------------------------------------------------------------------

ui <- 
  fluidPage(
    # theme = shinytheme("flatly"),
    
    fluidRow(
      column(8,
             offset = 2,
             br(),
             br(),
             br(),
             h1(align="center",
                "How have we changed?"))),
    
    fluidRow(
      column(8,
             offset = 2,
             br(),
             br(),
             p("It is diffcult to process scales of change. Only when we step back and look at it from the vantage point of yesterday, can we see the changes of today. There is clearly a distinction between change and progress as well. All change isn't progress but all progress is change. Here, we attempt to only navigate the waves of change, not necessarily qualifying it as progress or not. We attempt to navigate by riding on certain buckets of themes."),
             p("For instance in Saudi Arabia, at the beginning of this millenium, you would stand out in a crowd of even 100 people, if you possessed a mobile phone. Cut to 20 years late, and everyone in that crowd of 100 now has atleast one (or more) mobile phones. "),
             p("Select an indicator from the panel on the to swipe over the map of the world to get an overview of the countries that have experienced change between the last 20 years."),
             p("")
      )),
    
    fluidRow(
      sidebarPanel(
        strong("Themes"),
        tags$li("Electricity Consumption"),
        tags$li("Urban Population"),
        tags$li("Mobile Subscriptions"),
        tags$li("Internet users"),
        tags$li("CO2 emmissions"),
        tags$li("GDP per capita"),
        br(),
        wellPanel(
          style = "background-color: #c9ada7;",
          selectInput("map_indicator", 
                      label = h4("Select an indicator"), 
                      choices = unique(country_data$short_name)))),
      column(8, 
             # offset = 2,
      leafletOutput("gdpplot")
    )),
    
    br(),
    br(),
    br(),
    
    fluidRow(
      h2(align="center",
         "Who is making the most change?"),
      br(),
      column(4,
             # offset = 1, 
             h4(align="center",
                "Electric power consumption (kWh per capita)"),
             DT::dataTableOutput("elec_cons")),
      column(4, 
             h4(align="center",
                "Urban population (% of total population)"),
             DT::dataTableOutput("urban")),
      column(4,
             h4(align="center",
                "Mobile cellular subscriptions (per 100 people)"),
             DT::dataTableOutput("mobile"))),
    
    br(),
    br(),
    
    fluidRow(
      column(4,
             # offset = 1, 
             h4(align="center",
                "Individuals using the Internet (% of population)"),
             DT::dataTableOutput("internet")),
      column(4,
             h4(align="center",
                "CO2 emissions (metric tons per capita)"),
             DT::dataTableOutput("co2")),
      column(4,
             h4(align="center",
                "GDP per capita, PPP (const. 2017 int. $)"),
             DT::dataTableOutput("gdp_pp"))),
    
    br(),
    br(),
    br(),
    
    fluidRow(
      h2(align="center",
         "What does the change look like?"),
      br(),
      column(4, 
             offset = 2,
             wellPanel(
               style = "background-color: #c9ada7;",
               selectInput("select_indicator", 
                           label = h4("Select an indicator"), 
                           choices = unique(country_data$short_name)))),
      column(4, 
             wellPanel(
               style = "background-color: #c9ada7;",
               selectizeInput("select_country",
                              label = h4("Choose countries"),
                              choices = unique(country_data$country),
                              selected = c("China", "India", "United States"),
                              multiple = TRUE)
             ))),
    
    fluidRow(
      column(6, 
             h4("How much has changed?",
                align = "center"),
             plotOutput("dumbbell")),
      
      # br(),
      # br(),
      column(6,
             h4("How has it changed?",
                align = "center"),
             tabsetPanel(type = "tabs",
                         tabPanel("Plot",
                                  plotlyOutput("trend_line")),
                         tabPanel("Data",
                                  DT::dataTableOutput("trend_data")))
      ))
    
  )



# server ------------------------------------------------------------------

server <- 
  function(input, output) {
    
    ## swipemap ----------------------------------------------------------------
    
    output$gdpplot <- 
      renderLeaflet({
    filtered_mapdata <- 
      country_data %>% 
      # filter(short_name == "GDP per capita PPP") %>% 
      filter(short_name == input$map_indicator) %>% 
      drop_na(value) %>% 
      group_by(iso3c) %>% 
      filter(date == min(date) | date == max(date)) %>% 
      right_join(worldmapshapes,
                 ., 
                 by = join_by(iso_a3_eh == iso3c)) 
    
    
    mapviewOptions(basemaps.color.shuffle = TRUE,
                   homebutton = TRUE, 
                   basemaps = c("CartoDB.PositronNoLabels",
                                "CartoDB.VoyagerNoLabels"))
    
    map_before <- 
      mapview(
        filtered_mapdata %>% 
          group_by(iso_a3_eh) %>% 
          filter(date == min(date)), 
        zcol = "value",
        col.regions = RColorBrewer::brewer.pal(12, "Set3"),
        alpha.regions = .5,
        layer.name = "Initial level",
        popup = TRUE)
    

    
    map_after <- 
      mapview(
        filtered_mapdata %>% 
          group_by(iso_a3_eh) %>% 
          filter(date == max(date)), 
        zcol = "value",
        col.regions = RColorBrewer::brewer.pal(12, "Set3"),
        alpha.regions = .5,
        layer.name = "Latest level",
        popup = TRUE)
    
    
    combined_map <- (map_before | map_after)
    
    # cntr_crds <- c(mean(stat_sf_coordinates(filtered_mapdata)[, 1]),
    #                mean(stat_sf_coordinates(filtered_mapdata)[, 2]))
    
   
        combined_map@map
        # setView(cntr_crds[1], cntr_crds[2], zoom = 2)
      })
    

## leaderboard -------------------------------------------------------------

    changeboard <- 
      country_data %>%
      left_join(wb_geo, by = join_by("iso3c")) %>% 
      group_by(iso3c, short_name) %>% 
      drop_na(value) %>% 
      filter(date == min(date) | date == max(date)) %>% 
      group_by(iso3c, short_name) %>% 
      mutate(change = 
               round(last(value, order_by = date) - first(value, order_by = date), 2),
             time_period = 
               max(date) - min(date))
    
    output$elec_cons <- 
      DT::renderDataTable({
        DT::datatable(
          changeboard %>% 
          group_by(iso3c, short_name) %>% 
          filter(short_name == "Electricity Consumption" & date == max(date)) %>% 
          ungroup() %>% 
          slice_max(abs(change), n = 5) %>% 
          select(Country = country, 
                 Change = change), 
                 # `Time Period (yrs)` = time_period),
        options = list(
          paging = FALSE,
          dom = "t",
          ordering = FALSE,
          searching = FALSE))
      })
    
    output$urban <- 
      DT::renderDataTable({
        DT::datatable(
          changeboard %>% 
          group_by(iso3c, short_name) %>% 
          filter(short_name == "Urban Population" & date == max(date)) %>% 
          ungroup() %>% 
          slice_max(abs(change), n = 5) %>% 
          select(Country = country, 
                 Change = change), 
                 # `Time Period (yrs)` = time_period),
          options = list(
            paging = FALSE,
            dom = "t",
            ordering = FALSE,
            searching = FALSE
          ))
      })
    
    output$mobile <- 
      DT::renderDataTable({
        DT::datatable(
        changeboard %>% 
          group_by(iso3c, short_name) %>% 
          filter(short_name == "Mobile Connectivity" & date == max(date)) %>% 
          ungroup() %>% 
          slice_max(abs(change), n = 5) %>% 
          select(Country = country, 
                 Change = change), 
                 # `Time Period (yrs)` = time_period),
        options = list(
          paging = FALSE,
          dom = "t",
          ordering = FALSE,
          searching = FALSE
        ))
      })
    
    output$internet <- 
      DT::renderDataTable({
        DT::datatable(
        changeboard %>% 
          group_by(iso3c, short_name) %>% 
          filter(short_name == "Internet Connectivity" & date == max(date)) %>% 
          ungroup() %>% 
          slice_max(abs(change), n = 5) %>% 
          select(Country = country, 
                 Change = change), 
                 # `Time Period (yrs)` = time_period),
        options = list(
          paging = FALSE,
          dom = "t",
          ordering = FALSE,
          searching = FALSE
        ))
      })
    
    output$co2 <- 
      DT::renderDataTable({
        DT::datatable(
        changeboard %>% 
          group_by(iso3c, short_name) %>% 
          filter(short_name == "CO2 Emmissions" & date == max(date)) %>% 
          ungroup() %>% 
          slice_max(abs(change), n = 5) %>% 
          select(Country = country, 
                 Change = change), 
                 # `Time Period (yrs)` = time_period),
        options = list(
          paging = FALSE,
          dom = "t",
          ordering = FALSE,
          searching = FALSE
        ))
      })

    output$gdp_pp <- 
      DT::renderDataTable({
        DT::datatable(
        changeboard %>% 
          group_by(iso3c, short_name) %>% 
          filter(short_name == "GDP per capita PPP" & date == max(date)) %>% 
          ungroup() %>% 
          slice_max(abs(change), n = 5) %>% 
          select(Country = country, 
                 Change = change), 
                 # `Time Period (yrs)` = time_period),
        options = list(
          paging = FALSE,
          dom = "t",
          ordering = FALSE,
          searching = FALSE
        ))
      })
    
    
    ## plot:dumbbell -----------------------------------------------------------
    
    output$dumbbell <- renderPlot({
      country_data %>% 
        filter(short_name == input$select_indicator & 
                 country %in% input$select_country) %>% 
        drop_na(value) %>% 
        group_by(iso3c) %>% 
        filter(date == min(date) | date == max(date)) %>% 
        ggplot(aes(x = value, 
                   y = country)) +
        geom_line(aes(group = country), 
                  color = "#E7E7E7", 
                  linewidth=3.5) +
        geom_point(aes(color = date), 
                   size = 3) +
        geom_text_repel(aes(label = date, 
                            color = date),
                        size = 3.25,
                        vjust = 0, 
                        nudge_y = 0.1) +
        
        theme_minimal() +
        theme(legend.position = "none",
              axis.text = element_text(size = 11),
              axis.text.y = element_text(color="black"),
              axis.text.x = element_text(color="#989898"),
              axis.title = element_blank(),
              axis.line.x = element_line(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) #+
      # scale_x_continuous(labels = scales::percent_format(scale = 1))
      # scale_color_continuous(values=c("#436685", "#BF2F24"))
    })
    
    
    ## plot:trend line ---------------------------------------------------------
    
    output$trend_line <- 
      renderPlotly({
        ggplotly(
          country_data %>% 
            filter(short_name == input$select_indicator & 
                     country %in% input$select_country) %>% 
            ggplot(aes(x = date, y = value, group = country)) +
            geom_line(aes(color = country), 
                      alpha = 0.5) +
            geom_point(aes(color = country), 
                       size = 1) +
            theme_minimal() +
            theme(legend.position = "none",
                  axis.text.y = element_text(color="black"),
                  axis.text.x = element_text(color="#989898"),
                  axis.title = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank()) +
            paletteer::scale_colour_paletteer_d("fishualize::Antennarius_commerson")) %>%
          config(displayModeBar = FALSE)
      })
    
    
    output$trend_data <- 
      DT::renderDataTable({
        country_data %>% 
          filter(short_name == input$select_indicator & 
                   country %in% input$select_country) %>% 
          drop_na(value) %>% 
          select(Indicator = indicator,
                 Country = country,
                 Year = date,
                 Value = value,
                 `Last Update` = last_updated)
      })
    
    
    
    
  }
# shinyapp ----------------------------------------------------------------

shinyApp(ui = ui, server = server)
