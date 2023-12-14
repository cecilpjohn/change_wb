
# setup -------------------------------------------------------------------

library(DT)
library(rnaturalearth)
library(shiny)
library(wbstats)
library(ggrepel)
library(plotly)
library(mapview)
library(leaflet)
library(leaflet.extras2)
library(sf)
library(fresh)
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


## Import country background information from wbstats
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
  fluidPage(style = "background-color: #ebebeb;",
            tags$script(src = "https://kit.fontawesome.com/f377e22697.js"),
    # theme = shinytheme("flatly"),
    
    use_googlefont("Titillium Web"),
    use_theme(create_theme(
      theme = "default",
      bs_vars_font(
        family_sans_serif = "'Titillium Web', sans-serif"
      )
    )),
  
    fluidRow(
      column(8,
             offset = 2,
             br(),
             br(),
             br(),
             em(h1(align="center",
                "How has the world changed?")))),
    
    fluidRow(
      column(8,
             offset = 2,
             br(),
             br(),
             p("It is diffcult to process scales of change. Only when we step back and look at it from the vantage point of yesterday, can we see the changes of today. There is clearly a distinction between change and progress as well. All change isn't progress but all progress is change. Here, we attempt to only navigate the waves of change, not necessarily qualifying it as progress or not. The attempt is to navigate by riding on certain buckets of themes."),
             p("Take for instance Saudi Arabia. At the beginning of this millenium, you would stand out in a crowd of even 100 people if you possessed a mobile phone. Cut to 20 years late, and everyone in that crowd of 100 now has atleast one (or more) mobile phones. These changes indicate underlying changes in patterns of production, consumption and quality of life."),
             p("Select an indicator from the panel on the left to swipe over the map of the world to get an overview of the countries that have experienced change between the last 20 years. All data is retrieved from figures published by the World Bank and reflect values from 2000-2022, subject to its availability. The colors have been set to communicate a change rather than an increase/decrease in the value."),
             br(),
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
                      label = h4("Select an indicator to view it on the map"), 
                      choices = unique(country_data$short_name)))),
      column(8, 
             # offset = 2,
      leafletOutput("gdpplot")
    )),
    
    br(),
    br(),
    br(),
    
    fluidRow(
      em(h2(align="center",
         "Who has seen the most change?")),
      br(),
      br(),
      br(),
      column(4,
             # offset = 1, 
             tags$i(class="fa-solid fa-plug", 
                    style ="font-size:3rem;"),
             style = "text-align: center;",
             strong(style = "font-size:2rem;", 
                    "Electric power consumption (kWh per capita)"),
             # h4(align="center",
             #    "Electric power consumption (kWh per capita)"),
             DT::dataTableOutput("elec_cons")),
      column(4, 
             tags$i(class="fa-solid fa-city", 
                    style ="font-size:3rem;"),
             style = "text-align: center;",
             strong(style = "font-size:2rem;", 
                    "Urban population (% of total population)"),
             # h4(align="center",
             #    "Urban population (% of total population)"),
             DT::dataTableOutput("urban")),
      column(4,
             tags$i(class="fa-solid fa-mobile-screen-button", 
                    style ="font-size:3rem;"),
             style = "text-align: center;",
             strong(style = "font-size:2rem;", 
                    "Mobile cellular subscriptions (per 100 people)"),
             # h4(align="center",
             #    "Mobile cellular subscriptions (per 100 people)"),
             DT::dataTableOutput("mobile"))),
    
    br(),
    br(),
    
    fluidRow(
      column(4,
             # offset = 1, 
             tags$i(class="fa-solid fa-wifi", 
                    style ="font-size:3rem;"),
             style = "text-align: center;",
             strong(style = "font-size:2rem;", 
                    "Internet users (% of population)"),
             # h4(align="center",
             #    "Individuals using the Internet (% of population)"),
             DT::dataTableOutput("internet")),
      column(4,
             tags$i(class="fa-solid fa-smog", 
                    style ="font-size:3rem;"),
             style = "text-align: center;",
             strong(style = "font-size:2rem;", 
                    "CO2 emissions (metric tons per capita)"),
             # h4(align="center",
             #    "CO2 emissions (metric tons per capita)"),
             DT::dataTableOutput("co2")),
      column(4,
             tags$i(class="fa-solid fa-dollar-sign", 
                    style ="font-size:3rem;"),
             style = "text-align: center;",
             strong(style = "font-size:2rem;", 
                    "GDP per capita, PPP (const. 2017 int. $)"),
             # h4(align="center",
             #    "GDP per capita, PPP (const. 2017 int. $)"),
             DT::dataTableOutput("gdp_pp"))),
    
    br(),
    br(),
    br(),
    fluidRow(
      column(8,
             offset = 2,
             p("This leaderboard of countries make one aspect very clear - that there is diversity at the top! The tables above indicate absolute change in the respective indicators, and the countries are ranked based on the magnitude of change. These countries would have experienced a swing in either a postivie or negative direction. While we look at the tables, the trend is mainly towards all countries moving in the positive direction, CO2 emmissions and GDP per capita differ."),
             br(),
             p("In what is better for the planet, countries have shown greater reductions in their emmissions as compared to the quantum in increases. But interestingly, second-ranked Brunei Darussalam moves opposite to the direction of first-ranked Qatar. "),
             br(),
             br(),
             br()
      )
    ),
    
    fluidRow(
      em(h2(align="center",
         "What does change look like?"))),
      br(),
    fluidRow(
      column(8,
             offset = 2,
             p("Different countries have taken different paths for the change they have witnessed. Countries have had different starting points, and their trajectories reflect different rates of change. Some of the changes seen, especially those in population appear linear, while others appear to have travelled their own path."),
             p("")
      )),
    
      fluidRow(
      column(5, 
             offset = 1,
             wellPanel(
               style = "background-color: #c9ada7;",
               selectInput("select_indicator", 
                           label = h4("Select an indicator to explore"), 
                           choices = unique(country_data$short_name)))),
      column(5, 
             wellPanel(
               style = "background-color: #c9ada7;",
               selectizeInput("select_country",
                              label = h4("Choose one or more countries"),
                              choices = unique(country_data$country),
                              selected = c("China", "India", "United States"),
                              multiple = TRUE)
             ))),
    
    fluidRow(
      column(6, 
             em(h4("How much has changed?",
                align = "center")),
             plotOutput("dumbbell")),
      
      # br(),
      # br(),
      column(6,
             em(h4("How has it changed?",
                align = "center")),
             tabsetPanel(type = "tabs",
                         tabPanel("Plot",
                                  plotlyOutput("trend_line")),
                         tabPanel("Data",
                                  DT::dataTableOutput("trend_data")))
      )),
    br(),
    br()
    
    
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
              panel.grid.minor.y = element_blank()) + 
        labs(
          x = "Value")
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
            labs(
              x = "Year", 
              y = "Value") +
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
