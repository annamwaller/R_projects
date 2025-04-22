
library(shiny)
library(shinydashboard)
library(dplyr)
library(stringr)
library(raster)
library(ncdf4)
library(sf)
library(ggplot2)
library(rasterVis)
library(plotly)
library(here)
set_here("/Users/lola/Desktop/STAT408/Project")

# load netcdf
spei <- raster::brick(here("SPEI", "spei48.nc"))

# load Zambia shapefile
zambia_shapefile <- st_read(here("SPEI", "zm.shp"))

# set CRS
st_crs(zambia_shapefile) <- 4326

# reproject shapefile to netcdf CRS
zambia_shapefile <- st_transform(zambia_shapefile, crs = st_crs(spei))

# crop to Zambia
spei_zambia <- crop(spei, extent(zambia_shapefile))

# create date vector
raw_names <- str_remove(names(spei_zambia), "^X")
raw_names <- str_replace_all(raw_names, "[._]", "-") 
dates <- as.Date(raw_names, format = "%Y-%m-%d")

# filter from 2000 onward
index_2000 <- which(as.numeric(format(dates, "%Y")) >= 2000)
spei_2000 <- spei_zambia[[index_2000]]
dates_2000 <- dates[index_2000]
months <- format(dates_2000, "%Y-%m")

# identify individual provinces
provinces <- unique(zambia_shapefile$name)

# load mean SPEI
mean_spei <- read.csv(here("SPEI", "zambia_spei_province.csv"))
mean_spei$date <- as.Date(mean_spei$date)

# load forest cover change CSV
forest_cover <- read.csv(here("SPEI", "province_forest_cover.csv"))

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = 'Zambia'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Project Overview', tabName = 'overview', icon = icon('earth-africa')),
      menuItem('Drought Raster Data', tabName = 'raster', icon = icon('droplet')),
      menuItem('Drought Time Series', tabName = 'spei_timeseries', icon = icon('chart-line')),
      menuItem('Forest Loss', tabName = 'lcluc', icon = icon('tree'))
      )
    ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = 'overview',
              div(style = 'position: relative; width: 100%;',
                  tags$img(src = 'https://upload.wikimedia.org/wikipedia/commons/5/5c/Livingstone%2C_Zambia_-_panoramio_%289%29.jpg', 
                           width = '100%' ),
                  div(
                    style = '
                    position: absolute;
                    top: 20px;
                    left: 20px;
                    background-color: rgba(255, 255, 255, 0.85);
                    padding: 20px;
                    border-radius: 10px;
                    width: 40%;
                    max-width: 500px;
                    ',
                    h3('About the Project'),
                    p('In Zambia, a hydropower-reliant country in south-central Africa,
                      homes and businesses may spend up to 12 hours daily without power.
                      This project aims to explore potential relationships between drought
                      events and forest degradation, due to increased charcoal production
                      and agricultural expansion during times of low water availability.')
              
                    )
                
            )
      ),
      # raster heatmap tab
      tabItem(
        tabName = 'raster',
        fluidRow(
          box(width = 4,
              selectInput('province', 'Select province', choices = provinces),
              selectInput('date_index', 'Select date',
                          choices = as.vector(unique(months)),
                          selected = '2000-01'),
              textOutput('selected_date')
          ),
          box(width = 8,
              plotOutput('spei_map', height = '600px')
          )
        )
    ),
    # time series line graph
    tabItem(
      tabName = 'spei_timeseries',
      fluidRow(
        box(width = 4,
            selectInput('province_ts', 'Select province', choices = provinces)
        ),
        box(width = 8,
            plotlyOutput('spei_timeseries')
        )
      )
    ),
    # forest loss line graph
    tabItem(
      tabName = 'lcluc',
      fluidRow(
        box(width = 4,
            selectInput('province_lcluc', 'Select province', choices = unique(forest_cover$province))
        ),
        box(width = 8,
            plotlyOutput('forest_plot')
        )
      )
    )
  )
 )
)

server <- function(input, output) {

    output$selected_date <- renderText({
      paste('Province:', input$province, '| Date:', input$date_index)
    })
    output$spei_map <- renderPlot({
      date_index <- which(format(dates_2000, '%Y-%m') == input$date_index)
      selected_date <- dates_2000[date_index]
      # extract raster layer for selected date
      raster_layer <- spei_2000[[date_index]]
      # get province polygon
      select_province <- zambia_shapefile |>
        filter(name == input$province)
      # mask raster
      raster_mask <- mask(crop(raster_layer, extent(select_province)), select_province)
      levelplot(raster_mask, margin = FALSE,
                main = paste('SPEI in', input$province, '-', format(selected_date, '%B %Y')),
                col.regions = rev(terrain.colors(100)),
                at = seq(-3, 3, length = 100))
    })
    output$spei_timeseries <- renderPlotly({
      df <- mean_spei |>
        filter(province == input$province_ts)
      plot_ly(df, x = ~date, y = ~spei, type = 'scatter', mode = 'lines+markers') |>
        layout(
          title = paste('Mean SPEI -', input$province_ts),
          xaxis = list(title = 'Date'),
          yaxis = list(title = 'Average SPEI'))
      
    })
    
    output$forest_plot <- renderPlotly({
      df <- forest_cover |>
        filter(province == input$province_lcluc)
      plot_ly(df, x = ~year, y = ~forest_cover_ha, type = 'scatter', mode = 'lines+markers') |>
        layout(title = paste('Forest Cover Change -', input$province_lcluc),
               xaxis = list(title = 'Year'),
               yaxis = list(title = 'Forested area (ha)'))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
