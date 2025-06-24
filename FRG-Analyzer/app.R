library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(terra)
library(aws.s3)
library(plotly)  # New library
library(tools)   # For file_path_sans_ext

# UI
ui <- fluidPage(
  titlePanel("FRG by Landscape Analyzer"),
  
  # Introductory Text
  fluidRow(
    column(12,
           tags$div(
             p("Welcome to the FRG by Landscape Analyzer"),
             p("This tool allows you to upload a geopackage file and analyze the spatial distribution of LANDFIRE Fire Regime Groups (FRG) across a specific landscape. 
               The application will generate a map, a frequency table, and a frequency plot based on the data you provide."),
             p("To get started, simply upload your geopackage file using the 'Upload Geopackage' button on the left, and click 'Process' to generate the outputs. This tool is currently functional for CONUS, thus processing takes ~5 minutes. Grab a cup of coffee and wait for the Shiny magic!")
           )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("geopackage", "Upload Geopackage", accept = c(".gpkg")),
      actionButton("process", "Process"),
      downloadButton("downloadData", "Download Frequency Table")  # Download button
    ),
    mainPanel(
      leafletOutput("map"),
      tableOutput("freq_table"),
      plotlyOutput("freq_plot")  # Updated to Plotly
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values to store the processed data
  values <- reactiveValues(
    sf_data = NULL,
    freq_table = NULL,
    raster_data = NULL,
    no_data_count = NULL,
    gpkg_name = NULL  # For geopackage name
  )
  
  # Load the raster data from S3
  observe({
    s3_raster <- terra::rast("s3://vp-sci-grp/landfire/interim/bps/2.2.0/LF2020_FRG_220_CONUS.tif")
    values$raster_data <- s3_raster
  })
  
  # Process the uploaded geopackage
  observeEvent(input$process, {
    req(input$geopackage)
    
    # Extract the geopackage filename without extension
    values$gpkg_name <- file_path_sans_ext(input$geopackage$name)
    
    # Start the progress bar
    withProgress(message = 'Processing...', value = 0, {
      
      # Step 1: Read the uploaded geopackage
      sf_data <- st_read(input$geopackage$datapath)
      incProgress(0.3)  # Progress update
      
      # Step 2: Reproject sf_data to match the raster's CRS
      sf_data <- st_transform(sf_data, crs = st_crs(values$raster_data))
      values$sf_data <- sf_data
      incProgress(0.6)  # Progress update
      
      # Step 3: Extract raster values for the geopackage
      cropped_raster <- terra::crop(
        x = values$raster_data, 
        y = sf_data, 
        mask = TRUE
      )
      
      # Count No Data cells
      no_data_count <- sum(is.na(terra::values(cropped_raster)))
      values$no_data_count <- no_data_count
      
      # Calculate the frequency table
      freq_table <- terra::freq(x = cropped_raster) %>%
        select(-layer) %>%
        mutate(FRG = as.character(value)) %>%  # Convert FRG to character
        select(-value) %>%
        mutate(Proportion = count/sum(count))
      
      values$freq_table <- freq_table
      incProgress(1)  # Progress update to completion
    })
  })
  
  # Render the map
  output$map <- renderLeaflet({
    req(values$sf_data)
    
    leaflet() %>%
      addProviderTiles("OpenStreetMap") %>%
      addPolygons(data = st_transform(values$sf_data, 4326), color = "blue", weight = 1)
  })
  
  # Render the frequency table
  output$freq_table <- renderTable({
    req(values$freq_table)
    freq_table_with_na <- values$freq_table %>%
      bind_rows(tibble(FRG = "No Data", count = values$no_data_count, Proportion = NA))
    freq_table_with_na
  })
  
  # Render the frequency plot using Plotly
  output$freq_plot <- renderPlotly({
    req(values$freq_table)
    
    plot_ly(values$freq_table, x = ~FRG, y = ~count, type = 'bar', color = ~as.factor(FRG)) %>%
      layout(title = "Frequency of Fire Regime Groups",
             xaxis = list(title = "FRG"),
             yaxis = list(title = "Frequency"))
  })
  
  # Provide a download handler for the frequency table
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(values$gpkg_name, "frequency_table-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      freq_table_with_na <- values$freq_table %>%
        bind_rows(tibble(FRG = "No Data", count = values$no_data_count, Proportion = NA))
      write.csv(freq_table_with_na, file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)