library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(dplyr)
library(shinyjs)
library(ncdf4)
library(sf)
library(raster)
library(lubridate)
library(DT)
library(mapdeck)
library(shinyWidgets)

# File Paths
vic_file <- "D:/CHI-ASU/R_MaCRIA/app/data/VICOut2.nc"
grace_file <- "D:/CHI-ASU/R_MaCRIA/app/data/GRCTellus.JPL.200204_202401.GLO.RL06.1M.MSCNv03CRI.nc"
smap_file <- "D:/CHI-ASU/R_MaCRIA/app/data/SPL4SMGP.007_9km_aid0001.nc"
prism_file <- "D:/CHI-ASU/R_MaCRIA/app/data/CRB_PRISM_Calibrated.2024-01-01.nc"
snotel_file <- "D:/CHI-ASU/R_MaCRIA/app/data/snotel_CAP.csv"
snotel_metadata_file <- "D:/CHI-ASU/R_MaCRIA/app/data/snotel_metadata.csv"
basin_shapefile <- "D:/CHI-ASU/R_MaCRIA/app/shapefiles/crb_poly/basin_CRB_poly.shp"
huc10_shapefile <- "D:/CHI-ASU/R_MaCRIA/app/shapefiles/huc10s/huc10s/selected_huc10s.shp"


# Load and reproject spatial data
basin_data <- st_read(basin_shapefile) %>% st_transform(crs = 4326) # WGS84
huc10_data <- st_read(huc10_shapefile) %>% st_transform(crs = 4326) # WGS84

# Load SNOTEL Data
snotel_data <- read.csv(snotel_file)
snotel_metadata <- read.csv(snotel_metadata_file)

# Function to Get Variables from NetCDF Files
get_nc_variables <- function(file) {
    nc <- nc_open(file)
    vars <- names(nc$var)
    nc_close(nc)
    return(vars)
}

# Get Variables from NetCDF Files
vic_vars <- get_nc_variables(vic_file)
grace_vars <- get_nc_variables(grace_file)
smap_vars <- get_nc_variables(smap_file)
prism_vars <- get_nc_variables(prism_file)

# Combine All Variables
all_vars <- unique(c(vic_vars, grace_vars, smap_vars, prism_vars))

# Function to Compute Mean Annual Maps
compute_mean_annual <- function(nc_file, var_name) {
    nc <- nc_open(nc_file)
    data <- ncvar_get(nc, var_name)
    time <- ncvar_get(nc, "time")
    nc_close(nc)

    # Aggregate data to annual mean
    annual_mean <- apply(data, c(1, 2), mean, na.rm = TRUE)
    return(annual_mean)
}

# Shiny UI
ui <- dashboardPage(
    dashboardHeader(title = "Colorado River Basin Explorer"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Comparison", tabName = "comparison", icon = icon("balance-scale")),
            menuItem("Monitoring", tabName = "monitoring", icon = icon("sync")),
            menuItem("Time Series", tabName = "stats", icon = icon("chart-line")),
            menuItem("Spatial Maps", tabName = "spatial_maps", icon = icon("map")),
            menuItem("Infrastructure Assets", tabName = "assets", icon = icon("building")),
            menuItem("Stakeholder Engagement", tabName = "stakeholder", icon = icon("users")),
            menuItem("Help", tabName = "help", icon = icon("question-circle"))
        )
    ),
    dashboardBody(
        useShinyjs(),
        tabItems(
            # Dashboard Tab
            tabItem(
                tabName = "dashboard",
                h2("Explore Data"),
                fluidRow(
                    column(
                        width = 12,
                        box(
                            title = "Map",
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            leafletOutput("mainMap", height = 400)
                        )
                    )
                ),
                fluidRow(
                    lapply(all_vars, function(var) {
                        column(
                            width = 4,
                            box(
                                title = var,
                                width = NULL,
                                status = "info",
                                solidHeader = TRUE,
                                plotlyOutput(paste0("plot_", var), height = 200),
                                checkboxInput(paste0("compare_", var), "Compare", value = FALSE)
                            )
                        )
                    })
                )
            ),
            # Comparison Tab
            tabItem(
                tabName = "comparison",
                h2("Compare Data"),
                fluidRow(
                    column(
                        width = 12,
                        box(
                            title = "Comparison Graph",
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            plotlyOutput("comparisonPlot", height = 400),
                            fluidRow(
                          
                            )
                        )
                    )
                )
            ),
            # Monitoring Tab
            tabItem(
                tabName = "monitoring",
                h2("Automated Monitoring"),
                p("Real-time monitoring and historical comparisons of VIC outputs."),
                plotlyOutput("weeklyUpdates", height = 300),
                uiOutput("alerts")
            ),
            # Time Series Tab
            tabItem(
                tabName = "stats",
                h2("Statistical Analysis"),
                selectInput("variable", "Select Variable:", choices = all_vars),
                plotlyOutput("timeSeries", height = 400)
            ),
            # Spatial Maps Tab
            tabItem(
                tabName = "spatial_maps",
                h2("Explore Spatial Maps"),
                selectInput("mapLayer", "Select Layer:",
                    choices = list(
                        "Basin Boundaries" = "basin_boundaries",
                        "HUC-10 Basins" = "huc10_basins",
                        "SNOTEL Locations" = "snotel_locations",
                        "GRACE Anomalies" = "grace_anomalies",
                        "SMAP Soil Moisture" = "smap_soil_moisture"
                    )
                ),
                selectInput("baseLayer", "Select Base Layer:",
                    choices = list(
                        "OpenStreetMap" = "OpenStreetMap",
                        "Esri.WorldImagery" = "Esri.WorldImagery",
                        "CartoDB.Positron" = "CartoDB.Positron",
                        "Stamen.Terrain" = "Stamen.Terrain"
                    )
                ),
                leafletOutput("spatialMap", height = 600)
            ),
            # Infrastructure Assets Tab
            tabItem(
                tabName = "assets",
                h2("Infrastructure Asset Management"),
                fluidRow(
                    column(
                        width = 6,
                        box(
                            title = "Asset Overview",
                            width = NULL,
                            status = "info",
                            solidHeader = TRUE,
                            DTOutput("assetTable")
                        )
                    ),
                    column(
                        width = 6,
                        box(
                            title = "Asset Map",
                            width = NULL,
                            status = "success",
                            solidHeader = TRUE,
                            mapdeckOutput("assetMap", height = 500)
                        )
                    )
                )
            ),
            # Stakeholder Engagement Tab
            tabItem(
                tabName = "stakeholder",
                h2("Stakeholder Engagement"),
                fluidRow(
                    column(
                        width = 6,
                        box(
                            title = "Submit Feedback",
                            width = NULL,
                            status = "primary",
                            solidHeader = TRUE,
                            textInput("name", "Name"),
                            textInput("organization", "Organization"),
                            selectInput("category", "Question is About:",
                                choices = c("Data Accuracy", "Visualization", "Technical Issues", "General Feedback")
                            ),
                            textAreaInput("feedback", "Feedback/Question", rows = 3),
                            actionButton("submit", "Submit", class = "btn-primary")
                        )
                    ),
                    column(
                        width = 6,
                        box(
                            title = "Feedback Analytics",
                            width = NULL,
                            status = "info",
                            solidHeader = TRUE,
                            plotlyOutput("feedbackPlot")
                        )
                    )
                ),
                fluidRow(
                    box(
                        title = "Recent Feedback",
                        width = 12,
                        status = "success",
                        solidHeader = TRUE,
                        DTOutput("feedbackTable")
                    )
                )
            ),
            # Help Tab
            tabItem(
                tabName = "help",
                h2("Help and Documentation"),
                fluidRow(
                    box(
                        title = "User Guide",
                        width = 12,
                        status = "info",
                        solidHeader = TRUE,
                        includeMarkdown("D:/CHI-ASU/R_MaCRIA/help.md") # Add a markdown file for documentation
                    )
                )
            )
        )
    )
)

# Shiny Server
server <- function(input, output, session) {
    # Dashboard Tab
    output$mainMap <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addPolygons(data = basin_data, color = "blue", weight = 2, fillOpacity = 0.3) %>%
            setView(lng = -110, lat = 39, zoom = 5)
    })

    lapply(all_vars, function(var) {
        output[[paste0("plot_", var)]] <- renderPlotly({
            plot_ly(x = 1:10, y = rnorm(10), type = "scatter", mode = "lines", name = var) %>%
                layout(title = paste("Time Series for", var))
        })
    })

    # Comparison Tab
    output$comparisonPlot <- renderPlotly({
        selected_vars <- all_vars[sapply(all_vars, function(var) input[[paste0("compare_", var)]])]
        if (length(selected_vars) > 0) {
            p <- plot_ly()
            for (var in selected_vars) {
                p <- p %>% add_trace(x = 1:10, y = rnorm(10), type = "scatter", mode = "lines", name = var)
            }
            p %>%
                layout(title = "Comparison of Selected Variables") %>%
                animation_opts(frame = 100, transition = 0, redraw = FALSE)
        } else {
            plot_ly() %>% layout(title = "No graphs selected for comparison.")
        }
    })

    # Animation Controls
    observeEvent(input$startAnimation, {
        plotlyProxy("comparisonPlot", session) %>%
            plotlyProxyInvoke("restyle", list(visible = TRUE)) %>%
            plotlyProxyInvoke("relayout", list(title = "Animation Started"))
    })

    observeEvent(input$pauseAnimation, {
        plotlyProxy("comparisonPlot", session) %>%
            plotlyProxyInvoke("restyle", list(visible = FALSE)) %>%
            plotlyProxyInvoke("relayout", list(title = "Animation Paused"))
    })

    observeEvent(input$stopAnimation, {
        plotlyProxy("comparisonPlot", session) %>%
            plotlyProxyInvoke("restyle", list(visible = FALSE)) %>%
            plotlyProxyInvoke("relayout", list(title = "Animation Stopped"))
    })

    observeEvent(input$resetAnimation, {
        plotlyProxy("comparisonPlot", session) %>%
            plotlyProxyInvoke("restyle", list(visible = TRUE)) %>%
            plotlyProxyInvoke("relayout", list(title = "Animation Reset"))
    })

    # Monitoring Tab
    output$weeklyUpdates <- renderPlotly({
        plot_ly(
            x = seq.Date(from = as.Date("2024-01-01"), by = "week", length.out = 10),
            y = runif(10, 0, 100), type = "scatter", mode = "lines"
        ) %>%
            layout(title = "Weekly VIC Outputs", xaxis = list(title = "Date"), yaxis = list(title = "Value"))
    })

    # Time Series Tab
    output$timeSeries <- renderPlotly({
        req(input$variable)
        plot_ly(x = 1:10, y = rnorm(10), type = "scatter", mode = "lines") %>%
            layout(title = paste("Time Series for", input$variable), xaxis = list(title = "Time"), yaxis = list(title = input$variable))
    })

    # Spatial Maps Tab
    output$spatialMap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(input$baseLayer) %>%
            addPolygons(data = basin_data, color = "blue", weight = 2, fillOpacity = 0.3)
    })

    observeEvent(input$mapLayer, {
        leafletProxy("spatialMap") %>%
            clearShapes() %>%
            addPolygons(data = basin_data, color = "blue", weight = 2, fillOpacity = 0.3)

        if (input$mapLayer == "huc10_basins") {
            leafletProxy("spatialMap") %>%
                addPolygons(data = huc10_data, color = "green", weight = 2, fillOpacity = 0.3)
        } else if (input$mapLayer == "snotel_locations") {
            leafletProxy("spatialMap") %>%
                addCircleMarkers(
                    data = snotel_metadata, ~longitude, ~latitude,
                    popup = ~ paste("Station:", Station_Name, "<br>Elevation:", Elevation),
                    radius = 5, color = "red", fillOpacity = 0.8
                )
        } else if (input$mapLayer == "grace_anomalies") {
            leafletProxy("spatialMap") %>%
                addRasterImage(raster(grace_file), colors = "Spectral", opacity = 0.7)
        } else if (input$mapLayer == "smap_soil_moisture") {
            leafletProxy("spatialMap") %>%
                addRasterImage(raster(smap_file), colors = "Spectral", opacity = 0.7)
        }
    })

    observeEvent(input$baseLayer, {
        leafletProxy("spatialMap") %>%
            clearTiles() %>%
            addProviderTiles(input$baseLayer)
    })

    # Infrastructure Assets Tab
    output$assetTable <- renderDT({
        datatable(data.frame(
            Asset = c("Hoover Dam", "Glen Canyon Dam", "Parker Dam"),
            Status = c("Operational", "Operational", "Under Maintenance")
        ))
    })

    output$assetMap <- renderMapdeck({
        mapdeck(style = mapdeck_style("light"), zoom = 5) %>%
            add_scatterplot(
                data = data.frame(
                    lat = c(36.016, 36.936, 34.297),
                    lon = c(-114.737, -111.601, -113.226),
                    label = c("Hoover Dam", "Glen Canyon Dam", "Parker Dam")
                ),
                lat = "lat", lon = "lon", tooltip = "label", fill_color = "red", radius = 500
            )
    })

    # Stakeholder Engagement Tab
    output$feedbackPlot <- renderPlotly({
        plot_ly(x = 1:5, y = c(5, 4, 3, 2, 1), type = "bar", name = "Feedback Sentiment") %>%
            layout(title = "Feedback Sentiment")
    })

    output$feedbackTable <- renderDT({
        datatable(data.frame(
            Name = c("John Doe", "Jane Smith", "Tom Brown"),
            Feedback = c("Great tool!", "Needs more data.", "Love the interface."),
            Category = c("General Feedback", "Data Accuracy", "Visualization")
        ))
    })
}

# Run the Shiny App
shinyApp(ui, server)
