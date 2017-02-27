# Surplus scout

# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw the map
server <- function(input, output, session) {
  
# EXTRACT LA POLYGON ------------------------------------------------------
  # We need the polygon data only for our LA of interest, as the polygons
  # are not ordered we search or match by LA number or code
  # We can print the info to test
  polygon_la <- reactive({
    la_s_ll[match(as.numeric(sapply(strsplit(input$la_of_interest[[1]], split = " "), "[[", 1)), la_s_ll@data$LEA_CODE), ]
    #  select rows by position
  })
  
  output$la_details <- renderTable({
    polygon_la()@data %>%
    xtable()
  })
  
  output$la_name <- renderText({
    input$la_of_interest
  })
  
# APPLE DATA FOR SCHOOLS IN LA --------------------------------------
# Join data and filter for LA and school phase by user input
# This is called KS4 to map for my convenience, don't read into it (Primary can be mapped also)
  
  ks4_to_map <- reactive({
    dplyr::filter(fruits,
                  la_number == as.numeric(sapply(strsplit(input$la_of_interest[[1]], split = " "), "[[", 1))) %>%
      left_join(school_locations,
                by = c("urn", "la_number")) %>%
      dplyr::filter(phase == input$phase) %>%
      mutate(easting = easting.x, northing = northing.x) %>%
      select(-easting.y, -northing.y) %>%
      na.omit()
  })

  output$fruit_table_data <- DT::renderDataTable({
    expr = datatable(ks4_to_map() %>%  #  Notice the parentheses! ()
      select(school_name, apples, pears,
             urn) %>%
      mutate(cherry_status = round(apples * pears, 2)) %>%
      rename(School_Name = school_name, URN = urn),
      selection =  list(mode = 'multiple', selected = 1, target = 'row')  #  preselection, ?datatable
    )
    
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste("cherry_picker_app_", input$la_of_interest, '.csv', sep = '') },
    content = function(file) {
      write.csv(ks4_to_map(), file)
    }
  )
  
  # NATIONAL DISTRIBUTIONS for LA comparison --------------------------------
  
  # Provide variable distributions to aid comparison to rest of the country
  output$hist_progress_8 <- renderPlot({
    hist(ks4_data$progress_8, main = "Attainment",
         xlab = "Progress 8", col = "salmon", border = 'white',
         breaks = 20, xlim = c(-2, 2))
    rug(ks4_to_map()$progress_8, ticksize = -0.2, lwd = 3, col = "blue")
  })
  
  output$hist_backlog <- renderPlot({
    hist((left_join(condition_backlog_data, school_locations, by = "urn") %>% 
            filter(phase == "Secondary"))$cost_backlog,  #  just for Secondary Schools, remove Primary
         main = "Condition",
         xlab = "Backlog (£)", col = '#00DD00', border = 'white',
         breaks = 20)
    rug(ks4_to_map()$cost_backlog, ticksize = -0.15, lwd = 3, col = "blue")
  })
  
# FORMS OF ENTRY ----------------------------------------------------------
# Recommended area per pupil set in global.R
# https://rstudio.github.io/DT/shiny.html
  # row selection
  output$green_grocers <- DT::renderDataTable(datatable(
    slice(ks4_to_map() %>%
            select(school_name, total_site_area, total_ground_floor, surplus_land,
                   surplus_proportion, urn) %>%
            mutate(surplus_proportion = round(surplus_proportion, 2)) %>%
            arrange(desc(surplus_land)) %>%
            rename(School_Name = school_name, URN = urn),  #  creates identical table to slice from, see fruit_table_data
          input$fruit_table_data_rows_selected) %>%  
      mutate(green_grocers_required = (apples + pears) / (if_else(input$phase == "Secondary",
                                              50,  #  Secondary school children need more fruit!?
                                              10) * 30)
    ) %>%  #  we refine the datatable here and prettify
      select(School_Name, green_grocers_required)
  ) %>%  #  and prettify
    formatRound(c("School_Name", "Green grocers required"),
                0)
  )
  
  # # MAPPING SETUP -----------------------------------------------------------

  # Variables for holding the coordinate system types (see: # http://www.epsg.org/ for details)
  ukgrid <- "+init=epsg:27700"
  latlong <- "+init=epsg:4326"

  # Create coordinates variable, first argument
  # Create the SpatialPointsDataFrame, note coords and data are distinct slots in S4 object
  # Vestigial name from condainment app
  ks4_sp_ll <- reactive({
    #  we use x as a placeholder just within this reactive bit, helps with the last renaming step
    x <- spTransform(
      sp::SpatialPointsDataFrame(dplyr::select(ks4_to_map(), easting, northing),
                                 data = dplyr::select(ks4_to_map(), -easting, -northing),
                                 proj4string = CRS("+init=epsg:27700")),
      CRS(latlong)
    )
    # Convert from Eastings and Northings to Latitude and Longitude and rename columns
    colnames(x@coords)[colnames(x@coords) == "easting"] <- "longitude"
    colnames(x@coords)[colnames(x@coords) == "northing"] <- "latitude"
    
    x
  })
  
    # LEAFLET -----------------------------------------------------------------
  
  output$mymap <- renderLeaflet({
    pal11 <- colorNumeric(palette = "PuRd",
                          ks4_sp_ll()@data$apples)
    pal12 <- colorNumeric(palette = "PuBuGn",
                          ks4_sp_ll()@data$pears) 
    # pal13 <- colorNumeric(palette = "YlOrRd",
    #                       ks4_sp_ll()@data$total_area)
    
    m11 <- leaflet(data = ks4_sp_ll()@data) %>%
      addProviderTiles(provider = "Esri.WorldImagery", group = "Terrain") %>%
      addProviderTiles(provider = "OpenStreetMap.BlackAndWhite", group = "OSM (B & W)") %>%
      # addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      # Apples
      addCircles(lng = ks4_sp_ll()@coords[, "longitude"],
                 lat = ks4_sp_ll()@coords[, "latitude"],
                 color = "black",
                 opacity = 0.8,
                 weight = 0.5,
                 radius = 100,  #  Radius can be used for spatial stuff
                 fillOpacity = 0.5,
                 fillColor = pal11(ks4_sp_ll()@data$apples),
                 popup = NULL, group = "Apples") %>%
      addLegend("bottomright", pal = pal11,
                values = ks4_sp_ll()@data$apples,
                title = "Apples",
                labFormat = labelFormat(prefix = ""),
                opacity = 0.5, layerId = "Apples") %>%
      # Pears
      addCircles(lng = ks4_sp_ll()@coords[, "longitude"],
                 lat = ks4_sp_ll()@coords[, "latitude"],
                 color = "black",
                 opacity = 1, radius = 100, weight = 1,
                 fillOpacity = 0.3,
                 fillColor = pal12(ks4_sp_ll()@data$pears),
                 popup = NULL, group = "Pears") %>%
      addLegend("bottomleft", pal = pal12,
                values = ks4_sp_ll()@data$pears,
                title = "Pears",
                labFormat = labelFormat(prefix = ""),
                opacity = 0.5, layerId = "Pears") %>%
      # Pear outline
      addCircles(lng = ks4_sp_ll()@coords[, "longitude"],
                 lat = ks4_sp_ll()@coords[, "latitude"],
                 color = pal12(ks4_sp_ll()@data$pears),
                 opacity = 1,
                 radius = 100,
                 weight = 3,
                 fillOpacity = 0,
                 fillColor = pal12(ks4_sp_ll()@data$pears),
                 popup = NULL, group = "Pear outline") %>%
      # marker
      addMarkers(lng = ks4_sp_ll()@coords[, "longitude"],
                 lat = ks4_sp_ll()@coords[, "latitude"],
                 popup = as.character(paste(ks4_sp_ll()@data$school_name, 
                                            "has a total fruit consumption of",
                                            round(ks4_sp_ll()@data$apples + ks4_sp_ll()@data$pears),
                                            "pieces of fruit per student per day.",
                                            sep = "\n"
                 )),
                 options = popupOptions(closeButton = TRUE),
                 group = "Fruit Markers") %>%
      ### LA
      addPolygons(data = polygon_la(), 
                  stroke = TRUE, fillOpacity = 0, smoothFactor = 0.2, 
                  color = "black", weight = 3,
                  group = "LA boundary") %>%
      ### Groups
      hideGroup("Fruit Markers") %>%
      hideGroup("Pears") %>%
      hideGroup("Pear outline") %>%
      showGroup("Apples") %>%
      hideGroup("Terrain") %>%
      showGroup("OSM (B & W)") %>%
      showGroup("LA boundary") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("Terrain", "OSM (B & W)"),
        overlayGroups = c("Apples", "Pear outline",
                          "Pears",
                          "Fruit Markers"
                          ),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    m11
  })
  
}


