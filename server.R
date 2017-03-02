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
    expr = datatable(
      ks4_to_map() %>%  #  Notice the parentheses! ()
      select(school_name, apples, pears,
             urn) %>%
      mutate(apples = round(apples, 2), pears = round(pears, 2), cherry_status = round((apples + pears) / 2, 2)) %>%
      rename(School_Name = school_name, URN = urn),
      selection =  list(mode = 'multiple', selected = 1, target = 'row')  #  preselection, ?datatable
    ) %>%
      #  http://rstudio.github.io/DT/functions.html
      formatStyle(
        c("apples", "pears", "cherry_status"),
        color = styleInterval(0.5, c('red', 'blue'))  #  colour table font based on rule
      ) %>%
      formatCurrency(c('apples', 'pears'),  #  add currency symbols and round
                     '\U00A3',
                     digits = 2)  #%>%
      #  # formatStyle(
      #  #   'cherry_status',
      #  #   background = styleColorBar(ks4_to_map()$cherry_status, 'steelblue'),
      # #   backgroundSize = '100% 90%',
      # #   backgroundRepeat = 'no-repeat',
      # #   backgroundPosition = 'center'
      #  )
    
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste("cherry_picker_app_", input$la_of_interest, '.csv', sep = '') },
    content = function(file) {
      write.csv(ks4_to_map(), file)
    }
  )
  
  # NATIONAL DISTRIBUTIONS for LA comparison --------------------------------
  
  # Provide variable distributions to aid comparison to rest of the country
  output$hist_apples <- renderPlot({
    hist(fruits$apples,  #  Note this does all schools, doesn't filter for School Phase, which you may want
         main = "Apple of my eye",  #  Note how we call our pre-filtered data assigned in global!
         xlab = "Apples", col = "salmon", border = 'white', xlim = c(0, 1))
    rug(ks4_to_map()$apples, ticksize = -0.2, lwd = 3, col = "blue")  #  For the rug we use our reactive dataframes
  })
  
  output$hist_pears <- renderPlot({
    plot(density(fruits$pears), 
         main = "Pear shaped",
         xlab = "Pears", col = '#00DD00', xlim = c(0, 1))
    rug(ks4_to_map()$pears, ticksize = -0.15, lwd = 3, col = "blue")
  })
  
  output$scatter_fruit <- renderPlot({
    ggplot(fruits, aes(x = apples, y = pears, col = "red")) +
      # geom_bin2d() +
      geom_point(alpha = 0.2) +
      xlim(0, 1) + ylim(0, 1) +
      geom_point(data = slice(ks4_to_map(),
                              input$fruit_table_data_rows_selected), #  Add our selected schools from the previous tab's table
                 mapping = aes(x = apples, y = pears,
                               shape = "circle", colour = "blue",
                               size = 4.5)) +
      annotate( "rect", xmin = 0.8 , xmax = 1.0, ymin = 0.8, ymax = 1.0,
                alpha = 0.01, colour = "pink") +  #  Capture data points that are ripe for picking!
      annotate("text", x = 0.9, y = 0.9,
               label = "Cherry picking region", col = "black") +
      ggthemes::theme_tufte()

  })
  
# ANOTHER TAB ----------------------------------------------------------
  # Here we can use another tab to display some furter analysis or statistics
# https://rstudio.github.io/DT/shiny.html
  # row selection
  output$green_grocers <- DT::renderDataTable(datatable(
    slice(ks4_to_map() %>%
            select(school_name, apples, pears, urn) %>%
            arrange(desc(apples)) %>%
            rename(School_Name = school_name, URN = urn), #  creates identical table to slice from, see fruit_table_data
          input$fruit_table_data_rows_selected) %>%  
      mutate(made_up_statistic = (apples + pears) * (if_else(input$phase == "Secondary",
                                              3,  #  Secondary school children need more fruit!?
                                              1) * 30),
             cherry_status = round((apples + pears) / 2, 2)
    ) %>%  #  we refine the datatable here and prettify
      select(School_Name, cherry_status, made_up_statistic)
  ) %>%  #  and prettify
    formatRound(c("School_Name", "made_up_statistic"),
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
                 radius = 200,  #  Radius could be assigned to a another variable
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
                 opacity = 1, radius = 200, weight = 1,
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
                 radius = 201,
                 weight = 5,
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
                  color = "black", weight = 5,
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


