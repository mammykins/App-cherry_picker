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

  output$surplus_table_data <- DT::renderDataTable({
    expr = datatable(ks4_to_map() %>%
      select(school_name, total_site_area, total_ground_floor, surplus_land,
             surplus_proportion, urn) %>%
      mutate(surplus_proportion = round(surplus_proportion, 2)) %>%
      arrange(desc(surplus_land)) %>%
      rename(School_Name = school_name, URN = urn),
      selection =  list(mode = 'multiple', selected = 1, target = 'row')  #  preselection, ?datatable
    )
    
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste("surplus_scout_app_", input$la_of_interest, '.csv', sep = '') },
    content = function(file) {
      write.csv(ks4_to_map(), file)
    }
  )
  
  
  
  # NATIONAL DISTRIBUTIONS for LA comparison --------------------------------
  scap_to_plot <- reactive({
    dplyr::filter(la_scap_data,
                  la_number == as.numeric(sapply(strsplit(input$la_of_interest[[1]], split = " "), "[[", 1)) &
                  school_phase == tolower(input$phase)
                  )
  })
  # Provide variable distributions to aid comparison to rest of the country
  # Each La has two data, primary and secondary, need to compare relative to phase
  output$hist_no_pupils_in_excess <- renderPlot({
    hist(filter(la_scap_data,
                school_phase == tolower(input$phase)
                )$no_pupils_in_excess,
         main = "Pupil places shortage",
         xlab = "Number of places",
         breaks = 20,
         col = "salmon", border = 'white')
    rug(scap_to_plot()$no_pupils_in_excess, ticksize = -0.2, lwd = 3, col = "blue")
  })
  
  output$pupil_excess_as_percentage_of_places <- renderPlot({
    hist(filter(la_scap_data,
                school_phase == tolower(input$phase)
    )$pupil_excess_as_percentage_of_places,
    main = "Relative pupil places shortage",
    xlab = "%",
    breaks = 20,
    col = '#00DD00', border = 'white')
    rug(scap_to_plot()$pupil_excess_as_percentage_of_places, ticksize = -0.15, lwd = 3, col = "blue")
  })

# FORMS OF ENTRY ----------------------------------------------------------
# Recommended area per pupil set in global.R
# https://rstudio.github.io/DT/shiny.html
  # row selection
  output$form_of_entry <- DT::renderDataTable(datatable(
    slice(ks4_to_map() %>%
            select(school_name, total_site_area, total_ground_floor, surplus_land,
                   surplus_proportion, urn) %>%
            mutate(surplus_proportion = round(surplus_proportion, 2)) %>%
            arrange(desc(surplus_land)) %>%
            rename(School_Name = school_name, URN = urn),  #  creates identical table to slice from, see surplus_table_data
          input$surplus_table_data_rows_selected) %>%  # add FE, could write a function here to tidy
      mutate(one_FE = surplus_land / (if_else(input$phase == "Secondary",
                                              secondary_min_per_pupil,
                                              primary_min_per_pupil) * 30),
             two_FE = surplus_land / (if_else(input$phase == "Secondary",
                                              secondary_min_per_pupil,
                                              primary_min_per_pupil) * 60),
             three_FE = surplus_land / (if_else(input$phase == "Secondary",
                                              secondary_min_per_pupil,
                                              primary_min_per_pupil) * 90),
             four_FE = surplus_land / (if_else(input$phase == "Secondary",
                                              secondary_min_per_pupil,
                                              primary_min_per_pupil) * 120),
             five_FE = surplus_land / (if_else(input$phase == "Secondary",
                                              secondary_min_per_pupil,
                                              primary_min_per_pupil) * 150)
    ) %>%  #  we refine the datatable here and prettify
      select(School_Name, one_FE, two_FE, three_FE, four_FE, five_FE)
  ) %>%  #  and prettify
    formatRound(c("School_Name", "one_FE", "two_FE", "three_FE", "four_FE", "five_FE"),
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
                          ks4_sp_ll()@data$surplus_land)
    pal12 <- colorNumeric(palette = "PuBuGn",
                          ks4_sp_ll()@data$surplus_proportion) 
    # pal13 <- colorNumeric(palette = "YlOrRd",
    #                       ks4_sp_ll()@data$total_area)
    
    m11 <- leaflet(data = ks4_sp_ll()@data) %>%
      addProviderTiles(provider = "Esri.WorldImagery", group = "Terrain") %>%
      addProviderTiles(provider = "OpenStreetMap.BlackAndWhite", group = "OSM (B & W)") %>%
      # addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      # Surplus Land
      addCircles(lng = ks4_sp_ll()@coords[, "longitude"],
                 lat = ks4_sp_ll()@coords[, "latitude"],
                 color = "black",
                 opacity = 0.8,
                 weight = 0.5,
                 radius = sqrt(ks4_sp_ll()@data$surplus_land/pi),
                 fillOpacity = 0.5,
                 fillColor = pal11(ks4_sp_ll()@data$surplus_land),
                 popup = NULL, group = "Surplus Land") %>%
      addLegend("bottomright", pal = pal11,
                values = ks4_sp_ll()@data$surplus_land,
                title = "Surplus Land",
                labFormat = labelFormat(prefix = ""),
                opacity = 0.5, layerId = "Surplus Land") %>%
      # surplus_proportion
      addCircles(lng = ks4_sp_ll()@coords[, "longitude"],
                 lat = ks4_sp_ll()@coords[, "latitude"],
                 color = "black",
                 opacity = 1, radius = 80, weight = 1,
                 fillOpacity = 0.3,
                 fillColor = pal12(ks4_sp_ll()@data$surplus_proportion),
                 popup = NULL, group = "Surplus Proportion") %>%
      addLegend("bottomleft", pal = pal12,
                values = ks4_sp_ll()@data$surplus_proportion,
                title = "Surplus Proportion",
                labFormat = labelFormat(prefix = ""),
                opacity = 0.5, layerId = "Surplus Proportion") %>%
      # Surplus proportion outline
      addCircles(lng = ks4_sp_ll()@coords[, "longitude"],
                 lat = ks4_sp_ll()@coords[, "latitude"],
                 color = pal12(ks4_sp_ll()@data$surplus_proportion),
                 opacity = 1,
                 radius = sqrt(ks4_sp_ll()@data$surplus_land/pi),
                 weight = 3,
                 fillOpacity = 0,
                 fillColor = pal12(ks4_sp_ll()@data$surplus_proportion),
                 popup = NULL, group = "Relative free land") %>%
      # backlog marker
      addMarkers(lng = ks4_sp_ll()@coords[, "longitude"],
                 lat = ks4_sp_ll()@coords[, "latitude"],
                 popup = as.character(paste(ks4_sp_ll()@data$school_name, 
                                            "has a total site area of",
                                            round(ks4_sp_ll()@data$total_site_area),
                                            "m^2",
                                            sep = "\n"
                 )),
                 options = popupOptions(closeButton = TRUE),
                 group = "Total Site Area") %>%
      ### LA
      addPolygons(data = polygon_la(), 
                  stroke = TRUE, fillOpacity = 0, smoothFactor = 0.2, 
                  color = "black", weight = 3,
                  group = "LA boundary") %>%
      ### Groups
      hideGroup("Surplus Proportion") %>%
      hideGroup("Total Site Area") %>%
      hideGroup("Relative free land") %>%
      showGroup("Surplus Land") %>%
      hideGroup("Terrain") %>%
      showGroup("OSM (B & W)") %>%
      showGroup("LA boundary") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("Terrain", "OSM (B & W)"),
        overlayGroups = c("Surplus Land", "Surplus Proportion",
                          "Total Site Area", "Relative free land"
                          ),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    m11
  })
  
}


