# Surplus scout

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# This app should be used to develop your own map apps in Shiny
# It provides a useful skeleton to build on.

library(shiny)
library(leaflet)


# FANCY STYLE -------------------------------------------------------------

ui <- navbarPage("Cherry picker", id = "nav",
                 
           tabPanel("Interactive map",
                    
                    # div(class = "outer",
                    #     
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        ),
                        leafletOutput("mymap"), 
                    br(),
                    p("Imagine you wanted to plot two continuous variables associated with around eighteen thousand schools in the UK, 
                      here we provide an app to demonstrate one solution."),
                    p("We zoom in on a selected Local Authority (LA) and then show all the school data
                      filtered by phase for that LA."),
                    p("Importantly ", strong("all this data is made-up using a uniform distribution (between 0 and 1);"),
                      " any correlation between the simulated apple and pear variables is due to chance."),
                    
                        # Shiny versions prior to 0.11 should use class="modal" instead.

                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 180, bottom = "auto",
                                      width = 330,
                                      height = "auto",
                                      h4("User input"),
                                      selectInput(inputId = "la_of_interest", label = "Local Authority",
                                                  choices = la_user_friendly_list, selected = "202 - Camden"),
                                      selectInput(inputId = "phase", label = "School phase",
                                                  choices = c("Secondary", "Primary")),
                                      plotOutput("hist_apples", height = 200),
                                      plotOutput("hist_pears", height = 200),
                                      p("Here we provide apples and pears data distributions for all 152 LA for schools of the same phase in England. 
                                        To facilitate comparison the selected LA's relevant datum is represented by a blue tick below the x-axis.")
                                      )
                                      
                                      
                        ),
           
tabPanel("Data explorer", div(h3("Schools' details for the Local Authority of ", textOutput("la_name"))),
         br(),
         DT::dataTableOutput("fruit_table_data"),
         br(),
         h4("Table variables explained"),
         p("The variables range from between zero and one.
           The ", strong("apples"), "is simulated by the ", strong("uniform"), "distribution.",
           "The ", strong("pears"), " are also drawn from a uniform distribution.",
           "The ", strong("cherry_status"),  " is calculated by the ", strong("apples"),
           " multiplied by the ", strong("pears"), ".",
           "This statistic (", strong("cherry_status") ,
" provides an indication of the amount of spurious correlation between the two variables.
           One could cherry pick those Schools with a ", strong("cherry_status"),
           " to provide evidence for a spurious claim (e.g. eating lots of apples causes students to demand more pears).",
           style = "font-family: 'times'; font-si16pt"),
         downloadButton("download_data", "Download"),
         br(),
         p("ISSUE: Following file download you may have to restart the app.", 
           style = "font-family: 'times'; font-si16pt")
         ),

tabPanel("Fruit limit",
         h4("Schools selected from Data explorer tab"),
         DT::dataTableOutput("green_grocers"),
         plotOutput("scatter_fruit", height = 200),
         h4("How many items of fruit required to feed the Schools of interest?"),
         p("This app was developed to showcase a Shiny app in R using the Leaflet package. Use your imagination, make a map!",
           style = "font-family: 'times'; font-si16pt")
         ),


tabPanel("Data and methods",
         h3("Data origin"),
         p("This app combines data from various sources,
we join data from three seperate csv files using School URN.
Inspect the App data folder for  the .csv files. 
The LA polygon data can be found on the ",
           a(href = "https://www.ordnancesurvey.co.uk/business-and-government/products/boundary-line.html", "Ordnance Survey"), 
           " website or in the App repository.",
           style = "font-family: 'times'; font-si16pt"), 
         br(),
         h4("Mapping"),
         p("The linking of Secondary Schools' in England
         data was achieved using their Unique Reference Number (URN). The location of each School was provided by ",
           a(href = "http://www.education.gov.uk/edubase/home.xhtml", "Edubase."),
           "The data was downloaded and tidied prior to use for your convenience.",
           style = "font-family: 'times'; font-si16pt"),
         br(),
         h4("Apples"),
         p("Generated in Excel using RAND(). Often one receives data as a csv. ",
           "You can put some extra information here and share any relevant methodology with hyperlinks.",
           style = "font-family: 'times'; font-si16pt"),
         br(),
         h4("Pears"),
         p("Generated in Excel using RAND(). Often one receives data as a csv.",
           style = "font-family: 'times'; font-si16pt"),
         br(),
         h4("Complete cases"),
         p("Only complete cases were used,
           i.e. where a School had corresponding apples, pears and mapping data.",
           style = "font-family: 'times'; font-si16pt"),
         br()),
###
tags$a(img(src = "mg_logo.png", height = 144, width = 144),
       href = "http://www.machinegurning.com/posts/"),
# img(src = "mg_logo.png", height = 144, width = 144,
    # href = "http://www.machinegurning.com/posts/"),
br(),
tags$div(id = "cite",
         'App developed by ',
         a(href = "https://github.com/mammykins/App-cherry_picker", "Dr Matthew Gregory"), "."
), 
tags$blockquote("Correlation does not imply causation.", cite = "Anon.")


         
)
