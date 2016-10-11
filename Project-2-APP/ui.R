## ui
shinyUI(
  navbarPage(
  "NYC Oil Consumption Analysis", theme = "bootstrap.css",
  id = 'nav',
  tabPanel("Oil Consumption Map", 
           div(class="outer",
               tags$head(includeCSS("styles.css")),
               leafletOutput("map", width = "100%", height = "100%"),
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 60, left = 10, 
                             bottom = "auto", width = 330, height = "auto",
                             h3("Building Variables"),
                             sliderInput("number", label = "Number of Buildings on Map",
                                         500, 7900, value = 500, step = 100),
                             selectInput("buildtype", "Type of Building:",
                                         choices = c("All",levels(data[,12])[-1]), selected = "All"),
                             sliderInput("year", "Year Constructed", 
                                         1848, 2009, value = range(1848:2009), step = 5),
                             sliderInput("people", "Number of total units", 
                                         0, 2400, value = range(0:2400), step = 10)
               ),
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 60, right = 10, 
                             bottom = "auto", width = 330, height = "auto",
                             h3("Boiler Variables"),
                             selectInput("fueltype", "Type of Fuel:",
                                         choices = c("All", "#4", "#6"), selected = "All"),
                             checkboxInput("legend1", "DUAL FUEL", F),
                             checkboxInput("legend2", "Do Not Need to Comply with GGB Laws", F),
                             sliderInput("age", "Age of Boiler", 
                                         0, 55, value = range(0:55), step = 5),
                             sliderInput("boilers", "Number of Boiler", 
                                         1, 7, value = range(1:7), step = 1),
                             sliderInput("range", "Total Estimated Oil consumption", 
                                         min = 0, max = 1100000,
                                         value = range(0:1100000),
                                         step = 10000))
           )
  ),
  
  tabPanel("Scatter Plot",
           sidebarPanel(width = 3,
                        selectInput("fueltype3", "Type of Fuel:",
                                    choices = c("All", "#4", "#6"), selected = "All"),
                        
                        checkboxInput("legend13", "DUAL FUEL", F),
                        checkboxInput("legend23", "Do Not Need to Comply with GGB Laws", F),
                        
                        sliderInput("age3", "Age of Boiler", 
                                    0, 55,
                                    value = range(0:55),
                                    step = 5)
                        ),
           mainPanel(
             plotlyOutput("plot")
             )
  ),
  
  tabPanel("Histgram",
             sidebarPanel(width = 3,
                          
                          checkboxGroupInput("Green",
                                             label = h4('Green Law Compliant'),
                                             choices = c("Yes" = 'Yes', 'No' = ''),
                                             select = c('Yes', '')),
                          
                          checkboxGroupInput("Fuel_Type",
                                             label = h4('Fuel Type'),
                                             choices = c("#4" = '#4', '#6' = '#6'),
                                             select = c('#4', '#6')),
                          
                          checkboxGroupInput("Dual",
                                             label = h4('Dual Fuel'),
                                             choices = c("Yes" = 'DUAL FUEL', 'No' = ' '),
                                             select = c(' ', 'DUAL FUEL')),
                          
                          sliderInput('No_Floors',
                                      label = h4('No. of Floors'),
                                      min = min(oil$No_Floors), 
                                      max = max(oil$No_Floors),
                                      value = c(min(oil$No_Floors), max(oil$No_Floors)))),      
             
             mainPanel(
               h4('Average Gallon Consumption (Low Estimate) by Total Area', 
                  align = 'center'),
               plotOutput('Total_Area'),
               
               br(),
               h4('Average Gallon Consumption (Low Estimate) by Boiler Age', 
                  align = 'center'),
               plotOutput('Boiler_Age')
             )
    
  ),

  tabPanel("Bar Chart",
           plotlyOutput("buildingtype"),
           #verbatimTextOutput("info")
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                         width = 300, height = "auto",
                         
                         selectInput("fueltype2", "Type of Fuel:",
                                     choices = c("All", "#4", "#6"), selected = "All"),
                         
                         checkboxInput("legend12", "DUAL FUEL", F),
                         checkboxInput("legend22", "Do Not Need to Comply with GGB Laws", F),
                         
                         sliderInput("age2", "Age of Boiler", 
                                     0, 55,
                                     value = range(0:55),
                                     step = 5),
                         sliderInput("boilers2", "Number of Boiler", 
                                     1, 7,
                                     value = range(1:7),
                                     step = 1),
                         sliderInput("year2", "Year Constructed", 
                                     1848, 2009,
                                     value = range(1848:2009),
                                     step = 5),
                         sliderInput("people2", "Number of total units", 
                                     0, 2400,
                                     value = range(0:2400),
                                     step = 10))
  ),
  
  tabPanel("Data Table",
           sidebarPanel(
             checkboxInput("add",label="Facility Address",value=T),
             checkboxInput("nga",label="Natural Gas Utility",value=F),
             checkboxInput("noboil",label="Number of Boilers",value=T),
             checkboxInput("bcap",label="Boiler Capacity",value=T),
             checkboxInput("binstal",label="Boiler Installation Date",value=F),
             checkboxInput("dualboil",label="Boiler Type (dual or not)",value=F),
             checkboxInput("ageboil",label="Age range of boiler",value=T),
             checkboxInput("primfuel",label="Primary Fuel",value=F),
             checkboxInput("lcons",label="Total Est. Oil Consumption",value=T),
             checkboxInput("comply",label="Need to comply with GGB laws",value=F),
             checkboxInput("btype",label="Building Type",value=T),
             checkboxInput("blot",label="Total area of buildings",value=T),
             checkboxInput("nof",label="Number of floor",value=T),
             checkboxInput("nou",label="Number of Total Units",value=T),
             checkboxInput("yrc",label="Year Constructed",value=F)
           ),  
           mainPanel(
             fluidRow(
               column(4,
                      selectInput("ngasu",
                                  "Natural Gas Utility:",
                                  c("All",
                                    unique(as.character(oil1$`Natural Gas Utility`))))
               ),
               column(4,
                      selectInput("ages",
                                  "Age range of Boilder:",
                                  c("All",
                                    unique(as.character(oil1$`Age of Boiler`))))
               ),
               column(4,
                      selectInput("prifuel",
                                  "Primary Fuel:",
                                  c("All",
                                    unique(as.character(oil1$`Primary Fuel`))))
               )
             ),
             # Create a new row for the table.
             fluidRow(
               dataTableOutput("table")
             )
           )
  )
  )
)
