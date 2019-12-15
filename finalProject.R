library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(spdplyr)
library(rgdal)
library(tidyverse)
library(ggplot2)
library(DT)

#Import Datasets-All
census <- readOGR(dsn="2010_CensusData", layer = "2010_CensusData", stringsAsFactors = FALSE)
lights <- read.csv('Street_Lights.csv')
parks <- read.csv('Parks_Locations_and_Features.csv')
pubFac <- read.csv('Public_Facilities.csv') 
enforce <- read.csv('Code_Enforcement_Cases.csv')

#Data Import-Farhad
############# Districts ##############
districts_spatial <- readOGR(dsn = "City_Council_Districts/City_Council_Districts.shp", 
                             stringsAsFactors = FALSE)
district_spatial_point <- SpatialPointsDataFrame(coords = coordinates(districts_spatial), 
                                                 data = districts_spatial@data,
                                                 proj4string = CRS(proj4string(districts_spatial)))
names(districts_spatial@data)[3] <- "District"
districts_spatial$popup <- paste("<b>", "District: ", districts_spatial$District,"</b><br>",
                                 "Representative: ", districts_spatial$Council_Me, "<br>", 
                                 "Email: ",districts_spatial$Email, sep ="")

############# Parks ##############
parks_spatial <- SpatialPointsDataFrame(coords = parks[,c("Lon","Lat")], data = parks,
                                        proj4string = CRS(proj4string(districts_spatial)))
parks_ov <- over(parks_spatial,districts_spatial)
parks_spatial@data$District <- parks_ov$District
parks_table <- parks_spatial@data %>% select("Park Name" = Park_Name, 
                                             "Park Type" = Park_Type, Address, District)

############# Facilities ##############
facilities <- pubFac
facilities_spatial <- SpatialPointsDataFrame(coords = facilities[,c("Lon","Lat")], data = facilities,
                                             proj4string = CRS(proj4string(districts_spatial)))
facilities_ov <- over(facilities_spatial,districts_spatial)
facilities_spatial@data$District <- facilities_ov$District
facilities_table <- facilities_spatial@data %>% select("Facility Name" = POPL_NAME, 
                                                       "Facility Type" = POPL_TYPE,
                                                       Address = POPL_ADDR1, Phone = POPL_PHONE, District)
facilities_table$Address <- str_split(facilities_table$Address, "\\n", simplify = T)[,1]

############# Enforcment Cases ##############
EnfCases <- enforce %>% filter(Case_Year == 14)
EnfCases_spatial <- SpatialPointsDataFrame(coords = EnfCases[,c("Lon","Lat")], data = EnfCases,
                                           proj4string = CRS(proj4string(districts_spatial)))
EnfCases_ov <- over(EnfCases_spatial,districts_spatial)
EnfCases_spatial@data$District <- EnfCases_ov$District
EnfCases_table <- EnfCases_spatial@data %>%
  group_by(District, "Case Type" = Case_Type_Code_Description) %>%
  summarize(Cases = n())
EnfCases_ov <- EnfCases_table %>%
  group_by(District) %>%
  summarize(Total_Cases = sum(Cases))
names(EnfCases_table)[3] <- "Number of Cases"

############# Schools ##############
schools_boundaries <- readOGR(dsn = "School_Boundaries/School_Boundaries.shp", 
                              stringsAsFactors = FALSE)
schools_spatial <- SpatialPointsDataFrame(coords = coordinates(schools_boundaries), 
                                          data = schools_boundaries@data,
                                          proj4string = CRS(proj4string(districts_spatial)))
schools_ov <- over(schools_spatial,districts_spatial)
schools_spatial@data$District <- schools_ov$District
#SchoolAddress <- apply(coordinates(schools_boundaries), 1, revgeocode )
#saveRDS(SchoolAddress, file = "SchoolAddress.Rds")
SchoolAddress <- readRDS(file = "SchoolAddress.Rds")
schools_spatial$SchoolAddress <- SchoolAddress
schools_table <- schools_spatial@data %>% select(School, "School Type" = SchoolType, Address = SchoolAddress, District)

############# Abandoned Properties ##############
abandoned_property <- readOGR(dsn = "Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp", 
                              stringsAsFactors = FALSE)
abandoned_spatial <- SpatialPointsDataFrame(coords = coordinates(abandoned_property), 
                                            data = abandoned_property@data,
                                            proj4string = CRS(proj4string(districts_spatial)))
abandoned_ov <- over(abandoned_spatial,districts_spatial)
abandoned_spatial@data$District <- abandoned_ov$District
abandoned_table <- abandoned_spatial@data %>% select(Street_Nam, Suffix, Address_Nu, 
                                                     Structures, Status = Outcome_St, District)
abandoned_table <-  abandoned_table %>% unite("Property Address", Address_Nu, 
                                              Street_Nam, Suffix, sep = " ")

############# Census ##############
census_boundaries <- census
census_spatial <- SpatialPointsDataFrame(coords = coordinates(census_boundaries), 
                                         data = census_boundaries@data,
                                         proj4string = CRS(proj4string(districts_spatial)))
census_ov <- over(census_spatial,districts_spatial)
census_spatial@data$District <- census_ov$District
census_table <- census_spatial@data %>%
  drop_na() %>%
  mutate_all(as.numeric) %>% 
  group_by(District = as.character(District)) %>%
  summarize("Population" = sum(SE_T001_00),
            Male = sum(SE_T003_01),
            Female = sum(SE_T003_02),
            "Area Sq Miles" = sum(SE_T02A_00),
            "Number of Housing Unit" = sum(SE_T068_00))
#clean Parks Data
parks[is.na(parks)] <- 0
Parks.spatial <- SpatialPointsDataFrame(coords = parks[,c("Lon","Lat")], data =parks,
                                        proj4string = CRS("+proj=longlat +datum=WGS84"))
census$SE_T002_02<-as.integer(census$SE_T008_06)

#Create UI
ui <- navbarPage("South Bend Explorer", theme=shinytheme("flatly"),
                 #Data Explorer Tab-Farhad
                 tabPanel("District Analysis",
                                   h3("South Bend Districts By Farhad Salemi"),
                                   fluidRow(
                                     column(3,
                                            wellPanel(
                                              selectInput("district", label = h3("District:"), 
                                                          choices = list("ALL" = 0, 
                                                                         "1301" = 1, 
                                                                         "1302" = 2, 
                                                                         "1303" = 3, 
                                                                         "1304" = 4, 
                                                                         "1305" = 5, 
                                                                         "1306" = 6)), 
                                              imageOutput("image", height = "100%"),
                                              hr(),
                                              htmlOutput("info")
                                            )
                                     ),
                                     column(9,
                                            leafletOutput(outputId = "map")
                                     )
                                   ),
                                   h3("Districts Statistical Reports"),
                                   fluidRow(
                                     column(3,
                                            wellPanel(
                                              selectInput("parameter", label = h3("Parameter:"),
                                                          choices = list("Census" = 1,
                                                                         "Schools" = 2, 
                                                                         "Parks" = 3, 
                                                                         "Enforcement Case Summaries" = 4, 
                                                                         "Abandoned Properties" = 5, 
                                                                         "Public Facilities" = 6))
                                            )
                                     ),
                                     column(3,
                                            plotOutput(outputId = "plot")
                                     ),
                                     column(6,
                                            dataTableOutput(outputId = "table")
                                     )
                                   )
                          ),

                 #Lights Tab-Joyce
                 tabPanel("Lights Focus",
                          sidebarPanel(
                            selectInput("lightsInput", "Light Ownership",
                                        unique(lights$Ownership))
                          ),
                          mainPanel(
                            leafletOutput("lightsMap"),
                            br(), br(),
                            tableOutput("lightsResults"))
                          
                          
                 ),
                 
                 #Parks Tab-Neil
                 tabPanel("Parks Focus",
                          sidebarPanel(
                            selectInput("type_select","Select Park Type",Parks.spatial$Park_Type),
                            selectInput("bball_select","Select Basketball Court",Parks.spatial$Basketball),
                            selectInput("swim_select","Select Swimming Pools",Parks.spatial$Aqua_Feat__Pool),
                            actionButton("add","Add")
                          ),
                          mainPanel(
                            leafletOutput("parksMap"),
                            br(), br(),
                            tableOutput("parkResults"))
                          
                 ),
                 
                 #Facilities Tab-Brandon
                 tabPanel("Facilities Focus",
                          sidebarPanel(
                            selectInput("facInput", "Facilities Type",
                                        unique(pubFac$POPL_TYPE))
                          ),
                          mainPanel(
                            leafletOutput("facMap"),
                            br(), br(),
                            tableOutput("facResults"))
                 )
                 
)

#Create Server
server = function(input, output) {
  output$info <- renderText({
    if(as.integer(input$district) != 0){
      paste(#"<b>", "District: ", "</b>", districts_spatial$District[as.integer(input$district)],"<br>",
        "<b>", "Representative: ", "</b>", districts_spatial$Council_Me[as.integer(input$district)], "<br>",
        "<b>", "Email: ", "</b>", districts_spatial$Email[as.integer(input$district)])
    }
  })
  #Create District Analysis-Farhad
  output$image <- renderImage({
    return(list(
      src = paste0(input$district, ".jpg"), height= "15%", width= "40%"#, align="buttom"
    ))
  }, deleteFile = FALSE)
  
  dataset <- eventReactive(input$parameter,{
    i <- as.integer(input$parameter) 
    if (i == 1) return(list(census_table, "Total Population", census_table, census_table$Population, "identity", district_spatial_point))
    if (i == 2) return(list(schools_ov, "Number of Schools", schools_table, NULL, "count", schools_spatial))
    if (i == 3) return(list(parks_ov, "Number of Parks", parks_table, NULL, "count", parks_spatial))
    if (i == 4) return(list(EnfCases_ov, "Number of Enforcement Cases", EnfCases_table, EnfCases_ov$Total_Cases, "identity", EnfCases_spatial))
    if (i == 5) return(list(abandoned_ov, "Number of Abandoned Properties", abandoned_table, NULL, "count", abandoned_spatial))
    if (i == 6) return(list(facilities_ov, "Number of Facilities", facilities_table, NULL, "count", facilities_spatial))
  })
  
  data <- eventReactive(input$district,{
    i <- as.integer(input$district) 
    if (i == 0) {
      District <- districts_spatial$District
      districtColor <- colorFactor("Blues", District)(District)
      return(list(districts_spatial, districtColor))
    }
    districtColor <- "red"
    return(list(districts_spatial[i,], districtColor))
  })
  
  output$map <-  renderLeaflet({
    myMap <- leaflet()%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addTiles(group = "Basic") %>%
      addPolygons(data = data()[[1]], popup = ~popup, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~data()[[2]], 
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  group = "Districts")  
    if(as.integer(input$parameter) != 1)
      myMap <- myMap %>% addCircleMarkers(data = dataset()[[6]][data()[[1]],], stroke = 0, color = "navy",
                                          fillOpacity = .6, radius = 6, group = "Markers") #%>%
    myMap %>% 
      addLayersControl(
        baseGroups = c("Basic", "Satellite"),
        overlayGroups= c("Districts", "Markers"),
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomright") %>%
      #         hideGroup("Markers") %>% 
      setView(-86.25, 41.68, zoom = 11.4)
  })
  
  output$plot <- renderPlot({
    if(dataset()[[5]] == "identity"){  
      ggplot(drop_na(dataset()[[1]], District), aes(District, dataset()[[4]])) +
        geom_bar(fill = "navy", stat = dataset()[[5]]) +
        geom_text(aes(label=dataset()[[4]]), vjust=-.5) +
        labs(y=dataset()[[2]], 
             x="South Bend Districts", 
             title=paste0(dataset()[[2]], " per Districts")) +
        theme(plot.title = element_text(hjust = 0.5, size = 17))
    } 
    else{   
      ggplot(drop_na(dataset()[[1]], District), aes(District)) +
        geom_bar(fill = "navy", stat = dataset()[[5]]) +
        geom_text(stat='count', aes(label=..count..), vjust=-.5) +
        labs(y=dataset()[[2]], 
             x="South Bend Districts", 
             title=paste0(dataset()[[2]], " per Districts")) +
        theme(plot.title = element_text(hjust = 0.5, size = 17))
    }  
  })
  
  output$table <- renderDataTable({
    datatable(semi_join(dataset()[[3]], data()[[1]]@data), options = list(pageLength = 7))
  })
  
  
  
  #Create Lights Map-Joyce
  output$lightsMap <- renderLeaflet({
    filteredLights <- lights %>% filter(Ownership==input$lightsInput)
    leaflet(filteredLights) %>% 
      addTiles() %>%
      addPolygons(data=census,color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorQuantile("Blues",SE_T002_02 )(SE_T002_02),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),popup = ~paste("Population Density: ",as.character(SE_T002_02)))%>%
      addMarkers(lat=lights$Lat, lng=lights$Lon)
  })
  
  #Create Lights Table-Joyce
  output$facResults <- renderTable({
    filteredLights <- lights %>% filter(POPL_TYPE==input$Ownership) %>% select(OBJECTID, Ownership,	Pole_Num_1,	Bulb_Type, Wattage)
    filteredLights
  })
  
  
  
  #Create Parks Map-Neil
  points<-eventReactive(input$add,{Parks.spatial%>%filter(Parks.spatial$Park_Type==input$type_select,Parks.spatial$Basketball==input$bball_select
                                                          ,Parks.spatial$Aqua_Feat__Pool==input$swim_select)},ignoreNULL = FALSE)
  output$parksMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data=census,color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorQuantile("Blues",SE_T002_02 )(SE_T002_02),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),popup = ~paste("Population Density: ",as.character(SE_T002_02)))%>%
      addMarkers(data=points(),popup= ~paste("Name: ",Park_Name,"<br>","Basketball Courts",Basketball,"<br>","Park Type:",Park_Type))
  })
  
  #Create Parks Table-Neil
  output$results <- renderTable({
    points<-eventReactive(input$add,{Parks.spatial%>%filter(Parks.spatial$Park_Type==input$type_select,Parks.spatial$Basketball==input$bball_select
                                                            ,Parks.spatial$Aqua_Feat__Pool==input$swim_select)},ignoreNULL = FALSE)
    points
  })
  
  
  #Create Facilities Map-Brandon
  output$facMap <- renderLeaflet({
    filteredFac <- pubFac %>% filter(POPL_TYPE==input$facInput)
    leaflet(filteredFac) %>% 
      addTiles() %>%
      addPolygons(data=census,color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorQuantile("Blues",SE_T002_02 )(SE_T002_02),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),popup = ~paste("Population Density: ",as.character(SE_T002_02)))%>%
      addMarkers(lat=filteredFac$Lat, lng=filteredFac$Lon)
  })
  
  #Create Facilities Table-Brandon
  output$facResults <- renderTable({
    filteredFac <- pubFac %>% filter(POPL_TYPE==input$facInput) %>% select(POPL_NAME, POPL_TYPE, POPL_CITY, POPL_STATE,	POPL_ZIP)
    filteredFac
  })
  
}


# Run the application-All
shinyApp(ui = ui, server = server)

