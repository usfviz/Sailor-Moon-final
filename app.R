# set working directory to source file location
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
cat("\014")

#======================================== packages =============================================
packageList = c('geosphere', 'tidyverse', 'plotly', 'devtools', 'hrbrthemes', 'shiny', 'GGally', 'sp', 'rworldmap', 'tidyr', 'dplyr', 'googleVis', 'countrycode', 'leaflet', 'magrittr', 'maps')
for (i in 1:length(packageList)) {
  if(! is.element(packageList[i],installed.packages()[,1])) {
    install.packages(packageList[i])
  }
}

if ("trelliscopejs" %in% rownames(installed.packages())==F) {
  devtools::install_github("hafen/trelliscopejs")}

if ("gdeltr2" %in% rownames(installed.packages())==F) {
  devtools::install_github("abresler/gdeltr2")}
library(gdeltr2)
library(shiny)
library(GGally)
library(sp)
library(rworldmap)
library(tidyr)
library(dplyr)
library(googleVis)
library(countrycode)
library(leaflet)
library(magrittr)
library(maps)
library(geosphere)

#======================================== functions =============================================

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  #indices$continent   # returns the continent (6 continent model)
  indices$REGION   # returns the continent (7 continent model)
  #indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}

#======================================== shiny UI =============================================

ui <- navbarPage("GDELT: Project Sailor Moon",
                 
                 #======================================== Map =============================================
                 tabPanel("World Map",
                          sidebarPanel(
                            dateRangeInput("date_range_map", "Date range:",
                                           start = Sys.Date() - 2,
                                           end = Sys.Date() - 2,
                                           min = "2013-04-01",
                                           max = Sys.Date() - 2
                            ),
                            checkboxGroupInput("type", "Event Types", choiceValues=c("ARMEDCONFLICT", "EVACUATION", "SICKENED", "VIOLENT_UNREST","RELEASE_PRISON"), 
                                               choiceNames = list(HTML("<p><img src='http://icons.iconarchive.com/icons/icons8/ios7/32/Military-Rifle-icon.png' width=25, height=25 /><font color='green'> Armed Conflict</font></p>"),
                                                                  HTML("<p><img src='http://icons.iconarchive.com/icons/icons8/ios7/32/User-Interface-Exit-icon.png'width=25, height=25 /><font color='orange'> Eevacuation</font></p>"),
                                                                  HTML("<p><img src='http://icons.iconarchive.com/icons/icons8/ios7/32/Healthcare-Hospital-3-icon.png' width=25, height=25 /><font color='purple'> Sickened</font></p>"),
                                                                  HTML("<p><img src='http://icons.iconarchive.com/icons/icons8/ios7/32/Cinema-Action-icon.png' width=25, height=25 /><font color='red'> Violent Unrest</font></p>"),
                                                                  HTML("<p><img src='http://icons.iconarchive.com/icons/icons8/windows-8/32/City-Handcuffs-icon.png' width=25, height=25 /><font color='blue'> Release Prison</font></p>")),
                                               selected = c("ARMEDCONFLICT", "EVACUATION"))
                          ),
                          mainPanel(
                            leafletOutput("mymap", height=600)
                          )
                 ),
                 
                 #======================================== Radial Map =============================================
                 tabPanel("Interconnectedness Map",
                          sidebarPanel(
                            dateRangeInput("date_range_map2", "Date range:",
                                           start = Sys.Date() - 2,
                                           end = Sys.Date() - 2,
                                           min = "2013-04-01",
                                           max = Sys.Date() - 2
                            ),
                            checkboxGroupInput("cty", "Country or Region", choiceValues=c("CH", "SY", "IZ", "TW"), 
                                               choiceNames = list(HTML("<p><img src='http://icons.iconarchive.com/icons/custom-icon-design/all-country-flag/48/China-Flag-icon.png' width=25, height=25 /> China</p>"),
                                                                  HTML("<p><img src='http://icons.iconarchive.com/icons/custom-icon-design/all-country-flag/48/Syria-Flag-icon.png'width=25, height=25 /> Syria</p>"),
                                                                  HTML("<p><img src='http://icons.iconarchive.com/icons/custom-icon-design/all-country-flag/48/Iraq-Flag-icon.png' width=25, height=25 /> Iraq</p>"),
                                                                  HTML("<p><img src='http://icons.iconarchive.com/icons/custom-icon-design/all-country-flag/48/Taiwan-Flag-icon.png' width=25, height=25 /> Taiwan</p>")),
                                               selected = c("CH", "SY")),
                            checkboxGroupInput("type2", "Event Type", choiceValues=c("KILL", "ARREST", "AFFECT", "OTHERS"), 
                                               choiceNames = list(HTML("<p><img src='http://www.iconsdb.com/icons/download/color/C9FFFB/square-rounded-24.ico' width=20, height=20 /> Kill</p>"),
                                                                  HTML("<p><img src='http://www.iconsdb.com/icons/download/color/FFD6F7/square-rounded-24.ico' width=20, height=20 /> Arrest</p>"),
                                                                  HTML("<p><img src='http://www.iconsdb.com/icons/download/color/986DCF/square-rounded-24.ico' width=20, height=20 /> Affect</p>"),
                                                                  HTML("<p><img src='http://www.iconsdb.com/icons/download/color/79E9ED/square-rounded-24.ico'width=20, height=20 /> Others</p>")),
                                               selected = c("KILL", "ARREST", "AFFECT", "OTHERS")),
                            sliderInput("size", "Line Width", min = 1, max = 20, value = 3, ticks = FALSE)
                          ),
                          mainPanel(
                            plotOutput("mymap2")
                          )
                 ),
                 
                 
                 #======================================== sankey plot =============================================  
                 tabPanel("Sankey Plot",
                          sidebarLayout(
                            sidebarPanel(
                              dateRangeInput("date_range_sankey", "Date range:",
                                             start = Sys.Date() - 2,
                                             end = Sys.Date() - 2,
                                             min = "2013-04-01",
                                             max = Sys.Date() - 2
                              ),
                              radioButtons("end", "Sankey Plot Endpoints:",
                                           c("Continents", "Countries", "Continents + Countries", "United States")
                              ),
                              selectizeInput("type_sankey", "Select News Type", 
                                             c('KILL', 'ARREST', 'PROTEST', 'WOUND', 'AFFECT', 'CRISISLEX_T03_DEAD',
                                               'CRISISLEX_CRISISLEXREC', 'CRISISLEX_T02_INJURED', 'CRISISLEX_C07_SAFETY',
                                               'CRISISLEX_C03_WELLBEING_HEALTH'),
                                             selected = c('KILL', 'ARREST', 'PROTEST', 'WOUND', 'AFFECT'),
                                             multiple = TRUE
                              )
                              # img(src='sailor_moon.png', align = "left")
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                # tabPanel("Sankey Plot", verbatimTextOutput("text")),
                                tabPanel("Sankey Plot", htmlOutput("sankey", inline = TRUE))
                              )
                            )
                          )
                 ),
                 
                 #======================================== timeline plot =============================================  
                 tabPanel("Timeline Plot",
                          sidebarLayout(
                            sidebarPanel(
                              dateInput("date_timeline", "Date:", value = "2017-02-01",
                                        min = "2009-06-04", max = "2017-02-01"
                              ),
                              selectizeInput("type_timeline", "Select News Type", 
                                             c('BLOOMBERG', 'CNBC', 'CNNW', 'COM', 'CSPAN2', 'CSPAN3', 'CSPAN', 'FBC', 'FOXNEWSW', 'KCSM',
                                               'KGO', 'KNTV', 'KOFY', 'KPIX', 'KQED', 'KQEH', 'KRON', 'KTVU', 'KYW', 'LINKTV', 'MSNBCW',
                                               'SFGTV', 'WCAU', 'WJLA', 'WPVI', 'WRC', 'WTTG', 'WTXF', 'WUSA'
                                             ),
                                             selected = c('BLOOMBERG', 'CNBC', 'FOXNEWSW', 'MSNBCW', 'CSPAN'),
                                             multiple = TRUE
                              )
                              # img(src='sailor_moon.png', align = "left")
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                # tabPanel("Sankey Plot", verbatimTextOutput("text")),
                                tabPanel("TV News Timetable", htmlOutput("tv", inline = TRUE))
                              )
                            )
                          )
                 )
                 
                 
)




#======================================== shiny server =============================================
server <- function(input, output) {
  
  #======================================== Map =============================================
  data <- reactive({
    dates <- as.character(seq(input$date_range_map[1], input$date_range_map[2], by="days"))
    
    gkg_map <-
      get_data_gkg_days_summary(
        dates = dates,
        is_count_file = T,
        return_message = T
      )
    
    gkg_map <- data.frame(gkg_map)
    data <- gkg_map[c('countArticles', 'typeEvent', 'countObject',
                      'idCountry', 'latitude', 'longitude', 'sources',
                      'urlSources', 'location', 'typeObject')]
    
    for (col in c('typeEvent', 'idCountry')) {
      data[col] <- lapply(data[col], factor)
    }
    data <- subset(data, data$typeEvent %in% c("ARMEDCONFLICT", "EVACUATION", "SICKENED", "VIOLENT_UNREST","RELEASE_PRISON"))
    data$typeEvent <- droplevels(data$typeEvent)
    data
  })
  
  
  output$mymap <- renderLeaflet({
    df <- subset(data(), data()$typeEvent %in% input$type)
    getColor <- function(quakes) {
      sapply(df$typeEvent, function(typeEvent) {
        if(typeEvent == "ARMEDCONFLICT") {
          "green"
        } else if(typeEvent == "EVACUATION") {
          "orange"
        } else if(typeEvent == "SICKENED") {
          "purple"
        } else if(typeEvent == "RELEASE_PRISON") {
          "blue"
        } else if(typeEvent == "VIOLENT_UNREST"){
          "red"
        } })
    }
    icons <- awesomeIcons(
      icon = 'ios-close',
      library = 'ion',
      markerColor = getColor(df)
    )
    m <- leaflet(df) %>% addProviderTiles(providers$Esri.WorldTopoMap)
    if (!is.null(input$type)) {
      m <- m %>% addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon=~icons,
                                   label= ~typeEvent,
                                   popup = ~paste("<b> Object: ", typeObject, "</b>",
                                                  "<br><b> Location: ", location, "</b>",
                                                  "<br><b> Source: ", sources, "</b>",
                                                  "<br><a><href>",  urlSources, "</a>"))
    }
    m
  })
  
  
  #======================================== Radial Map =============================================
  
  data2 <- reactive({
    dates <- as.character(seq(input$date_range_map2[1], input$date_range_map2[2], by="days"))
    
    gkg_map <-
      get_data_gkg_days_summary(
        dates = dates,
        is_count_file = T,
        return_message = T
      )
    
    gkg_map <- data.frame(gkg_map)
    data <- gkg_map[c('typeEvent', 'idCountry', 'latitude', 'longitude')]
    
    data <- subset(data, data$idCountry %in% c("CH", "SY", "IZ", "IN", "TW", "IS", "UP", "BG", "AF",
                                               "GM", "KE", "AM", "BH", "BD", "BT", "HK", "IR", "NP"))
    data$typeEvent[!(data$typeEvent %in% c("KILL", "ARREST", "AFFECT"))] <- "OTHERS"
    data
  })
  
  output$mymap2 <- renderPlot({
    df <- subset(data2(), data2()$typeEvent %in% input$type2)
    map("world",  fill=T, col="grey27", bg="grey3", xlim=c(5, 135), ylim = c(0, 60))
    for (cty in input$cty) {
      loc <- NULL
      if (cty == "SY") {
        loc <- c(38, 35)
      } else if (cty == "CH") {
        loc <- c(105, 35)
      } else if (cty == "IZ") {
        loc <- c(44, 44)
      } else if (cty == "TW") {
        loc <- c(121, 23.5)
      } 
      if (!is.null(loc)) {
        for (i in (1:dim(df)[1])) { 
          inter <- gcIntermediate(loc, c(df$longitude[i], df$latitude[i]), n=50)
          if (df$typeEvent[i] == "KILL"){
            lines(inter, lwd=0.02*input$size, col="lightskyblue2")
          } else if (df$typeEvent[i] == "ARREST") {
            lines(inter, lwd=0.02*input$size, col="pink2")
          } else if (df$typeEvent[i] == "AFFECT") {
            lines(inter, lwd=0.02*input$size, col="blueviolet")
          } else {
            lines(inter, lwd=0.02*input$size, col="paleturquoise") 
          }
          points(df$longitude[i], df$latitude[i], pch=1*input$size, cex=0.1, col="lightyellow")
        }
      }
    }
  })
  
  
  #======================================== sankey plot =============================================  
  # Reactive
  
  # Date
  gkg_summary <- reactive({
    
    dates <- as.character(seq(input$date_range_sankey[1], input$date_range_sankey[2], by="days"))
    # dates <- c('2017-03-29', '2017-03-30')
    gkg_summary <- get_data_gkg_days_summary(dates = dates, is_count_file = T, return_message = F) %>% 
      drop_na(longitude, latitude)
    
    gkg_summary$continent <- coords2continent(gkg_summary[,c('longitude', 'latitude')])
    gkg_summary$country <- countrycode(gkg_summary$idCountry, "iso2c", "country.name")
    
    gkg_summary %>%
      subset(!is.na(latitude)) %>%
      subset(!is.na(longitude)) %>%
      filter(typeLocation %in% c('country', 'usState', 'usCity'))
  })
  
  
  df <- reactive ({
    gkg_summary() %>%
      filter(typeEvent %in% input$type_sankey)
  })
  
  # End
  df2 <- reactive ({
    if (input$end == "Continents") {
      df() %>% 
        group_by(typeEvent, continent) %>%
        summarize(count = n()) %>% 
        na.omit()
    } else if (input$end == "Countries") {
      df() %>% 
        group_by(typeEvent, country) %>%
        summarize(count = n()) %>% 
        na.omit()
    } else if (input$end == "Continents + Countries") {
      df1 <-  df() %>% 
        group_by(typeEvent, continent) %>%
        summarize(count = n()) %>% 
        na.omit()
      
      df3 <- df() %>% 
        group_by(typeEvent, continent, country) %>%
        summarize(count = n()) %>% 
        subset(select = c("continent", "country", "count")) %>% 
        na.omit()
      
      colnames(df1) <- c("from", "to", "count")
      colnames(df3) <- c("from", "to", "count")
      df13 <- rbind(data.frame(df1), data.frame(df3))
      df13
      
    } else if (input$end == "United States") {
      df() %>% 
        filter(idCountry=='US') %>% 
        group_by(typeEvent, idADM1CodeAction) %>%
        summarize(count = n()) %>% 
        na.omit()
    }
  })  
  
  # Output
  # output$text <- renderText(df())
  
  output$sankey <- renderGvis(gvisSankey(df2(), from=colnames(df2())[1],
                                         to=colnames(df2())[2], weight=colnames(df2())[3],
                                         options=list(height=2000, 
                                                      width=1500,
                                                      sankey = "{
                                                      node: {
                                                            colors: ['#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f', '#cab2d6', '#ffff99', '#1f78b4', '#33a02c'],
                                                            label: {fontName: 'Times-Roman', color: '#871b47', fontSize: 16, bold: true, italic: true}
                                                            },
                                                      link: {
                                                            colorMode: 'gradient', 
                                                            colors: ['#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f', '#cab2d6', '#ffff99', '#1f78b4', '#33a02c']}
                                                      }"
  )
  )
  )
  
  
  
  #======================================== timeline plot =============================================
  
  # Date
  gkg_tv_test <- reactive({
    dates <- as.character(input$date_timeline)
    gkg_tv_test <- get_data_gkg_tv_days(dates = dates, return_message = T)
    gkg_tv_test
  })
  
  # UI
  # output$type <- renderUI({
  #   selectizeInput("type", "Select News Type", 
  #                unique(gkg_tv_test()$nameSource),
  #                selected = c('BLOOMBERG'),
  #                multiple = TRUE
  #                )
  #   })
  
  df_tv <- reactive ({
    df <- gkg_tv_test()
    df <- df[with(df, order(nameSource, dateTimeDocument)),]
    df <- 
      df %>%
      group_by(nameSource) %>% 
      mutate(end_time = lead(dateTimeDocument, 1))
    df$end_time[is.na(df$end_time)] <- df$dateTimeDocument[is.na(df$end_time)]+1*60*60
    df
  })
  
  # Type
  df_tv_2 <- reactive ( df_tv() %>% filter(nameSource %in% input$type_timeline) )
  
  # Output
  # output$text <- renderText(df())
  
  output$tv <- renderGvis(
    gvisTimeline(data=df_tv_2(), 
                 rowlabel="nameSource", barlabel="nameTVShow", 
                 start="dateTimeDocument", end="end_time",
                 options=list(height=1000,width=1000)
    ))
  
  
  
  
  }

#### Run ####
shinyApp(ui = ui, server = server)
