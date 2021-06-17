if (interactive()) {
  
  library(shiny)
  library(leaflet)
  library(dplyr)
  library(shinyWidgets)
  library(ggplot2)
  library(shinyTime)
  library(shinythemes)
  library(magrittr)
  
  dat<-read.csv("C:\\Users\\norazyrabinti.omar\\Documents\\R Project\\sabah.csv")
  dat_sabah = data.frame(dat)
  
  ui<-fluidPage(
    theme = shinytheme("cerulean"),
    setBackgroundImage(src = "https://www.collinsdictionary.com/images/full/river_377603497_1000.jpg"),
    wellPanel(tags$style(".well {background-color:rgba(255,255,255,0.8);}")),
    navbarPage("Know Your River Water Level",
               tabPanel("About",
                        sidebarPanel(
                          h3("Introduction"),
                          h4("Write your introduction here...."),
                          br(),
                          br(),
                          h3("Why this app?"),
                          h4("1. Reason 1"),
                          h4("2. Reason 2"),
                          h4("3. Reason 3"),
                          br(),
                          br(),
                          h3("Who we are?"),
                          h4("Include your names here..")
                        )
               ),
               tabPanel("Find Your River"
                        ,fluidPage(sidebarLayout(
                          sidebarPanel(
                            selectInput("River","River : ", c("Select River",unique(dat_sabah$River)))
                            , selectInput("District","District : ", c("Select District",unique(dat_sabah$District)))
                            , selectInput("Date","Date : ", c("Select Date",unique(dat_sabah$Date)))
                            , em("*The data is only available for the past 7 days")
                            , selectInput("Time","Time : ", c("Select Time",unique(dat_sabah$Time)))
                          )
                          ,mainPanel(
                            fluidPage(
                              leafletOutput("mymap",width="100%",height=600)
                              ,fluidRow(verbatimTextOutput("map_marker_click"))
                              ,fluidRow(hr())
                              
                            )
                          )
                        )
                        ,dataTableOutput("table")
                        )
               ),
               tabPanel("Insights"
                        , sidebarLayout(sidebarPanel(selectInput("Plot_Select:","Select Plot",c("Bar Chart"
                                                                                                ,"Pie Chart"
                        )
                        , selectize = F,size=11
                        )
                        )
                        ,mainPanel(plotOutput("plot",width="100%",height=800))))
    ))
  
  server<- function(input, output, session) {
    
    dat<-read.csv("C:\\Users\\norazyrabinti.omar\\Documents\\R Project\\sabah.csv")
    dat_sabah = data.frame(dat)
    
    dat_temp<-reactive({
      if (input$River=="All"){dat_sabah}
      else{filter(dat_sabah,River==input$River)}
    })
    
    # 1st layer filtering (district)
    observe({
      dat_temp2 <- dat_sabah$District[dat_sabah$River == input$River]
      updateSelectInput(session, "District", label = "Select District", choices = c("Select District", dat_temp2))
    })
    
    # 2nd layer filtering (date)
    observe({
      dat_temp3 <- dat_sabah$Date[dat_sabah$District == input$District]
      updateSelectInput(session, "Date", label = "Select Date", choices = c("Select Date", dat_temp3))
    })
    
    # 3rd layer filtering (time)
    observe({
      dat_temp4 <- dat_sabah$Time[dat_sabah$Date == input$Date]
      updateSelectInput(session, "Time", label = "Select Time", choices = c("Select Time", dat_temp4))
    })
    
    
    
    
    dat_temp5<-reactive({if(as.character(input$River) != "Sg. Kinabatangan" | as.character(input$River) != "Sg. Padas"){
      
      data.frame(River="There is no river that fulfill the criteria."
                 ,District=NA,Date=NA,Time=NA,Lon=115.944486 ,Lat=5.124731
                 ,Water=NA )
      
    }else{col=if_else(dat_sabah$Water>150
                              ,"red"
                              ,if_else(dat_sabah$Water<=150 && dat_sabah$Water>100
                                       ,"green","blue"
                              )
                 )
    
    }
    }
    )
      
    
    output$mymap<-renderLeaflet(
      addLegend(
        #addCircleMarkers(
        addAwesomeMarkers(
          addProviderTiles(leaflet()
                           ,provider = providers$Stamen.Toner
                           ,options = providerTileOptions(opacity = 0.5))
          ,lng=dat_temp5()$Lon
          ,lat=dat_temp5()$Lat
          ,icon = awesomeIcons(icon = "ios-home", library = 'ion'
                               ,iconColor = "#ffffff"
                               ,markerColor = dat_temp5()$col)
          ,popup=paste0("River Name : "
                        ,input$River,"<br>","District : "
                        ,input$District,"<br>","Date : "
                        ,input$Date,"<br>","Time : "
                        ,input$Time,"<br>","Longitude : "
                        ,dat_temp5()$Lon,"<br>","Latitude : "
                        ,dat_temp5()$Lat,"<br>","Water Level : "
                        ,dat_temp5()$Water,"<br>"
                        
          )
          ,label=input$River
          ,labelOptions = labelOptions(textsize = "15px")
        )
        #,lng=dat_temp_metro()$lon,lat=dat_temp_metro()$lat
        #,popup = dat_temp_metro()$Station,color = "black")
        
        ,position = "bottomright"
        #,title="Prices within the selection"
        ,labFormat = labelFormat()
        ,colors=c("red","green","blue")
        ,labels=c("Danger"
                  ,"Warning"
                  ,"Normal")
      ))
    
    observe({
      if(is.null(input$mymap_marker_click)){
        return()
      }else{
        lat1 <- input$mymap_marker_click$lat
        lon1 <- input$mymap_marker_click$lng
        
        addPolylines(leafletProxy("mymap"),lat = c(lat1),lng = c(lon1),color = "red"
                     ,layerId = "foo")
        
        addCircleMarkers(leafletProxy("mymap"),lng=lon1
                         ,lat=lat1
                         #,label  = paste0(station_name," Station")
                         ,labelOptions = labelOptions(noHide = T,direction = 'auto'
                                                      ,style = list(
                                                        "color" = "white",
                                                        #"font-family" = "serif",
                                                        #"font-style" = "italic",
                                                        #"box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                        "font-size" = "15px",
                                                        "border-color" = "rgba(0,0,0,0.5)",
                                                        "background-color"="black")
                         )
                         ,color = "red"
                         ,layerId = "foo2")
      }
    }
    )
  }
  
  shinyApp(ui = ui, server = server)
  
}