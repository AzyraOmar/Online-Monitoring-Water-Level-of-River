if (interactive()) {
  
  library(shiny)
  library(leaflet)
  library(dplyr)
  library(shinyWidgets)
  library(ggplot2)
  library(shinyTime)
  library(shinythemes)
  library(magrittr)
  library(DT)
  library(ggpubr)
  
  dat_selangor<-read.csv("selangor.csv")
  dat_selangor
  Name1 <- c("Country Homes", "Jalan Seri Tanjung", "Kg Asahan", "Kg Delek", "Kg. Sg Selisek", "Kg. Sungai Buaya", "Pekan Meru", "Selat Muara", 
             "Sg. Buloh di Parit Mahang", "Sg. Kundang di Kg. Kundang", "Sg. Langat di Batu 12", "Sg. Langat di JPS Sg. Manggis", 
             "Sg.Batang Kali di Batang kali", "Sg.Bernam di Jambatan SKC", "Sg.Bernam di Sg.Air Tawar", "Sg.Bernam di Tanjung Malim", 
             "Sg.Buloh di Sri Aman", "Sg.Damansara di Batu 3", "Sg.Damansara di Kg.Melayu Subang", "Sg.Damansara di Taman Mayang", 
             "Sg.Damansara di TTDI Jaya", "Sg.Kelang di Bandar Klang", "Sg.Kelang di Puchong Drop", "Sg.Kelang di Tmn Sri Muda 1", 
             "Sg.Kelang di Tugu Keris", "Sg.Kerling di Kerling", "Sg.Labu di Kg.Salak Tinggi", "Sg.Langat di Batu 15", "Sg.Langat di Batu 20", 
             "Sg.Langat di Dengkil", "Sg.Langat di Kajang", "Sg.Langat di TNB Ponsoon", "Sg.Lui di Kg. Sg. Lui", "Sg.Penchala di Jalan 222",
             "Sg.Rawang di Tmn.Tun Teja", "Sg.Selangor di Ampang Pecah", 
             "Sg.Selangor di Rantau Panjang", "Sg.Semenyih di Kg.Pasir", "Sg.Semenyih di Rinching", "Sg.Serendah di Serendah")
  
  
  ui<-fluidPage(
    theme = shinytheme("cerulean"),
    setBackgroundImage(src = "https://www.collinsdictionary.com/images/full/river_377603497_1000.jpg"),
    wellPanel(tags$style(".well {background-color:rgba(255,255,255,0.8);}")),
    navbarPage("Know Your River Water Level",
               tabPanel("About",
                        sidebarPanel(width = 20,
                                     h1("Introduction"),
                                     h3("For every year during the monsoon season, Malaysia is impacted by floods which is regular natural disasters in country. From October to March of monsoon season, the area most prone to flooding is at east coast of peninsular Malaysia. In year end 2014, Malaysia flood have been described as the worst floods in decades which cause more than 200,000 people were affected and evacuated, while 21 were killed. The major reason for this flood is typically due to rising of river water levels from heavy rain especially during monsoon season."),
                                     br(),
                                     br(),
                                     h1("Objective"),
                                     h3("To identify high risk rivers in state of Selangor which potentially cause flood during raining. This provide a pre-alert to population nearby the river area in which they may expected flood during the raining. For long term measure, the authority should take proactive actions to keep the river water level within the safe range by ensuring the water flow, deepening, safety wall or overflow system such as SMART tunnel."),
                                     br(),
                                     br(),
                                     h1("Who we are?"),
                                     h3("Yuan Lii Tan(S2024825"),
                                     h3("Nor Azyra Binti Omar (17120332)"),
                                     h3("Nur Farzanah Binti Roslan (17089384)"),
                                     h3("Amri Bin Abu Bakar (S2035949)")
                        )
               ),
               tabPanel("Find Your River"
                        ,fluidPage(sidebarLayout(
                          sidebarPanel(
                            h1("Find Your River")
                            , selectInput("River","River: ", c("Select River",unique(dat_selangor$RIVERNAME)))
                            , selectInput("District","District: ", NULL)
                            , selectInput("Station", "Station: ", NULL)
                            , selectInput("Date","Date: ", NULL)
                            , em("*The data is only available for the past 7 days")
                            , br()
                            , br()
                            , selectInput("Time","Time: ", NULL)
                            , actionButton("submitButton",
                                           label = "Submit",
                                           class = "btn btn-primary"))
                          ,mainPanel(
                            h1('River Water Level'),
                            verbatimTextOutput('contents'),
                            br(),
                            div(DT::dataTableOutput('tabledata'),style = "font-size:14px")
                          )))),
               tabPanel("Insights",
                        fluidPage(sidebarLayout(sidebarPanel(
                          width = 6,
                          h1("Rain amount and river level graph"),
                          selectInput("Name", "Select Area", c("Select Area", Name1)),
                          numericInput("Day", value = 9, min = 8, max = 14, label = "Select the day between 8 to 14"),
                          actionButton("line", "Line Chart (Rain)"),
                          actionButton("line2", "Line Chart (River)"),
                          actionButton("scatter", "Scatter Plot (All Day)"),
                          br(),
                          br(),
                          h1("6 days of total rain amount in every river area"),
                          plotOutput("Plot1", height = 800, width = 1000)
                        ),
                        mainPanel(
                          width = 4,
                          h1("Plot"),
                          plotOutput("Plot", height = 800, width = 1000)
                        ))))))
  
  server<- function(input, output, session) {
    
    dat_temp<-reactive({
      if (input$River=="All"){dat_selangor}
      else{filter(dat_selangor,River==input$River)}
    })
    
    # 1st layer filtering (district)
    observe({
      dat_temp2 <- dat_selangor$DISTRICT[dat_selangor$RIVERNAME == input$River]
      updateSelectInput(session, "District", label = "Select District", choices = c("Select District", dat_temp2))
    })
    
    # 2nd layer filtering (station)
    observe({
      dat_temp3 <- dat_selangor$STATION[dat_selangor$DISTRICT == input$District]
      updateSelectInput(session, "Station", label = "Select Station", choices = c("Select Station", dat_temp3))
    })
    
    observe({
      dat_temp4 <- dat_selangor$DATE[dat_selangor$STATION == input$Station]
      updateSelectInput(session, "Date", label = "Select Date", choices = c("Select Date", dat_temp4))
    })
    
    # 3rd layer filtering (time)
    observe({
      dat_temp5 <- dat_selangor$TIME[dat_selangor$DATE == input$Date]
      updateSelectInput(session, "Time", label = "Select Time", choices = c("Select Time", dat_temp5))
    })
    
    # Status/Output Text Box
    output$contents <- renderPrint({
      if(input$submitButton > 0) {
        isolate("The status of your river water level is as below.")
      } else {
        return("Ready to check your river water level.")
      }
    })
    
    # Result
    datasetInput <- reactive({
      level <- dat_selangor %>%
        filter(RIVERNAME == input$River) %>%
        filter(DISTRICT == input$District) %>%
        filter(STATION == input$Station) %>%
        filter(DATE == input$Date) %>%
        filter(TIME == input$Time) %>%
        select(RIVERNAME, DISTRICT, STATION, DATE, TIME, LEVEL.m., Level)
      names(level) <- c("River", "District", "Station", "Date", "Time", "Level (m)", "Status")
      print(level)
    }
    )
    
    # Prediction results table
    output$tabledata <- renderDataTable({
      if(input$submitButton > 0) {
        isolate(datasetInput())
      }
    })
    
    # Scatter Plot
    observeEvent(input$scatter, {
      x2 <- dat_selangor %>%
        filter(NAME == input$Name)
      f <- ggscatter(x2, x = "DAILYRF", y = "LEVEL.m.",
                     add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "pearson",
                     xlab = "Rain Fall Amount(mm)", ylab = "River Water Level(M)")
      output$Plot <- renderPlot({
        f
      })
    })
    observeEvent(input$line, {
      x1 <- dat_selangor %>%
        filter(Day == input$Day & NAME == input$Name)
      
      a <- ggplot(x1) +
        geom_line(aes(x = TIME, y = DAILYRF, color=NAME, group=NAME))+
        xlab("Time (1 plot = Every 15min)")+
        ylab("Rain Amount(mm)")
      output$Plot <- renderPlot({
        a
      })
    })
    observeEvent(input$line2, {
      x3 <- dat_selangor %>%
        filter(Day == input$Day & NAME == input$Name)
      
      b <- ggplot(x3) +
        geom_line(aes(x = TIME, y = LEVEL.m., color=NAME, group=NAME))+
        xlab("Time (1 plot = Every 15min)")+
        ylab("River Water Level(M)")
      output$Plot <- renderPlot({
        b
      })
    })
    
    output$Plot1 <- renderPlot({
      #TEST bar plot
      ab <- dat_selangor %>%
        group_by(NAME) %>%
        summarise(summ = sum(DAILYRF))
      
      ggplot(ab, aes(x= reorder(NAME, +summ), y = summ, fill = NAME)) + 
        geom_bar(stat = "identity") +
        theme(legend.position = "none") +
        xlab("Area Name") +
        ylab("Total Rain Amount(mm)") +
        coord_flip()
    })
    
  }
  
  shinyApp(ui = ui, server = server)
  
}