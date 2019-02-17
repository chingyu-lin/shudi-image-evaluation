library(shiny)
library(shinydashboard)
library(png) # For writePNG function
library(shinythemes)
library(glue)
library(stringr)
library(tidyverse)
library(DT)
library(shinycssloaders)

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("科研"),
  verticalLayout(
  fluidRow(
    column(5,
           
           
           verticalLayout(
             
           numericInput( "userID", label = "受試者編號",
                         value = 0, min = 1, max = 99, step = 1 ),
           numericInput( "picID", label = "起始照片編碼",
                         value = 0, min = 0, max = 4000, step = 1 ),
           actionButton(inputId = "Initialize", label= "初始化"),
             
             
           tags$b("圖像中是否有人？" ),
           tabBox( id = "People", selected = "-1", side = "left", width = 10,
                   tabPanel(title = "是", value = '1'),
                   tabPanel(title = "否", value = '0'),
                   tabPanel(title = "無法判斷", value = '-1')
                   
                  ), #End of tabBox
            
           tags$b("圖像中是否在醫院？" ),
           
           tabBox( id = "Hospital", selected = "-1", side = "left", width = 10,
                   tabPanel(title = "是", value = '1'),
                   tabPanel(title = "否", value = '0'),
                   tabPanel(title = "無法判斷", value = '-1')
                   
           ), #End of tabBox 
            
           tags$b("圖像中是否有診斷證明書？" ),
           
           tabBox( id = "Diagnose", selected = "-1", side = "left", width = 10,
                   tabPanel(title = "是", value = '1'),
                   tabPanel(title = "否", value = '0'),
                   tabPanel(title = "無法判斷", value = '-1')
                   
           ), #End of tabBox 
           actionButton(inputId = "Next", label= "下一張"),
           actionButton(inputId = "Finish", label= "存檔")
             
                  
           ))#end of left column
    
    ,column(4,
            htmlOutput("currentpicID"),
            withSpinner(imageOutput("image", height = 300))
    )
  ),#End of fluidRow
  withSpinner(DTOutput(outputId ="surveyTable"))
  )
)
######server########

server <- function(input, output, session) {
  
  param <- reactiveValues(
    userID = 1,
    picID = 0,
    startpicID = 0,
    picIDpadded = str_pad(0,4, side = c("left"), pad = "0"),
    survey = data.frame(
      "picID" = integer(),
      "People" = integer(),
      "Hospital" = integer(),
      "Diagose" = integer()),
    currentPeople = "-1",
    currentHospital = "-1",
    currentDiagnoise = "-1"
  )
  
  #Initialize the survey
  observe({
    input$Initialize
    isolate({
      param$userID <- str_pad(input$userID,3,side = c("left"),pad = "0")
      param$picID <- input$picID
      param$startpicID <- input$picID
      param$picIDpadded <- str_pad(input$picID,4, side = c("left"), pad = "0")
      param$survey <- data.frame(
        "picID" = integer(),
        "People" = integer(),
        "Hospital" = integer(),
        "Diagose" = integer())

    })
  })
  #Record the entry and show the next picture
  observe({
    input$Next
    isolate({
      
      param$currentPeople <- input$People
      param$currentHospital <- input$Hospital
      param$currentDiagonoise <- input$Diagnoise


      param$survey[nrow(param$survey)+1,1] <- param$picID
      param$survey[nrow(param$survey),2] <- as.integer(param$currentPeople)
      param$survey[nrow(param$survey),3] <- as.integer(param$currentHospital)
      param$survey[nrow(param$survey),4] <- as.integer(param$currentDiagnoise)
      
      
      param$picID <- param$picID + 1
      param$picIDpadded <- str_pad(param$picID,4, side = c("left"), pad = "0")
       
    })
  })
  #Save the dataframe
  observe({
    input$Finish
    isolate({
      
      param$currentPeople <- input$People
      param$currentHospital <- input$Hospital
      param$currentDiagonoise <- input$Diagnoise
      
      
      param$survey[nrow(param$survey)+1,1] <- param$picID
      param$survey[nrow(param$survey),2] <- as.integer(param$currentPeople)
      param$survey[nrow(param$survey),3] <- as.integer(param$currentHospital)
      param$survey[nrow(param$survey),4] <- as.integer(param$currentDiagnoise)
      
      write.csv(param$survey, file = glue("survey_{param$userID}_{param$startpicID}_{param$picID}.csv"))
      
      
    })
  })
  
  
  
  
  
  
  
  # image sends pre-rendered images
  output$image <- renderImage({
      return(list(
        src = glue("../Images/pic1_{param$picIDpadded}.jpeg"),
        width = 300,
        filetype = "image/jpeg",
        alt = "無此照片"
      ))
    
  }, deleteFile = FALSE)
  
  output$currentpicID <- renderText({
    glue("當前照片編號{param$picIDpadded}")
    
  })

  

  output$surveyTable <- renderDT({
    param$survey
  })
  
}
  
shinyApp(ui, server)