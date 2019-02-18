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
             
             
           tags$b("这是一张什么类型的图片？ 请在以下选项中选择。" ),
           tabBox( id = "Q1", side = "left", width = 10,
                   tabPanel(title = "人像（包含一个或多个人物，或人物展示身份证件或诊断书等）", value = 'A'),
                   tabPanel(title = "人体局部器官图", value = 'B'),
                   tabPanel(title = "只有病情诊断书", value = 'C'),
                   tabPanel(title = "只有身份证件", value = 'D'),
                   tabPanel(title = "其它", value = 'E')
                   
                  ), #End of Q1
            
           tags$b("Q2: 这是一张病人的治疗照还是生活照？
" ),
           
           tabBox( id = "Q2", side = "left", width = 10,
                   tabPanel(title = "治疗照", value = 'A'),
                   tabPanel(title = "生活照", value = 'B')

           ), #End of Q2
            
           tags$b("Q3: 您是否能够在图片中明确识别出谁是目标病人？？" ),
           tabBox( id = "Q3", side = "left", width = 10,
                   tabPanel(title = "能", value = '1'),
                   tabPanel(title = "不能", value = '0')
           ), #End of Q3
           
           
           
           actionButton(inputId = "Next", label= "下一張"),
           actionButton(inputId = "Finish", label= "存檔")
             
                  
           ))#end of left column
    
    ,column(4,
            htmlOutput("currentpicID"),
            withSpinner(imageOutput("image", height = 300))
    )
  )#,#End of fluidRow
  #withSpinner(DTOutput(outputId ="surveyTable"))
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
      "Q1" = character(),
      "Q2" = character(),
      "Q3" = character(),
      stringsAsFactors=FALSE),
    
    currentQ1 = "1",
    currentQ2 = "1",
    currentQ3 = "1"
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
        "Q1" = character(),
        "Q2" = character(),
        "Q3" = character(), 
        stringsAsFactors=FALSE)

    })
  })
  #Record the entry and show the next picture
  observe({
    input$Next
    isolate({
      
      param$currentQ1 <- input$Q1
      param$currentQ2 <- input$Q2
      param$currentQ3 <- input$Q3


      param$survey[nrow(param$survey)+1,1] <- param$picID
      param$survey[nrow(param$survey),2] <- (param$currentQ1)
      param$survey[nrow(param$survey),3] <- (param$currentQ2)
      param$survey[nrow(param$survey),4] <- (param$currentQ3)
      
      
      param$picID <- param$picID + 1
      param$picIDpadded <- str_pad(param$picID,4, side = c("left"), pad = "0")
       
    })
  })
  #Save the dataframe
  observe({
    input$Finish
    isolate({
      
      param$currentQ1 <- input$Q1
      param$currentQ2 <- input$Q2
      param$currentQ3 <- input$Q3
      
      
      param$survey[nrow(param$survey)+1,1] <- param$picID
      param$survey[nrow(param$survey),2] <- (param$currentQ1)
      param$survey[nrow(param$survey),3] <- (param$currentQ2)
      param$survey[nrow(param$survey),4] <- (param$currentQ3)
      
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

  

  #output$surveyTable <- renderDT({
  #  param$survey
  #})
  
}
  
shinyApp(ui, server)