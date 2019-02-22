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
  theme = shinytheme("readable"),
  titlePanel("Fundraising Platform Research"),
  br(),
  br(),
  sidebarLayout(position = "right",
                sidebarPanel(width = 6,
                  tags$b("Q1: 这是一张什么类型的图片？" ),
                  selectInput("Q1",
                              label = "请在以下选项中选择:",
                              selected = NULL, choices = c("人像（包含一个或多个人物，或人物展示身份证件或诊断书等）" = 'A', 
                                                        "人体局部器官图" = 'B', 
                                                        "只有病情诊断书" = 'C',
                                                        "只有身份证件" = 'D',
                                                        "其它" = 'E'
                              )
                  ),
                  #End of Q1
                  
                  ##tabBox( id = "Q1", side = "left", width = 10,
                  #tabPanel(title = "人像（包含一个或多个人物，或人物展示身份证件或诊断书等）", value = 'A'),
                  #tabPanel(title = "人体局部器官图", value = 'B'),
                  #tabPanel(title = "只有病情诊断书", value = 'C'),
                  #tabPanel(title = "只有身份证件", value = 'D'),
                  #tabPanel(title = "其它", value = 'E')
                  
                  #),
                  
                  tags$b("Q2: 这是一张病人的治疗照还是生活照？"),
                  selectInput("Q2",
                              label = "请在以下选项中选择:",
                              selected = NULL, choices = c("治疗照" = 'A', 
                                                           "生活照" = 'B'
                              )
                  ),
                  #End of Q2
                  
                  #tabBox( id = "Q2", side = "left", width = 10,
                  #tabPanel(title = "治疗照", value = 'A'),
                  #tabPanel(title = "生活照", value = 'B')
                  
                  #), 
                  
                  tags$b("Q3: 您是否能够在图片中明确识别出谁是目标病人？"),
                  selectInput("Q3",
                              label = "请在以下选项中选择:",
                              selected = NULL, choices = c("能" = '1', 
                                                        "不能" = '2'
                              )
                  ),
                  #tabBox( id = "Q3", side = "left", width = 10,
                  #tabPanel(title = "能", value = '1'),
                  #tabPanel(title = "不能", value = '0')
                  #), #End of Q3
                  
                  tags$b("Q4: 您觉得这张图片中病人的面部清晰度如何？"),
                  radioButtons("Q4",
                               label = "1-非常清晰  5-非常不清晰:",
                               inline = TRUE,
                               choices= c(
                                 "1" = "1",
                                 "2" = "2",
                                 "3" = "3",
                                 "4" = "4",
                                 "5" = "5"
                               )
                  ),
                  
                  tags$b("Q5: 您觉得图片中病人的面部表情所表达的情绪有多消极/积极？"),
                  radioButtons("Q5",
                               label = "1-非常消极  5-非常积极:",
                               inline = TRUE,
                               choices= c(
                                 "1" = "1",
                                 "2" = "2",
                                 "3" = "3",
                                 "4" = "4",
                                 "5" = "5"
                               )
                  ),
                  
                  tags$b("Q6: 您觉得图片中病人的面部表情所传达的唤醒度有多强？"),
                  radioButtons("Q6",
                               label = "1-非常弱  5-非常强:",
                               inline = TRUE,
                               choices= c(
                                 "1" = "1",
                                 "2" = "2",
                                 "3" = "3",
                                 "4" = "4",
                                 "5" = "5"
                               )
                  ),
                  
                  tags$b("Q7: 您觉得图片中病人的面部表情多大程度上传递了一种悲观/乐观的态度？"),
                  radioButtons("Q7",
                               label = "1-非常悲观  5-非常乐观:",
                               inline = TRUE,
                               choices= c(
                                 "1" = "1",
                                 "2" = "2",
                                 "3" = "3",
                                 "4" = "4",
                                 "5" = "5"
                               )
                  ),
                  
                  tags$b("Q8: 您觉得图片中病人的外表形象如何？"),
                  radioButtons("Q8",
                               label = "1-非常不好看  5-非常好看:",
                               inline = TRUE,
                               choices= c(
                                 "1" = "1",
                                 "2" = "2",
                                 "3" = "3",
                                 "4" = "4",
                                 "5" = "5"
                               )
                  ),
                  
                  tags$b("Q9: 您觉得图片中的病人看起来有多悲惨？"),
                  radioButtons("Q9",
                               label = "1-一点都不悲惨  5-非常悲惨:",
                               inline = TRUE,
                               choices= c(
                                 "1" = "1",
                                 "2" = "2",
                                 "3" = "3",
                                 "4" = "4",
                                 "5" = "5"
                               )
                  ),
                  
                  tags$b("Q10: 您觉得图片中的病人看起来有多需要您的帮助？"),
                  radioButtons("Q10",
                               label = "1-一点都不需要  5-非常需要:",
                               inline = TRUE,
                               choices= c(
                                 "1" = "1",
                                 "2" = "2",
                                 "3" = "3",
                                 "4" = "4",
                                 "5" = "5"
                               )
                  ),
                  
                  tags$b("Q11: 您觉得评估这张图片在多大程度上会引起了您的不适感？"),
                  radioButtons("Q11",
                               label = "1-完全没有  5-非常不适:",
                               inline = TRUE,
                               choices= c(
                                 "1" = "1",
                                 "2" = "2",
                                 "3" = "3",
                                 "4" = "4",
                                 "5" = "5"
                               )
                  ),
                  
                  tags$b("Q12: 您对所评估的图片是否有发现任何异常需要报告？例如，图中有多个人无法区分目标病人，图中没有人脸无法识别面部表情，或人脸非常模糊等。"),
                  textAreaInput("Q12",
                                label = "如果您有任何反馈请填入下方；如没有，可直接跳过该题。",
                                placeholder = "请输入"
                  )
                              ),
                mainPanel("", 
                          width = 6,
                          htmlOutput("currentpicID"),
                          br(),
                          br(),
                          withSpinner(imageOutput("image", inline = TRUE)),
                          br(),
                          actionButton(inputId = "Previous", label= "上一张", width = '25%', style = 'margin-left:2em'),
                          span(style = 'margin-left:2em'),
                          actionButton(inputId = "Next", label= "下一张", width = '25%'),
                          span(style = 'margin-left:2em'),
                          actionButton(inputId = "Finish", label= "保存", width = '25%'))
  ),
  
  verticalLayout(
  fluidRow(
    column(5,
           
           verticalLayout(
             
           numericInput( "userID", label = "参与者编号",
                         value = 0, min = 1, max = 99, step = 1 ),
           numericInput( "picID", label = "起始照片编码",
                         value = 0, min = 0, max = 4000, step = 1 ),
           actionButton(inputId = "Initialize", label= "初始化")

           ))#end of left column
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
      "Q4" = character(),
      "Q5" = character(),
      "Q6" = character(),
      "Q7" = character(),
      "Q8" = character(),
      "Q9" = character(),
      "Q10" = character(),
      "Q11" = character(),
      "Q12" = character(),
      stringsAsFactors=FALSE),
    
    currentQ1 = "1",
    currentQ2 = "1",
    currentQ3 = "1",
    currentQ4 = "1",
    currentQ5 = "1",
    currentQ6 = "1",
    currentQ7 = "1",
    currentQ8 = "1",
    currentQ9 = "1",
    currentQ10 = "1",
    currentQ11 = "1",
    currentQ12 = "1"
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
        "Q4" = character(),
        "Q5" = character(),
        "Q6" = character(),
        "Q7" = character(),
        "Q8" = character(),
        "Q9" = character(),
        "Q10" = character(),
        "Q11" = character(),
        "Q12" = character(),
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
      param$currentQ4 <- input$Q4
      param$currentQ5 <- input$Q5
      param$currentQ6 <- input$Q6
      param$currentQ7 <- input$Q7
      param$currentQ8 <- input$Q8
      param$currentQ9 <- input$Q9
      param$currentQ10 <- input$Q10
      param$currentQ11 <- input$Q11
      param$currentQ12 <- input$Q12     
      
      param$survey[nrow(param$survey)+1,1] <- param$picID
      param$survey[nrow(param$survey)+1,2] <- param$currentQ1
      param$survey[nrow(param$survey)+1,3] <- param$currentQ2
      param$survey[nrow(param$survey)+1,4] <- param$currentQ3
      param$survey[nrow(param$survey)+1,5] <- param$currentQ4
      param$survey[nrow(param$survey)+1,6] <- param$currentQ5
      param$survey[nrow(param$survey)+1,7] <- param$currentQ6
      param$survey[nrow(param$survey)+1,8] <- param$currentQ7
      param$survey[nrow(param$survey)+1,9] <- param$currentQ8
      param$survey[nrow(param$survey)+1,10] <- param$currentQ9
      param$survey[nrow(param$survey)+1,11] <- param$currentQ10
      param$survey[nrow(param$survey)+1,12] <- param$currentQ11
      param$survey[nrow(param$survey)+1,13] <- param$currentQ12
      
      param$picID <- param$picID + 1
      param$picIDpadded <- str_pad(param$picID,4, side = c("left"), pad = "0")
       
    })
  })

  observe({
    input$Previous
    isolate({
      
      param$currentQ1 <- input$Q1
      param$currentQ2 <- input$Q2
      param$currentQ3 <- input$Q3
      param$currentQ4 <- input$Q4
      param$currentQ5 <- input$Q5
      param$currentQ6 <- input$Q6
      param$currentQ7 <- input$Q7
      param$currentQ8 <- input$Q8
      param$currentQ9 <- input$Q9
      param$currentQ10 <- input$Q10
      param$currentQ11 <- input$Q11
      param$currentQ12 <- input$Q12     
      
      param$survey[nrow(param$survey)+1,1] <- param$picID
      param$survey[nrow(param$survey)+1,2] <- param$currentQ1
      param$survey[nrow(param$survey)+1,3] <- param$currentQ2
      param$survey[nrow(param$survey)+1,4] <- param$currentQ3
      param$survey[nrow(param$survey)+1,5] <- param$currentQ4
      param$survey[nrow(param$survey)+1,6] <- param$currentQ5
      param$survey[nrow(param$survey)+1,7] <- param$currentQ6
      param$survey[nrow(param$survey)+1,8] <- param$currentQ7
      param$survey[nrow(param$survey)+1,9] <- param$currentQ8
      param$survey[nrow(param$survey)+1,10] <- param$currentQ9
      param$survey[nrow(param$survey)+1,11] <- param$currentQ10
      param$survey[nrow(param$survey)+1,12] <- param$currentQ11
      param$survey[nrow(param$survey)+1,13] <- param$currentQ12
      
      param$picID <- param$picID - 1
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
      param$currentQ4 <- input$Q4
      param$currentQ5 <- input$Q5
      param$currentQ6 <- input$Q6
      param$currentQ7 <- input$Q7
      param$currentQ8 <- input$Q8
      param$currentQ9 <- input$Q9
      param$currentQ10 <- input$Q10
      param$currentQ11 <- input$Q11
      param$currentQ12 <- input$Q12
      
      param$survey[nrow(param$survey)+1,1] <- param$picID
      param$survey[nrow(param$survey)+1,2] <- param$currentQ1
      param$survey[nrow(param$survey)+1,3] <- param$currentQ2
      param$survey[nrow(param$survey)+1,4] <- param$currentQ3
      param$survey[nrow(param$survey)+1,5] <- param$currentQ4
      param$survey[nrow(param$survey)+1,6] <- param$currentQ5
      param$survey[nrow(param$survey)+1,7] <- param$currentQ6
      param$survey[nrow(param$survey)+1,8] <- param$currentQ7
      param$survey[nrow(param$survey)+1,9] <- param$currentQ8
      param$survey[nrow(param$survey)+1,10] <- param$currentQ9
      param$survey[nrow(param$survey)+1,11] <- param$currentQ10
      param$survey[nrow(param$survey)+1,12] <- param$currentQ11
      param$survey[nrow(param$survey)+1,13] <- param$currentQ12
      
      write.csv(param$survey, file = glue("survey_{param$userID}_{param$startpicID}_{param$picID}.csv"))
      
    })
  })
  

  # image sends pre-rendered images
  output$image <- renderImage({
      return(list(
        src = glue("../Images/pic1_{param$picIDpadded}.jpeg"),
        width = 400,
        style= "display: block; margin-left: auto; margin-right: auto;", 
        filetype = "image/jpeg",
        alt = "无此照片"
      ))
    
  }, deleteFile = FALSE)
  
  output$currentpicID <- renderText({
   glue("当前照片编号{param$picIDpadded}")
    
  })

  

  #output$surveyTable <- renderDT({
  #  param$survey
  #})
  
}
  
shinyApp(ui, server)