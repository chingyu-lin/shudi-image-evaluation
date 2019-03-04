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
  shinyjs::useShinyjs(),
  theme = shinytheme("readable"),
  titlePanel("Fundraising Platform Research"),
  br(),
  br(),
  
  navbarPage( "", 
              
    tabPanel( "设定", 
    theme = shinytheme("readable"),
    
                                
              
              
      verticalLayout(
      fluidRow(
        column(12,
                
                 htmlOutput("welcomePage"),
                 #應改為當前測試組號較佳
                 conditionalPanel( 'input.Initialize == input.Finish ',
                 #textInput("name", label = "姓名", ""), #尚未儲存此訊息
                 br(),
                 numericInput( "randID", label = "随机编号: 1,2,3",
                               value = 1, min = 1, max = 3, step = 1 ),
                 numericInput( "groupID", label = "照片组号: 1~93",
                               value = 1, min = 1, max = 93, step = 1 ),
                 actionButton(inputId = "Initialize", label= "初始化")#,
               
                 #passwordInput( "password", label = "请输入密码", value = "password" )
                 
               )#End of conditionalPanel for initialize bottom
               )#end of left column
      )#,#End of fluidRow
 
      #withSpinner(DTOutput(outputId ="surveyTable"))
    
    )) #End of tab 1
    
    , 
    
    tabPanel( "实验", 
    conditionalPanel( 'input.Initialize == input.Finish + 1',
                      
    #Find better way to code this condition 
    conditionalPanel( '(input.Next < 50*input.Initialize && input.groupID != 93) || (input.groupID == 93 && input.Next< 26)',
    
    #conditionalPanel( 'input.password == "hecra"', #Set the passsword
                                        
    
              
              
              
              
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
                                         
                                         conditionalPanel( 'input.Q1 == "A" ', 
                                         
                                         
                                         tags$b("Q2: 这是一张病人的治疗照还是生活照？"),
                                         selectInput("Q2",
                                                     label = "请在以下选项中选择:",
                                                     selected = NULL, choices = c("治疗照" = 'A', 
                                                                                  "生活照" = 'B'
                                                     )
                                         ),
                                         #End of Q2
                                         
                                         
                                         
                                         
                                         selectInput("Q3",
                                                     label = "Q3: 您能否判断这张图片中谁是病人？",
                                                     selected = NULL, choices = c("能" = '1', 
                                                                                  "不能" = '2'))
                                         ,
                                         
                                         conditionalPanel( 'input.Q3 == "1" ', 
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
                                                           )
                                                           
                                                           
                                         )),#End of conditionalPanel
                                         
                                         
                                         
                                         
                                         
                                         
                                         
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
                                      #actionButton(inputId = "Previous", label= "上一张", width = '25%', style = 'margin-left:2em'),
                                      span(style = 'margin-left:15em'),
                                      actionButton(inputId = "Next", label= "下一张", width = '25%')
                                      # span(style = 'margin-left:2em'),
                                      # actionButton(inputId = "Finish", label= "保存", width = '25%'))
              )
              # ,
              # verticalLayout(
              #   fluidRow(
              #     column(5,
              #            
              #            verticalLayout(
              #              
              #              numericInput( "userID", label = "参与者编号",
              #                            value = 0, min = 1, max = 99, step = 1 ),
              #              numericInput( "picID", label = "起始照片编码",
              #                            value = 0, min = 0, max = 4000, step = 1 ),
              #              actionButton(inputId = "Initialize", label= "初始化")
              #              
              #            ))#end of left column
              #   )#,#End of fluidRow
              #   #withSpinner(DTOutput(outputId ="surveyTable"))
              # )
    )))),#End of second panel,
    
    
    tabPanel("保存上传",
             sidebarLayout(
               sidebarPanel(width = 5,
                            p("恭喜您已经完成本次实验！"),
                            p("请务必点击下面的按钮以保存并提交本次实验结果。"),
                            actionButton(inputId = "Finish", label= "保存提交")
               ),
               mainPanel(width = 7,
                         p(textOutput("thanks", container = span)),
                         code(textOutput("range", container = span))
               )
             )
    )#End of third panel)
    
    ) #End of navbarPage
  
  
  
  
  #)


)


#--------------------------------------------------------server--------------------------------------------------------

server <- function(input, output, session) {
  index_dict <- read_csv("index_dict.csv")
  
  param <- reactiveValues(
    randID = 1, #1~3
    groupID = 1, #1~93
    index = 1, #1~50

    df_dict = index_dict %>% filter(group == 1) ,
    picID = 0,
    picID = index_dict %>% filter(group == 1) %>% filter(index == 1) %>% select(rand1) %>% as.integer(),
    picIDpadded = str_pad(1,4, side = c("left"), pad = "0"),
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
    
    currentQ1 = NA,
    currentQ2 = NA,
    currentQ3 = NA,
    currentQ4 = NA,
    currentQ5 = NA,
    currentQ6 = NA,
    currentQ7 = NA,
    currentQ8 = NA,
    currentQ9 = NA,
    currentQ10 = NA,
    currentQ11 = NA,
    currentQ12 = NA
  )
  
  #Initialize the survey
  observe({
    input$Initialize
    isolate({
      #param$userID <- str_pad(input$userID,3,side = c("left"),pad = "0")
      param$randID <- input$randID
      param$groupID <- input$groupID
      param$index <- 1
      param$df_dict <- index_dict %>% filter(group == input$groupID) %>% filter(index == param$index)
      param$picID <- param$df_dict[glue('rand{param$randID}')] %>% as.integer()
      #picIDpadded <- str_pad(param$picID,4, side = c("left"), pad = "0")
      
      
      param$picIDpadded <- str_pad(param$picID,4, side = c("left"), pad = "0")
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
  
  
  observe({
    shinyjs::toggleState("Initialize", !is.null(input$groupID) && input$groupID != ""  )
  })
  
  
  
  #Record the entry and show the next picture
  
  #Entry of question specific conditions
  observe({
    if( input$Q1 != 'A'){
      param$currentQ2 <- NA
      param$currentQ3 <- NA
      param$currentQ4 <- NA
      param$currentQ5 <- NA
      param$currentQ6 <- NA
      param$currentQ7 <- NA
      param$currentQ8 <- NA
      param$currentQ9 <- NA
      param$currentQ10 <- NA
      param$currentQ11 <- NA
      
    }else if (input$Q3 == "2"){
      param$currentQ2 <- input$Q2
      param$currentQ3 <- input$Q3
      param$currentQ4 <- NA
      param$currentQ5 <- NA
      param$currentQ6 <- NA
      param$currentQ7 <- NA
      param$currentQ8 <- NA
      param$currentQ9 <- NA
      param$currentQ10 <- NA
      param$currentQ11 <- NA
    } 
    else {
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
    }
  })
  
  
  
  observe({
    if( input$Q3 == '2'){
      param$currentQ4 <- NA
      param$currentQ5 <- NA
      param$currentQ6 <- NA
      param$currentQ7 <- NA
      param$currentQ8 <- NA
      param$currentQ9 <- NA
      param$currentQ10 <- NA
      param$currentQ11 <- NA
      
    } else {
      
      param$currentQ4 <- input$Q4
      param$currentQ5 <- input$Q5
      param$currentQ6 <- input$Q6
      param$currentQ7 <- input$Q7
      param$currentQ8 <- input$Q8
      param$currentQ9 <- input$Q9
      param$currentQ10 <- input$Q10
      param$currentQ11 <- input$Q11
    }
  })
  
  
  observe({
    input$Next
    isolate({
      
      param$currentQ1 <- input$Q1
      param$currentQ2 <- input$Q2
      param$currentQ3 <- input$Q3
      param$currentQ12 <- input$Q12
      
      param$survey[nrow(param$survey)+1,1] <- param$picID
      param$survey[nrow(param$survey),2] <- param$currentQ1
      param$survey[nrow(param$survey),3] <- param$currentQ2
      param$survey[nrow(param$survey),4] <- param$currentQ3
      param$survey[nrow(param$survey),5] <- param$currentQ4
      param$survey[nrow(param$survey),6] <- param$currentQ5
      param$survey[nrow(param$survey),7] <- param$currentQ6
      param$survey[nrow(param$survey),8] <- param$currentQ7
      param$survey[nrow(param$survey),9] <- param$currentQ8
      param$survey[nrow(param$survey),10] <- param$currentQ9
      param$survey[nrow(param$survey),11] <- param$currentQ10
      param$survey[nrow(param$survey),12] <- param$currentQ11
      param$survey[nrow(param$survey),13] <- param$currentQ12
      
      param$index <- param$index + 1
      param$df_dict <- index_dict %>% filter(group == input$groupID) %>% filter(index == param$index)
      param$picID <- param$df_dict[glue('rand{input$randID}')]  %>% as.integer()
      param$picIDpadded <- str_pad(param$picID,4, side = c("left"), pad = "0")
      
      
      updateSelectInput(session, "Q1",
                        label = "请在以下选项中选择:",
                        selected = NULL, choices = c("人像（包含一个或多个人物，或人物展示身份证件或诊断书等）" = 'A', 
                                                     "人体局部器官图" = 'B', 
                                                     "只有病情诊断书" = 'C',
                                                     "只有身份证件" = 'D',
                                                     "其它" = 'E')
      )
      
      updateSelectInput(session, "Q2",
                        label = "请在以下选项中选择:",
                        selected = NULL, choices = c("治疗照" = 'A', 
                                                     "生活照" = 'B')
      )
      
      updateSelectInput(session, "Q3",
                        label = "Q3: 您能否判断这张图片中谁是病人？",
                        selected = NULL, choices = c("能" = '1', 
                                                     "不能" = '2')
      )
      
      updateRadioButtons(session, "Q4",
                         label = "1-非常清晰  5-非常不清晰:",
                         inline = TRUE,
                         choices= c(
                           "1" = "1",
                           "2" = "2",
                           "3" = "3",
                           "4" = "4",
                           "5" = "5")
      )
      
      updateRadioButtons(session, "Q5",
                         label = "1-非常消极  5-非常积极:",
                         inline = TRUE,
                         choices= c(
                           "1" = "1",
                           "2" = "2",
                           "3" = "3",
                           "4" = "4",
                           "5" = "5")
      )
      
      updateRadioButtons(session, "Q6",
                         label = "1-非常弱  5-非常强:",
                         inline = TRUE,
                         choices= c(
                           "1" = "1",
                           "2" = "2",
                           "3" = "3",
                           "4" = "4",
                           "5" = "5")
      )
      
      updateRadioButtons(session, "Q7",
                         label = "1-非常悲观  5-非常乐观:",
                         inline = TRUE,
                         choices= c(
                           "1" = "1",
                           "2" = "2",
                           "3" = "3",
                           "4" = "4",
                           "5" = "5")
      )
      
      updateRadioButtons(session, "Q8",
                         label = "1-非常不好看  5-非常好看:",
                         inline = TRUE,
                         choices= c(
                           "1" = "1",
                           "2" = "2",
                           "3" = "3",
                           "4" = "4",
                           "5" = "5")
      )
      
      updateRadioButtons(session, "Q9",
                         label = "1-一点都不悲惨  5-非常悲惨:",
                         inline = TRUE,
                         choices= c(
                           "1" = "1",
                           "2" = "2",
                           "3" = "3",
                           "4" = "4",
                           "5" = "5")
      )
      
      updateRadioButtons(session, "Q10",
                         label = "1-一点都不需要  5-非常需要:",
                         inline = TRUE,
                         choices= c(
                           "1" = "1",
                           "2" = "2",
                           "3" = "3",
                           "4" = "4",
                           "5" = "5")
      )
      
      updateRadioButtons(session, "Q11",
                         label = "1-完全没有  5-非常不适:",
                         inline = TRUE,
                         choices= c(
                           "1" = "1",
                           "2" = "2",
                           "3" = "3",
                           "4" = "4",
                           "5" = "5")
      )
      
      updateTextAreaInput(session, "Q12",
                          label = "如果您有任何反馈请填入下方；如没有，可直接跳过该题。",
                          value = "",
                          placeholder = "请输入"
      )
    })
  })
  
  # observe({
  #   input$Previous
  #   isolate({
  #     
  #     param$currentQ1 <- input$Q1
  #     param$currentQ2 <- input$Q2
  #     param$currentQ3 <- input$Q3
  #     param$currentQ4 <- input$Q4
  #     param$currentQ5 <- input$Q5
  #     param$currentQ6 <- input$Q6
  #     param$currentQ7 <- input$Q7
  #     param$currentQ8 <- input$Q8
  #     param$currentQ9 <- input$Q9
  #     param$currentQ10 <- input$Q10
  #     param$currentQ11 <- input$Q11
  #     param$currentQ12 <- input$Q12     
  #     
  #     param$survey[nrow(param$survey)+1,1] <- param$picID
  #     param$survey[nrow(param$survey),2] <- param$currentQ1
  #     param$survey[nrow(param$survey),3] <- param$currentQ2
  #     param$survey[nrow(param$survey),4] <- param$currentQ3
  #     param$survey[nrow(param$survey),5] <- param$currentQ4
  #     param$survey[nrow(param$survey),6] <- param$currentQ5
  #     param$survey[nrow(param$survey),7] <- param$currentQ6
  #     param$survey[nrow(param$survey),8] <- param$currentQ7
  #     param$survey[nrow(param$survey),9] <- param$currentQ8
  #     param$survey[nrow(param$survey),10] <- param$currentQ9
  #     param$survey[nrow(param$survey),11] <- param$currentQ10
  #     param$survey[nrow(param$survey),12] <- param$currentQ11
  #     param$survey[nrow(param$survey),13] <- param$currentQ12
  #     
  #     param$picID <- param$picID - 1
  #     param$picIDpadded <- str_pad(param$picID,4, side = c("left"), pad = "0")
  #     
  #   })
  # })
  
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
      param$survey[nrow(param$survey),2] <- param$currentQ1
      param$survey[nrow(param$survey),3] <- param$currentQ2
      param$survey[nrow(param$survey),4] <- param$currentQ3
      param$survey[nrow(param$survey),5] <- param$currentQ4
      param$survey[nrow(param$survey),6] <- param$currentQ5
      param$survey[nrow(param$survey),7] <- param$currentQ6
      param$survey[nrow(param$survey),8] <- param$currentQ7
      param$survey[nrow(param$survey),9] <- param$currentQ8
      param$survey[nrow(param$survey),10] <- param$currentQ9
      param$survey[nrow(param$survey),11] <- param$currentQ10
      param$survey[nrow(param$survey),12] <- param$currentQ11
      param$survey[nrow(param$survey),13] <- param$currentQ12
      
      write.csv(param$survey, file = glue("survey_{param$randID}_{param$groupID}.csv"))
      
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
    glue("当前照片编号{param$picIDpadded},  第{param$index}张")
    
  })
  
  
  output$welcomePage <- renderText({
    
    "<TT><font size=3>亲爱的同学，您好！<br><br>在本工作中，我们需要您依次浏览若干张在医疗众筹案例中所呈现的图片，并对每张图片进行一些评估。<br><br>

    注意：这些图片均来自于真实的医疗众筹案例，其中某些图片可能会让您感觉不适。一旦感觉不适，您可以随时暂停浏览，待休息调整好之后再继续浏览评估图片。您也可以随时选择停止浏览图片，停止该工作。
    <br>
    <br>
    感谢您的支持和参与！
    </TT>"
  })
  
  observeEvent(input$Initialize, 
               
               output$welcomePage <- renderText({
                 
                 "<TT><font size=3>请点选实验开始。
    </TT>"
               })
  )
  
  doneWork <- eventReactive(input$Finish, {
    #a <- str_pad(param$startpicID,4, side = c("left"), pad = "0")
    #b <- str_pad(nrow(param$survey) + param$startpicID - 1, 4, side = c("left"), pad = "0")
    glue("rand:{input$randID}, group:{input$groupID}")
  })
  
  
  thankText <- eventReactive(input$Finish, {
    "您已成功提交，再次感谢您的支持和参与！您本次完成的组号为："
  })
  
  output$range <- renderText({
    doneWork()
  })
  
  output$thanks <- renderText({
    thankText()
  })
  
  #output$surveyTable <- renderDT({
  #  param$survey
  #})
  
}

shinyApp(ui, server)