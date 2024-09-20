

tm_custom_lineplot <- function(label, dataname,  param_var, aval_var, xaxis_var){
  module(
    label=label, 
    
    ui = function(id){
      ns <- NS(id)
      fluidPage(
        tagList(
        fluidRow(
          column(width=3, box(title ="Encodings", solidHeader =TRUE, status = "primary",width = NULL,
                              
                              box( status = "primary",width = NULL,
                                   inputPanel(uiOutput(ns("subjid")))),
                              
                              box( status = "primary",width = NULL,
                                   inputPanel(uiOutput(ns("par_var")))),
                              
                              box( status = "primary",width = NULL,
                                   inputPanel(uiOutput(ns("paramcd")))),
                              box( status = "primary",width = NULL,
                                   inputPanel(uiOutput(ns("aval_var")))), 
                              box( status = "primary",width = NULL,
                                   inputPanel(uiOutput(ns("xaxis_var")))) 
                              
                              )), 
          column(width=9, box(title = " ", width = NULL, status = "primary", solidHeader = TRUE ,
                              plotlyOutput(ns("spPlot"),  width = "100%", height = "500px"))) 
          ))
      )
        
      },
    
    server = function(id, data){
      moduleServer(id,
                   function(input, output, session) {
                     ns <- session$ns
                     
                     lb <-reactive({
                       data()[[dataname]]
                     }) 
                       
                     output$par_var = renderUI ({
                       
                         selectInput(inputId =ns("par_var"), label = "Select a parameter variable:",  multiple=FALSE,
                                     choices = param_var$choices, selected = param_var$selected )
                     })
                     
                     output$aval_var = renderUI ({
                       selectInput(inputId =ns("aval_var"), label = "Select a variable for y-axis:",  multiple=FALSE,
                                   choices = aval_var$choices, selected = aval_var$selected )
                     })
                     
                     output$xaxis_var = renderUI ({
                       selectInput(inputId =ns("xaxis_var"), label = "Select a variable for x-axis:",  multiple=FALSE,
                                   choices = xaxis_var$choices, selected = xaxis_var$selected )
                     })
                     
                     paramval  <-  reactive({ 
                       c(unique(lb()[[input$par_var]])) 
                       })
                     
                     subjval <-  reactive({ c(unique(lb()$SUBJID)) })
                
                     output$paramcd = renderUI ({
                       selectInput(inputId =ns("paramcd"), label = "Select a parameter:",  multiple=TRUE,
                                   choices = paramval(), selected = paramval()[1] )
                     })
                     
                     output$subjid = renderUI ({
                       selectInput(inputId =ns("subjid"), label = "Select a subjid:",  multiple=FALSE,
                                   choices = subjval(), selected = subjval()[1] )
                     })
                
                     
                     dataInput1 <- reactive({
                       LB2 <- lb()[(lb()[[input$par_var]] %in% c(input$paramcd) & lb()$SUBJID %in% c(input$subjid) ),]
                       LB2
                     })
                     
                     output$spPlot <- renderPlotly({ 
                       myPlot <- ggplot(dataInput1(), aes(x = dataInput1()[[input$xaxis_var]], 
                                                          y = dataInput1()[[input$aval_var]], group=dataInput1()[[param_var$selected]], col=dataInput1()[[param_var$selected]])) + 
                         labs(title = "Line plot by PARAMCD for Lab Result", 
                              x=input$xaxis_var, y = "Lab Result") +
                         
                         geom_line(size=1) + 
                         
                         scale_color_manual(values=c("red","black","green","purple", "pink","blue","orange","yellow")) +
                         theme(axis.text.x = element_text(angle = 60)) +
                         guides(fill=guide_legend(title=NULL)) +
                         theme(axis.text.y = element_text(size = 8)) 
                         
                       p = ggplotly(myPlot, tooltip = "text") %>% plotly::layout(legend = list(title=list(text='Parameter'), orientation = "v", x = 1.05, y = 0.95))
                       p
                       
                       
                     }) 
                     
                   }) 
      
      
    }
    
    
  )
    
  
}
