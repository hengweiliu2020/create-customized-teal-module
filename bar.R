#create bar chart for a categorical variable



library(ggplot2)
library(haven)
library(dplyr)
# Read in the SAS data
vs <- read_sas("adsl.sas7bdat")


library(shiny)
data.frame(vs)

# Define UI for app 
ui <- fluidPage(
  
  # App title ----
  titlePanel("Race Category"),
  
  # Main panel for displaying outputs ----
  mainPanel(
    plotOutput(outputId = "distPlot")
    
  )
)

# Define server logic 
server <- function(input, output) {
  
  agg <- count(vs, RACE)
  agg <- agg[c("RACE","n")]
  
  output$distPlot <- renderPlot({
    ggplot(agg ) + 
      geom_bar(aes(x=RACE, y=n, group=agg$RACE, col=agg$RACE, fill=agg$RACE), stat="identity") +
      scale_color_manual(values=c("red","black","green","purple", "pink","blue","orange","yellow")) 
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

