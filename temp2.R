
# call the tm_g_bar module and generate bar plot

library(nestcolor)
library(teal)
library(teal.modules.clinical)
library(teal.modules.general)
library(haven)
library(formatters)
library(rtables)
library(tern)
library(dplyr)

source("tm_g_bar.R")

data <- teal_data( )
data = within(data,{
  
  adsl <- read_sas("adsl.sas7bdat")
  adlb <- read_sas("adlb.sas7bdat")
  adlb <- adlb[(adlb$DTYPE!="LOT" & adlb$DTYPE!="MAXIMUM" & adlb$DTYPE!="MINIMUM" & adlb$ANL01FL=='Y' 
                
                & (adlb$PARAMCD=="ALT" | adlb$PARAMCD=="AST" | adlb$PARAMCD=="BILI" | adlb$PARAMCD=="ALP" ) & adlb$AVISITN<=1101
  ),]
  
  adlb$AVISIT <- factor(adlb$AVISIT, 
                        
                        levels=c("Baseline","Cycle 1 Day 1","Cycle 1 Day 8","Cycle 1 Day 15", "Cycle 2 Day 1", "Cycle 3 Day 1", "Cycle 4 Day 1", 
                                 "Cycle 5 Day 1", "Cycle 6 Day 1", "Cycle 7 Day 1", "Cycle 8 Day 1", "Cycle 9 Day 1", "Cycle 10 Day 1", "Cycle 11 Day 1"
                                 
                        ))
 
 
  

  ADSL <- adsl
  ADLB <- adlb
})

datanames(data) <- c("ADSL", "ADLB")

jk <- join_keys(
  join_key("ADSL", "ADSL", c("STUDYID", "SUBJID")),
  join_key("ADLB", "ADLB", c("STUDYID", "SUBJID", 'PARAMCD'))
)

join_keys(data) <- jk



ADLB <- data[["ADLB"]]
ADSL <- data[["ADSL"]]

app <- init(
  
  data = data, 
  
  modules = modules(
    
    tm_g_bar(
      
      label = "bar chart",
      
      dataname = "ADSL",
      
      parentname = "ADSL",
      
      patient_col = "SUBJID",
      
      plot_height = c(600L, 200L, 2000L),
      
      aval_var = choices_selected(
        
        choices = variable_choices(ADSL, c("RACE","SEX", 'COUNTRY')),
        
        selected = "RACE"
        
      )
      
    
     
    )
  )
)
shinyApp(app$ui, app$server)