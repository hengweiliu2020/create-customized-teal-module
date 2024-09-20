library(haven)
library(shinydashboard)
library(DT)
library(tidyr)
library(shiny)
library(ggplot2)
library(dplyr)
library(stringi)
library(shinythemes)
library(nestcolor)
library(teal)
library(teal.modules.clinical)
library(teal.modules.general)
library(plotly)
library(teal.widgets)
source("tm_custom_lineplot.R")

data <- teal_data( )
data <- eval_code(data, readLines("convert_to_factor.R"))
data = within(data,{
  
  adlb <- read_sas("adlb.sas7bdat")
  adsl <- read_sas("adsl.sas7bdat")
  
  adlb <- adlb[(adlb$DTYPE!="LOT" & adlb$DTYPE!="MAXIMUM" & adlb$DTYPE!="MINIMUM" & adlb$ANL01FL=='Y' 
                
                & (adlb$PARAMCD=="ALT" | adlb$PARAMCD=="AST" | adlb$PARAMCD=="BILI" | adlb$PARAMCD=="ALP" ) & adlb$AVISITN<=1101
  ),]
  
  adlb$AVISIT <- factor(adlb$AVISIT, 
                        
                        levels=c("Baseline","Cycle 1 Day 1","Cycle 1 Day 8","Cycle 1 Day 15", "Cycle 2 Day 1", "Cycle 3 Day 1", "Cycle 4 Day 1", 
                                 "Cycle 5 Day 1", "Cycle 6 Day 1", "Cycle 7 Day 1", "Cycle 8 Day 1", "Cycle 9 Day 1", "Cycle 10 Day 1", "Cycle 11 Day 1"
                                 
                        ))
  
  adlb$VISIT <- factor(adlb$VISIT, levels=( c("SCREENING", "CYCLE 1 DAY 1", "CYCLE 1 DAY 8", "CYCLE 1 DAY 15", "CYCLE 2 DAY 1", "CYCLE 3 DAY 1", "CYCLE 4 DAY 1", 
                                              "CYCLE 5 DAY 1", "CYCLE 6 DAY 1", "CYCLE 7 DAY 1", "CYCLE 8 DAY 1", "CYCLE 9 DAY 1", "CYCLE 10 DAY 1", 
                                              "CYCLE 11 DAY 1"
    
  )))
  
  adlb <- adlb[c("SUBJID","PARAMCD","PARAM","AVISITN",'AVISIT',"TRTA", 'AVAL','CHG','ANRHI','ANRLO', 'ADY', 'VISITNUM','VISIT')]
  
  temp <- adsl[c("SUBJID",'SEX','RACE')]
  adlb <- merge(adlb, temp, by=c("SUBJID"), all.x=TRUE)
  
  ADLB <- adlb
  ADLB <- convert_to_factor(ADLB)
})

datanames(data) <- "ADLB"

app <- init(
  
  data=data, 
  
  modules =modules(
    
    tm_custom_lineplot(label="Lab", 
                     dataname="ADLB", 
                     
                     param_var=choices_selected(
                       choices=c("PARAMCD","PARAM"), 
                       selected="PARAM"), 
                     aval_var=choices_selected(
                         choices=c("AVAL","CHG"), 
                         selected="AVAL"),
                     xaxis_var=choices_selected(
                       choices=c("AVISIT","ADY", 'VISIT'), 
                       selected="AVISIT")
                     
                     
                     ) 
  ) 
  )

shinyApp(app$ui, app$server)

