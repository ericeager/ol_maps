#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(formattable)

#setwd("C:/Users/eags8/Dropbox/pff_bdue")
NAMES <- read.csv("ol_names.csv", stringsAsFactors = FALSE) %>% pull(displayName)

# Define UI for application that draws a histogram

shinyUI(fluidPage(
  
  titlePanel("Sumer OL Pass Blocking Analysis"),
  
  sidebarLayout(
    position = "right",
    sidebarPanel(  
        selectInput("player", "Player", NAMES, "Jawaan Taylor"), 
        selectInput("position", "Position", c("LT", "LG", "C", "RG", "RT"), "RT"), 
        numericInput("N", "Snaps Played", 50), 
        selectInput("cluster", "Cluster", c("All", "Deep", "Shallow"), "All"), 
        numericInput("TTT_l", "Time to Throw Lower Bound", 2), 
        numericInput("TTT_u", "Time to Throw Upper Bound", 3.5), 
        selectInput("play_action", "Play Action", c("yes", "no", "all"), "no"), 
       # selectInput("set", "Set", c("All", "Other", "Down Block", "Vertical Set", "Left Guard", "Right Guard"), "All"),
        selectInput("depth", "Did He Reach Depth?", c("yes", "no", "all"), "all"),
        selectInput("beaten", "Beaten on Play", c("yes", "no", "all"), "all"), 
        selectInput("success", "Offensive Success", c("yes", "no", "all"), "all")),
    
    
    mainPanel(align = "center",
              plotOutput("plot"),
              br(),
              align = "center",
              dataTableOutput("table"),
              
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
              )           
              
    ))))