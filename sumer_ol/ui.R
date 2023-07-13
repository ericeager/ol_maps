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
  
  # Application title
  titlePanel("Sumer OL Pass Blocking Analysis"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    column(width = 12, sidebarPanel(
      selectInput("player", "Player", NAMES, "Jawaan Taylor"), 
      selectInput("position", "Position", c("LT", "LG", "C", "RG", "RT"), "RT"))), 
    
    # Show a plot of the generated distribution
    mainPanel(align = "center",
              plotOutput("plot", width = 750, height = 400),
              dataTableOutput("table")))))
