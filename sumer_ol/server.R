#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

data_frame <- read.csv("wrangled_pbp.csv", stringsAsFactors = FALSE)
source("ol_function.R")

shinyServer(function(input, output) {
  df <- reactive({ol_function(data_frame, input$player, input$position, input$TTT_l, input$TTT_u, input$play_action, input$beaten, 
                              input$depth)})
  
  output$plot <- ({renderPlot(df()$plot)})
  output$table <- ({renderDataTable(df()$table)})
})