library(shiny)

#TO DO
#1. ASB Add value calculator

source("ggSeriesData_v1.R")

shinyServer(function(input, output) {
  
  #Testing selectize
  data <- reactive(ggSeriesData(input$select.in, start=input$date.range[1], end=input$date.range[2]))
  
  #Graphing Step
  'output$plot <- renderPlot(
    ggplot(data(), aes(x=date, y=candleMiddle, group=symbol)) + 
  geom_line(aes(color=symbol)) + facet_wrap(~symbol))'
  
'  output$plot <- renderPlot(
    ggplot(data(), aes(x=date)) + geom_boxplot(aes(ymin=low, lower=candleLower, middle=candleMiddle, 
                                                                upper=candleUpper, ymax=high, colour=fill), stat="identity") + 
    facet_wrap(~symbol))'
    
  output$plot <- renderPlot(
    ggplot(data(), aes(x=date)) + geom_boxplot(aes(ymin=low, lower=candleLower, middle=candleMiddle, 
                                                      upper=candleUpper, ymax=high, colour=fill), stat="identity") + 
    geom_line(aes(x=date, y=maShort, linetype="50 MA"), color="#404040") + 
    geom_line(aes(x=date, y=maLong, linetype="200 MA"), color="#404040") + 
    scale_color_manual(values = c("Loss"="#de2d26", "Gain"="#31a354"), guide = guide_legend(title = "Daily G/L")) + 
    scale_linetype_manual(values=c("200 MA" = 3, "50 MA" = 2), guide = guide_legend(title = "Moving Average")) +
    labs(x=NULL, y="Price (USD)") +
    theme(panel.background = element_rect(fill = "#FFFFFF", colour="#C9D9E1"), 
          panel.grid.major=element_line(colour="#C9D9E1")) + 
    facet_wrap(~symbol)
    )
  
  'output$table <- renderDataTable({
#    data()[(as.Date(data()$Date) >= as.Date(input$date.range[2]) - 5),]
    data.in <- data()[(as.Date(data()$Date) >= as.Date(input$date.range[2]) - 5),]
    data.in <- data.in[c(1:2,7:8)]
    }, options = list(searching=FALSE, paging=FALSE, info=FALSE))'
  
})