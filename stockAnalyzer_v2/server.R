library(shiny)

source("ggSeriesData_v1.R")

shinyServer(function(input, output) {
  
  data <- reactive(ggSeriesData(input$select.in, start=input$date.range[1], end=input$date.range[2]))
  
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
  
})
