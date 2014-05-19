# Server
shinyServer(function(input, output) {
  
  load('dev/logs/currentorders.RData')
  
  output$table <- renderDataTable({
    currentorders %.% 
      filter(asset %in% input$currencies & unit == input$base) %.%
      select(exchange, asset, unit, type, price, volume) %.%
      arrange(price)
  })
  
  output$plot <- renderPlot(
    print(
      currentorders %.% 
      filter(asset %in% input$currencies & unit == input$base) %.% 
      ggplot(aes(x=price, y=volume, colour=asset, shape=type)) + geom_point()
    )
  )
})