# Server

library(dplyr)
library(ggplot2)

shinyServer(function(input, output) {
  
  currentorders <- reactiveFileReader(
    intervalMillis=1000,
    filePath = 'dev/logs/currentorders.rds',
    session = NULL,
    readFunc = readRDS
  )
  
  asset_currencies <- reactive({unique(currentorders()$asset)})
  unit_currencies <- reactive({unique(currentorders()$unit)})
  exchanges <- reactive({unique(currentorders()$exchange)})
  
  output$assetselection <- renderUI({
    selectInput(inputId = 'currencies',
                label = 'Currencies',
                choices = asset_currencies(),
                selected = 'LTC',
                multiple = TRUE
    )
  })
  
  output$unitselection <- renderUI({
    selectInput(inputId = 'base',
                label = 'Base',
                choices = unit_currencies(),
                selected='BTC'
    )
  })
  
  output$exchangeselection <- renderUI({
    checkboxGroupInput(inputId = 'exchanges',
                       label = 'Exchanges',
                       choices = sort(exchanges()),
                       selected = exchanges()
    )
  })
  
  ordertable <- reactive({
    currentorders() %.% 
      filter(asset %in% input$currencies & unit == input$base & exchange %in% input$exchanges) %.%
      select(exchange, asset, unit, type, price, volume) %.%
      arrange(price)
  })
  
  output$table <- renderDataTable({
    ordertable()
  })
  
  output$plot <- renderPlot({
    minsells <- ordertable() %.% 
      filter(type=='sell') %.% 
      group_by(interaction(asset,unit,exchange)) %.% 
      mutate(minsell = min(price)) %.% 
      getElement('minsell') %.% 
      unique()
    
    maxbuys <- ordertable() %.% 
      filter(type=='buy') %.% 
      group_by(interaction(asset,unit,exchange)) %.% 
      mutate(maxbuy = max(price)) %.% 
      getElement('maxbuy') %.% 
      unique()
    
    print(
      ordertable() %.% 
        ggplot(aes(x=price, y=volume, colour=asset, shape=type)) + 
        geom_point() + 
        coord_cartesian(xlim = c(0.9*min(maxbuys,minsells), 1.1*max(maxbuys, minsells))) +
        scale_y_log10()
    )
  })
})