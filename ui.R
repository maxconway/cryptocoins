# UI
shinyUI(fluidPage(
  titlePanel("Cryptocurrency Explorer"),
  
  sidebarLayout(
    sidebarPanel(h3("Select Currencies"),
                 uiOutput('assetselection'),
                 uiOutput('unitselection'),
                 uiOutput('exchangeselection')
                 ),
    mainPanel(
      tabsetPanel(type = 'tabs',
        tabPanel('Summary', 
                 dataTableOutput('bestbuys'),
                 dataTableOutput('bestsells')
                 ),
        tabPanel('Plot', plotOutput('plot')),
        tabPanel('Table', dataTableOutput(outputId = 'table'))
      )
    )
  )
))