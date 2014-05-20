# UI
shinyUI(fluidPage(
  titlePanel("Cryptocurrency Explorer"),
  
  sidebarLayout(
    sidebarPanel(h3("Select Currencies"),
                 uiOutput('assetselection'),
                 uiOutput('unitselection')
                 ),
    mainPanel(
      tabsetPanel(type = 'tabs',
        tabPanel('Plot', plotOutput('plot')),
        tabPanel('Table', dataTableOutput(outputId = 'table'))
      )
    )
  )
))