# UI
shinyUI(fluidPage(
  titlePanel("Cryptocurrency Explorer"),
  
  sidebarLayout(
    sidebarPanel(h3("Select Currencies"),
                 uiOutput('assetselection'),
                 uiOutput('unitselection')
                 ),
    mainPanel(h3("Data"),
              plotOutput('plot'),
              dataTableOutput(outputId = 'table'))
  )
))