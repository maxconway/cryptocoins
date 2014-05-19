# UI
shinyUI(fluidPage(
  titlePanel("title panel"),
  
  sidebarLayout(
    sidebarPanel("sidebar panel",
                 selectInput(inputId = 'currencies',
                             label = 'Currencies',
                             choices = currencies,
                             multiple=TRUE
                 ),
                 selectInput(inputId = 'base',
                             label = 'Base',
                             choices = currencies,
                             selected='BTC'
                 )),
    mainPanel("main panel",
              plotOutput('plot'),
              dataTableOutput(outputId = 'table'))
  )
))