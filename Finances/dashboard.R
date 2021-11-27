## app.R ##
library(shinydashboard)
# library(shinyWidgets)

# Liste des mois
mois <- c(1:12)
names(mois) <- month.name


ui <- dashboardPage(
  dashboardHeader(title = "Finances NK"),
  dashboardSidebar(
    menuItem("Patrimoine", tabName = "patrimoine", icon = icon("dashboard")),
    menuItem("Revenus", tabName = "revenus", icon = icon("th")),
    menuItem("Depenses", tabName = "depenses", icon = icon("th")),
    sliderTextInput(
      inputId = "Id096",
      label = "Période d'intérêt", 
      choices = month.abb,
      selected = month.abb[c(4, 8)]
    )
    
    
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      # First tab content
      tabItem(tabName = "patrimoine",
                fluidRow(
                  box(plotlyOutput("g1")),
                  box(plotlyOutput("g2")),
                  
                ),
              # fluidRow(
              #   box(
              #     title = "Controls",
              #     sliderTextInput(
              #       inputId = "STI_mois",
              #       label = "Pick a month:",
              #       choices = mois
              #     )
              #   )
              # )
      ),
      
      # Second tab content
      tabItem(tabName = "revenus",
              fluidRow(
                box(plotlyOutput("g3")),
                # box(plotlyOutput("g2")),
                box(DTOutput('tab_rev'))
              ),
              
              
              
      ),
      
      # Third tab content
      tabItem(tabName = "depenses",
              fluidRow(
                box(plotlyOutput("g4")),
                # box(plotlyOutput("g2")),
                box(plotlyOutput("g5"))
              ),
              
              
              
      )
    )
    
  ) # end body
)



server <- function(input, output) {

  output$g1 <- renderPlotly({
    ggplotly(g1)
  })
  
  output$g2 <- renderPlotly({
    ggplotly(g2)
  })
  
  output$g3 <- renderPlotly({
    ggplotly(g3)
  })
  
  output$tab_rev = renderDT(
    tab_rev, options = list(lengthChange = FALSE)
  )
  
  output$g4 <- renderPlotly({
    ggplotly(g4)
  })
  
  output$g5 <- renderPlotly({
    ggplotly(g5)
  })
  
  
  
  
}

shinyApp(ui, server)