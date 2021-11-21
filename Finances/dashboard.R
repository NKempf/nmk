## app.R ##
library(shinydashboard)
# library(shinyWidgets)

# Liste des mois
mois <- c(1:12)
names(mois) <- month.name


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotlyOutput("g1")),
      
      # box(
      #   title = "Controls",
      #   sliderTextInput(
      #     inputId = "STI_mois",
      #     label = "Pick a month:",
      #     choices = mois
      #   )
      # )
    )
  )
)





server <- function(input, output) {

  output$g1 <- renderPlotly({
    ggplotly(g1)
  })
}

shinyApp(ui, server)