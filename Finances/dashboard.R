## app.R ##

# Ressource : https://mastering-shiny.org/preface.html
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# Données
load("input/Comptes/synthese_fin.RData")


# Liste des mois
mois <- c(1:12)
names(mois) <- month.name


ui <- dashboardPage(
  dashboardHeader(title = "Finances NK"),
  dashboardSidebar(
    
    sliderTextInput(
      inputId = "si_periode_filtre",
      label = "Période d'intérêt", 
      choices = unique(compte$date),
      selected = c(min(unique(compte$date)),max(unique(compte$date)))
    ),
    
    sidebarMenu(
    menuItem("Patrimoine", tabName = "patrimoine", icon = icon("landmark")),
    menuItem("Revenus", tabName = "revenus", icon = icon("coins")),
    menuItem("Depenses", tabName = "depenses", icon = icon("credit-card")),
    menuItem("Epargne", tabName = "epargne", icon = icon("piggy-bank")),
    menuItem("PEA", tabName = "pea", icon = icon("chart-line"))
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
              fluidRow(
                box(
                  title = "Controls",
                  sliderTextInput(
                    inputId = "STI_mois",
                    label = "Pick a month:",
                    choices = mois
                  )
                ),
                box(
                  title = "Test",
                  verbatimTextOutput("test")
                  )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "revenus",
              fluidRow(
                box(plotlyOutput("rev.g1")),
                # box(plotlyOutput("g2")),
                box(DTOutput('tab_rev'))
              ),
              fluidRow(
                box(plotlyOutput("rev.g2")),
                # box(plotlyOutput("g2")),
                # box(DTOutput('tab_rev'))
              )
              
              
      ),
      
      # Third tab content
      tabItem(tabName = "depenses",
              fluidRow(
                box(plotlyOutput("dep.g1")),
                # box(plotlyOutput("g2")),
                box(DTOutput("dep.g2"))
                
              ),
              fluidRow(
                box(plotlyOutput("g5"))
              )
              
              
              
      ),
      # 4th tab content
      tabItem(tabName = "pea",
              fluidRow(
                box(plotlyOutput("divi.g2")),
                # box(plotlyOutput("g2")),
                # box(plotlyOutput("g5"))
              ),
              
              
              
      )
      
      
      
    )
    
  ) # end body
)



server <- function(input, output) {

# SERVER Reactive Values----
  # rv <- reactiveValues(
  #   compte.db = compte,
  #   fin.db = fin2
  # )
  # 
  
  # Compte filtré par la période d'intérêt
  compte.db <- reactive({
    compte %>% 
      filter(date >= input$si_periode_filtre[1] & date <= input$si_periode_filtre[2])
    
  })
  
  fin.db <- reactive({
    fin2 %>% 
      filter(date >= input$si_periode_filtre[1] & date <= input$si_periode_filtre[2])
  })
  
  
  output$test <- renderPrint({
    paste0("MIN = : ",min(compte.db()$date)," et MAX = : " ,max(compte.db()$date))
  })
  
  
  
  
  
  
  
  
  
  output$g1 <- renderPlotly({
    ggplotly(g1)
  })
  
  output$g2 <- renderPlotly({
    ggplotly(g2)
  })
  
  output$rev.g1 <- renderPlotly({
    ggplotly(rev.g1)
  })
  
  output$tab_rev = renderDT(
    tab_rev, options = list(lengthChange = FALSE)
  )
  
  output$rev.g2 <- renderPlotly({
    ggplotly(rev.g2)
  })
  
  output$dep.g1 <- renderPlotly({
    ggplotly(dep.g1)
  })
  
  
  output$dep.g2 = renderDT(
    dep.g2, options = list(lengthChange = FALSE)
  )
  
  output$g5 <- renderPlotly({
    ggplotly(g5)
  })
  
  
  
  output$divi.g2 <- renderPlotly({
    divi.g2
  })
  
  
}

shinyApp(ui, server)
