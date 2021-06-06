library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

ui <- dashboardPage(skin = "red",
    dashboardHeader(
        title = "PROBEST - Probabilidade e Estatística nas Ciências Ambientais",
        titleWidth = 610),
    
    dashboardSidebar(width = 250,
                     sidebarMenu(
                         h3(""),
                         menuItem("Distribuição normal", 
                                  tabName = "normdens", 
                                  icon = icon("signal"))
                         )
                     ),
    dashboardBody(
        tabItems(
            tabItem("normdens",
                    normal_density_UI("normal_density_mod")
                    )
            )
        )
    )

server <- function(input, output, session) {
    normal_density_server("normal_density_mod")
    }

shinyApp(ui, server)