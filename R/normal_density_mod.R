normal_density_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBoxOutput(ns("infBox1"), width = 2),
      valueBoxOutput(ns("infBox2"), width = 2),
      valueBoxOutput(ns("infBox3"), width = 2),
      valueBoxOutput(ns("infBox4"), width = 2),
      valueBoxOutput(ns("infBox5"), width = 2),
      valueBoxOutput(ns("infBox6"), width = 2)
      ),
    fluidRow(
      column(width = 8,
             box(title = "Função de densidade de probabilidade", 
                 height = "100%", width = 200, 
                 solidHeader = TRUE, status = "primary",
                 plotOutput(ns("dnorm_plt"), height = 700)
                 )
             ),
      column(width = 2,
             box(title = "Parâmetros e ajustes",
                 height = 520,width = 50,
                 solidHeader = TRUE, status = "primary",
                 fluidRow(
                   column(width = 6,
                          numericInput(inputId = ns('mu'),
                                       label = withMathJax(helpText("\\(\\mu\\)")),
                                       value = 0,
                                       step = 0.1,
                                       width = "80%")
                          ),
                   column(width = 6,
                          numericInput(inputId = ns('sigma'),
                                       label = withMathJax(helpText("\\(\\sigma\\)")),
                                       value = 1,
                                       step = 0.1,
                                       width = "100%")
                          )
                   ),
                 fluidRow(
                   column(width = 7,
                          radioButtons(
                            inputId = ns("type"),
                            label = "Distribuição",
                            width = "100%",
                            choiceNames = c("Normal padrão", "Normal em \\(X\\)",
                                            "Normal em \\(\\overline{X}\\)"),
                            choiceValues = c("Z", "X", "Xb"))
                          ),
                   column(width = 5,
                          numericInput(inputId = ns('n'),
                                       label = 'n',
                                       value = 1,
                                       min = 1,
                                       max = 100,
                                       step = 1,
                                       width = "100%")
                          )
                   ),
                 fluidRow(
                   column(width = 6,
                          numericInput(inputId = ns('qinf'),
                                       label = withMathJax(helpText("\\(q_{inf}\\)")),
                                       value = -1.96,
                                       step = 0.01,
                                       width = "100%")
                          ),
                   column(width = 6,
                          numericInput(inputId = ns('qsup'),
                                       label = withMathJax(helpText("\\(q_{sup}\\)")),
                                       value = 1.96,
                                       step = 0.01,
                                       width = "100%")
                          )
                   )
                 ),
             box(title = "Limites em X", height = 230, width = 50,
                 solidHeader = TRUE, status = "primary",
                 checkboxInput(inputId = ns("xauto"),
                               label = 'Eixo X automático',
                               value = TRUE,
                               width = "100%"),
                 fluidRow(
                   column(width = 6,
                          numericInput(inputId = ns('xlim_inf'),
                                       label = withMathJax(helpText("\\(X_{inf}\\)")),
                                       value = -4,
                                       step = 0.5,
                                       width = "100%")
                          ),
                   column(width = 6,
                          numericInput(inputId = ns('xlim_sup'),
                                       label = withMathJax(helpText("\\(X_{sup}\\)")),
                                       value = 4,
                                       step = 0.5,
                                       width = "100%")
                          )
                   )
                 )
             ),
      column(width = 2,
             box(title = "Nível de confiança", height = 520, width = 200,
                 solidHeader = TRUE, status = "primary",
                 checkboxInput(inputId = ns("from_alpha"),
                               label = 'Nível de significância', 
                               value = FALSE, 
                               width = "100%"),
                 numericInput(inputId = ns('alpha'),
                              label = '\\(\\alpha\\)', 
                              value = 0.05, 
                              min = 0, 
                              max = 1,
                              step = 0.001,
                              width = "50%"),
                 radioButtons(
                   inputId = ns("tail"),
                   label = "Alternativa",
                   width = "50%",
                   choiceNames = c("Unicaudal inferior", "Unicaudal superior", 
                                   "Bicaudal", "Não mostrar"),
                   choiceValues = c("lower", "upper", "two-sided", "none"),
                   selected = "none"),
                 radioButtons(
                   inputId = ns("fill_area"),
                   label = "Preenchimento",
                   width = "50%",
                   choiceNames = c("Interno", "Externo", "Nenhum"),
                   choiceValues = c("inner", "outer", "none"),
                   selected = "none")
                 ),
             box(title = "Limites em Y", height = 230, width = 50, 
                 solidHeader = TRUE, status = "primary",
                 checkboxInput(inputId = ns("yauto"),
                               label ='Eixo Y automatico', 
                               value = TRUE, 
                               width = "100%"),
                 fluidRow(
                   column(width = 6,
                          numericInput(inputId = ns('ylim_inf'),
                                       label = withMathJax(helpText("\\(Y_{inf}\\)")), 
                                       value = 0,
                                       step = 0.01,
                                       min = 0,
                                       width = "100%")
                          ),
                   column(width = 6,
                          numericInput(inputId = ns('ylim_sup'),
                                       label = withMathJax(helpText("\\(Y_{sup}\\)")), 
                                       value = 0.5,
                                       step = 0.01,
                                       max = 1,
                                       width = "100%")
                          )
                   )
                 )
             )
      )
  )
  }

#____________________________________________________________
normal_density_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session){
      # Garante n >= 1
      n <- reactive(
        ifelse(input$n < 1, yes = 1, no = input$n)
      )
      # Garante que 0 <= alpha <= 1
      alpha <- reactive(
        ifelse (input$alpha < 0 | is.na(input$alpha), yes = 0,
                no = ifelse(input$alpha > 1, yes = 1,
                no = input$alpha))
      )
      # Garante mu = 0 na distribuição Z
      mu <- reactive(
        if (input$type == 'Z') {
          0
          } else {
            input$mu
            }
        )
      # Garante sigma maior que zero
      sd <- reactive(
        ifelse(input$sigma <= 0, yes = 1, no = input$sigma)
        )
      # Calcula erro padrão na distribuição das médias amostrais e
      # desvio = 1 na distribuição Z
      sigma <- reactive(
        if (input$type == 'Xb') {
          sd() / sqrt(n())
          } else if (input$type == 'Z') {
            1
            } else if (input$type == 'X') {
              sd()
              }
        )
      # Define limites gráficos em x
      xlimits <- reactive(
        if (!input$xauto) {
          c(input$xlim_inf, input$xlim_sup)
          } else if (input$xauto) {
            c(mu() - 4 * sigma(), mu() + 4 * sigma())
            }
        )
      # Define limites gráficos em y
      ylimits <- reactive(
        if (!input$yauto) {
          c(input$ylim_inf, input$ylim_sup)
          } else if (input$yauto) {
            c(0, dnorm(x = mu(), mean = mu(), sd = sigma()))
            }
        )
      # Ajusta limites da área de rejeição
      qlimits <- reactive(
        if (!input$from_alpha) {
          if (input$tail == 'lower') {
            c(xlimits()[1], input$qinf)
            } else if (input$tail == 'upper') {
              c(input$qsup, xlimits()[2])
              } else if (input$tail == "two-sided") {
                c(input$qinf, input$qsup)
                } else {
                  xlimits()
                  }
          } else if (input$from_alpha){
            if (input$tail == 'lower') {
              c(xlimits()[1], qnorm(p = alpha(),
                                    mean = mu(), sd = sigma()))
              } else if (input$tail == 'upper') {
                c(qnorm(p = 1 - alpha(),
                        mean = mu(), sd = sigma()), xlimits()[2])
                } else if (input$tail == "two-sided") {
                  c(qnorm(p = c(alpha()/2, 1 - alpha()/2),
                          mean = mu(), sd = sigma()))
                  } else {
                    xlimits()
                  }
            }
        )
      # Define segmentos dos limites das áreas de rejeição
      segmentos_df <- reactive(
        data.frame(x = qlimits(),
                   y = dnorm(qlimits(), 
                             mean = mu(), 
                             sd = sigma()))
        )
      # Define áreas de preenchimento
      x_fill <- reactive(
        if (input$fill_area == "inner") {
          if (input$tail == 'lower') {
            c(qlimits()[2], xlimits()[2])
            } else if (input$tail == 'upper') {
              c(xlimits()[1], qlimits()[1])
              } else if (input$tail == "two-sided") {
                qlimits()
                } else {
                  rep(xlimits()[1],2)
                  }
          } else if (input$fill_area == "outer") {
            if (input$tail == 'lower') {
              qlimits()
              } else if (input$tail == 'upper') {
                qlimits()
                } else if (input$tail == "two-sided") {
                  xlimits()
                  } else {
                    rep(xlimits()[1],2)
                    }
            } else {
              rep(xlimits()[1],2)
              }
        )
      # Define cor do preenchimento
      color_outer <- '#d14143'
      color_inner <- '#0e6d9c'
      color_area <- reactive(
        if (input$fill_area == "inner") {
          color_inner
          } else if (input$fill_area == "outer") {
            color_outer
            } else {
              NA
              }
        )
      # Calcula área inferior unicaudal
      area_inferior <- reactive(
        if (input$tail == 'lower') {
          pnorm(qlimits()[2], mean = mu(), sd = sigma(), lower.tail = TRUE)
          } else if (input$tail == 'upper'){
            pnorm(qlimits()[1], mean = mu(), sd = sigma(), lower.tail = TRUE)
            } else if (input$tail == 'two-sided') {
              pnorm(qlimits()[1], mean = mu(), sd = sigma(), lower.tail = TRUE)
              } else {
                0
                }
        )
      # Calcula área superior unicaudal
      area_superior <- reactive(
        if (input$tail == 'lower') {
          1 - area_inferior()
          } else if (input$tail == 'upper'){
            1 - area_inferior()
            } else if (input$tail == 'two-sided') {
              pnorm(qlimits()[2], mean = mu(), sd = sigma(), lower.tail = FALSE)
              } else {
                0
                }
        )
      # Calcula área externa bicaudal
      area_externa <- reactive(
        if (input$tail == 'lower') {
          area_inferior()
          } else if (input$tail == 'upper'){
            area_superior()
            } else if (input$tail == 'two-sided') {
              area_inferior() + area_superior()
              } else {
                0
                }
        )
      # Calcula área interna bicaudal
      area_interna <- reactive(
        if (input$tail == 'lower') {
          1 - area_inferior()
          } else if (input$tail == 'upper'){
            1 - area_superior()
            } else if (input$tail == 'two-sided') {
              1 - (area_inferior() + area_superior())
              } else {
                0
                }
        )
      # Limite inferior
      L_inferior <- reactive(
        if (input$tail == 'lower') {
          qlimits()[2]
          } else if (input$tail == 'upper'){
            NA
            } else if (input$tail == 'two-sided') {
              qlimits()[1]
              } else {
                NA
                }
        )
      # Limite superior
      L_superior <- reactive(
        if (input$tail == 'lower') {
          NA
          } else if (input$tail == 'upper'){
            qlimits()[1]
            } else if (input$tail == 'two-sided') {
              qlimits()[2]
              } else {
                NA
                }
        )
      # Info box
      output$infBox1 <- renderValueBox({
        valueBox(
          value = round(area_inferior(), 3),
          subtitle = "Cauda inferior",
          icon = icon("fill-drip"), color = 'light-blue')
        })
      output$infBox2 <- renderValueBox({
        valueBox(
          value = round(area_superior(), 3),
          subtitle = "Cauda superior",
          icon = icon("fill-drip"), color = "light-blue")
        })
      output$infBox3 <- renderValueBox({
        valueBox(
          value = round(area_interna(), 3),
          subtitle = "Área interna",
          icon = icon("fill-drip"), color = "light-blue")
        })
      output$infBox4 <- renderValueBox({
        valueBox(
          value = round(area_externa(), 3),
          subtitle = "Área externa",
          icon = icon("fill-drip"), color = "light-blue")
        })
      output$infBox5 <- renderValueBox({
        valueBox(
          value = round(L_inferior(), 2),
          subtitle = "q inferior",
          icon = icon("fill-drip"), color = "red")
        })
      output$infBox6 <- renderValueBox({
        valueBox(
          value = round(L_superior(), 2),
          subtitle = "q superior",
          icon = icon("fill-drip"), color = "red")
        })
      # Parâmetros da distribuição normal
      params <- reactive(list(mean = mu(), sd = sigma()))
      # Label do eixo X
      x_label <- reactive(
        if(input$type == "Xb"){
          expression(bar('X'))
          } else if (input$type == "Z" | input$type == 'X'){
            input$type
          }
        )
      # Gráfico da distribuição normal
      output$dnorm_plt <- renderPlot({
        plt_dnorm <- ggplot(data = data.frame(x = xlimits()), 
                            aes(x = x)) +
          stat_function(geom = "line", fun = dnorm, n = 1001,
                        args = params(), size = 2) +
          stat_function(geom = "area", fun = dnorm, n = 1001,
                        args = params(), xlim = x_fill(),
                        fill = color_area(), alpha = 0.6) +
          geom_segment(data = segmentos_df(), 
                       mapping = aes(x = x, xend = x, y = 0, yend = y), 
                       color = color_outer, alpha = 0.5,
                       size = 2, linetype = 'solid') +
          lims(x = xlimits(), y = ylimits()) +
          ylab(expression(italic('f(x)'))) +
          xlab(x_label()) +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5, size = 20),
                axis.title.x = element_text(size = 30),
                axis.title.y = element_text(hjust = 0.5, size = 20, angle = 90),
                axis.text = element_text(size = 20))
        if (input$fill_area == "outer" & input$tail == "two-sided") {
          plt_dnorm +
            stat_function(geom = "area", fun = dnorm, n = 1001,
                          args = params(), xlim = qlimits(),
                          fill = "white")
          } else {
            plt_dnorm
            }
      })
      }
  )
}
