#' filtros UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#' @import bs4Dash
mod_filtros_ui <- function(id, nivel){
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::bs4Card(
        width = 12,
        title = HTML("<b> Filtros </b>"),
        icon = icon("filter"),
        status = "primary",
        fluidRow(
          column(
            width = 4,
            if (nivel == 1) {
              numericInput(
                inputId = ns("ano"),
                label = "Ano",
                value = 2020,
                min = 2012,
                max = 2020,
                width = "75%"
              )
            } else {
              sliderInput(
                inputId = ns("ano"),
                label = "Intervalo de anos",
                min = 2012,
                max = 2020,
                value = c(2012,2020),
                sep = '',
                width = "75%"
              )
            }
          ),
          column(
            width = 4,
            selectInput(
              inputId = ns("nivel"),
              label = "Nível de análise",
              choices = c("Nacional", "Regional", "Macrorregião de saúde", "Microrregião de saúde", "Estadual", "Municipal"),
              width = "75%"
            )
          ),
          column(
            width = 4,
            conditionalPanel(
              condition = "input.nivel == 'Regional'",
              ns = ns,
              selectInput(
                inputId = ns("regiao"),
                label = "Região",
                choices = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"),
                width = "75%"
              )
            ),
            conditionalPanel(
              condition = "input.nivel == 'Macrorregião de saúde'",
              ns = ns,
              selectizeInput(
                inputId = ns("macro"),
                label = "Macrorregião de saúde",
                choices = macro_r_saude_choices,
                options = list(
                  placeholder = "Digite o nome da macrorregião de saúde", maxOptions = 100
                ),
                width = "75%"
              )
            ),
            conditionalPanel(
              condition = "input.nivel == 'Microrregião de saúde'",
              ns = ns,
              selectizeInput(
                inputId = ns("micro"),
                label = "Microrregião de saúde",
                choices = micro_r_saude_choices,
                options = list(
                  placeholder = "Digite o nome da microregião de saúde", maxOptions = 100
                ),
                width = "75%"
              )
            ),
            conditionalPanel(
              condition = "input.nivel == 'Estadual'",
              ns = ns,
              selectInput(
                inputId = ns("estado"),
                label = "Estado",
                choices = estados_choices,
                width = "75%"
              )
            ),
            conditionalPanel(
            condition = "input.nivel == 'Municipal'",
            ns = ns,
            selectizeInput(
              inputId = ns("municipio"),
              label = "Município",
              choices = municipios_choices,
              options = list(
                placeholder = "Digite o nome do município", maxOptions = 100
                ),
              width = "75%"
              )
            )
          ),
          column(
            width = 4,
            if(nivel==2){
              selectInput(
                inputId = ns("nivel_comp"),
                label = "Comparar com outro nível?",
                choices = c("Sim","Não"),
                selected = "Não",
                width = "75%"
              )
            }
          ),
          column(
            width = 4,
            conditionalPanel(
              condition = "input.nivel_comp == 'Sim'",
              ns = ns,
              selectInput(
                inputId = ns("nivel2"),
                label = "Nível de análise",
                choices = c("Nacional", "Regional", "Macrorregião de saúde", "Microrregião de saúde", "Estadual", "Municipal"),
                width = "75%"
              )
            )
          ),
          column(
            width = 4,
            conditionalPanel(
              condition = "input.nivel2 == 'Regional' & input.nivel_comp == 'Sim'",
              ns = ns,
              selectInput(
                inputId = ns("regiao2"),
                label = "Região",
                choices = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"),
                width = "75%"
              )
            ),
            conditionalPanel(
              condition = "input.nivel2 == 'Macrorregião de saúde' & input.nivel_comp == 'Sim'",
              ns = ns,
              selectizeInput(
                inputId = ns("macro2"),
                label = "Macrorregião de saúde",
                choices = macro_r_saude_choices,
                options = list(
                  placeholder = "Digite o nome da macrorregião de saúde", maxOptions = 100
                ),
                width = "75%"
              )
            ),
            conditionalPanel(
              condition = "input.nivel2 == 'Microrregião de saúde' & input.nivel_comp == 'Sim'",
              ns = ns,
              selectizeInput(
                inputId = ns("micro2"),
                label = "Microrregião de saúde",
                choices = micro_r_saude_choices,
                options = list(
                  placeholder = "Digite o nome da microregião de saúde", maxOptions = 100
                ),
                width = "75%"
              )
            ),
            conditionalPanel(
              condition = "input.nivel2 == 'Estadual' & input.nivel_comp == 'Sim'",
              ns = ns,
              selectInput(
                inputId = ns("estado2"),
                label = "Estado",
                choices = estados_choices,
                width = "75%"
              )
            ),
            conditionalPanel(
              condition = "input.nivel2 == 'Municipal' & input.nivel_comp == 'Sim'",
              ns = ns,
              selectizeInput(
                inputId = ns("municipio2"),
                label = "Município",
                choices = municipios_choices,
                options = list(
                  placeholder = "Digite o nome do município", maxOptions = 100
                ),
                width = "75%"
              )
            )
          )
        )
      )
    )
  )
}

#' filtros Server Functions
#'
#' @noRd
mod_filtros_server <- function(id, nivel){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    valores_do_filtro <- reactive({
      list(
        ano = input$ano,
        nivel = input$nivel,
        regiao = input$regiao,
        estado = input$estado,
        municipio = input$municipio,
        macro = input$macro,
        micro = input$micro,
        nivel_comp = input$nivel_comp,
        nivel2 = input$nivel2,
        regiao2 = input$regiao2,
        estado2 = input$estado2,
        municipio2 = input$municipio2,
        macro2 = input$macro2,
        micro2 = input$micro2
        )
    })

    return(valores_do_filtro)

    # updateSelectizeInput(session, inputId = "macro", choices = macro_r_saude_choices, server = TRUE)
    # updateSelectizeInput(session, inputId = "micro", choices = micro_r_saude_choices, server = TRUE)
    # updateSelectizeInput(session, inputId = "estado", choices = estados_choices, server = TRUE)
    # updateSelectizeInput(session, inputId = "municipio", choices = municipios_choices, server = TRUE)
  })
}

## To be copied in the UI
# mod_filtros_ui("filtros_1")

## To be copied in the server
# mod_filtros_server("filtros_1")
