#' bloco_3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bloco_3_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_filtros_ui(ns("filtros_1"), nivel = 2),
    "Em construção"
  )
}

#' bloco_3 Server Functions
#'
#' @noRd
mod_bloco_3_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filtros <- mod_filtros_server("filtros_1", nivel = 2)

  })
}

## To be copied in the UI
# mod_bloco_3_ui("bloco_3_1")

## To be copied in the server
# mod_bloco_3_server("bloco_3_1")
