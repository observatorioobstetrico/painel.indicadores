#' bloco_6 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bloco_6_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_filtros_ui(ns("filtros_1"), nivel = 2),
    "Em construção"
  )
}

#' bloco_6 Server Functions
#'
#' @noRd
mod_bloco_6_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filtros <- mod_filtros_server("filtros_1", nivel = 2)

  })
}

## To be copied in the UI
# mod_bloco_6_ui("bloco_6_1")

## To be copied in the server
# mod_bloco_6_server("bloco_6_1")
