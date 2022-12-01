#' nivel_3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_nivel_3_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' nivel_3 Server Functions
#'
#' @noRd 
mod_nivel_3_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_nivel_3_ui("nivel_3_1")
    
## To be copied in the server
# mod_nivel_3_server("nivel_3_1")
