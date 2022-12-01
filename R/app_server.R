#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_nivel_1_server("nivel_1_1")
  mod_bloco_1_server("bloco_1_1")
  mod_bloco_2_server("bloco_2_1")
  mod_bloco_3_server("bloco_3_1")
  mod_bloco_4_server("bloco_4_1")
  mod_bloco_5_server("bloco_5_1")
  mod_bloco_6_server("bloco_6_1")
  mod_nivel_3_server("nivel_3_1")
}
