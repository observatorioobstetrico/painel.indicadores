#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#'
#' @noRd
#'
#' @import bs4Dash
#'
#' @import highcharter
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(
    ),
    # Your application UI logic
    bs4Dash::bs4DashPage(
      bs4Dash::bs4DashNavbar(
        title = bs4Dash::bs4DashBrand(
          title = "Indicadores Obstétricos",
          color = "primary",
          href = "https://observatorioobstetricobr.org/",
          image = "www/logo-oobr2.png"
        ),
        status = "primary"
      ),
      bs4Dash::bs4DashSidebar(
        width = "390px",
        status = "navy",
        skin = "light",
        collapsed = TRUE,
        bs4Dash::bs4SidebarMenu(
          bs4Dash::bs4SidebarMenuItem(
            text = "Nível 1",
            tabName = "nivel_1",
            icon = icon("chart-gantt")
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = "Nível 2",
            icon = icon("chart-line"),
            startExpanded = TRUE,
            bs4Dash::bs4SidebarMenuSubItem(
              text = "- Indicadores socioeconômicos",
              tabName = "bloco_1",
              icon = icon("1")
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "- Planejamento reprodutivo",
              tabName = "bloco_2",
              icon = icon("2")
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "- Assistência pré-natal",
              tabName = "bloco_3",
              icon = icon("3")
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "- Assistência ao parto",
              tabName = "bloco_4",
              icon = icon("4")
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "- Condições de nascimento",
              tabName = "bloco_5",
              icon = icon("5")
            ),
            bs4Dash::bs4SidebarMenuSubItem(
              text = "- Mortalidade materna",
              tabName = "bloco_6",
              icon = icon("6")
            )
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = "Nível 3",
            tabName = "nivel_3",
            icon = icon("chart-column")
          )
        )
      ),
      bs4Dash::bs4DashBody(
        bs4Dash::bs4TabItems(
          bs4Dash::bs4TabItem(
            tabName = "nivel_1",
            mod_nivel_1_ui("nivel_1_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "bloco_1",
            mod_bloco_1_ui("bloco_1_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "bloco_2",
            mod_bloco_2_ui("bloco_2_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "bloco_3",
            mod_bloco_3_ui("bloco_3_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "bloco_4",
            mod_bloco_4_ui("bloco_4_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "bloco_5",
            mod_bloco_5_ui("bloco_5_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "bloco_6",
            mod_bloco_6_ui("bloco_6_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "nivel_3",
            mod_nivel_3_ui("nivel_3_1")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "painel.indicadores"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
