#' bloco_4 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bloco_4_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Assistência ao parto: série histórica"),
    hr(),
    mod_filtros_ui(ns("filtros_1"), nivel = 2),
    fluidRow(
      column(
        width = 4,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          title = HTML("<b> Resumo do período </b>"),
          reactable::reactableOutput(ns("tabela_resumo"))
        )
      ),
      column(
        width = 8,
        fluidRow(
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              title = HTML("<b> Porcentagem de nascidos vivos por grupo de Robson </b>"),
              reactable::reactableOutput(ns("tabela1"))
            )
          ),
          column(
            width = 6,
            bs4Dash::bs4Card(
              width = 12,
              status = "primary",
              title = HTML("<b> Contribuição do grupo de Robson para a taxa global de cesáreas </b>"),
              reactable::reactableOutput(ns("tabela2"))
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        bs4Dash::bs4Card(
          width = 12,
          status = "secondary",
          headerBorder = FALSE,
          HTML(
            "<div>
            A adequada assistência ao parto, com rescursos disponíveis e atendimento oportuno, é essencial
            para o manejo de complicações e redução da mortalidade materna. O uso apropriado de tecnologias
            médicas, e o cuidado centrado nas necessidades da mulher e da sua família, estão entre as recomendações
            mais recentes da Organização Mundial da Saúde para a assistência ao parto.
            </div>
            "
          )
        )
      ),
      column(
        width = 6,
        bs4Dash::bs4Card(
          width = 12,
          status = "secondary",
          headerBorder = FALSE,
          HTML(
            "<div>
            Neste bloco, apresentamos a porcentagem de nascidos de nascidos vivos segundo grupo de Robson e a
            contribuição relativa do número de cesáreas de cada grupo para a taxa global de cesarianas.
            </div>
            "
          )
        )
      )
    )
  )
}

#' bloco_4 Server Functions
#'
#' @noRd
mod_bloco_4_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filtros <- mod_filtros_server("filtros_1", nivel = 2)

    options(reactable.theme = reactable::reactableTheme(
      borderColor = "#dfe2e5",
      stripedColor = "#e5efff",
      highlightColor = "#CDDEFC",
      cellPadding = "8px 12px",
      searchInputStyle = list(width = "100%")
    ))

    data_resumo <- reactive({
      bloco4 |>
        dplyr::filter(ano >= filtros()$ano[1] & ano <= filtros()$ano[2]) |>
        #if(filtros()$nivel == "Estadual") dplyr::filter(uf==filtros()$estado)
        dplyr::filter(
          if (filtros()$nivel == "Nacional")
            ano == filtros()$ano
          else if (filtros()$nivel == "Regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "Estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "Macrorregião de saúde")
            macro_r_saude ==filtros()$macro
          else if(filtros()$nivel == "Microrregião de saúde")
            r_saude == filtros()$micro
          else if(filtros()$nivel == "Municipal")
            municipio == filtros()$municipio
        ) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          mulheres_com_parto_cesariana = sum(mulheres_com_parto_cesariana),
          prop_nasc_robson1 = round((sum(mulheres_dentro_do_grupo_de_robson_1) / total_de_nascidos_vivos) * 100, 2),
          prop_nasc_robson2 = round((sum(mulheres_dentro_do_grupo_de_robson_2) / total_de_nascidos_vivos) * 100, 2),
          prop_nasc_robson3 = round((sum(mulheres_dentro_do_grupo_de_robson_3) / total_de_nascidos_vivos) * 100, 2),
          prop_nasc_robson4 = round((sum(mulheres_dentro_do_grupo_de_robson_4) / total_de_nascidos_vivos) * 100, 2),
          prop_nasc_robson5 = round((sum(mulheres_dentro_do_grupo_de_robson_5) / total_de_nascidos_vivos) * 100, 2),
          prop_nasc_robson6_a_9 = round((sum(mulheres_dentro_do_grupo_de_robson_6_ao_9) / total_de_nascidos_vivos) * 100, 2),
          prop_nasc_robson10 = round((sum(mulheres_dentro_do_grupo_de_robson_10) / total_de_nascidos_vivos) * 100, 2),
          prop_contribuicao_robson1_cesariana = round((sum(total_cesariana_grupo_robson_1) / mulheres_com_parto_cesariana) * 100, 2),
          prop_contribuicao_robson2_cesariana = round((sum(total_cesariana_grupo_robson_2) / mulheres_com_parto_cesariana) * 100, 2),
          prop_contribuicao_robson3_cesariana = round((sum(total_cesariana_grupo_robson_3) / mulheres_com_parto_cesariana) * 100, 2),
          prop_contribuicao_robson4_cesariana = round((sum(total_cesariana_grupo_robson_4) / mulheres_com_parto_cesariana) * 100, 2),
          prop_contribuicao_robson5_cesariana = round((sum(total_cesariana_grupo_robson_5) / mulheres_com_parto_cesariana) * 100, 2),
          prop_contribuicao_robson6_a_9_cesariana = round((sum(total_cesariana_grupo_robson_6_ao_9) / mulheres_com_parto_cesariana) * 100, 2),
          prop_contribuicao_robson10_cesariana = round((sum(total_cesariana_grupo_robson_10) / mulheres_com_parto_cesariana) * 100, 2)
        )
    })

    output$tabela_resumo <- reactable::renderReactable({
      data.frame(
        "Grupo de Robson" = c("1", "2", "3", "4", "5", "6 ao 9", "10"),
        "% de nascidos" = c(
          data_resumo()$prop_nasc_robson1,
          data_resumo()$prop_nasc_robson2,
          data_resumo()$prop_nasc_robson3,
          data_resumo()$prop_nasc_robson4,
          data_resumo()$prop_nasc_robson5,
          data_resumo()$prop_nasc_robson6_a_9,
          data_resumo()$prop_nasc_robson10
        ),
        "% de contribuição para a taxa de cesáreas" = c(
          data_resumo()$prop_contribuicao_robson1_cesariana,
          data_resumo()$prop_contribuicao_robson2_cesariana,
          data_resumo()$prop_contribuicao_robson3_cesariana,
          data_resumo()$prop_contribuicao_robson4_cesariana,
          data_resumo()$prop_contribuicao_robson5_cesariana,
          data_resumo()$prop_contribuicao_robson6_a_9_cesariana,
          data_resumo()$prop_contribuicao_robson10_cesariana
        ),
        check.names = FALSE
      ) |>
        reactable::reactable(
          defaultColDef = reactable::colDef(
            footerStyle = list(fontWeight = "bold")
          ),
          sortable = TRUE,
          resizable = TRUE,
          highlight = TRUE,
          striped = TRUE,
          bordered = FALSE,
          pagination = FALSE,
        )

    })

    data4 <- reactive({
      bloco4 |>
        dplyr::filter(ano >= filtros()$ano[1] & ano <= filtros()$ano[2]) |>
        dplyr::filter(
          if (filtros()$nivel == "Nacional")
            ano >= filtros()$ano[1] & ano <= filtros()$ano[2]
          else if (filtros()$nivel == "Regional")
            regiao == filtros()$regiao
          else if (filtros()$nivel == "Estadual")
            uf == filtros()$estado
          else if (filtros()$nivel == "Macrorregião de saúde")
            macro_r_saude ==filtros()$macro
          else if(filtros()$nivel == "Microrregião de saúde")
            r_saude == filtros()$micro
          else if(filtros()$nivel == "Municipal")
            municipio == filtros()$municipio
        ) |>
        dplyr::select(1, 10:25) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          mulheres_com_parto_cesariana = sum(mulheres_com_parto_cesariana),
          prop_nasc_robson1 = round((sum(mulheres_dentro_do_grupo_de_robson_1) / total_de_nascidos_vivos) * 100, 2),
          prop_nasc_robson2 = round((sum(mulheres_dentro_do_grupo_de_robson_2) / total_de_nascidos_vivos) * 100, 2),
          prop_nasc_robson3 = round((sum(mulheres_dentro_do_grupo_de_robson_3) / total_de_nascidos_vivos) * 100, 2),
          prop_nasc_robson4 = round((sum(mulheres_dentro_do_grupo_de_robson_4) / total_de_nascidos_vivos) * 100, 2),
          prop_nasc_robson5 = round((sum(mulheres_dentro_do_grupo_de_robson_5) / total_de_nascidos_vivos) * 100, 2),
          prop_nasc_robson6_a_9 = round((sum(mulheres_dentro_do_grupo_de_robson_6_ao_9) / total_de_nascidos_vivos) * 100, 2),
          prop_nasc_robson10 = round((sum(mulheres_dentro_do_grupo_de_robson_10) / total_de_nascidos_vivos) * 100, 2),
          prop_contribuicao_robson1_cesariana = round((sum(total_cesariana_grupo_robson_1) / mulheres_com_parto_cesariana) * 100, 2),
          prop_contribuicao_robson2_cesariana = round((sum(total_cesariana_grupo_robson_2) / mulheres_com_parto_cesariana) * 100, 2),
          prop_contribuicao_robson3_cesariana = round((sum(total_cesariana_grupo_robson_3) / mulheres_com_parto_cesariana) * 100, 2),
          prop_contribuicao_robson4_cesariana = round((sum(total_cesariana_grupo_robson_4) / mulheres_com_parto_cesariana) * 100, 2),
          prop_contribuicao_robson5_cesariana = round((sum(total_cesariana_grupo_robson_5) / mulheres_com_parto_cesariana) * 100, 2),
          prop_contribuicao_robson6_a_9_cesariana = round((sum(total_cesariana_grupo_robson_6_ao_9) / mulheres_com_parto_cesariana) * 100, 2),
          prop_contribuicao_robson10_cesariana = round((sum(total_cesariana_grupo_robson_10) / mulheres_com_parto_cesariana) * 100, 2)
        ) |>
        dplyr::ungroup() |>
        tibble::rownames_to_column() |>
        tidyr::pivot_longer(!rowname, names_to = "col1", values_to = "col2") |>
        tidyr::pivot_wider(names_from = "rowname", values_from = "col2") |>
        dplyr::slice(-c(1:3)) |>
        setNames(c("col1", as.character(filtros()$ano[1]:filtros()$ano[2]))) |>
        dplyr::mutate("localidade" = dplyr::case_when(
          filtros()$nivel == "Nacional" ~ "Brasil",
          filtros()$nivel == "Regional" ~ filtros()$regiao,
          filtros()$nivel == "Estadual" ~ filtros()$estado,
          filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
          filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
          filtros()$nivel == "Municipal" ~ filtros()$municipio
          ),
        .after = col1
        )
    })

    data4_comp <- reactive({
      bloco4 |>
        dplyr::filter(ano >= filtros()$ano[1] & ano <= filtros()$ano[2]) |>
        dplyr::filter(
          if (filtros()$nivel2 == "Nacional")
            ano >= filtros()$ano[1]
          else if (filtros()$nivel2 == "Regional")
            regiao == filtros()$regiao2
          else if (filtros()$nivel2 == "Estadual")
            uf == filtros()$estado2
          else if (filtros()$nivel2 == "Macrorregião de saúde")
            macro_r_saude ==filtros()$macro2
          else if(filtros()$nivel2 == "Microrregião de saúde")
            r_saude == filtros()$micro2
          else if(filtros()$nivel2 == "Municipal")
            municipio == filtros()$municipio2
        ) |>
        dplyr::group_by(ano) |>
        dplyr::select(1, 10:25) |>
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          mulheres_com_parto_cesariana = sum(mulheres_com_parto_cesariana),
          prop_nasc_robson1 = round((sum(mulheres_dentro_do_grupo_de_robson_1) / total_de_nascidos_vivos) * 100, 2),
          prop_nasc_robson2 = round((sum(mulheres_dentro_do_grupo_de_robson_2) / total_de_nascidos_vivos) * 100, 2),
          prop_nasc_robson3 = round((sum(mulheres_dentro_do_grupo_de_robson_3) / total_de_nascidos_vivos) * 100, 2),
          prop_nasc_robson4 = round((sum(mulheres_dentro_do_grupo_de_robson_4) / total_de_nascidos_vivos) * 100, 2),
          prop_nasc_robson5 = round((sum(mulheres_dentro_do_grupo_de_robson_5) / total_de_nascidos_vivos) * 100, 2),
          prop_nasc_robson6_a_9 = round((sum(mulheres_dentro_do_grupo_de_robson_6_ao_9) / total_de_nascidos_vivos) * 100, 2),
          prop_nasc_robson10 = round((sum(mulheres_dentro_do_grupo_de_robson_10) / total_de_nascidos_vivos) * 100, 2),
          prop_contribuicao_robson1_cesariana = round((sum(total_cesariana_grupo_robson_1) / mulheres_com_parto_cesariana) * 100, 2),
          prop_contribuicao_robson2_cesariana = round((sum(total_cesariana_grupo_robson_2) / mulheres_com_parto_cesariana) * 100, 2),
          prop_contribuicao_robson3_cesariana = round((sum(total_cesariana_grupo_robson_3) / mulheres_com_parto_cesariana) * 100, 2),
          prop_contribuicao_robson4_cesariana = round((sum(total_cesariana_grupo_robson_4) / mulheres_com_parto_cesariana) * 100, 2),
          prop_contribuicao_robson5_cesariana = round((sum(total_cesariana_grupo_robson_5) / mulheres_com_parto_cesariana) * 100, 2),
          prop_contribuicao_robson6_a_9_cesariana = round((sum(total_cesariana_grupo_robson_6_ao_9) / mulheres_com_parto_cesariana) * 100, 2),
          prop_contribuicao_robson10_cesariana = round((sum(total_cesariana_grupo_robson_10) / mulheres_com_parto_cesariana) * 100, 2)
        ) |>
        dplyr::ungroup() |>
        tibble::rownames_to_column() |>
        tidyr::pivot_longer(!rowname, names_to = "col1", values_to = "col2") |>
        tidyr::pivot_wider(names_from = "rowname", values_from = "col2") |>
        dplyr::slice(-c(1:3)) |>
        setNames(c("col1", as.character(filtros()$ano[1]:filtros()$ano[2]))) |>
        dplyr::mutate("localidade" = dplyr::case_when(
          filtros()$nivel2 == "Nacional" ~ "Brasil",
          filtros()$nivel2 == "Regional" ~ filtros()$regiao2,
          filtros()$nivel2 == "Estadual" ~ filtros()$estado2,
          filtros()$nivel2 == "Macrorregião de saúde" ~ filtros()$macro2,
          filtros()$nivel2 == "Microrregião de saúde" ~ filtros()$micro2,
          filtros()$nivel2 == "Municipal" ~ filtros()$municipio2
          ),
        .after = col1
        )
    })

    #junção das duas tabelas
    data_juncao <- reactive({
      rbind(data4(), data4_comp())
    })



    output$tabela1 <- reactable::renderReactable({
      if (filtros()$nivel_comp == "Não") {
        data4() |>
          dplyr::slice(1:7) |>
          dplyr::mutate(
            grupo_robson = c(as.character(1:5), "6 a 9", "10"),
            .before = localidade
          ) |>
          dplyr::select(-c(col1, localidade)) |>
          reactable::reactable(
            defaultColDef = reactable::colDef(
              minWidth = 60,
              footerStyle = list(fontWeight = "bold")
              ),
            columns = list(
              grupo_robson = reactable::colDef(
                name = "Grupo de Robson",
                minWidth = 90
              )
            ),
            sortable = TRUE,
            resizable = TRUE,
            highlight = TRUE,
            striped = TRUE,
            bordered = FALSE,
            pagination = FALSE,
          )
      } else {
        data_juncao() |>
          dplyr::slice(1:7, 15:21) |>
          dplyr::mutate(
            grupo_robson = rep(c(as.character(1:5), "6 a 9", "10"), times = 2),
            .after = col1
          ) |>
          dplyr::select(-col1) |>
          reactable::reactable(
            groupBy = "grupo_robson",
            defaultColDef = reactable::colDef(
              minWidth = 60,
              footerStyle = list(fontWeight = "bold")
            ),
            columns = list(
              grupo_robson = reactable::colDef(
                name = "Grupo de Robson",
                minWidth = 90
              ),
              localidade = reactable::colDef(
                name = "Localidade",
                minWidth = 105
              )
            ),
            sortable = TRUE,
            resizable = TRUE,
            highlight = TRUE,
            striped = TRUE,
            bordered = FALSE,
            pagination = FALSE
          )
      }
    })



    output$tabela2 <- reactable::renderReactable({
      if (filtros()$nivel_comp == "Não") {
        data4() |>
          dplyr::slice(8:14) |>
          dplyr::mutate(
            grupo_robson = c(as.character(1:5), "6 a 9", "10"),
            .after = col1
          ) |>
          dplyr::select(-c(col1, localidade)) |>
          reactable::reactable(
            defaultColDef = reactable::colDef(
              minWidth = 60,
              footerStyle = list(fontWeight = "bold")
              ),
            columns = list(
              grupo_robson = reactable::colDef(
                name = "Grupo de Robson",
                minWidth = 90
              )
            ),
            sortable = TRUE,
            resizable = TRUE,
            highlight = TRUE,
            striped = TRUE,
            bordered = FALSE,
            pagination = FALSE,
          )
      } else {
        data_juncao() |>
          dplyr::slice(8:14, 22:28) |>
          dplyr::mutate(
            grupo_robson = rep(c(as.character(1:5), "6 a 9", "10"), times = 2),
            .after = col1
          ) |>
          dplyr::select(-col1) |>
          reactable::reactable(
            groupBy = "grupo_robson",
            defaultColDef = reactable::colDef(
              minWidth = 60,
              footerStyle = list(fontWeight = "bold")
            ),
            columns = list(
              grupo_robson = reactable::colDef(
                name = "Grupo de Robson",
                minWidth = 90
              ),
              localidade = reactable::colDef(
                name = "Localidade",
                minWidth = 105
              )
            ),
            sortable = TRUE,
            resizable = TRUE,
            highlight = TRUE,
            striped = TRUE,
            bordered = FALSE,
            pagination = FALSE
          )
      }
    })

  })
}

## To be copied in the UI
# mod_bloco_4_ui("bloco_4_1")

## To be copied in the server
# mod_bloco_4_server("bloco_4_1")
