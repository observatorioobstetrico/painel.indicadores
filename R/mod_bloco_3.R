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
    h2("Assistência pré-natal: série histórica"),
    hr(),
    mod_filtros_ui(ns("filtros_1"), nivel = 2),
    fluidRow(
      column(
        width = 4,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          title = HTML("<b> Resumo do período </b>"),
          htmlOutput(ns("texto3"))
          )
        ),
        column(
          width = 4,
          bs4Dash::bs4Card(
            width = 12,
            status = "primary",
            title = HTML("<b> Proporção de mulheres com assistência pré natal </b>"),
            highcharter::highchartOutput(ns("plot1"))
          )
        ),
        column(
          width = 4,
          bs4Dash::bs4Card(
            width = 12,
            status = "primary",
            title = HTML("<b> Proporção de mulheres com inicio precoce ao pré natal </b>"),
            highcharter::highchartOutput(ns("plot2"))
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          bs4Dash::bs4Card(
            width = 12,
            status = "primary",
            title = HTML("<b> Proporção de mulheres com mais de 7 consultas </b>"),
            highcharter::highchartOutput(ns("plot3"))
          )
        ),
        column(
          width = 6,
          bs4Dash::bs4Card(
            width = 12,
            status = "primary",
            title = HTML("<b> Proporção de incidências sífilis congênita </b>"),
            highcharter::highchartOutput(ns("plot4"))
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
              "<p class = negrito>
            A assistência pré-natal é uma ação de saúde efetiva para a redução da mortalidade materna ao permitir:
            <br>
            <b> a) </b> o diagnóstico e o tratamento precoce de doenças pré-existentes e de complicações na gravidez (tais como hipertensão arterial,
             diabetes, sífilis e outras doenças infecciosas;
            <br>
            <b> b) </b> a adoção de medidas preventivas, como vacinas e suplementos alimentares;
            <br>
            <b> c) </b> e o fornecimento de orientações e preparação para o parto e o aleitamento
            materno, bem como para redução/cessação do fumo, do uso do álcool e de outras drogas.
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
              "<p class = negrito>
              Neste bloco, o gestor pode companhar a proporção de mulheres que tem pelo menos
              uma consulta de pré-natal,a proporção de mulheres com início precoce do acompanhamento
              e a proporção de mulheres com o número mínimo de consultas recomendado pela Organização Mundial de Saúde.
              Pode também avaliar as incidência de sífilis congênita, que é considerado um evento sentinela da qualidade da assistência pré-natal.
              <br>
              "
              )
            )
          )
        ),
    )
  }

#' bloco_3 Server Functions
#'
#' @noRd
mod_bloco_3_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filtros <- mod_filtros_server("filtros_1", nivel = 2)

    #cores pros graficos
    cols <- viridis::inferno(5)
    cols <- substr(cols,0,10)
    cols2 <- c(cols[2],cols[3])


    #dados com filtro e calculos
    data3 <- reactive({
      bloco3 |>
        dplyr::filter(ano >= filtros()$ano[1] & ano <= filtros()$ano[2]) |>
        #if(filtros()$nivel == "Estadual") dplyr::filter(uf==filtros()$estado)
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
        dplyr::group_by(ano) |>
        dplyr::summarise(total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
                         porc_inicio_prec = round(sum(mulheres_com_inicio_precoce_do_prenatal)/total_de_nascidos_vivos*100,2),
                         porc_1con = round(sum(mulheres_com_pelo_menos_uma_consulta_prenatal)/total_de_nascidos_vivos*100,2),
                         porc_7 = round(sum(mulheres_com_mais_de_sete_consultas_prenatal)/total_de_nascidos_vivos*100,2),
                         por_sc = round(sum(casos_sc)/total_de_nascidos_vivos*100,2),
                         class = dplyr::case_when(
                           filtros()$nivel == "Nacional" ~ "Brasil",
                           filtros()$nivel == "Regional" ~ filtros()$regiao,
                           filtros()$nivel == "Estadual" ~ filtros()$estado,
                           filtros()$nivel == "Macrorregião de saúde" ~ filtros()$macro,
                           filtros()$nivel == "Microrregião de saúde" ~ filtros()$micro,
                           filtros()$nivel == "Municipal" ~ filtros()$municipio
                         )) |>
        dplyr::ungroup()
    })

    #dados resumo geral anos
    data_resumo <- reactive({
      bloco3 |>
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
        dplyr::summarise(total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
                         porc_inicio_prec = round(sum(mulheres_com_inicio_precoce_do_prenatal)/total_de_nascidos_vivos*100,2),
                         porc_1con = round(sum(mulheres_com_pelo_menos_uma_consulta_prenatal)/total_de_nascidos_vivos*100,2),
                         porc_7 = round(sum(mulheres_com_mais_de_sete_consultas_prenatal)/total_de_nascidos_vivos*100,2),
                         por_sc = round(sum(casos_sc)/total_de_nascidos_vivos*100,2)
        ) |>
        dplyr::ungroup()
    })

    output$texto3 <- renderUI({
      HTML(paste("<b> % início precoce </b>", data_resumo()$porc_inicio_prec,
                 "<b> % pelo menos 1 consulta </b>", data_resumo()$porc_1con,
                 "<b> % 7 ou mais consultas </b>", data_resumo()$porc_7,
                 "<b> % incidencia sífilis </b>", data_resumo()$por_sc,
                 sep = '<br/>'))
    })

    #dados comparação 2 com filtros e calculos porcentagens (segunda seleção)
    data3_comp <- reactive({
      bloco3 |>
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
        dplyr::summarise(total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
                         porc_inicio_prec = round(sum(mulheres_com_inicio_precoce_do_prenatal)/total_de_nascidos_vivos*100,2),
                         porc_1con = round(sum(mulheres_com_pelo_menos_uma_consulta_prenatal)/total_de_nascidos_vivos*100,2),
                         porc_7 = round(sum(mulheres_com_mais_de_sete_consultas_prenatal)/total_de_nascidos_vivos*100,2),
                         por_sc = round(sum(casos_sc)/total_de_nascidos_vivos*100,2),
                         class = dplyr::case_when(
                           filtros()$nivel2 == "Nacional" ~ "Brasil",
                           filtros()$nivel2 == "Regional" ~ filtros()$regiao2,
                           filtros()$nivel2 == "Estadual" ~ filtros()$estado2,
                           filtros()$nivel2 == "Macrorregião de saúde" ~ filtros()$macro2,
                           filtros()$nivel2 == "Microrregião de saúde" ~ filtros()$micro2,
                           filtros()$nivel2 == "Municipal" ~ filtros()$municipio2
                         )) |>
        dplyr::ungroup()
    })

    #junção dois gráficos
    data_juncao <- reactive({
      rbind(data3(),data3_comp())
    })

    #gráfico proporção cobertura pré-natal
    output$plot1 <- highcharter::renderHighchart({

      if(filtros()$nivel_comp=="Não"){
        highcharter::hchart(data3(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = porc_1con
                            )
        ) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary()) |>
          highcharter::hc_colors(cols2)
      }
      else{
        highcharter::hchart(data_juncao(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = porc_1con,group=class,colour=class)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }

    })

    #ggráfico porcentagem inicio precoce
    output$plot2 <- highcharter::renderHighchart({

      if(filtros()$nivel_comp=="Não"){
        highcharter::hchart(data3(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = porc_inicio_prec)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary()) |>
          highcharter::hc_colors(cols2)
      }
      else{
        highcharter::hchart(data_juncao(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y =porc_mais_3pt,group=class,colour=class)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
    })

    #gráficos proporção mais de 7 consultas
    output$plot3 <- highcharter::renderHighchart({
      if(filtros()$nivel_comp=="Não"){
        highcharter::hchart(data3(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = porc_7)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
      else{
        highcharter::hchart(data_juncao(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = porc_7,group=class,colour=class)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
    })

    #gráficos proporção mais de 7 consultas
    output$plot4 <- highcharter::renderHighchart({
      if(filtros()$nivel_comp=="Não"){
        highcharter::hchart(data3(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = por_sc)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
      else{
        highcharter::hchart(data_juncao(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = por_sc,group=class,colour=class)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
    })
  })
}

## To be copied in the UI
# mod_bloco_3_ui("bloco_3_1")

## To be copied in the server
# mod_bloco_3_server("bloco_3_1")
