#' bloco_5 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#'@import bs4Dash
#'
#'@import highcharter
mod_bloco_5_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Série histórica dos indicadores do Bloco 5: Condições de nascimento"),
    hr(),
    mod_filtros_ui(ns("filtros_1"), nivel = 2),
    fluidRow(
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          title = " - Condições de nascimento",
          icon = icon("5"),
          htmlOutput(ns("texto5"))
        )
      ),
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          title = "Proporção de nascidos vivos com baixo peso",
          highcharter::highchartOutput(ns("plot1"))
        )
      ),
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          title = "Proporção de nascimentos prematuros",
          highcharter::highchartOutput(ns("plot2"))
        )
      ),
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          title = "Proporção de nascimentos termo precoce",
          highcharter::highchartOutput(ns("plot3"))
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
            As condições de nascimento do recém-nato dependem da saúde materna. Embora sejam indicadores relacionados ao bebê,
            devem ser monitorados porque refletem a qualidade dos cuidados recebidos pela gestante durante a assistência pré-natal
            e ao parto.
            <br>
            <br>
            A prematuridade (idade gestacional < 37 semanas) e o baixo peso ao nascer (<2500g) são os principais fatores de risco
            para a mortalidade infantil.
            <br>
            <br>
            Já os recém-nascidos termo precoce (nascidos com 37 e 38 semanas) apresentam maior risco de complicações do que os
            recém-nascidos nascidos termo pleno (nascidos com 39 e 40 semanas).
            <br>
            <br>
            No Brasil, quanto maior a proporção de nascimentos por cesariana, maior a proporção de nascimentos precoces.
            </p>
            "
          )
        )
      ),
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          status = "secondary",
          headerBorder = FALSE,
          height = "160px",
          HTML(
            "A prematuridade (idade gestacional < 37 semanas) e o baixo peso ao nascer (<2500g) são os principais fatores de risco
            para a mortalidade infantil."
          )
        )
      ),
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          status = "secondary",
          headerBorder = FALSE,
          height = "160px",
          HTML(
            "Recém-nascidos termo precoce (nascidos com 37 e 38 semanas) apresentam maior risco de complicações do que os
            recém-nascidos termo pleno (nascidos com 39 e 40 semanas)."
          )
        )
      )
    )
  )
}

#' bloco_5 Server Functions
#'
#' @noRd
mod_bloco_5_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filtros <- mod_filtros_server("filtros_1", nivel = 2)

    #cores pros graficos
    cols <- viridis::inferno(5)
    cols <- substr(cols,0,10)
    cols2 <- c(cols[2],cols[3])

    #dados com filtros e calculos porcentagens
  data5 <- reactive({
    bloco5 |>
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
      dplyr::group_by(ano) |>
      dplyr::summarise(total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
                       porc_baixo_peso = sum(nascidos_vivos_com_baixo_peso)/total_de_nascidos_vivos,
                       porc_baixo_peso2 = round(porc_baixo_peso*100,2),
                       porc_premat = sum(nascidos_vivos_prematuros)/total_de_nascidos_vivos,
                       porc_premat2 = round(porc_premat*100,2),
                       porc_termo_precoce = sum(nascidos_vivos_termo_precoce)/total_de_nascidos_vivos,
                       porc_termo_precoce2 = round(porc_termo_precoce*100,2),
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
      bloco5 |>
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
                         porc_baixo_peso = sum(nascidos_vivos_com_baixo_peso)/total_de_nascidos_vivos,
                         porc_baixo_peso2 = round(porc_baixo_peso*100,2),
                         porc_premat = sum(nascidos_vivos_prematuros)/total_de_nascidos_vivos,
                         porc_premat2 = round(porc_premat*100,2),
                         porc_termo_precoce = sum(nascidos_vivos_termo_precoce)/total_de_nascidos_vivos,
                         porc_termo_precoce2 = round(porc_termo_precoce*100,2)) |>
        dplyr::ungroup()
    })

    output$texto5 <- renderUI({
      HTML(paste("% baixo peso: ",round(data_resumo()$porc_baixo_peso*100,2),
                 "% prematuros ",round(data_resumo()$porc_premat*100,2),
                 "% nascidos termo precoce ",round(data_resumo()$porc_termo_precoce*100,2),
                 sep = '<br/>'))
    })



    #dados comparação 2 com filtros e calculos porcentagens
    data5_comp <- reactive({
      bloco5 |>
        dplyr::filter(ano >= filtros()$ano[1] & ano <= filtros()$ano[2]) |>
        dplyr::filter(
          if (filtros()$nivel2 == "Nacional")
            ano >= filtros()$ano[1] & ano <= filtros()$ano[2]
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
                         porc_baixo_peso = sum(nascidos_vivos_com_baixo_peso)/total_de_nascidos_vivos,
                         porc_baixo_peso2 = round(porc_baixo_peso*100,2),
                         porc_premat = sum(nascidos_vivos_prematuros)/total_de_nascidos_vivos,
                         porc_premat2 = round(porc_premat*100,2),
                         porc_termo_precoce = sum(nascidos_vivos_termo_precoce)/total_de_nascidos_vivos,
                         porc_termo_precoce2 = round(porc_termo_precoce*100,2),
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
      rbind(data5(),data5_comp())
    })

    #gráfico proporção baixo peso
    output$plot1 <- highcharter::renderHighchart({

      if(filtros()$nivel_comp=="Não"){
        highcharter::hchart(data5(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = porc_baixo_peso2
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
                                               y = porc_baixo_peso2,group=class,colour=class)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }

    })

    #gráfico proporção prematuros
    output$plot2 <- highcharter::renderHighchart({

      if(filtros()$nivel_comp=="Não"){
      highcharter::hchart(data5(), type = "line",
                          highcharter::hcaes(x = ano,
                                             y = porc_premat2)) |>
        highcharter::hc_xAxis(title = list(text = "")) |>
        highcharter::hc_yAxis(title = list(text = "%")) |>
        highcharter::hc_add_theme(highcharter::hc_theme_elementary()) |>
        highcharter::hc_colors(cols2)
      }
      else{
        highcharter::hchart(data_juncao(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = porc_premat2,group=class,colour=class)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
    })

    #gráficos proporção termo precoce
    output$plot3 <- highcharter::renderHighchart({
      if(filtros()$nivel_comp=="Não"){
        highcharter::hchart(data5(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = porc_termo_precoce2)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
     }
     else{
      highcharter::hchart(data_juncao(), type = "line",
                          highcharter::hcaes(x = ano,
                                             y = porc_termo_precoce2,group=class,colour=class)) |>
        highcharter::hc_xAxis(title = list(text = "")) |>
        highcharter::hc_yAxis(title = list(text = "%")) |>
        highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
        highcharter::hc_colors(cols2)
       }
    })

    #gráfico proporção baixo peso
    # output$plot1comp <- highcharter::renderHighchart({
    #   data_juncao <- rbind(data5(),data5())
    #   highcharter::hchart(data_juncao, type = "line",
    #                       highcharter::hcaes(x = ano,
    #                                          y = porc_baixo_peso2,group=class,colour=class)) |>
    #     highcharter::hc_xAxis(title = list(text = "")) |>
    #     highcharter::hc_yAxis(title = list(text = "%")) |>
    #     highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
    #     highcharter::hc_colors(cols2)
    # })

    # #gráfico proporção prematuros
    # output$plot2 <- highcharter::renderHighchart({
    #   highcharter::hchart(data5(), type = "line",
    #                       highcharter::hcaes(x = ano,
    #                                          y = porc_premat2)) |>
    #     highcharter::hc_xAxis(title = list(text = "")) |>
    #     highcharter::hc_yAxis(title = list(text = "%")) |>
    #     highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
    #     highcharter::hc_colors(cols2)
    # })
    #
    # #gráficos proporção termo precoce
    # output$plot3 <- highcharter::renderHighchart({
    #   highcharter::hchart(data5(), type = "line",
    #                       highcharter::hcaes(x = ano,
    #                                          y = porc_termo_precoce2)) |>
    #     highcharter::hc_xAxis(title = list(text = "")) |>
    #     highcharter::hc_yAxis(title = list(text = "%")) |>
    #     highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
    #     highcharter::hc_colors(cols2)
    # })


  })
}

## To be copied in the UI
# mod_bloco_5_ui("bloco_5_1")

## To be copied in the server
# mod_bloco_5_server("bloco_5_1")
