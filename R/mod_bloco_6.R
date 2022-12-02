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
    h2("Mortalidade materna: série histórica"),
    hr(),
    mod_filtros_ui(ns("filtros_1"), nivel = 2),
    fluidRow(
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          title = HTML("<b> Resumo do período </b>"),
          htmlOutput(ns("texto6"))
        )
      ),
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          title = HTML("<b> Frequência de óbitos </b>"),
          highcharter::highchartOutput(ns("plot1"))
        )
      ),
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          title = HTML("<b> Razão de mortalidade por 100000 nascidos vivos </b>"),
          highcharter::highchartOutput(ns("plot2"))
        )
      ),
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          title = HTML("<b> Proporção de morte por causas obstétricas diretas </b>"),
          highcharter::highchartOutput(ns("plot3"))
        )
      )
    ),
    fluidRow(
        column(
          width = 3,
          bs4Dash::bs4Card(
            width = 12,
            status = "primary",
            title = HTML("<b> Proporção de mortes por aborto </b>"),
            highcharter::highchartOutput(ns("plot4"))
          )
        ),
        column(
          width = 3,
          bs4Dash::bs4Card(
            width = 12,
            status = "primary",
            title = HTML("<b> Proporção de mortes por hipertensão </b>"),
            highcharter::highchartOutput(ns("plot5"))
          )
        ),
        column(
          width = 3,
          bs4Dash::bs4Card(
            width = 12,
            status = "primary",
            title = HTML("<b> Proporção de mortes por hemorragia </b>"),
            highcharter::highchartOutput(ns("plot6"))
          )
        ),
        column(
          width = 3,
          bs4Dash::bs4Card(
            width = 12,
            status = "primary",
            title = HTML("<b> Proporção de morte por infecção </b>"),
            highcharter::highchartOutput(ns("plot7"))
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
            TEXTO AQUI
            <br>
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
            TEXTO AQUI
            <br>
            </div>
            "
            )
          )
        )
      )
    )
}

#' bloco_6 Server Functions
#'
#' @noRd
mod_bloco_6_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filtros <- mod_filtros_server("filtros_1", nivel = 2)

    #cores pros graficos
    cols <- viridis::inferno(5)
    cols <- substr(cols,0,10)
    cols2 <- c(cols[2],cols[3])


    #dados com filtro e calculos
    data6 <- reactive({
      bloco6 |>
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
        dplyr::summarise(total_de_nascidos_vivos = sum(nascidos),
                         obitos = sum(obitos_mat_totais),
                         obitos_razao = round(sum(obitos_mat_totais)/total_de_nascidos_vivos*100000,2),
                         obitos_diretos = round(sum(obitos_mat_diretos)/sum(obitos_mat_totais)*100,2),
                         obitos_abortos = round(sum(obitos_mat_aborto)/sum(obitos_mat_totais)*100,2),
                         obitos_hipertens = round(sum(obitos_mat_hipertensao)/sum(obitos_mat_totais)*100,2),
                         obitos_hemo = round(sum(obitos_mat_hemorragia)/sum(obitos_mat_totais)*100,2),
                         obitos_infec = round(sum(obitos_mat_infec_puerperal)/sum(obitos_mat_totais)*100,2),
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

    data_resumo <- reactive({
      bloco6 |>
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
        dplyr::summarise(total_de_nascidos_vivos = sum(nascidos),
                         obitos = sum(obitos_mat_totais),
                         obitos_razao = round(sum(obitos_mat_totais)/total_de_nascidos_vivos*100000,2),
                         obitos_diretos = round(sum(obitos_mat_diretos)/sum(obitos_mat_totais)*100,2),
                         obitos_abortos = round(sum(obitos_mat_aborto)/sum(obitos_mat_totais)*100,2),
                         obitos_hipertens = round(sum(obitos_mat_hipertensao)/sum(obitos_mat_totais)*100,2),
                         obitos_hemo = round(sum(obitos_mat_hemorragia)/sum(obitos_mat_totais)*100,2),
                         obitos_infec = round(sum(obitos_mat_infec_puerperal)/sum(obitos_mat_totais)*100,2)
                         ) |>
        dplyr::ungroup()
    })

    output$texto6 <- renderUI({
      HTML(paste("<b> Óbitos maternos </b>", data_resumo()$obitos,
                 "<b> Razão de mortalidade materna </b>", data_resumo()$obitos_razao,
                 "<b> % de óbitos por causas obstétricas diretas </b>", data_resumo()$obitos_diretos,
                 "<b> % de óbitos por aborto </b>", data_resumo()$obitos_abortos,
                 "<b> % de óbitos por hemorragia </b>", data_resumo()$obitos_hemo,
                 "<b> % de óbitos por infecção puerperal </b>", data_resumo()$obitos_infec,
                 "<b> % de óbitos por hipertensão </b>", data_resumo()$obitos_hipertens,
                 sep = '<br/>'))

    })


    #dados comparação 2 com filtros e calculos porcentagens (segunda seleção)
    data6_comp <- reactive({
      bloco6 |>
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
        dplyr::summarise(total_de_nascidos_vivos = sum(nascidos),
                         obitos = sum(obitos_mat_totais),
                         obitos_razao = round(sum(obitos_mat_totais)/total_de_nascidos_vivos*100000,2),
                         obitos_diretos = round(sum(obitos_mat_diretos)/sum(obitos_mat_totais)*100,2),
                         obitos_abortos = round(sum(obitos_mat_aborto)/sum(obitos_mat_totais)*100,2),
                         obitos_hipertens = round(sum(obitos_mat_hipertensao)/sum(obitos_mat_totais)*100,2),
                         obitos_hemo = round(sum(obitos_mat_hemorragia)/sum(obitos_mat_totais)*100,2),
                         obitos_infec = round(sum(obitos_mat_infec_puerperal)/sum(obitos_mat_totais)*100,2),
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
      rbind(data6(),data6_comp())
    })

    #gráfico frequencia obitos
    output$plot1 <- highcharter::renderHighchart({

      if(filtros()$nivel_comp=="Não"){
        highcharter::hchart(data6(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = obitos
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
                                               y = obitos,group=class,colour=class)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }

    })

    #ggráfico razao mortalidade
    output$plot2 <- highcharter::renderHighchart({

      if(filtros()$nivel_comp=="Não"){
        highcharter::hchart(data6(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = obitos_razao)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary()) |>
          highcharter::hc_colors(cols2)
      }
      else{
        highcharter::hchart(data_juncao(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y =obitos_razao,group=class,colour=class)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
    })

    #gráficos proporção obitos diretos
    output$plot3 <- highcharter::renderHighchart({
      if(filtros()$nivel_comp=="Não"){
        highcharter::hchart(data6(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = obitos_diretos)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
      else{
        highcharter::hchart(data_juncao(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = obitos_diretos,group=class,colour=class)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
    })

    #gráfico obitos abortos
    output$plot4 <- highcharter::renderHighchart({
      if(filtros()$nivel_comp=="Não"){
        highcharter::hchart(data6(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = obitos_abortos)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
      else{
        highcharter::hchart(data_juncao(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = obitos_abortos,group=class,colour=class)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
    })

    #gráfico obitos hipertensão
    output$plot5 <- highcharter::renderHighchart({
      if(filtros()$nivel_comp=="Não"){
        highcharter::hchart(data6(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = obitos_hipertens)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
      else{
        highcharter::hchart(data_juncao(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = obitos_hipertens,group=class,colour=class)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
    })

    #gráfico obitos hemorragia
    output$plot6 <- highcharter::renderHighchart({
      if(filtros()$nivel_comp=="Não"){
        highcharter::hchart(data6(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = obitos_hemo)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
      else{
        highcharter::hchart(data_juncao(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = obitos_hemo,group=class,colour=class)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
    })

    #gráfico obitos infec
    output$plot7 <- highcharter::renderHighchart({
      if(filtros()$nivel_comp=="Não"){
        highcharter::hchart(data6(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = obitos_infec)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
      else{
        highcharter::hchart(data_juncao(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = obitos_infec,group=class,colour=class)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
    })
  })
}

## To be copied in the UI
# mod_bloco_6_ui("bloco_6_1")

## To be copied in the server
# mod_bloco_6_server("bloco_6_1")
