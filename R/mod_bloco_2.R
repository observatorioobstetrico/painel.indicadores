#' bloco_2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bloco_2_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Planejamento Reprodutivo: série histórica"),
    hr(),
    mod_filtros_ui(ns("filtros_1"), nivel = 2),
    fluidRow(
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          title = HTML("<b> Resumo do período </b>"),
          htmlOutput(ns("texto2"))
        )
      ),
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          title = HTML("<b> Proporção de mulheres com < 20 anos </b>"),
          highcharter::highchartOutput(ns("plot1"))
        )
      ),
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          title = HTML("<b> Proporção de mulheres com 3 partos anteriores </b>"),
          highcharter::highchartOutput(ns("plot2"))
        )
      ),
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          title = HTML("<b> Proporção de internações por aborto inseguro </b>"),
          highcharter::highchartOutput(ns("plot3"))
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
            O acesso ao planejamento produtivo e a prevenção de gestações indesejadas ou de alto risco
            é fundamental para que as mulheres tenham uma gestação segura.
            <br>
            Dados sobre o acesso a métodos contraceptivos não estão disponíveis nos sistemas de informação
            brasileiros de uso rotineiro.
            <br>
            "
            )
          )
        ),
        column(
          width = 5,
          bs4Dash::bs4Card(
            width = 12,
            status = "secondary",
            headerBorder = FALSE,
            height = "160px",
            HTML(
              "Os indicadores desse bloco apresentam as necessidades não atendidas de contracepção de forma indireta,
              por meio da taxa de fertilidade em adolescentes, da proporção de mulheres com número elevado de partos,
              e das taxas de aborto inseguro."
            )
          )
        )
      )
    )
  )
}

#' bloco_2 Server Functions
#'
#' @noRd
mod_bloco_2_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filtros <- mod_filtros_server("filtros_1", nivel = 2)

    #cores pros graficos
    cols <- viridis::inferno(5)
    cols <- substr(cols,0,10)
    cols2 <- c(cols[2],cols[3])


    #dados com filtro e calculos
    data2 <- reactive({
      bloco2 |>
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
                         porc_menor20 = round(sum(nvm_menor_que_20)/sum(pop_feminina_10_a_19)*1000,2),
                         porc_mais_3pt = round(sum(mulheres_com_mais_de_tres_partos_anteriores)/total_de_nascidos_vivos*100,2),
                         porc_int_aborto = round(sum(internacoes_aborto)*0.75*2*(1+(sum(pop_fem_10_49_com_plano_saude)/sum(pop_fem_10_49)))/sum(pop_fem_10_49)*1000,2),
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
      bloco2 |>
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
                         porc_menor20 = round(sum(nvm_menor_que_20)/sum(pop_feminina_10_a_19)*1000,2),
                         porc_mais_3pt = round(sum(mulheres_com_mais_de_tres_partos_anteriores)/total_de_nascidos_vivos*100,2),
                         porc_int_aborto = round(sum(internacoes_aborto)*0.75*2*(1+(sum(pop_fem_10_49_com_plano_saude)/sum(pop_fem_10_49)))/sum(pop_fem_10_49)*1000,2)
        ) |>
        dplyr::ungroup()
    })

    output$texto2 <- renderUI({
      HTML(paste("<b> % menos de 20 anos </b>",data_resumo()$porc_menor20,
                 "<b> % mais de 3 partos </b>",data_resumo()$porc_mais_3pt,
                 "<b> taxa de internações aborto </b>",data_resumo()$porc_int_aborto,
                 sep = '<br/>'))
    })

    #dados comparação 2 com filtros e calculos porcentagens (segunda seleção)
    data2_comp <- reactive({
      bloco2 |>
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
                         porc_menor20 = round(sum(nvm_menor_que_20)/sum(pop_feminina_10_a_19)*1000,2),
                         porc_mais_3pt = round(sum(mulheres_com_mais_de_tres_partos_anteriores)/total_de_nascidos_vivos*100,2),
                         porc_int_aborto = round(sum(internacoes_aborto)*0.75*2*(1+(sum(pop_fem_10_49_com_plano_saude)/sum(pop_fem_10_49)))/sum(pop_fem_10_49)*1000,2),
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
      rbind(data2(),data2_comp())
    })

    #gráfico proporção fertilidade <20 anos
    output$plot1 <- highcharter::renderHighchart({

      if(filtros()$nivel_comp=="Não"){
        highcharter::hchart(data2(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = porc_menor20
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
                                               y = porc_menor20,group=class,colour=class)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }

    })

    #ggráfico porcentagem mulheres com 3 partos anteriores
    output$plot2 <- highcharter::renderHighchart({

      if(filtros()$nivel_comp=="Não"){
        highcharter::hchart(data2(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = porc_mais_3pt)) |>
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

    #gráficos proporção internações aborto
    output$plot3 <- highcharter::renderHighchart({
      if(filtros()$nivel_comp=="Não"){
        highcharter::hchart(data2(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = porc_int_aborto)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
      else{
        highcharter::hchart(data_juncao(), type = "line",
                            highcharter::hcaes(x = ano,
                                               y = porc_int_aborto,group=class,colour=class)) |>
          highcharter::hc_xAxis(title = list(text = "")) |>
          highcharter::hc_yAxis(title = list(text = "%")) |>
          highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
          highcharter::hc_colors(cols2)
      }
    })


  })
}

## To be copied in the UI
# mod_bloco_2_ui("bloco_2_1")

## To be copied in the server
# mod_bloco_2_server("bloco_2_1")
