#' nivel_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#' @import bs4Dash
#'
#' @import highcharter
mod_nivel_1_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Resultados por blocos de indicadores"),
    hr(),
    mod_filtros_ui(ns("filtros_1"), nivel = 1),
    fluidRow(
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          title = HTML("<b style='font-size:20px'> - Indicadores socioeconômicos </b>"),
          icon = icon("1"),
          status = "primary",
          htmlOutput(ns("texto1"))
        )
      ),
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          title = HTML("<b style='font-size:20px'> - Planejamento reprodutivo </b>"),
          icon = icon("2"),
          status = "primary",
          htmlOutput(ns("texto2"))
        ),
        bs4Dash::bs4Card(
          width = 12,
          title = HTML("<b style='font-size:20px'> - Assistência pré-natal </b>"),
          icon = icon("3"),
          status = "primary",
          htmlOutput(ns("texto3"))
        )
      ),
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          title = HTML("<b style='font-size:20px'> - Assistência ao parto </b>"),
          icon = icon("4"),
          status = "primary",
          htmlOutput(ns("table4"))
        ),
        bs4Dash::bs4Card(
          width = 12,
          title = HTML("<b style='font-size:20px'> - Condição de nascimento </b>"),
          icon = icon("5"),
          status = "primary",
          htmlOutput(ns("texto5"))
        )
      ),
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          title = HTML("<b style='font-size:20px'> - Mortalidade materna </b>"),
          icon = icon("6"),
          status = "primary",
          htmlOutput(ns("texto6"))
        )
      ),
      column(
        width = 4,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          title = "Escolaridade",
          highcharter::highchartOutput(ns("plot1"))

        )
      ),
      column(
        width = 4,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          title = "Faixa etária",
          highcharter::highchartOutput(ns("plot2"))

        )
      ),
      column(
        width = 4,
        bs4Dash::bs4Card(
          width = 12,
          status = "primary",
          title = "Cor da pele",
          highcharter::highchartOutput(ns("plot3"))

        )
      )
    )
  )
}

#' nivel_1 Server Functions
#'
#' @noRd
mod_nivel_1_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filtros <- mod_filtros_server("filtros_1", nivel = 1)

    data1 <- reactive({
      bloco1 |>
        dplyr::filter(ano == filtros()$ano) |>
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
        dplyr::group_by(ano) |>
        dplyr::summarise(total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
                         porc_menor20 = sum(nvm_menor_que_20_anos)/total_de_nascidos_vivos,
                         porc_20_34 = sum(nvm_entre_20_e_34_anos)/total_de_nascidos_vivos,
                         porc_mais34 = sum(nvm_maior_que_34_anos)/total_de_nascidos_vivos,
                         porc_pele_preta = sum(nvm_com_cor_da_pele_preta)/total_de_nascidos_vivos,
                         porc_indi = sum(nvm_indigenas)/total_de_nascidos_vivos,
                         porc_escate3 = sum(nvm_com_escolaridade_ate_3)/total_de_nascidos_vivos,
                         porc_esc4_7 = sum(nvm_com_escolaridade_de_4_a_7)/total_de_nascidos_vivos,
                         porc_esc8_11 = sum(nvm_com_escolaridade_de_8_a_11)/total_de_nascidos_vivos,
                         porc_escmais11 = sum(nvm_com_escolaridade_acima_de_11)/total_de_nascidos_vivos) |>
        dplyr::ungroup()
    })

    output$texto1 <- renderUI({
      HTML(paste("<b> Nascidos vivos </b>", data1()$total_de_nascidos_vivos,
                 "<b> % menos de 20 anos </b>", round(data1()$porc_menor20*100,2),
                 "<b> % 20 a 34 anos </b>", round(data1()$porc_20_34*100,2),
                 "<b> % mais de 34 anos </b>", round(data1()$porc_mais34*100,2),
                 "<b> % raça preta </b>", round(data1()$porc_pele_preta*100,2),
                 "<b> % raça indígena </b>", round(data1()$porc_indi*100,2),
                 "<b> % até 3 anos de estudos </b>", round(data1()$porc_escate3*100,2),
                 "<b> % 4 a 7 anos de estudos </b>", round(data1()$porc_esc4_7*100,2),
                 "<b> % 8 a 11 anos de estudos </b>", round(data1()$porc_esc8_11*100,2),
                 "<b> % mais de 11 anos de estudos </b>", round(data1()$porc_escmais11*100,2),
                 sep = '<br/>'))
        })

    data2 <- reactive({
      bloco2 |>
        dplyr::filter(ano == filtros()$ano) |>
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
        dplyr::group_by(ano) |>
        dplyr::summarise(
          total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
          porc_menor20 = round(sum(nvm_menor_que_20)/sum(pop_feminina_10_a_19)*1000,2),
          porc_mais_3pt = round(sum(mulheres_com_mais_de_tres_partos_anteriores)/total_de_nascidos_vivos*100,2),
          porc_int_aborto = round(sum(internacoes_aborto)*0.75*2*(1+(sum(pop_fem_10_49_com_plano_saude)/sum(pop_fem_10_49)))/sum(pop_fem_10_49)*1000,2)
          ) |>
        dplyr::ungroup()
    })

    output$texto2 <- renderUI({
      HTML(paste("<b> % menos de 20 anos </b>",data2()$porc_menor20,
                 "<b> % mais de 3 partos </b>",data2()$porc_mais_3pt,
                 "<b> taxa de internações aborto </b>",data2()$porc_int_aborto,
                 sep = '<br/>'))
    })

    data3 <- reactive({
      bloco3 |>
        dplyr::filter(ano == filtros()$ano) |>
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
        dplyr::group_by(ano) |>
        dplyr::summarise(total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
                         porc_inicio_prec = sum(mulheres_com_inicio_precoce_do_prenatal)/total_de_nascidos_vivos,
                         porc_1con = sum(mulheres_com_pelo_menos_uma_consulta_prenatal)/total_de_nascidos_vivos,
                         porc_7 = sum(mulheres_com_mais_de_sete_consultas_prenatal)/total_de_nascidos_vivos,
                         por_sc = sum(casos_sc)/total_de_nascidos_vivos) |>
        dplyr::ungroup()
    })

    output$texto3 <- renderUI({
      HTML(paste("<b> % início precoce </b>", round(data3()$porc_inicio_prec*100,2),
                 "<b> % pelo menos 1 consulta </b>", round(data3()$porc_1con*100,2),
                 "<b> % 7 ou mais consultas </b>", round(data3()$porc_7*100,2),
                 "<b> % incidencia sífilis </b>", round(data3()$por_sc*100,2),
                 sep = '<br/>'))
    })

    data5 <- reactive({
      bloco5 |>
        dplyr::filter(ano == filtros()$ano) |>
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
        dplyr::group_by(ano) |>
        dplyr::summarise(total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
                         porc_baixo_peso = sum(nascidos_vivos_com_baixo_peso)/total_de_nascidos_vivos,
                         porc_premat = sum(nascidos_vivos_prematuros)/total_de_nascidos_vivos,
                         porc_termo_precoce = sum(nascidos_vivos_termo_precoce)/total_de_nascidos_vivos) |>
        dplyr::ungroup()
    })

    output$texto5 <- renderUI({
      HTML(paste("<b> % baixo peso </b>", round(data5()$porc_baixo_peso*100,2),
                 "<b> % prematuros </b>", round(data5()$porc_premat*100,2),
                 "<b> % nascidos termo precoce </b>", round(data5()$porc_termo_precoce*100,2),
                 sep = '<br/>'))
    })

    data4 <- reactive({
      bloco4 |>
        dplyr::filter(ano == filtros()$ano) |>
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
        dplyr::group_by(ano) |>
        dplyr::summarise(total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
                         part_robs1 = round(sum(mulheres_dentro_do_grupo_de_robson_1)/total_de_nascidos_vivos*100,2),
                         part_robs2=round(sum(mulheres_dentro_do_grupo_de_robson_2)/total_de_nascidos_vivos*100,2),
                         part_robs3=round(sum(mulheres_dentro_do_grupo_de_robson_3)/total_de_nascidos_vivos*100,2),
                         part_robs4=round(sum(mulheres_dentro_do_grupo_de_robson_4)/total_de_nascidos_vivos*100,2),
                         part_robs5=round(sum(mulheres_dentro_do_grupo_de_robson_5)/total_de_nascidos_vivos*100,2),
                         part_robs6_9=round(sum(mulheres_dentro_do_grupo_de_robson_6_ao_9)/total_de_nascidos_vivos*100,2),
                         part_robs10=round(sum(mulheres_dentro_do_grupo_de_robson_10)/total_de_nascidos_vivos*100,2),
                         ces_robs1 = round(sum(total_cesariana_grupo_robson_1)/sum(mulheres_com_parto_cesariana)*100,2),
                         ces_robs2 = round(sum(total_cesariana_grupo_robson_2)/sum(mulheres_com_parto_cesariana)*100,2),
                         ces_robs3 = round(sum(total_cesariana_grupo_robson_3)/sum(mulheres_com_parto_cesariana)*100,2),
                         ces_robs4 = round(sum(total_cesariana_grupo_robson_4)/sum(mulheres_com_parto_cesariana)*100,2),
                         ces_robs5 = round(sum(total_cesariana_grupo_robson_5)/sum(mulheres_com_parto_cesariana)*100,2),
                         ces_robs6_9 = round(sum(total_cesariana_grupo_robson_6_ao_9)/sum(mulheres_com_parto_cesariana)*100,2),
                         ces_robs10 = round(sum(total_cesariana_grupo_robson_10)/sum(mulheres_com_parto_cesariana)*100,2)) |>
        dplyr::ungroup()
    })

    output$table4 <- renderTable({
      data.frame(
        `Grupo de Robson` = c(
          "Grupo Robson 1",
          "Grupo Robson 2",
          "Grupo Robson 3",
          "Grupo Robson 4",
          "Grupo Robson 5",
          "Grupo Robson 6 ao 9",
          "Grupo Robson 10"
        ),
        "Parto" = c(
          data4()$part_robs1,
          data4()$part_robs2,
          data4()$part_robs3,
          data4()$part_robs4,
          data4()$part_robs5,
          data4()$part_robs6_9,
          data4()$part_robs10
        ),
        "Cesáreas" = c(
          data4()$ces_robs1,
          data4()$ces_robs2,
          data4()$ces_robs3,
          data4()$ces_robs4,
          data4()$ces_robs5,
          data4()$ces_robs6_9,
          data4()$ces_robs10
        ),
        check.names = FALSE
      )
    })

    data6 <- reactive({
      bloco6 |>
        dplyr::filter(ano == filtros()$ano) |>
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
        dplyr::group_by(ano) |>
        dplyr::summarise(total_de_nascidos_vivos = sum(nascidos),
                         obitos = sum(obitos_mat_totais),
                         obitos_razao = sum(obitos_mat_totais)/total_de_nascidos_vivos,
                         obitos_diretos = sum(obitos_mat_diretos)/sum(obitos_mat_totais),
                         obitos_abortos = sum(obitos_mat_aborto)/sum(obitos_mat_totais),
                         obitos_hipertens = sum(obitos_mat_hipertensao)/sum(obitos_mat_totais),
                         obitos_hemo = sum(obitos_mat_hemorragia)/sum(obitos_mat_totais),
                         obitos_infec = sum(obitos_mat_infec_puerperal)/sum(obitos_mat_totais)) |>
        dplyr::ungroup()
    })

    output$texto6 <- renderUI({
      HTML(paste("<b> Óbitos maternos </b>", data6()$obitos,
                 "<b> Razão de mortalidade materna </b>", round(data6()$obitos_razao*100000,2),
                 "<b> % de óbitos por causas obstétricas diretas </b>", round(data6()$obitos_diretos*100,2),
                 "<b> % de óbitos por aborto </b>", round(data6()$obitos_abortos*100,2),
                 "<b> % de óbitos por hemorragia </b>", round(data6()$obitos_hemo*100,2),
                 "<b> % de óbitos por infecção puerperal </b>", round(data6()$obitos_infec*100,2),
                 "<b> % de óbitos por hipertensão </b>", round(data6()$obitos_hipertens*100,2),
                 sep = '<br/>'))
    })

    data1_graph <- reactive({
      bloco1 |>
        dplyr::filter(ano == filtros()$ano) |>
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
        )
    })


    #cores pros graficos
    cols <- viridis::inferno(5)
    cols <- substr(cols,0,10)
    cols2 <- c(cols[2],cols[3])


    output$plot1 <- highcharter::renderHighchart({

      total_nvm_com_escolaridade_ate_3 = sum(data1_graph()$nvm_com_escolaridade_ate_3)
      total_nvm_com_escolaridade_de_4_a_7 = sum(data1_graph()$nvm_com_escolaridade_de_4_a_7)
      total_nvm_com_escolaridade_de_8_a_11 = sum(data1_graph()$nvm_com_escolaridade_de_8_a_11)
      total_nvm_com_escolaridade_acima_de_11 = sum(data1_graph()$nvm_com_escolaridade_acima_de_11)

      total <- c(total_nvm_com_escolaridade_ate_3, total_nvm_com_escolaridade_de_4_a_7,
                 total_nvm_com_escolaridade_de_8_a_11, total_nvm_com_escolaridade_acima_de_11)
      categorias <- c("0 a 3 anos", "4 a 7 anos", "8 a 11 anos", "Acima de 11 anos")
      df1 <- data.frame(total, categorias)
      df1$categorias <- factor(df1$categorias, levels = c("0 a 3 anos", "4 a 7 anos", "8 a 11 anos", "Acima de 11 anos"))
      highcharter::hchart(df1, type = "column",highcharter::hcaes(x = categorias, y = total)) |>
        highcharter::hc_xAxis(title = list(text = "")) |>
        highcharter::hc_yAxis(title = list(text = "Soma de nascidos vivos")) |>
        highcharter::hc_add_theme(highcharter::hc_theme_elementary()) |>
        highcharter::hc_colors(cols2)


    })

    output$plot2 <- highcharter::renderHighchart({
      total_nvm_menor_que_20_anos = sum(data1_graph()$nvm_menor_que_20_anos)
      total_nvm_entre_20_e_34_anos = sum(data1_graph()$nvm_entre_20_e_34_anos)
      total_nvm_maior_que_34_anos = sum(data1_graph()$nvm_maior_que_34_anos)

      total <- c(total_nvm_menor_que_20_anos, total_nvm_entre_20_e_34_anos,
                 total_nvm_maior_que_34_anos)
      categorias <- c("Menor que 20 anos", "20 a 34 anos", "35 anos e mais")
      df2 <- data.frame(total, categorias)
      df2$categorias <- factor(df2$categorias,
                               levels = c("Menor que 20 anos", "20 a 34 anos", "35 anos e mais"))
      highcharter::hchart(df2, type = "column",highcharter::hcaes(x = categorias, y = total))|>
        highcharter::hc_xAxis(title = list(text = "")) |>
        highcharter::hc_yAxis(title = list(text = "Soma de nascidos vivos")) |>
        highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
        highcharter::hc_colors(cols2)


    })

    output$plot3 <- highcharter::renderHighchart({
      total_nvm_com_cor_da_pele_branca = sum(data1_graph()$nvm_com_cor_da_pele_branca)
      total_nvm_com_cor_da_pele_preta = sum(data1_graph()$nvm_com_cor_da_pele_preta)
      total_nvm_com_cor_da_pele_amarela = sum(data1_graph()$nvm_com_cor_da_pele_amarela)
      total_nvm_com_cor_da_pele_parda = sum(data1_graph()$nvm_com_cor_da_pele_parda)
      total_nvm_indigenas = sum(data1_graph()$nvm_indigenas)

      total <- c(total_nvm_com_cor_da_pele_branca, total_nvm_com_cor_da_pele_preta,
                 total_nvm_com_cor_da_pele_amarela, total_nvm_com_cor_da_pele_parda,
                 total_nvm_indigenas)
      categorias <- c("Branca", "Preta", "Amarela", "Parda", "Indígena")
      df3 <- data.frame(total, categorias)
      df3$categorias <- factor(df3$categorias,
                               levels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
      highcharter::hchart(df3, type = "column",highcharter::hcaes(x = categorias, y = total))|>
        highcharter::hc_xAxis(title = list(text = "")) |>
        highcharter::hc_yAxis(title = list(text = "Soma de nascidos vivos")) |>
        highcharter::hc_add_theme(highcharter::hc_theme_elementary())|>
        highcharter::hc_colors(cols2)


    })

  })
}

## To be copied in the UI
# mod_nivel_1_ui("nivel_1_1")

## To be copied in the server
# mod_nivel_1_server("nivel_1_1")
