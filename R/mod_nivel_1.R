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
          title = "- Indicadores socioeconômicos",
          icon = icon("1"),
          status = "primary",
          htmlOutput(ns("texto1"))
        )
      ),
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          title = "- Planejamento reprodutivo",
          icon = icon("2"),
          status = "primary",
          htmlOutput(ns("texto2"))
        ),
        bs4Dash::bs4Card(
          width = 12,
          title = "- Assistência pré-natal",
          icon = icon("3"),
          status = "primary",
          htmlOutput(ns("texto3"))
        )
      ),
      column(
        width = 4,
        bs4Dash::bs4Card(
          width = 12,
          title = "- Assistência ao parto",
          icon = icon("4"),
          status = "primary",
          htmlOutput(ns("table4"))
        ),
        bs4Dash::bs4Card(
          width = 12,
          title = "- Condição de nascimento",
          icon = icon("5"),
          status = "primary",
          htmlOutput(ns("texto5"))
        )
      ),
      column(
        width = 3,
        bs4Dash::bs4Card(
          width = 12,
          title = "- Mortalidade materna",
          icon = icon("6"),
          status = "primary",
          htmlOutput(ns("texto6"))
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
      HTML(paste("Nascidos vivos ",data1()$total_de_nascidos_vivos,
                 "% menos de 20 anos ",round(data1()$porc_menor20*100,2),
                 "% 20 a 34 anos ",round(data1()$porc_20_34*100,2),
                 "% mais de 34 anos ",round(data1()$porc_mais34*100,2),
                 "% raça preta ",round(data1()$porc_pele_preta*100,2),
                 "% raça indígena ",round(data1()$porc_indi*100,2),
                 "% até 3 anos de estudos ",round(data1()$porc_escate3*100,2),
                 "% 4 a 7 anos de estudos ",round(data1()$porc_esc4_7*100,2),
                 "% 8 a 11 anos de estudos ",round(data1()$porc_esc8_11*100,2),
                 "% mais de 11 anos de estudos ",round(data1()$porc_escmais11*100,2),
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
        dplyr::summarise(total_de_nascidos_vivos = sum(total_de_nascidos_vivos),
                         porc_menor20 = sum(nvm_menor_que_20)/total_de_nascidos_vivos,
                         porc_mais_3pt = sum(mulheres_com_mais_de_tres_partos_anteriores)/total_de_nascidos_vivos,
                         porc_int_aborto = sum(internacoes_aborto)/total_de_nascidos_vivos) |>
        dplyr::ungroup()
    })

    output$texto2 <- renderUI({
      HTML(paste("% menos de 20 anos ",round(data2()$porc_menor20*100,2),
                 "% mais de 3 partos ",round(data2()$porc_mais_3pt*100,2),
                 "% internações aborto ",round(data2()$porc_int_aborto*100,2),
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
      HTML(paste("% início precoce ",round(data3()$porc_inicio_prec*100,2),
                 "% pelo menos 1 consulta ",round(data3()$porc_1con*100,2),
                 "% 7 ou mais consultas ",round(data3()$porc_7*100,2),
                 #"% incidencia sífilis ",round(data3()$por_sc*100,2),
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
      HTML(paste("% baixo peso ",round(data5()$porc_baixo_peso*100,2),
                 "% prematuros ",round(data5()$porc_premat*100,2),
                 "% nascidos termo precoce ",round(data5()$porc_termo_precoce*100,2),
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
                         part_robs1 = sum(mulheres_dentro_do_grupo_de_robson_1)/total_de_nascidos_vivos,
                         part_robs2=sum(mulheres_dentro_do_grupo_de_robson_2)/total_de_nascidos_vivos,
                         part_robs3=sum(mulheres_dentro_do_grupo_de_robson_3)/total_de_nascidos_vivos,
                         part_robs4=sum(mulheres_dentro_do_grupo_de_robson_4)/total_de_nascidos_vivos,
                         part_robs5=sum(mulheres_dentro_do_grupo_de_robson_5)/total_de_nascidos_vivos,
                         part_robs6_9=sum(mulheres_dentro_do_grupo_de_robson_6_ao_9)/total_de_nascidos_vivos,
                         part_robs10=sum(mulheres_dentro_do_grupo_de_robson_10)/total_de_nascidos_vivos,
                         ces_robs1 = sum(total_cesariana_grupo_robson_1)/part_robs1,
                         ces_robs2 = sum(total_cesariana_grupo_robson_2)/part_robs2,
                         ces_robs3 = sum(total_cesariana_grupo_robson_3)/part_robs3,
                         ces_robs4 = sum(total_cesariana_grupo_robson_4)/part_robs4,
                         ces_robs5 = sum(total_cesariana_grupo_robson_5)/part_robs5,
                         ces_robs6_9 = sum(total_cesariana_grupo_robson_6_ao_9)/part_robs6_9,
                         ces_robs10 = sum(total_cesariana_grupo_robson_10)/part_robs10) |>
      dplyr::ungroup()
    })

    output$table4 <- renderTable({
      data.frame(
        "Grupo Robson" = c(
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
        )
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
                         obitos_hipertens=sum(obitos_mat_hipertensao)/sum(obitos_mat_totais),
                         obitos_hemo = sum(obitos_mat_hemorragia)/sum(obitos_mat_totais),
                         obitos_infec = sum(obitos_mat_infec_puerperal)/sum(obitos_mat_totais)) |>
        dplyr::ungroup()
    })

    output$texto6 <- renderUI({
      HTML(paste("Óbitos ",data6()$obitos,
                 "Razao de mortalidade ",round(data6()$obitos_razao*100000,2),
                 "Causas obstétricas diretas ",round(data6()$obitos_diretos*100,2),
                 "Aborto ", round(data6()$obitos_abortos*100,2),
                 "Hemorragia ", round(data6()$obitos_hemo*100,2),
                 "Infecção ",round(data6()$obitos_infec*100,2),
                 "Hipertensão ",round(data6()$obitos_hipertens*100,2),
                 sep = '<br/>'))
    })


  })
}

## To be copied in the UI
# mod_nivel_1_ui("nivel_1_1")

## To be copied in the server
# mod_nivel_1_server("nivel_1_1")
