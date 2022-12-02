#Carregando a base auxiliar que contém variáveis referentes ao nome do município, UF, região e micro e macrorregiões de saúde
aux_municipios <- read.csv("data-raw/csv/tabela_aux_municipios.csv")

#Lendo os arquivos originais
bloco1_aux <- read.csv("data-raw/csv/indicadores_bloco1_socioeconomicos_2012-2020.csv") |>
  janitor::clean_names()

bloco2_aux <- read.csv("data-raw/csv/indicadores_bloco2_planejamento_reprodutivo_2012-2020.csv") |>
  janitor::clean_names()

bloco3_aux <- read.csv("data-raw/csv/indicadores_bloco3_assistencia_pre-natal_2012-2020.csv") |>
  janitor::clean_names()

bloco4_aux <- read.csv("data-raw/csv/indicadores_bloco4_assistencia_ao_parto_2012-2020.csv") |>
  janitor::clean_names()

bloco5_aux <- read.csv("data-raw/csv/indicadores_bloco5_condicao_de_nascimento_2012-2020.csv") |>
  janitor::clean_names()

bloco6_aux <- read.csv("data-raw/csv/indicadores_bloco6_mortalidade_materna_2012-2020.csv")

#Adicionando as variáveis referentes ao nome do município, UF, região e micro e macrorregiões de saúde
bloco1 <- dplyr::left_join(bloco1_aux, aux_municipios) |>
  dplyr::select(ano, codmunres, municipio, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude, 3:19)

bloco2 <- dplyr::left_join(bloco2_aux, aux_municipios) |>
  dplyr::select(ano, codmunres, municipio, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude, 3:9)

bloco3 <- dplyr::left_join(bloco3_aux, aux_municipios) |>
  dplyr::select(ano, codmunres, municipio, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude, 3:7)

bloco4 <- dplyr::left_join(bloco4_aux, aux_municipios) |>
  dplyr::select(ano, codmunres, municipio, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude, 3:18)

bloco5 <- dplyr::left_join(bloco5_aux, aux_municipios) |>
  dplyr::select(ano, codmunres, municipio, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude, 3:6)

bloco6 <- dplyr::left_join(bloco6_aux, aux_municipios) |>
  dplyr::select(ano, codmunres, municipio, uf, regiao, cod_r_saude, r_saude, cod_macro_r_saude, macro_r_saude, 6:12)

usethis::use_data(bloco1, overwrite = TRUE)
usethis::use_data(bloco2, overwrite = TRUE)
usethis::use_data(bloco3, overwrite = TRUE)
usethis::use_data(bloco4, overwrite = TRUE)
usethis::use_data(bloco5, overwrite = TRUE)
usethis::use_data(bloco6, overwrite = TRUE)



