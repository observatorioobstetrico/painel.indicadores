aux_municipios <- read.csv("data-raw/csv/tabela_aux_municipios.csv")

municipios_choices <- aux_municipios |>
  dplyr::pull(municipio)

estados_choices <- aux_municipios |>
  dplyr::pull(uf) |>
  unique()

micro_r_saude_choices <- aux_municipios |>
  dplyr::pull(r_saude) |>
  unique()

macro_r_saude_choices <- aux_municipios |>
  dplyr::pull(macro_r_saude) |>
  unique()

usethis::use_data(municipios_choices, overwrite = TRUE)
usethis::use_data(estados_choices, overwrite = TRUE)
usethis::use_data(micro_r_saude_choices, overwrite = TRUE)
usethis::use_data(macro_r_saude_choices, overwrite = TRUE)
