# dados brutos ------------------------------------------------------------

# Fonte da tabela: https://painel-estatistica.stg.cloud.cnj.jus.br/estatisticas.html

da_assunto <- readr::read_csv(
  "data-raw/datajud_raw/tbl_assunto.csv",
  lazy = FALSE
)

rx_escravidao <- stringr::regex("escrav", ignore_case = TRUE)
da_escravidao <- da_assunto |>
  dplyr::filter(stringr::str_detect(nome_completo_assunto, rx_escravidao))

readr::write_rds(da_escravidao, "data-raw/levantamentos/da_escravidao.rds")

# dados filtrados ---------------------------------------------------------

da_escravidao <- readr::read_rds("data-raw/da_escravidao.rds")

da_plot <- da_escravidao |>
  dplyr::filter(
    stringr::str_detect(sigla_tribunal, "^TJ"),
    sigla_grau == "G1",
    ano %in% 2020:2022
  ) |>
  dplyr::transmute(
    id_orgao_julgador = as.character(id_orgao_julgador),
    sigla_tribunal, ano, nome_completo_assunto,
    ind1, ind2, ind3
  ) |>
  tidyr::pivot_longer(ind1:ind3) |>
  ## para pegar os municípios das varas
  dplyr::inner_join(obsCIEE::varas, c("id_orgao_julgador" = "pd_seq_orgao")) |>
  ## para pegar latitude e longitude dos municípios
  dplyr::inner_join(abjData::muni, c("id_municipio" = "muni_id")) |>
  dplyr::select(id_municipio, sigla_tribunal, ano, name, value, lon, lat) |>
  dplyr::group_by(id_municipio, lon, lat, ano, name) |>
  dplyr::summarise(
    value = sum(value),
    .groups = "drop"
  ) |>
  dplyr::mutate(name = dplyr::case_match(
    name,
    "ind1" ~ "Novos",
    "ind2" ~ "Pendentes",
    "ind3" ~ "Baixados"
  ))


# visualizacao ------------------------------------------------------------

sf <- geobr::read_state()

codigo_link <- "https://github.com/abjur/dataJudEscravidao"

da_plot |>
  dplyr::filter(name == "Pendentes", ano == 2022) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = sf) +
  ggplot2::geom_point(
    ggplot2::aes(lon, lat, size = value),
    alpha = .8,
    colour = viridis::viridis(1, 1, .2, .8)
  ) +
  ggplot2::geom_point(
    ggplot2::aes(lon, lat),
    alpha = 1,
    size = .5,
    colour = viridis::viridis(2, 1, .2, .8)[2]
  ) +
  ggplot2::theme_void(15) +
  ggplot2::labs(
    x = "",
    y = "",
    size = "Quantidade",
    title = "Casos pendentes em 2022",
    subtitle = "Processos relacionados a trabalho análogo à escravidão",
    caption = glue::glue("Fonte: DataJud\nCódigo: {codigo_link}")
  )
