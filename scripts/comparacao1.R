library(abjData)
library(dplyr)

data("pnud_muni")

idh_municipios <- pnud_muni %>%
  filter(ano == 2010) %>%
  select(municipio, idhm) %>%
  # Padronizar nomes (se necessário)
  mutate(municipio = case_when(
    municipio == "Miravânia" ~ "Miravânia",
    municipio == "Mirabela" ~ "Mirabela",
    municipio == "Bocaiúva" ~ "Bocaiúva",
    municipio == "São João da Lagoa" ~ "São João da Lagoa",
    municipio == "Francisco Sá" ~ "Francisco Sá",
    TRUE ~ municipio
  ))

dados_completos <- dados_intermitencia %>%
  left_join(idh_municipios, by = c("Municipality" = "municipio"))

# Depois use o mesmo gráfico, substituindo size = population_2020 por size = idhm