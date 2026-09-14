# Instale o pacote jsonlite se ainda não tiver: install.packages("jsonlite")
library(readxl)
library(dplyr)
library(stringr)
library(jsonlite)

# 1. Carrega a sua planilha do SINISA e filtra as cidades
caminho_ind <- "C:/Users/.../SINISA_AGUA_Indicadores_Base Municipal_2024.xlsx"
raw_ind <- suppressMessages(read_excel(caminho_ind, skip = 9))

regex_exata <- "(?i)^(Montes Claros|Bocai[uú]va|Bonito de Minas|Capit[aã]o En[eé]as|Claro dos Po[cç][oõ]es|C[oô]nego Marinho|Cora[cç][aã]o de Jesus|Francisco S[aá]|Glaucil[aâ]ndia|Janu[aá]ria|Juramento|Juven[ií]lia|Lontra|Manga|Mirabela|Mirav[aâ]nia|Montalv[aâ]nia|Patis|S[aã]o Jo[aã]o da Lagoa|S[aã]o Jo[aã]o do Pacu[ií])$"

dados_sinisa <- raw_ind %>%
  rename(Municipio = 3, UF = 4) %>%
  filter(UF == "MG") %>%
  filter(str_detect(Municipio, regex_exata))

# Pega a coluna do volume
coluna_consumo <- dados_sinisa %>%
  filter(str_detect(Municipio, "(?i)Lagoa")) %>%
  select(where(~ any(as.character(.x) %in% "5.27"))) %>%
  names()

tabela_bruta <- dados_sinisa %>%
  mutate(Volume_Original_m3 = as.numeric(.data[[coluna_consumo[1]]])) %>%
  select(Municipio, Volume_Original_m3) %>%
  filter(!is.na(Volume_Original_m3))

# -----------------------------------------------------------------------------
# 2. A MÁGICA: Puxando o Censo 2022 DIRETO da API do IBGE (Tabela 4712)
# -----------------------------------------------------------------------------
cat("\nConsultando a API do IBGE (Censo 2022)... aguarde.\n")
url_ibge <- "https://apisidra.ibge.gov.br/values/t/4712/n6/in%20n3%2031/p/all/v/all"
raw_ibge <- fromJSON(url_ibge)

# A primeira linha do SIDRA é o cabeçalho descritivo, removemos com [-1, ]
dados_ibge <- as.data.frame(raw_ibge[-1, ]) %>%
  select(Municipio_IBGE = D1N, Media_Moradores_Censo = V) %>%
  mutate(
    # Extrai só o nome da cidade, removendo " - MG" do IBGE
    Municipio_Limpo = str_remove(Municipio_IBGE, " - MG$"),
    Media_Moradores_Censo = as.numeric(Media_Moradores_Censo)
  )

# -----------------------------------------------------------------------------
# 3. Cruzamento Perfeito e Recálculo com Dados Reais
# -----------------------------------------------------------------------------
tabela_final_exata <- tabela_bruta %>%
  left_join(dados_ibge, by = c("Municipio" = "Municipio_Limpo")) %>%
  mutate(
    # A FÓRMULA BLINDADA: Volume * 1000 / (30 dias * Moradores Reais do IBGE)
    Consumo_Exato_L_hab_dia = round((Volume_Original_m3 * 1000) / (30 * Media_Moradores_Censo), 2)
  ) %>%
  arrange(Consumo_Exato_L_hab_dia)

cat("\n================ RANKING DE CONSUMO (CENSO 2022 APLICADO) ================\n")
print(as.data.frame(tabela_final_exata), row.names = FALSE)
cat("=========================================================================\n")