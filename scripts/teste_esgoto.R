library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

# 1. Carrega a planilha de INDICADORES DE ESGOTO
caminho_esgoto <- "C:/Users/.../Esgoto - Base Municipal/SINISA_ESGOTO_Indicadores_Base Municipal_2024.xlsx"
raw_esgoto <- suppressMessages(read_excel(caminho_esgoto, skip = 9))

# 2. Filtro BLINDADO para os 20 municípios
regex_exata <- "(?i)^(Montes Claros|Bocai[uú]va|Bonito de Minas|Capit[aã]o En[eé]as|Claro dos Po[cç][oõ]es|C[oô]nego Marinho|Cora[cç][aã]o de Jesus|Francisco S[aá]|Glaucil[aâ]ndia|Janu[aá]ria|Juramento|Juven[ií]lia|Lontra|Manga|Mirabela|Mirav[aâ]nia|Montalv[aâ]nia|Patis|S[aã]o Jo[aã]o da Lagoa|S[aã]o Jo[aã]o do Pacu[ií])$"

dados_esgoto <- raw_esgoto %>%
  rename(Municipio = 3, UF = 4) %>%
  filter(UF == "MG") %>%
  filter(str_detect(Municipio, regex_exata))

# 3. Os números-chave do seu texto
valores_texto_esgoto <- c("85.2", "77.9", "18.9", "19.2", "11.1", "90.1", "100")
regex_esgoto <- paste0("^(", paste(valores_texto_esgoto, collapse="|"), ")")

# 4. Busca Reversa: Onde estão esses dados?
auditoria_esgoto <- dados_esgoto %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = -Municipio, names_to = "Coluna", values_to = "Valor_Encontrado") %>%
  filter(str_detect(Valor_Encontrado, regex_esgoto)) %>%
  select(Municipio, Coluna, Valor_Encontrado) %>%
  arrange(Municipio)

# 5. Lista os 5 municípios que você disse que não têm dados para checarmos se estão vivos na planilha
cidades_na <- c("Bonito de Minas", "Cônego Marinho", "Miravânia", "São João da Lagoa", "São João do Pacuí")
status_na <- dados_esgoto %>% filter(Municipio %in% cidades_na) %>% select(Municipio)

# 6. Exibe os resultados
cat("\n================ AUDITORIA DE ESGOTO: VALORES DO TEXTO ================\n")
if(nrow(auditoria_esgoto) == 0) {
  cat("ALERTA: Nenhum dos valores foi encontrado. A estrutura da planilha pode ser diferente.\n")
} else {
  print(auditoria_esgoto, n = 50)
}
cat("\n================ AUDITORIA DOS MUNICÍPIOS INVISÍVEIS ================\n")
if(nrow(status_na) == 0) {
  cat("Confirmado: Nenhuma dessas 5 cidades enviou dados (Invisibilidade total confirmada).\n")
} else {
  cat("Cuidado! Algumas dessas cidades estão na planilha. Precisamos checar se as colunas estão vazias.\n")
  print(status_na)
}
cat("=======================================================================\n")