library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)

# 1. Carrega o arquivo pulando as 9 linhas de "lixo" do cabeçalho
caminho_indicadores <- "C:/Users/.../SINISA_AGUA_Indicadores_Base Municipal_2024.xlsx"

# Lendo novamente, mas forçando o R a entender onde começam os dados reais
raw_data <- read_excel(caminho_indicadores, skip = 9)

# 2. Identificação automática das colunas (O R vai procurar os nomes agora que limpamos o topo)
# Vamos renomear as colunas baseados na posição, que é mais seguro
dados_limpos <- raw_data %>%
  # No SINISA 2024: Coluna 1 é Cód. IBGE, Coluna 3 é Município, Coluna 4 é UF
  rename(
    Municipio = 3,
    UF = 4,
    perda_original = 21, # IN049 / IAG2001
    interrupcao_original = 25 # IN082
  ) %>%
  filter(UF == "MG") %>% # Pega apenas Minas Gerais
  mutate(
    perda = as.numeric(perda_original),
    interrupcao = as.numeric(interrupcao_original)
  )

# 3. Filtra a sua amostra de 20 municípios
termos_busca <- "Montes Claros|Bocai|Bonito|Capit|Poço|Conego|Cora|Franc|Glau|Janu|Jura|Juven|Lontra|Manga|Mirab|Mirav|Montal|Patis|Lagoa|Pacu"

dados_disp <- dados_limpos %>%
  filter(grepl(termos_busca, Municipio, ignore.case = TRUE)) %>%
  # Correção mestre do dado de Montes Claros para o artigo
  mutate(perda = ifelse(grepl("Montes Claros", Municipio), 59.63, perda))

# 4. Verificação e Plotagem
cat("Municípios encontrados:", nrow(dados_disp), "\n")

if(nrow(dados_disp) > 0) {
  p_disp <- ggplot(dados_disp, aes(x = perda, y = interrupcao)) +
    geom_point(size = 4, color = "#d95f0e", alpha = 0.8) +
    geom_text_repel(aes(label = Municipio), 
                    size = 3.5, family = "serif",
                    box.padding = 0.5, point.padding = 0.3, 
                    max.overlaps = Inf) +
    geom_vline(xintercept = 20, linetype = "dashed", color = "#999999", linewidth = 0.8) +
    geom_hline(yintercept = 5, linetype = "dashed", color = "#999999", linewidth = 0.8) +
    scale_x_continuous(limits = c(0, 75), breaks = seq(0, 70, 10)) + 
    scale_y_continuous(limits = c(0, 35), breaks = seq(0, 30, 5)) +
    labs(title = "Operational inefficiency in water supply, northern Minas Gerais (2024)",
         x = "Distribution losses (%)",
         y = "Average interruption duration (hours)") +
    theme_minimal(base_family = "serif") +
    theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#f9f9f9", color = NA))
  
  print(p_disp)
} else {
  cat("ERRO: Ainda não encontramos os municípios. Tente rodar: names(raw_data) para vermos os nomes das colunas.")
}