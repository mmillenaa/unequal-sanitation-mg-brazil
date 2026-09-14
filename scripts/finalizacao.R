# ==============================================================================
# PACOTES NECESSÁRIOS
# ==============================================================================
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(tibble)
library(readxl)

# ==============================================================================
# PASSO 1: PAINEL DE CONTROLE DAS COLUNAS E CAMINHOS
# -> ATENÇÃO: Substitua os números abaixo pelas colunas exatas do seu Excel!
# ==============================================================================
# Caminhos
caminho_gestao   <- "C:/Users/.../copy_of_SINISA_GESTAO_MUNICIPAL_Informacoes_2024 (1).xlsx"
caminho_agua     <- "C:/Users/.../SINISA_AGUA_Planilhas_2024/Água - Base Municipal/SINISA_AGUA_Indicadores_Base Municipal_2024.xlsx"
caminho_esgoto   <- "C:/Users/.../SINISA_ESGOTO_Planilhas_2024/Esgoto - Base Municipal/SINISA_ESGOTO_Indicadores_Base Municipal_2024.xlsx"
caminho_residuos <- "C:/Users/.../SINISA_RESIDUOS_planilhas_2024/SINISA_RESIDUOS_planilhas_2024/SINISA_RESIDUOS_Informacoes_Formulario_Infraestrutura_Destinacao_Final_2024.xlsx"
caminho_drenagem <- "C:/Users/.../SINISA_AGUASPLUVIAIS_Informacoes_Indicadores_2025/SINISA_AGUASPLUVIAIS_Informacoes_2024/SINISA_AGUASPLUVIAIS_Informacoes_Formularios_Tecnicos.xlsx"

# Colunas (Coloque aqui os números que você achou no View)
col_g_plano      <- 10   # Qual coluna diz se tem Plano Municipal?
col_g_agencia    <- 15   # Qual coluna diz se tem Agência Reguladora?
col_w_cobertura  <- 11   # Qual coluna tem a cobertura de Água?
col_s_cobertura  <- 11   # Qual coluna tem a cobertura de Esgoto?
col_r_aterro     <- 11   # Qual coluna tem o Aterro Sanitário?
col_r_coleta     <- 15   # ---> MUDE AQUI: Qual coluna tem a Coleta Seletiva?
col_d_rede       <- 12   # Qual coluna tem a Rede Subterrânea?
col_d_risco      <- 15   # ---> MUDE AQUI: Qual coluna tem o Plano de Risco?

regex_exata <- "(?i)^(Montes Claros|Bocai[uú]va|Bonito de Minas|Capit[aã]o En[eé]as|Claro dos Po[cç][oõ]es|C[oô]nego Marinho|Cora[cç][aã]o de Jesus|Francisco S[aá]|Glaucil[aâ]ndia|Janu[aá]ria|Juramento|Juven[ií]lia|Lontra|Manga|Mirabela|Mirav[aâ]nia|Montalv[aâ]nia|Patis|S[aã]o Jo[aã]o da Lagoa|S[aã]o Jo[aã]o do Pacu[ií])$"

municipios_base <- data.frame(
  Municipio = c("Bocaiúva", "Bonito de Minas", "Capitão Enéas", "Claro dos Poções", 
                "Coração de Jesus", "Cônego Marinho", "Francisco Sá", "Glaucilândia", 
                "Januária", "Juramento", "Juvenília", "Lontra", "Manga", "Mirabela", 
                "Miravânia", "Montalvânia", "Montes Claros", "Patis", 
                "São João da Lagoa", "São João do Pacuí")
)

# ==============================================================================
# PASSO 2: EXTRAÇÃO E CÁLCULO DOS 5 EIXOS (Pesos baseados na Lei/WHO)
# ==============================================================================

# G: Governança (50% Plano + 50% Agência)
gestao_raw <- suppressMessages(read_excel(caminho_gestao, skip = 9)) %>%
  rename(Municipio = 3, UF = 4) %>% filter(UF == "MG" & str_detect(Municipio, regex_exata))
gov_calc <- gestao_raw %>%
  mutate(
    v1 = ifelse(str_detect(toupper(.[[col_g_plano]]), "SIM"), 1, 0),
    v2 = ifelse(str_detect(toupper(.[[col_g_agencia]]), "SIM"), 1, 0),
    G = (v1 * 0.5 + v2 * 0.5) * 100
  ) %>% select(Municipio, G)

# W: Água (100% Cobertura)
agua_raw <- suppressMessages(read_excel(caminho_agua, skip = 9)) %>%
  rename(Municipio = 3, UF = 4) %>% filter(UF == "MG" & str_detect(Municipio, regex_exata))
agua_calc <- agua_raw %>%
  mutate(W = as.numeric(.[[col_w_cobertura]])) %>% select(Municipio, W)

# S: Esgoto (100% Cobertura)
esgoto_raw <- suppressMessages(read_excel(caminho_esgoto, skip = 9)) %>%
  rename(Municipio = 3, UF = 4) %>% filter(UF == "MG" & str_detect(Municipio, regex_exata))
esgoto_calc <- esgoto_raw %>%
  mutate(S = as.numeric(.[[col_s_cobertura]])) %>% select(Municipio, S)

# R: Resíduos (70% Aterro + 30% Coleta)
res_raw <- suppressMessages(read_excel(caminho_residuos, skip = 9)) %>%
  rename(Municipio = 3, UF = 4) %>% filter(UF == "MG" & str_detect(Municipio, regex_exata))
res_calc <- res_raw %>%
  mutate(
    v1 = ifelse(str_detect(toupper(.[[col_r_aterro]]), "ATERRO SANITÁRIO"), 1, 0),
    v2 = ifelse(str_detect(toupper(.[[col_r_coleta]]), "SIM"), 1, 0),
    R = (v1 * 0.7 + v2 * 0.3) * 100
  ) %>% group_by(Municipio) %>% summarise(R = max(R, na.rm=T))

# D: Drenagem (70% Rede Subterrânea + 30% Mapeamento de Risco)
dren_raw <- suppressMessages(read_excel(caminho_drenagem, skip = 9)) %>%
  rename(Municipio = 3, UF = 4) %>% filter(UF == "MG" & str_detect(Municipio, regex_exata))
dren_calc <- dren_raw %>%
  mutate(
    v1 = ifelse(str_detect(toupper(.[[col_d_rede]]), "SIM") | !is.na(.[[col_d_rede+1]]), 1, 0),
    v2 = ifelse(str_detect(toupper(.[[col_d_risco]]), "SIM"), 1, 0),
    D = (v1 * 0.7 + v2 * 0.3) * 100
  ) %>% select(Municipio, D)

# ==============================================================================
# PASSO 3: UNIFICAÇÃO DA MATRIZ E AUDITORIA (Confira o Console)
# ==============================================================================
matriz_completa <- municipios_base %>%
  left_join(gov_calc, by = "Municipio") %>%
  left_join(agua_calc, by = "Municipio") %>%
  left_join(esgoto_calc, by = "Municipio") %>%
  left_join(res_calc, by = "Municipio") %>%
  left_join(dren_calc, by = "Municipio") %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

cat("\n--- AUDITORIA: MATRIZ DE CONFORMIDADE (ICN) ---\n")
print(as.data.frame(matriz_completa))
cat("-----------------------------------------------\n")

# ==============================================================================
# PASSO 4: PREPARAÇÃO PARA O RADAR (Ordenação e Inglês Britânico)
# ==============================================================================
dados_radar <- matriz_completa %>%
  pivot_longer(cols = -Municipio, names_to = "Axis", values_to = "Value") %>%
  mutate(Axis = factor(Axis, levels = c("G", "W", "S", "R", "D")))

# Ordenando do Melhor para o Pior Desempenho
ranking <- dados_radar %>% group_by(Municipio) %>% summarise(mean_val = mean(Value)) %>% arrange(desc(mean_val))
dados_radar$Municipio <- factor(dados_radar$Municipio, levels = ranking$Municipio)

# Alerta Exclusivo para São João do Pacuí
alerta_pacui <- data.frame(
  Municipio = factor("São João do Pacuí", levels = levels(dados_radar$Municipio)),
  Axis = factor("W", levels = levels(dados_radar$Axis)),
  Value = 15,
  label = "No data\nreported"
)

# ==============================================================================
# PASSO 5: PLOTAGEM DO GRÁFICO (Design Acadêmico Clean)
# ==============================================================================
figura_radar <- ggplot(dados_radar, aes(x = Axis, y = Value, group = 1)) + 
  geom_polygon(fill = "#00326f", alpha = 0.4, color = "#00326f", linewidth = 0.4) +
  geom_point(color = "#00326f", size = 0.7) +
  geom_hline(yintercept = c(25, 50, 75, 100), color = "grey92", linewidth = 0.2) +
  
  # A Nota do Pacuí
  geom_text(data = alerta_pacui, aes(x = Axis, y = Value, label = label), 
            inherit.aes = FALSE, size = 2.2, color = "#d95f0e", 
            family = "serif", fontface = "italic", lineheight = 0.9) +
  
  coord_polar(clip = "off") +
  scale_y_continuous(limits = c(0, 115), breaks = c(0, 50, 100)) +
  facet_wrap(~ Municipio, ncol = 5) +
  
  labs(
    title = "Standardised performance across sanitation axes (ICN)",
    subtitle = "Northern Minas Gerais, 2024 (ordered by average performance)",
    caption = "Axes: G = Governance, W = Water, S = Sewage, R = Waste, D = Drainage\nSource: Elaborated by the authors based on SINISA (2024)."
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, margin = margin(b = 10)),
    strip.text = element_text(face = "bold", size = 7, color = "grey20"),
    axis.text.x = element_text(size = 7, color = "grey30", face = "bold"),
    axis.text.y = element_blank(), 
    panel.grid.major = element_line(color = "grey95"),
    plot.margin = margin(10, 10, 10, 10)
  )

print(figura_radar)

# Salvar em alta resolução
ggsave("Figura_8_Radar_Definitiva.png", plot = figura_radar, width = 10, height = 8, dpi = 300, bg = "white")