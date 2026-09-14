library(readxl)
library(dplyr)

# 1. Caminho (Verifique se o nome do arquivo está 100% igual ao da pasta)
caminho_financeiro <- "C:/Users/.../Água - Base Municipal/SINISA_AGUA_Informações_Gestão Administrativa e Financeira_Base Municipal_2024.xlsx"

# 2. Lendo as primeiras 15 linhas para reconstruir o cabeçalho
header_block <- read_excel(caminho_financeiro, n_max = 15, col_names = FALSE)

# Criando o "Super Cabeçalho" (juntando as linhas 8 a 12 que costumam ter os nomes)
full_header <- apply(header_block, 2, function(x) paste(na.omit(x), collapse = " "))
full_header <- tolower(trimws(full_header)) # Remove espaços extras e padroniza

# 3. BUSCA COM "AVISO DE FALHA"
# Tentamos achar por código técnico OU por palavras-chave em português
pos_invest_agua  <- which(grepl("fn015|investimento.*água", full_header))[1]
pos_invest_total <- which(grepl("fn033|investimento.*total", full_header))[1]
pos_despesa_expl <- which(grepl("fn017|despesa.*exploração", full_header))[1]

# --- ÁREA DE DIAGNÓSTICO ---
cat("--- RESULTADO DA BUSCA DE COLUNAS ---\n")
if(is.na(pos_invest_agua))  cat("ERRO: Não achei Investimento em Água (FN015)\n") else cat("OK: Invest. Água na Coluna", pos_invest_agua, "\n")
if(is.na(pos_invest_total)) cat("ERRO: Não achei Investimento Total (FN033)\n") else cat("OK: Invest. Total na Coluna", pos_invest_total, "\n")
if(is.na(pos_despesa_expl)) cat("ERRO: Não achei Despesa de Exploração (FN017)\n") else cat("OK: Despesa Expl. na Coluna", pos_despesa_expl, "\n")

# Se alguma falhou, vamos imprimir as primeiras 100 colunas do cabeçalho para VOCÊ ler
if(any(is.na(c(pos_invest_agua, pos_invest_total, pos_despesa_expl)))) {
  cat("\n--- CONTEÚDO DO CABEÇALHO DETECTADO (Primeiras 50 colunas) ---\n")
  print(full_header[1:50])
  stop("O script parou porque as colunas não foram mapeadas. Verifique os nomes acima.")
}

# 4. EXTRAÇÃO DOS DADOS (Só roda se as posições forem encontradas)
dados_fin_raw <- read_excel(caminho_financeiro, skip = 11, col_names = FALSE)

cidades_artigo <- c("Montes Claros", "Bocaiúva", "Bonito de Minas", "Capitão Enéas", 
                    "Claro dos Poções", "Cônego Marinho", "Coração de Jesus", 
                    "Glaucilândia", "Januária", "Juramento", "Juvenília", 
                    "Lontra", "Manga", "Mirabela", "Miravânia", 
                    "Montalvânia", "São João da Lagoa", "São João do Pacuí", 
                    "Francisco Sá", "Patis")

resumo_financeiro <- dados_fin_raw %>%
  select(
    Municipality = 3, # Nome do Município
    UF = 4,           # Estado
    Invest_Agua = all_of(pos_invest_agua),
    Invest_Total = all_of(pos_invest_total),
    Despesa_DEX = all_of(pos_despesa_expl)
  ) %>%
  filter(Municipality %in% cidades_artigo & UF == "MG") %>%
  mutate(across(c(Invest_Agua, Invest_Total, Despesa_DEX), 
                ~suppressWarnings(as.numeric(as.character(.)))))

print("--- TABELA FINANCEIRA GERADA COM SUCESSO ---")
print(resumo_financeiro)
View(resumo_financeiro)