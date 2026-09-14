# ==============================================================================
# MAPA FINAL: ALTO CONTRASTE, BORDAS NÍTIDAS E BRASIL SEM MOLDURA
# ==============================================================================
library(geobr)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(cowplot)
library(sf)

# 1. Carregar Mapas
br <- read_country(year = 2020, showProgress = FALSE)
mg <- read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE)

# 2. Criar a Borda Externa de Minas (Contorno Geral)
mg_outline <- st_union(mg)

# 3. Lista das 20 cidades e Filtro
cidades_artigo <- c("Montes Claros", "Bocaiúva", "Bonito de Minas", "Capitão Enéas", 
                    "Claro dos Poções", "Cônego Marinho", "Coração de Jesus", 
                    "Glaucilândia", "Januária", "Juramento", "Juvenília", 
                    "Lontra", "Manga", "Mirabela", "Miravânia", 
                    "Montalvânia", "São João da Lagoa", "São João do Pacuí", 
                    "Francisco Sá", "Patis")

mapa_foco <- mg %>% 
  filter(name_muni %in% cidades_artigo) %>%
  mutate(Status = ifelse(name_muni == "Montes Claros", "Regional hub", "Peripheral municipalities"))

# -----------------------------------------------------------------------------
# 4. MAPA PRINCIPAL (MINAS GERAIS)
# -----------------------------------------------------------------------------
p_principal <- ggplot() +
  # Fundo: municípios de MG com borda cinza escuro (contraste forte)
  geom_sf(data = mg, fill = "white", color = "#4d4d4d", linewidth = 0.25) +
  
  # Borda externa de Minas Gerais (nítida e escura)
  geom_sf(data = mg_outline, fill = NA, color = "#333333", linewidth = 1.0) +
  
  # As 20 cidades do estudo (alto contraste, bordas pretas)
  geom_sf(data = mapa_foco, aes(fill = Status), color = "black", linewidth = 0.5) +
  
  scale_fill_manual(values = c("Regional hub" = "#FFD700", 
                               "Peripheral municipalities" = "#00204D")) +
  theme_minimal(base_family = "serif") +
  labs(title = "Study area location",
       subtitle = "Spatial distribution in northern Minas Gerais, Brazil",
       fill = "Classification") +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.text = element_text(size = 10)
  ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_y = unit(0.4, "in"), style = north_arrow_nautical())

# -----------------------------------------------------------------------------
# 5. MAPA DO BRASIL (INSET) - SEM QUADRADO/MOLDURA
# -----------------------------------------------------------------------------
p_brasil <- ggplot() +
  geom_sf(data = br, fill = "white", color = "black", linewidth = 0.1) +
  geom_sf(data = mg_outline, fill = "#00204D", color = NA) +
  theme_void() +
  theme(
    plot.background = element_blank(),
    panel.border = element_blank()
  )

# -----------------------------------------------------------------------------
# 6. JUNTAR TUDO
# -----------------------------------------------------------------------------
mapa_final <- ggdraw(p_principal) +
  draw_plot(p_brasil, x = 0.72, y = 0.05, width = 0.28, height = 0.28)

# 7. Salvar (TIFF com alta resolução)
caminho_final <- "C:/Users/.../Figure1_Location_Map_Final.tiff"
ggsave(caminho_final, plot = mapa_final, width = 10, height = 8, dpi = 300, compression = "lzw")

message("Mapa gerado com sucesso! Bordas agora em cinza escuro para melhor contraste.")

ggsave(
  "Figure1_Map.png",
  plot = mapa_final,
  width = 10,
  height = 8,
  dpi = 300
)
