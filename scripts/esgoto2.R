library(ggplot2)

# 1. Data Preparation
data_comparison <- data.frame(
  Municipality = c("Montes Claros", "Bocaiúva", "Francisco Sá", "Januária", "Manga", "Juvenília", "Bonito de Minas", "São João da Lagoa"),
  Reported_Treatment = c(95.4, 67.5, 37.7, 51.4, 69.1, 49.0, 0, 0),
  Physical_STPs = c(2, 0, 0, 1, 1, 1, 0, 0)
)

# 2. Plot with Serif Font and Brand Colors
ggplot(data_comparison, aes(x = reorder(Municipality, Reported_Treatment))) +
  geom_bar(aes(y = Reported_Treatment, fill = (Physical_STPs == 0)), stat = "identity", alpha = 0.9) +
  geom_point(aes(y = Physical_STPs * 15, size = Physical_STPs), color = "black") +
  scale_fill_manual(values = c("FALSE" = "#00326f", "TRUE" = "#e7d159"), 
                    labels = c("Verified infrastructure", "Data inconsistency (no STP)")) +
  coord_flip() +
  theme_minimal(base_family = "serif") + # Ensuring Serif font
  labs(
    title = "Sanitary contradiction: nominal treatment vs. physical infrastructure",
    subtitle = "In gold: municipalities reporting treatment without registered STPs",
    x = "", y = "Reported treatment rate (%)",
    fill = "Infrastructure status",
    size = "Number of STPs (scale x15)"
  ) +
  theme(
    legend.position = "bottom",
    text = element_text(family = "serif"),
    plot.title = element_text(face = "bold")
  )