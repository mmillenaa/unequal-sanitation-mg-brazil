ggplot(evolucao_residuos, aes(x = `Ano de Referência`, y = percent, fill = Destinacao_Status_eng)) +
  geom_area(alpha = 0.7, color = "white", linewidth = 0.2) +
  facet_wrap(~ Destinacao_Status_eng, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = cores_paleta) +
  scale_x_continuous(breaks = unique(evolucao_residuos$`Ano de Referência`)) +
  labs(
    title = "Historical evolution of waste disposal (2017–2022)",
    subtitle = "Peripheral municipalities, northern Minas Gerais",
    x = "Year (SINISA)",
    y = "Percentage of municipalities (%)",
    caption = "Each panel shows a separate disposal category. Adequate in blue, controlled landfill in gold, open dump in orange."
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),      # único negrito
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 10)),
    plot.caption = element_text(size = 8, hjust = 0, face = "italic"),
    axis.title = element_text(size = 11),                     # sem negrito
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 10),                     # sem negrito
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3)
  )