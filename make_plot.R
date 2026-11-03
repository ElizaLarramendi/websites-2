library(tidyverse)

top_songs <- read_rds("clean_data.rds")

chart <- billboard |>
  mutate(song = paste(track, artist, sep = " — ")) |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank"
  ) |>
  filter(!is.na(rank))

billboard_plot <- chart |>
  semi_join(top_songs, by = "song") |>
  filter(as.integer(week) <= 20) |>
  ggplot(aes(x = as.integer(week), y = rank, color = song, group = song)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  geom_point(size = 2) +
  scale_y_reverse(breaks = c(1, 25, 50, 75, 100)) +
  scale_color_viridis_d(
    option = "turbo",
    labels = \(labels) stringr::str_wrap(labels, width = 24)
  ) +
  guides(color = guide_legend(ncol = 2)) +
  labs(
    title = "The long runners",
    subtitle = "Billboard chart positions for the 8 longest-charting songs",
    x = "Week on the chart",
    y = "Billboard rank (higher position is better)",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#10131C", color = NA),
    panel.background = element_rect(fill = "#10131C", color = NA),
    panel.grid.minor = element_blank(),
    text = element_text(color = "#F4F1EA"),
    axis.text = element_text(color = "#F4F1EA"),
    plot.title = element_text(face = "bold", size = 20),
    legend.position = "bottom",
    legend.text = element_text(size = 9)
  )

ggsave("billboard.png", plot = billboard_plot)
