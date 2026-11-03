library(tidyverse)

chart <- billboard |>
  mutate(song = paste(track, artist, sep = " — ")) |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank"
  ) |>
  filter(!is.na(rank))

top_songs <- chart |>
  count(song, name = "weeks_on_chart") |>
  slice_max(weeks_on_chart, n = 8, with_ties = FALSE)

write_rds(top_songs, file = "clean_data.rds")
