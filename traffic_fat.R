library(cbsodataR)

meta <- cbs_get_meta("71426ned")
d <- cbs_get_data("71426ned")

d1 <- 
  d |> 
  cbs_add_label_columns()

library(tidyverse)
d1 |> View()

d2 <- 
  d1 |> 
  filter(
    Geslacht == "T001038",
    Leeftijd == "10000",
    !str_detect(RegioS, "PV99"),
  ) |>
  mutate(
    year = Perioden_label |> as.character() |> as.integer(),
    value = Verkeersdoden_1,
    region = RegioS_label
  ) |> 
  select(region, year, value) |> 
  glimpse()

col <- "#0058b8"


library(tidyverse)
p2 <- d2 |> 
  filter(region == "Nederland") |> 
  ggplot(aes(x = year, y = value)) + 
  geom_col(fill=col) +
  labs(x = "", y = "", title="Traffic fatalities") + 
  theme_minimal()

p2
ggsave("img/traffic.png", p2)


p2_prov <- d2 |> 
  mutate(
    region = str_remove(region, " \\(PV\\)")
  ) |>
  filter(region %in% c("Gelderland","Limburg", "Noord-Brabant", "Zuid-Holland")) |>
  glimpse() |> 
  ggplot(aes(x = year, y = value)) + 
  geom_col(fill=col) +
  labs(x = "", y = "", title="Traffic fatalities") + 
  theme_minimal() + 
  facet_grid(~region)

p2_prov
ggsave("img/traffic_prov.png", p2_prov)
