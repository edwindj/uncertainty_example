library(cbsodataR)

meta <- cbs_get_meta("85454NED")
key <- meta$DataProperties$Key[c(1:3, 46)]

d <- cbs_get_data(
  "85454NED", 
  select = key
)


d1 <- 
  d |> 
  cbs_add_label_columns()

d1 |> View()

library(tidyverse)
d2 <- d1 |> 
  filter(Persoonskenmerken == "T009002") |> 
  mutate(
    year = Perioden_label |> as.character() |> as.integer(),
    diabetes = DiabetesSuikerziekteType2_30,
    value = factor(Marges, levels=c(meta$Marges$Key), labels=c("value", "lower", "upper"))
  ) |> 
  select(year, diabetes,value) |>
  pivot_wider(names_from = value, values_from = diabetes) |> 
  glimpse()

d2 |> write.csv("data/diabetes_prevalence_2014-2024.csv", row.names = FALSE)

col <- "#0058b8"

p_2 <- 
  d2 |> 
  ggplot(aes(x = year, y = value)) + 
  geom_line(color=col) +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 0.1 ),
                     limits = c(0,4.7), expand = c(0,NA)) +
  scale_x_continuous(n.breaks=6) +
  labs(x = "", y = "", title="Diabetes 2 prevalance") +
  theme_minimal()

p_2
ggsave("img/all.png", p_2)

p2_conf <- p_2 + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill=col) 

p2_conf  
ggsave("img/all_conf.png", p2_conf)

kenmerken <- meta$Persoonskenmerken$Key

d3 <- d1 |> 
  filter(Persoonskenmerken %in% kenmerken[8:13]) |> 
  mutate(
    year = Perioden_label |> as.character() |> as.integer(),
    diabetes = DiabetesSuikerziekteType2_30,
    value = factor(Marges, levels=c(meta$Marges$Key), labels=c("value", "lower", "upper")),
    age = gsub("Leeftijd: ", "", Persoonskenmerken_label),
    age = gsub(" jaar", "", age),
    age = sub("tot", "to", age)
  ) |> 
  glimpse() |> 
  select(year, diabetes,value,age) |>
  pivot_wider(names_from = value, values_from = diabetes) |> 
  glimpse()

d3 |> write.csv("data/diabetes_prevalence_2014-2024_per_agegroup.csv", row.names = FALSE)

p3 <- 
  d3 |> 
  ggplot(aes(x = year, y = value)) + 
  geom_line(color=col) +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 0.1 ),
                     limits = c(0,15.4), expand = c(0,NA)) +
  scale_x_continuous(n.breaks=3) +
  labs(
    x = "", 
    y = "", 
    title="Diabetes 2 prevalance (per age group) 2014-2024"
  ) +
  facet_grid(~age) +
  theme_minimal()

p3
ggsave("img/agegroups.png", p3)

p3_conf <- 
  p3 +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    fill = col,
    alpha = 0.2
  )

p3_conf
ggsave("img/agegroups_conf.png", p3_conf)
