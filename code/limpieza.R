# sobre: limpieza

library(magrittr)
library(tidyverse)
library(robservable)
source("code/funciones.R")

#-------------------
# importar/limpiar
#-------------------
# confirmadas
url <- "https://raw.githubusercontent.com/dquintani/covid/main/casos_diarios.csv"
df <- read_csv(url)
colnames(df)[1] <- "date"
limpieza(df) -> confirmados

# fallecidos
url <- "https://raw.githubusercontent.com/dquintani/covid/main/muertes_diarias.csv"
df <- read_csv(url)
colnames(df)[1] <- "date"
limpieza(df) -> fallecidos

# letalidad
confirmados %>% 
  select(-n) %>%  
  rename(confirmados = value) -> temp

fallecidos %>% 
  select(-n) %>%  
  rename(fallecidos = value) %>% 
  left_join(temp, .) %>% 
  remove_missing() %>% 
  mutate(
    value = fallecidos/confirmados *100,
    value = round(value, 0)
  ) %>%
  select(-confirmados, -fallecidos) %>% 
  arrange(date) %>% 
  mutate(date = as.character(date))-> letalidad

# por millón de habitantes
poblacion <- rio::import("data/PC20106.xlsx", skip = 2) %>% 
  janitor::remove_empty() %>% 
  filter(AÑO == "2022") %>% 
  select(-BOLIVIA, -AÑO) %>% 
  gather(id, poblacion) %>% 
  mutate(id = str_to_title(id))

confirmados %>% 
  left_join(., poblacion) %>% 
  mutate(millon = value/poblacion *1000000) %>% 
  select(-n, -value, -poblacion) %>% 
  rename(value = millon) -> confirmados_millon

fallecidos %>% 
  left_join(., poblacion) %>% 
  mutate(millon = value/poblacion *1000000) %>% 
  select(-n, -value, -poblacion) %>% 
  rename(value = millon) -> fallecidos_millon

rm(df, poblacion, url, temp)
#----------
# graficas
#----------
# confirmados aacumulados
robservable(
  "https://observablehq.com/@rafalopezv/bar-chart-race",
  include = c("viewof date", "chart", "draw", "styles"),
  hide = "draw",
  input = list(
    data = confirmados,
    title = "COVID-19: confirmados acumulados",
    subtitle = "Inicia cuando los 9 departamentos reportaron casos",
    source = "rafalopezv con datos de Boligráfica"
  ),
  width = "100%",
  height = "100%"
) -> confirmados_g

# confirmados acumulados
robservable(
            "https://observablehq.com/@rafalopezv/bar-chart-race",
            include = c("viewof date", "chart", "draw", "styles"),
            hide = "draw",
            input = list(
              data = confirmados_millon,
              title = "COVID-19: confirmados acumulados por millón de habitantes",
              subtitle = "Inicia cuando los 9 departamentos reportaron casos",
              source = "rafalopezv con datos de Boligráfica"
            ),
            width = "100%",
            height = "100%"
) -> confirmados_millon_g 

# fallecidos acumulados
robservable(elementId = "fallecidos",
            "https://observablehq.com/@rafalopezv/bar-chart-race",
            include = c("viewof date", "chart", "draw", "styles"),
            hide = "draw",
            input = list(
              data = fallecidos,
              title = "COVID-19: fallecidos acumulados",
              subtitle = "Inicia cuando los 9 departamentos reportaron casos",
              source = "rafalopezv con datos de Boligráfica"
            ),
            width = "100%",
            height = "100%"
) -> fallecidos_g

# fallecidos acumulados millón
robservable(
            "https://observablehq.com/@rafalopezv/bar-chart-race",
            include = c("viewof date", "chart", "draw", "styles"),
            hide = "draw",
            input = list(
              data = fallecidos_millon,
              title = "COVID-19: confirmados acumulados por millón de habitantes",
              subtitle = "Inicia cuando los 9 departamentos reportaron casos",
              source = "rafalopezv con datos de Boligráfica"
            ),
            width = "100%",
            height = "100%"
) -> fallecidos_millon_g 

# letalidad
robservable(
            "https://observablehq.com/@rafalopezv/bar-chart-race",
            include = c("viewof date", "chart", "draw", "styles"),
            hide = "draw",
            input = list(
              data = letalidad,
              title = "COVID-19: letalidad",
              subtitle = "¿Cuántos de los contagiados fallecen por cada 100 personas (en casos acumulados)",
              source = "rafalopezv con datos de Boligráfica"
            ),
            width = "100%",
            height = "100%"
) -> letalidad_g

# frecuencia de casos
graf_estaticas(confirmados) -> confirmados_es
graf_estaticas(confirmados_millon) -> confirmados_millon_es
graf_estaticas(fallecidos) -> fallecidos_es
graf_estaticas(fallecidos_millon) -> fallecidos_millon_es
graf_estaticas(letalidad) -> letalidad_es


# ggplot
confirmados_millon_es %>% 
  ggplot(aes(fct_reorder(id, num, .desc = F), prop, fill = es)) +
  geom_col() +
  coord_flip() + 
  hrbrthemes::theme_ipsum_rc(base_family = "Roboto Condensed") +
  theme(
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Santa Cruz ha tenido los casos acumulados más altos el 2% del tiempo",
    subtitle = "Casos confirmados acumulados por millón de habitantes",
    y = "Tiempo de duración de la pandemia hasta el 16 de enero de 2021",
    x = "",
    caption = "rafalopezv\nDatos: Boligráfica"
  ) +
  scale_fill_manual(values = ggthemes::tableau_color_pal(palette = "Tableau 10")(2)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(aes(label = paste0(prop*100)), family = "Roboto Condensed", 
            size = 6,  hjust = 1)
ggsave("img/confirmados.jpg", width = 10, height = 7)


# fallecidos millón
fallecidos_millon_es %>% 
  ggplot(aes(fct_reorder(id, num, .desc = F), prop, fill = es)) +
  geom_col() +
  coord_flip() + 
  hrbrthemes::theme_ipsum_rc(base_family = "Roboto Condensed") +
  theme(
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Santa Cruz ha tenido los casos acumulados más altos menos del 1% del tiempo",
    subtitle = "Fallecidos acumulados por millón de habitantes",
    y = "Tiempo de duración de la pandemia hasta el 15 de enero de 2021",
    x = "",
    caption = "rafalopezv\nDatos: Boligráfica"
  ) +
  scale_fill_manual(values = ggthemes::tableau_color_pal(palette = "Tableau 10")(2)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(aes(label = paste0(prop*100)), family = "Roboto Condensed", 
            size = 6,  hjust = 1)
ggsave("img/fallecidos.jpg", width = 10, height = 7)

# letalidad
letalidad_es %>% 
  ggplot(aes(fct_reorder(id, num, .desc = F), prop, fill = es)) +
  geom_col() +
  coord_flip() + 
  hrbrthemes::theme_ipsum_rc(base_family = "Roboto Condensed") +
  theme(
    legend.position = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Santa Cruz nunca ha tenido la peor letalidad",
    subtitle = "Casos acumulados por millón de habitantes [confirmados y fallecidos]",
    y = "Tiempo de duración de la pandemia hasta el 15 de enero de 2021",
    x = "",
    caption = "rafalopezv\nDatos: Boligráfica"
  ) +
  scale_fill_manual(values = ggthemes::tableau_color_pal(palette = "Tableau 10")(2)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(aes(label = paste0(prop*100)), family = "Roboto Condensed", 
            size = 6,  hjust = 1)
ggsave("img/letalidad.jpg", width = 10, height = 7)


