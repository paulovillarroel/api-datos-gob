library(tidyverse)
library(arrow)

## Descarga del dataset ----

# Opción 1
url <- "https://datos.gob.cl/dataset/606ef5bb-11d1-475b-b69f-b980da5757f4/resource/ae6c9887-106d-4e98-8875-40bf2b836041/download/at_urg_respiratorio_semanal.parquet"

data <- read_parquet(url)


# Opción 2
library(curl)

output_path <- "at_urg_respiratorio_semanal.parquet"

curl_download(url, output_path)

data2 <- read_parquet(output_path)


# Opción 3
library(httr2)

output_path <- "at_urg_respiratorio_semanal.parquet"

request <- request(url) |>
  req_perform(path = output_path)

data3 <- read_parquet(output_path)


# Opción 4
library(ckanr)

ckanr_setup(url = "https://datos.gob.cl")

resource_id <- "ae6c9887-106d-4e98-8875-40bf2b836041"

resource_info <- resource_show(resource_id)

data4 <- read_parquet(resource_info$url)


## Exploración de los datos ----
data <- data |> 
  janitor::clean_names()

casos_neumonia_anio <- data |> 
  filter(tipo_establecimiento == "Hospital",
         causa == "Neumonía (J12-J18)") |>
  summarise(total_casos = sum(num_total), .by = anio)

casos_neumonia_anio |> 
  ggplot(aes(anio, total_casos)) +
  geom_line()


data |> 
  filter(tipo_establecimiento == "Hospital",
         causa == "Neumonía (J12-J18)") |>
  pivot_longer(
    cols = matches("num"),
    names_to = "edad_grupo",
    values_to = "casos"
  ) |>
  filter(edad_grupo != "num_total") |> 
  summarise(total_casos = sum(casos),
            .by = c(anio, edad_grupo, semana_estadistica)) |>
  ggplot(aes(semana_estadistica, total_casos, color = edad_grupo)) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  facet_wrap( ~ anio)


data |> 
  filter(tipo_establecimiento == "Hospital",
         causa == "Neumonía (J12-J18)",
         anio == 2024) |>
  pivot_longer(
    cols = matches("num"),
    names_to = "edad_grupo",
    values_to = "casos"
  ) |>
  filter(edad_grupo != "num_total") |> 
  summarise(total_casos = sum(casos),
            .by = c(edad_grupo, semana_estadistica)) |>
  #filter(edad_grupo == "num5a14anios") |>
  ggplot(aes(semana_estadistica, total_casos, color = edad_grupo)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1")


data |> 
  filter(tipo_establecimiento == "Hospital",
         causa == "Neumonía (J12-J18)",
         anio == 2024) |>
  pivot_longer(
    cols = matches("num"),
    names_to = "edad_grupo",
    values_to = "casos"
  ) |>
  filter(edad_grupo == "num5a14anios") |> 
  summarise(total_casos = sum(casos),
            .by = semana_estadistica) |>
  ggplot(aes(semana_estadistica, total_casos)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1")

# Extrae la última semana estadística del año 2024
ultima_semana <- data |>
  filter(anio == 2024) |>
  distinct(semana_estadistica) |>
  arrange(semana_estadistica) |>
  pull(semana_estadistica) |>
  last()

# Filtra los datos para excluir el año 2024 con la última semana estadística encontrada
data <- data |>
  filter(!(anio == 2024 & semana_estadistica == ultima_semana))


data |> 
  filter(tipo_establecimiento == "Hospital",
         causa == "Neumonía (J12-J18)",
         anio == 2024) |> 
  summarise(total_casos = sum(num5a14anios),
            .by = semana_estadistica) |>
  ggplot(aes(semana_estadistica, total_casos)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Casos de Neumonía Grupo Edad 5 a 14 años en 2024",
       x = "Semana Estadística",
       y = "Total de Casos",
       color = "Grupo de Edad") +
  theme_minimal()


## Generación de mapas ----

library(chilemapas)

grafico_comunas <- mapa_comunas |> 
  st_set_geometry(mapa_comunas$geometry) |>
  ggplot() +
  geom_sf()

grafico_comunas + 
  coord_sf(xlim = c(-77, -65)) +
  theme_classic()

mapa_regiones <- mapa_comunas |> 
  group_by(codigo_region) |> 
  summarize(geometry = st_union(geometry))

grafico_regiones <- mapa_regiones |> 
  st_set_geometry(mapa_regiones$geometry) |> # especificar la geometría del mapa
  ggplot() + 
  geom_sf() + # capa geográfica
  coord_sf(xlim = c(-77, -65)) # recortar coordenadas

grafico_regiones +
  theme_classic()


# Generamos nuestro dataset 

neumonias_2024 <- data |> 
  filter(causa == "Neumonía (J12-J18)",
         anio == 2024) |> 
  summarise(total_casos = sum(num_total), .by = c(comuna_codigo, comuna_glosa))

mapa_comunas_2 <- mapa_comunas |>
  left_join(neumonias_2024, by = c("codigo_comuna" = "comuna_codigo")) |> 
  mutate(total_casos = replace_na(total_casos, 0))

mapa_comunas_2 |> 
  st_set_geometry(mapa_comunas_2$geometry) |> # asignar geometría
  ggplot() + 
  aes(fill = total_casos) +
  geom_sf(linewidth = 0.1, color = "gray40") + # capa geométrica
  theme_classic() +
  scale_fill_distiller(type = "seq", palette = "PuRd", direction = "rev") + # colores
  scale_x_continuous(breaks = seq(-76, -65, length.out = 3)) + # escala x
  coord_sf(xlim = c(-77, -65)) + # recortar coordenadas
  theme(legend.key.width = unit(3, "mm"))


# Filtrar por RM

mapa_comunas_filtrado <- mapa_comunas_2 |> 
  filter(codigo_region == "13")

# Mapa
mapa_comunas_filtrado |> 
  st_set_geometry(mapa_comunas_filtrado$geometry) |> # asignar geometría
  ggplot() + 
  aes(fill = total_casos) +
  geom_sf(linewidth = 0.1, color = "gray40") + # capa geométrica
  geom_sf_text(aes(label = comuna_glosa), size = 2.5, color = "black", check_overlap = TRUE) +
  theme_classic() +
  scale_fill_distiller(type = "seq", palette = "PuRd", direction = "rev") + # colores
  scale_x_continuous(breaks = seq(-76, -65, length.out = 3)) +
  theme(legend.key.width = unit(3, "mm"), 
        axis.title = element_blank())


# Mapa interactivo
library(plotly)

# Preparar el mapa
p <- mapa_comunas_filtrado |> 
  st_set_geometry(mapa_comunas_filtrado$geometry) |> # asignar geometría
  ggplot() + 
  aes(fill = total_casos) +
  geom_sf(linewidth = 0.1, color = "gray40") + # capa geométrica
    geom_sf_text(aes(label = comuna_glosa), size = 2.5, color = "black", check_overlap = TRUE) +
  theme_classic() +
  scale_fill_distiller(type = "seq", palette = "PuRd", direction = "rev") + # colores
  scale_x_continuous(breaks = seq(-76, -65, length.out = 3)) +
  theme(legend.key.width = unit(3, "mm"), 
        axis.title = element_blank())

ggplotly(p)
