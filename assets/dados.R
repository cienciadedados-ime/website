if (!require(pacman)) {
  install.packages('pacman')
  library(pacman)
}
p_load(readxl, writexl, readODS)
p_load(ggthemes)
p_load(glue)
p_load(statBasics)
p_load(janitor, dados)
p_load(tidyverse)

path_dezembro <- "paginas/material/2024/dezembro/"
path_xlsx <- "paginas/material/2024/dezembro/amostra_5k/xlsx/"
path_csv <- "paginas/material/2024/dezembro/amostra_5k/csv/"
arquivos_xlsx <- list.files(path_xlsx)

for (nome_xlsx in arquivos_xlsx) {
    print(glue("{path_xlsx}{nome_xlsx}"))
    dados <- read_xlsx(glue("{path_xlsx}{nome_xlsx}"))
    dados$q005 <- as.numeric(dados$q005)
    cidade <- str_split(nome_xlsx, "\\.")[[1]][1]
    write_csv(dados, glue("{path_csv}{cidade}.csv"))
    write_xlsx(dados, glue("{path_xlsx}{cidade}.xlsx"))
}

dados <- read_xlsx(glue("{path_xlsx}{cidade}.xlsx"))
glimpse(dados)

count(dados, q005)

ggplot(mtcarros) +
  geom_point(aes(x = milhas_por_galao, y = peso))

carros <- mtcarros |>
  mutate(id = paste0("id_", seq(32))) |>
  relocate(id, .before = milhas_por_galao)

write_csv(carros, glue("{path_dezembro}mtcarros.csv"))
