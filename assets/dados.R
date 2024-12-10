if (!require(pacman)) {
  install.packages('pacman')
  library(pacman)
}
p_load(readxl, writexl, readODS)
p_load(ggthemes)
p_load(glue)
p_load(statBasics)
p_load(janitor)
p_load(tidyverse)

path_xlsx <- "paginas/material/2024/dezembro/cidades_enem_2023/xlsx/"
path_csv <- "paginas/material/2024/dezembro/cidades_enem_2023/csv/"
arquivos_xlsx <- list.files(path)

for (nome_xlsx in arquivos_xlsx) {
    print(glue("{path_xlsx}{nome_xlsx}"))
    dados <- read_xlsx(glue("{path_xlsx}{nome_xlsx}"))
    cidade <- str_split(nome_xlsx, "\\.")[[1]][1]
    write_csv(dados, glue("{path_csv}{cidade}.csv"))
}
