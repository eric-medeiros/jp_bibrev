graf_met <- function(raw_data) {
  
  # Carregar pacotes necessários
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(scales)
  
  # Dicionário de mapeamento para renomear metodologias
  metodologia_labels <- c(
    ACUSTIC = "Acústica",
    DRONE = "Drone",
    ENCALHE = "Encalhe",
    ENTREVISTA = "Entrevista",
    FILMAGEM = "Filmagem",
    FOTOID = "Foto-ID",
    GEN = "Genética",
    OD = "Observação Direta",
    REVISAO = "Revisão",
    TAG = "Tag"
  )
  
  # Preencher anos e metodologias ausentes com zero
  processed_data <- raw_data$base %>%
    separate_rows(familia, sep = "; ") %>%
    filter(familia == "Delphinidae") %>%
    separate_rows(metodologia, sep = "; ") %>%
    group_by(ano, metodologia) %>%
    summarise(n_publicacoes = n(), .groups = "drop") %>%
    complete(ano = full_seq(ano, 1), metodologia, fill = list(n_publicacoes = 0)) %>%
    arrange(ano)
  
  # Calcular o total geral por ano
  totals <- processed_data %>%
    group_by(ano) %>%
    summarise(n_publicacoes = sum(n_publicacoes), .groups = "drop") %>%
    mutate(metodologia = "Total Geral") %>%
    complete(ano = full_seq(ano, 1), fill = list(n_publicacoes = 0))
  
  # Ajustar a ordem da legenda e combinar dados com total geral
  final_data <- bind_rows(processed_data, totals) %>%
    mutate(
      metodologia = factor(
        metodologia,
        levels = c(setdiff(unique(metodologia), "Total Geral"), "Total Geral"),
        labels = c(recode(setdiff(unique(metodologia), "Total Geral"), !!!metodologia_labels), "Total Geral")
      ),
      size = case_when(
        metodologia == "Total Geral" ~ 0.8,
        TRUE ~ 0.3
      )
    )
  
  # Gerar o gráfico com "Total Geral" em preto e espessura de 0.8 na legenda
  graf <- ggplot(final_data, aes(x = ano, y = n_publicacoes, color = metodologia, group = metodologia, size = size)) +
    geom_line() +
    scale_color_manual(
      values = c(
        "Acústica" = "#e7298a",
        "Drone" = "#a6761d",
        "Encalhe" = "#7570b3",
        "Entrevista" = "#66a61e",
        "Filmagem" = "#1b9e77",
        "Foto-ID" = "#e6ab02",
        "Genética" = "gray20",
        "Observação Direta" = "#d95f02",
        "Revisão" = "#666666",
        "Tag" = "#1f78b4",
        "Total Geral" = "black"
      )
    ) +
    scale_size_identity() +
    scale_y_continuous(limits = c(0, 35)) +
    labs(
      title = "Publicações por Ano por Metodologia Adotada",
      x = "Ano",
      y = "Número de Publicações",
      color = "Metodologia"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 14),
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  # Retornar o gráfico e a tabela como uma lista
  return(list(grafico = graf, tabela = final_data))
}
