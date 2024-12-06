graf_tipo_dado <- function(raw_data) {

# Carregar pacotes necessários
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  
  # Preencher anos e tipos de dado ausentes com zero
  processed_data <- raw_data %>%
    separate_rows(tipo_de_dado, sep = "; ") %>%
    group_by(ano, tipo_de_dado) %>%
    summarise(n_publicacoes = n(), .groups = "drop") %>%
    complete(ano, tipo_de_dado, fill = list(n_publicacoes = 0)) %>%  # Preenche anos e tipos ausentes com zero
    arrange(ano)
  
  # Calcular o total geral por ano
  totals <- processed_data %>%
    group_by(ano) %>%
    summarise(n_publicacoes = sum(n_publicacoes), .groups = "drop") %>%
    mutate(tipo_de_dado = "Total Geral")
  
  # Ajustar a ordem da legenda e combinar dados com total geral
  final_data <- bind_rows(processed_data, totals) %>%
    mutate(
      tipo_de_dado = factor(tipo_de_dado, levels = c(setdiff(unique(tipo_de_dado), "Total Geral"), "Total Geral")),
      size = case_when(
        tipo_de_dado == "Total Geral" ~ 0.8,
        TRUE ~ 0.3
      )
    )
  
  # Gerar o gráfico com "Total Geral" em preto e espessura de 1, mantendo na legenda
  graf <- 
    ggplot(final_data, aes(x = ano, y = n_publicacoes, color = tipo_de_dado, group = tipo_de_dado)) +
    geom_line(size = final_data$size) +
    scale_color_manual(
      values = c(
        "registro" = "gray20",
        "fotos" = "#1b9e77",
        "vídeos aéreos" = "#d95f02",
        "faixas acústicas" = "#7570b3",
        "amostras genéticas" = "#e7298a",
        "dados sociais" = "#66a61e",
        "outros" = "#e6ab02",
        "drone" = "#a6761d",
        "Total Geral" = "black"
      )
    ) +  # Cores nomeadas para cada classe
    scale_y_continuous(limits = c(0, 4)) +  # Eixo Y começa do zero e tem apenas números inteiros
    labs(
      title = "Publicações por Ano por Tipo de Dado Coletado",
      x = "Ano",
      y = "Número de Publicações",
      color = "Dado"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 14),
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centralizar e estilizar o título
      panel.grid.major = element_blank(),  # Remove o grid maior
      panel.grid.minor = element_blank()   # Remove o grid menor
    )
  
  return(graf)
  
}