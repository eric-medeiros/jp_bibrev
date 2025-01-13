graf_pais <- function(raw_data) {

# Carregar pacotes necessários
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(scales)
  
  # Preencher anos e tipos de dado ausentes com zero
  processed_data <- raw_data %>%
    separate_rows(local, sep = "; ") %>%
    group_by(ano, local) %>%
    summarise(n_publicacoes = n(), .groups = "drop") %>%
    complete(ano = full_seq(ano, 1), local, fill = list(n_publicacoes = 0)) %>%  # Preenche anos e tipos ausentes com zero
    arrange(ano)
  
  # Calcular o total geral por ano
  totals <- processed_data %>%
    group_by(ano) %>%
    summarise(n_publicacoes = sum(n_publicacoes), .groups = "drop") %>%
    mutate(local = "Total Geral") %>%
    complete(ano = full_seq(ano, 1), fill = list(n_publicacoes = 0))
  
  # Ajustar a ordem da legenda e combinar dados com total geral
  final_data <- bind_rows(processed_data, totals) %>%
    mutate(
      local = factor(local, levels = c(setdiff(unique(local), "Total Geral"), "Total Geral"))
    )
  
  n_local <- length(levels(final_data$local))
  
  # Gerar o gráfico com "Total Geral" em preto e espessura de 1, mantendo na legenda
  graf <-  
    ggplot(final_data, aes(x = ano, y = n_publicacoes, color = local, group = local)) +
    geom_line(size = 0.3) +
    geom_line(data = subset(final_data, local == "Total Geral"), color = "black", size = 0.8) +
    scale_y_continuous(limits = c(0, 30)) +  # Eixo Y começa do zero e tem apenas números inteiros
    labs(
      title = "Publicações por Ano por País",
      x = "Ano",
      y = "Número de Publicações",
      color = "País"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 14),
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centralizar e estilizar o título
      panel.grid.major = element_blank(),  # Remove o grid maior
      panel.grid.minor = element_blank()   # Remove o grid menor
    ) +
  guides(
    colour = guide_legend(
      override.aes = list(
        colour = c(hue_pal()(n_local)[1:n_local-1], "black"),
        size = c(rep(0.3, n_local-1), 0.8)
        )
      )
    )

  return(graf)
  
}