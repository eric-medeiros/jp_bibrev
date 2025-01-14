graf_reg <- function(raw_data) {
  
  # Carregar pacotes necessários
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(scales)
  
  # Preencher anos e gêneros ausentes com zero
  processed_data <- raw_data$base %>%
    separate_rows(local, sep = "; ") %>%
    left_join(raw_data$pais_regiao, by = "local") %>%
    separate_rows(familia, sep = "; ") %>%
    filter(familia == "Delphinidae") %>%
    group_by(ano, regiao) %>%
    summarise(n_publicacoes = n(), .groups = "drop") %>%
    complete(ano = full_seq(ano, 1), regiao, fill = list(n_publicacoes = 0)) %>%
    arrange(ano)
  
  # Calcular o total geral por ano
  totals <- processed_data %>%
    group_by(ano) %>%
    summarise(n_publicacoes = sum(n_publicacoes), .groups = "drop") %>%
    mutate(regiao = "Total Geral") %>%
    complete(ano = full_seq(ano, 1), fill = list(n_publicacoes = 0))
  
  # Ajustar a ordem da legenda e combinar dados com total geral
  final_data <- bind_rows(processed_data, totals) %>%
    mutate(
      regiao = factor(regiao, levels = c(setdiff(unique(regiao), "Total Geral"), "Total Geral"))
    )
  
  # Definir cores personalizadas
  n_regiao <- length(levels(final_data$regiao))
  cores <- hue_pal()(n_regiao - 1) # Gerar cores para os gêneros
  cores <- c(cores, "black")        # Adicionar o preto para "Total Geral"
  names(cores) <- levels(final_data$regiao)
  
  # Gerar o gráfico com "Total Geral" em preto e espessura de 1
  graf <- ggplot(final_data, aes(x = ano, y = n_publicacoes, color = regiao, group = regiao)) +
    geom_line(linewidth = 0.3) +
    geom_line(data = subset(final_data, regiao == "Total Geral"), linewidth = 0.8) +
    scale_color_manual(values = cores) +
    scale_y_continuous(limits = c(0, 30)) +
    labs(
      title = "Publicações por Ano por Região Geopolítica",
      x = "Ano",
      y = "Número de Publicações",
      color = "Região"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 14),
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(graf)
}
