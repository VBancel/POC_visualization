### function to build histogram

plot_lat_lon_depth <- function(data, var_input){
  plot <- ggplot(data, aes(x = var_bins, y = count, fill = count)) +
  geom_bar(stat = "identity", color = "white", show.legend = FALSE) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Palette de couleurs plus esthétique
  labs(
    title = paste0("Nombre de mesures par tranche de ", var_input),
    subtitle = paste0("Distribution des mesures POC en fonction de ", var_input),
    x = toTitleCase(var_input), 
    y = "Nombre de mesures"
  ) +
  scale_x_discrete(labels = function(x) gsub("\\[|\\)", "", x)) +
  theme_minimal() +  # Utilisation d'un thème minimaliste
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )
  return(plot)
}




plot_other <- function(data, var_input){
  ggplot(data, aes(x = date_value, y = count, fill = count)) +
    geom_bar(stat = "identity", color = "white", show.legend = FALSE) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Palette de couleurs plus esthétique
    labs(
      title = paste0("Nombre de mesures en fonction de ", var_input),
      x = toTitleCase(var_input), 
      y = "Nombre de mesures"
    ) +
    theme_minimal() +  # Utilisation d'un thème minimaliste
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
} 
