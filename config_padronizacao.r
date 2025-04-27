
# Cores padrão para concentrações (suavizadas)
cores_concentracao <- c(
   "0"  = "#6E6E6E", # Cinza médio (suavizado)
  "0.1" = "#A8A8A8",  # Cinza médio (mais escuro que #D3D3D3)
  "0.2" = "#A88E82",  # Marrom café suavizado
  "0.3" = "#C55A5A",  # Vermelho fosco suavizado (mais avermelhado)
  "0.4" = "#A56E6E",  # Vinho fosco suavizado
  "0.5" = "#C7A98C",  # Bege queimado suavizado
  "0.6" = "#B5947A",  # Marrom noz suavizado
  "0.7" = "#E0966F",  # Vermelho terracota suavizado (mais alaranjado)
  "0.8" = "#C5B8A8",  # Bege acinzentado suavizado
  "0.9" = "#9C8A7F",  # Chocolate escuro suavizado
  "1.0" = "#8A8580"   # Cinza carvão suavizado
)
# Limites dos eixos
limite_x <- c(0, 2880)  # Tempo de 0 a 2880 minutos
limite_y <- c(0.001, 2) # DO de 0.001 a 2

# Tema padrão com contorno quadrado
tema_padrao <- theme_bw() +  # Usar theme_bw() como base para o contorno quadrado
  theme(
    text = element_text(size = 12),  # Tamanho da fonte
    axis.text = element_text(color = "black"),  # Cor do texto dos eixos
    legend.position = "right",  # Posição da legenda (esquerda)
    #panel.border = element_rect(color = "black", fill = NA, size = 0.8),  # Contorno quadrado
    panel.grid.major = element_line(color = "gray80", size = 0.5),  # Grade principal
    panel.grid.minor = element_line(color = "gray90", size = 0.2)   # Grade secundária
  )  
  


