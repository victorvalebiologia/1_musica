# ============================================================================
# ANÁLISE COMPLETA DE ÁLBUNS MUSICAIS
# ============================================================================
# ORGANIZAÇÃO POR ASSUNTO:
# 1. Configuração e Importação
# 2. Análise Temporal (Ano de publicação, séculos, calendário)
# 3. Análise de Gêneros e Subgêneros
# 4. Análise Geográfica (Países, Continentes)
# 5. Análise de Editoras
# 6. Análise de Texto (Títulos e Resenhas)
# 7. Dashboard e Visualizações Interativas
# ============================================================================

# ============================================================================
# 1. CONFIGURAÇÃO E IMPORTAÇÃO
# ============================================================================

# ===== 1.1 CONFIGURAÇÃO INICIAL =====
rm(list = ls())
gc()
setwd("/home/victor-vale/Área de trabalho/R/Musica")

cat("\n", rep("=", 70), "\n", sep = "")
cat("🎵 ANÁLISE DE ÁLBUNS MUSICAIS\n")
cat(rep("=", 70), "\n\n", sep = "")

# ===== 1.2 PACOTES =====
pacotes <- c(
  "magrittr", "dplyr", "reshape2", "ggplot2", "ggrepel",
  "lubridate", "gghighlight", "forcats", "iNEXT", "tidyr",
  "tibble", "vegan", "ggside", "googledrive", "googlesheets4",
  "readxl", "patchwork", "ggfortify", "cluster", "ade4",
  "spaa", "recluster", "analogue", "ape", "tm", "xml2",
  "SnowballC", "wordcloud", "wesanderson", "wordcloud2", "stringr",
  "fmsb", "plotly", "scales", "openxlsx", "dendextend", "ggdendro",
  "gplots", "colorspace"
)

novos <- pacotes[!pacotes %in% installed.packages()]
if(length(novos) > 0) install.packages(novos)
invisible(lapply(pacotes, library, character.only = TRUE))

cat("✅ Pacotes carregados!\n")

# ===== 1.3 FUNÇÕES AUXILIARES =====
cores_musica <- function(n) {
  if(n <= 8) RColorBrewer::brewer.pal(n, "Set2")
  else if(n <= 12) RColorBrewer::brewer.pal(n, "Paired")
  else colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n)
}

tema_musica <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = base_size + 4, color = "#2C3E50"),
      plot.subtitle = element_text(hjust = 0.5, size = base_size, color = "#7F8C8D"),
      axis.title = element_text(face = "bold", size = base_size, color = "#34495E"),
      axis.text = element_text(size = base_size - 1, color = "#2C3E50"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = base_size, color = "#2C3E50"),
      legend.text = element_text(size = base_size - 1, color = "#34495E"),
      legend.key.size = unit(0.7, "cm"),
      legend.background = element_rect(fill = "white", color = "#E0E0E0", size = 0.3),
      legend.margin = margin(6, 10, 6, 10),
      panel.grid.major = element_line(color = "#ECF0F1", size = 0.4),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(15, 20, 15, 20)
    )
}

dir.create("resultados", recursive = TRUE, showWarnings = FALSE)

salvar_pdf <- function(plot, nome, largura = 20, altura = 10) {
  caminho <- paste0("resultados/", nome, ".pdf")
  ggsave(filename = caminho, plot = plot, width = largura, height = altura, device = "pdf")
  cat("  ✅ Salvo:", caminho, "\n")
}

salvar_png <- function(plot, nome, largura = 12, altura = 8, dpi = 300) {
  caminho <- paste0("resultados/", nome, ".png")
  ggsave(filename = caminho, plot = plot, width = largura, height = altura, dpi = dpi, device = "png")
  cat("  ✅ Salvo:", caminho, "\n")
}

# ===== 1.4 IMPORTAR DADOS =====
cat("\n📥 Importando dados...\n")

drive_auth()
pasta <- drive_get(as_id("18mQkRX4In5R_KoQ06AGp2YvNLnfFSjvR"))
arquivo <- drive_ls(pasta) %>% filter(name == "livro.xlsx")

if(nrow(arquivo) == 1) {
  drive_download(as_id(arquivo$id), path = "livro.xlsx", overwrite = TRUE)
  planilhatotal <- read.xlsx("livro.xlsx", sheet = 1, colNames = TRUE, na.strings = "NA")
  cat("✅ Arquivo importado:", nrow(planilhatotal), "linhas,", ncol(planilhatotal), "colunas\n")
} else {
  stop("❌ Arquivo 'livro.xlsx' não encontrado!")
}

# ===== 1.5 PREPARAR DADOS =====
cat("\n🔧 Preparando dados...\n")

planilhatotal <- planilhatotal[, !is.na(names(planilhatotal)) & names(planilhatotal) != ""]
names(planilhatotal) <- make.names(names(planilhatotal), unique = TRUE)

planilhatotal <- planilhatotal %>%
  filter(!is.na(Ano_leitura), !is.na(Ano_publicação), !is.na(Pontos))

cat("✅ Dados preparados:", nrow(planilhatotal), "registros\n")
cat("📅 Período:", min(planilhatotal$Ano_publicação), "a", max(planilhatotal$Ano_publicação), "\n")

# ===== 1.6 FILTRAR POR LEITOR =====
LEITOR <- "Victor V."  # ou "Victor V." "Tulipa S. V."
cat("\n📌 Leitor selecionado:", LEITOR, "\n")

p2 <- planilhatotal %>% filter(Leitor == LEITOR)

# ===== TRATAMENTO DE DATAS =====

cat("\n📅 Criando colunas de data...\n")

# 1. Data de Publicação (Data)
p2 <- p2 %>%
  mutate(
    Mês = as.numeric(ifelse(is.na(Mês) | Mês == "", 1, Mês)),
    Dia = as.numeric(ifelse(is.na(Dia) | Dia == "", 1, Dia)),
    Data = make_date(Ano_publicação, Mês, Dia)
  )

# 2. Data da História (Data.H) - mantém anos < 1000 como NA
p2 <- p2 %>%
  mutate(
    Mes.H = as.numeric(ifelse(is.na(Mes.H) | Mes.H == "", 1, Mes.H)),
    Dia.H = as.numeric(ifelse(is.na(Dia.H) | Dia.H == "", 1, Dia.H)),
    Data.H = if_else(Ano.H >= 1000, make_date(Ano.H, Mes.H, Dia.H), NA_Date_)
  )

# ===== VERIFICAÇÃO =====
cat("✅ Datas criadas!\n")
cat("📅 Classe de Data:", class(p2$Data), "\n")
cat("📅 Classe de Data.H:", class(p2$Data.H), "\n")
cat("📊 Data.H (válidas):", sum(!is.na(p2$Data.H)), "de", nrow(p2), "\n")


# ============================================================================
# 2. ANÁLISE TEMPORAL (Ano de publicação, séculos, calendário)
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("📊 ANÁLISE TEMPORAL\n")
cat(rep("=", 70), "\n\n", sep = "")

# ===== 2.1 MATRIZ DE ACUMULAÇÃO =====
acum <- dcast(p2, Ano_leitura ~ Título, value.var = "Página_Total", fun.aggregate = sum)
acum <- data.frame(acum, row.names = 1)
acum[is.na(acum)] <- 0

# ===== GRÁFICO 01: MÉTODOS COMPARATIVOS =====
cat("📊 01. Curvas de acumulação comparativas...\n")

sp1 <- specaccum(acum, method = "rarefaction")
sp2 <- specaccum(acum, method = "exact")
sp3 <- specaccum(acum, method = "random")
sp4 <- specaccum(acum, method = "collector")

df_metodos <- data.frame(
  Sites = sp1$sites,
  Rarefacao = sp1$richness,
  Riqueza_Esperada = sp2$richness,
  Sitios_Aleatorios = sp3$richness,
  Coletor = sp4$richness
) %>% pivot_longer(-Sites, names_to = "Metodo", values_to = "Richness")

g_metodos <- ggplot(df_metodos, aes(x = Sites, y = Richness, color = Metodo)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12")) +
  labs(title = "Curvas de Acumulação - Métodos Comparativos",
       x = "Anos de Leitura", y = "Número de Livros") +
  tema_musica() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 2))

print(g_metodos)
salvar_pdf(g_metodos, "01_metodos_comparativos")

# ===== GRÁFICO 02: ACUMULAÇÃO POR TÍTULO (COM SÉCULO) =====
cat("📊 02. Acumulação por título com século...\n")

acum <- dcast(p2, Ano_publicação ~ Título, value.var = "Página_Total", fun.aggregate = sum)
acum <- data.frame(acum, row.names = 1)
acum[is.na(acum)] <- 0


S <- rowSums(acum)
spAbund <- specnumber(acum)

df_acum <- data.frame(
  Ano_publicação = as.numeric(rownames(acum)),
  S = S,
  spAbund = spAbund
) %>%
  mutate(
    Seculo = case_when(
      Ano_publicação >= 1700 & Ano_publicação <= 1799 ~ "Séc. XVIII",
      Ano_publicação >= 1800 & Ano_publicação <= 1899 ~ "Séc. XIX",
      Ano_publicação >= 1900 & Ano_publicação <= 1999 ~ "Séc. XX",
      Ano_publicação >= 2000 & Ano_publicação <= 2099 ~ "Séc. XXI",
      TRUE ~ "Outro"
    ),
    Seculo = factor(Seculo, levels = c("Séc. XVIII", "Séc. XIX", "Séc. XX", "Séc. XXI", "Outro"))
  )

cores_seculo <- c(
  "Séc. XVIII" = "#8E44AD",
  "Séc. XIX" = "#2980B9",
  "Séc. XX" = "#27AE60",
  "Séc. XXI" = "#E67E22",
  "Outro" = "#95A5A6"
)

g_acum_side <- ggplot(df_acum, aes(x = Ano_publicação, y = S, color = Seculo)) +
  geom_point(aes(size = spAbund), alpha = 0.7) +
  geom_smooth(aes(fill = Seculo), method = "loess", se = TRUE, alpha = 0.2, size = 0.8) +
  geom_label_repel(aes(label = Ano_publicação), size = 2.8, alpha = 0.8, show.legend = FALSE,
                   box.padding = 0.3, point.padding = 0.5, max.overlaps = 20) +
  geom_ysideboxplot(aes(fill = Seculo), alpha = 0.5, width = 0.6, show.legend = FALSE) +
  # ===== MUDANÇA: geom_xsidehistogram em vez de geom_xsidedensity =====
  geom_xsidehistogram(aes(fill = Seculo, y = after_stat(count)), alpha = 0.5, show.legend = FALSE) +
  scale_color_manual(values = cores_seculo, name = "Século") +
  scale_fill_manual(values = cores_seculo, name = "Século") +
  scale_size_continuous(range = c(2, 12), name = "Nº de Escritores") +
  labs(title = "Número de Livros por Ano",
       subtitle = "Boxplot lateral (S) e histograma superior (Ano) por século",
       x = "Ano de Publicação", y = "Número de Páginas") +
  tema_musica() +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1),
        ggside.panel.scale.x = 0.25, ggside.panel.scale.y = 0.25) +
  guides(color = guide_legend(title = "Século", override.aes = list(size = 3)),
         fill = guide_legend(title = "Século"))

print(g_acum_side)
salvar_pdf(g_acum_side, "02_acumulacao_titulo_side", largura = 22, altura = 12)


# ===== GRÁFICO 03: BOXPLOT POR CONTINENTE =====
cat("📊 03. Boxplot por continente...\n")

if("Continente" %in% names(p2) && "Região" %in% names(p2)) {
  p2_cont <- p2 %>% filter(!is.na(Continente), !is.na(Região))
  
  g_boxplot <- ggplot(p2_cont, aes(x = Continente, y = Ano_publicação)) +
    geom_boxplot(aes(fill = Região), alpha = 0.6) +
    scale_fill_manual(values = cores_musica(length(unique(p2_cont$Região)))) +
    labs(title = "Distribuição de Anos por Continente", 
         x = "Continente", y = "Ano de Publicação") +
    tema_musica() +
    theme(legend.position = "bottom")
  
  print(g_boxplot)
}

# ===== GRÁFICO 04: CALENDÁRIO DE LEITURA =====
cat("📊 04. Calendário de Leitura...\n")

if("Data" %in% names(p2)) {
  p2_calendario <- p2 %>%
    filter(!is.na(Data)) %>%
    mutate(Mes = month(Data, label = TRUE),
           Tempo = paste0(floor(year(Data) / 10) * 10, "s"))  # Ex: "1990s", "2000s"
  
  g_calendario <- ggplot(p2_calendario, aes(x = Mes)) +
    geom_bar(aes(fill = Tempo), position = "dodge") +  # Mudança: fill = Tempo
    labs(title = "Distribuição de Leitura por Mês",
         x = "Mês", y = "Número de Livros", fill = "Década") +
    tema_musica() +
    theme(legend.position = "bottom")
  
  print(g_calendario)
}



# ============================================================================
# 3. ANÁLISE DE GÊNEROS NO TEMPO
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("📊 ANÁLISE DE GÊNEROS E SUBGÊNEROS\n")
cat(rep("=", 70), "\n\n", sep = "")

# ===== GRÁFICO 05: EVOLUÇÃO DOS GÊNEROS =====
cat("📊 05. Evolução dos Gêneros ao Longo do Tempo...\n")

evolucao <- p2 %>%
  filter(!is.na(Gênero), !is.na(Ano_publicação)) %>%
  group_by(Ano_publicação, Gênero) %>%
  summarise(Total = n(), .groups = "drop")

g_evolucao <- ggplot(evolucao, aes(x = Ano_publicação, y = Total, fill = Gênero)) +
  geom_area(alpha = 0.7) +
  labs(title = "Evolução dos Gêneros Musicais ao Longo do Tempo",
       x = "Ano", y = "Número de Álbuns") +
  tema_musica() +
  theme(legend.position = "bottom")

print(g_evolucao)


# ===== GRÁFICO: REGIÃO vs ANO DE LANÇAMENTO (COM FACET POR CONTINENTE) =====
cat("📊 Gráfico: Região vs Ano da História...\n")

# ===========================================
# ===== CONFIGURAÇÃO DA SELEÇÃO =====
#   "TOTAL", "País", "Região" ou "Continente"
# ===========================================

SELECAO <- "TOTAL"
FILTRO_VALOR <- "TOTAL"

# ===== PREPARAÇÃO DOS DADOS =====

dados_base <- p2 %>%
  filter(!is.na(Região), 
         !is.na(País), 
         !is.na(Ano.H))  # <-- USANDO Ano.H

# Aplica filtro ou mantém total
if (SELECAO == "TOTAL") {
  
  dados <- dados_base %>%
    group_by(Região, País, Ano.H) %>%
    summarise(Quantidade = n(), .groups = "drop") %>%
    filter(Quantidade > 0)
  
  titulo <- "Distribuição de Lançamentos por Região e Ano da História (Todos)"
  subtitulo <- "Sem filtro aplicado"
  
} else {
  
  if (!(SELECAO %in% names(dados_base))) {
    stop(paste("Coluna", SELECAO, "não encontrada."))
  }
  
  dados <- dados_base %>%
    filter(!!sym(SELECAO) == FILTRO_VALOR) %>%
    group_by(Região, País, Ano.H) %>%
    summarise(Quantidade = n(), .groups = "drop") %>%
    filter(Quantidade > 0)
  
  titulo <- paste("Distribuição de Lançamentos -", FILTRO_VALOR)
  subtitulo <- paste("Filtro por", SELECAO)
}

# ===== GRÁFICO =====

g_regiao_data <- ggplot(dados, aes(x = Região, 
                                    y = Ano.H, 
                                    size = Quantidade, 
                                    color = País)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(1, 10)) +
  scale_y_continuous(
    breaks = seq(-3000, 2024, 500)  # Ajuste conforme seus dados
  ) +
  labs(title = titulo,
       subtitle = subtitulo,
       x = "Região", 
       y = "Ano da História", 
       size = "Quantidade", 
       color = "País") +
  tema_musica() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(g_regiao_data)

# ===== GRÁFICO 06: REDE DE CO-OCORRÊNCIA com Tempo =====

cat("📊 06. Rede de Co-ocorrência de Gêneros e Tempo (com Ano.H)...\n")

# ===========================================
# ===== CONFIGURAÇÃO DA SELEÇÃO =====
# ===========================================

TIPO_FILTRO <- "País.H" #TOTAL, "Continente"
FILTRO_VALOR <- "Brasil" #TOTAL, "Brasil"

# ===== PREPARAÇÃO DOS DADOS =====

rede_base <- p2 %>%
  filter(!is.na(Ano.H),  # <-- USANDO Ano.H
         !is.na(Tempo),
         !is.na(Origem.local))

# Aplica filtro ou mantém total
if (TIPO_FILTRO == "TOTAL") {
  
  rede <- rede_base %>%
    group_by(Ano.H, Tempo) %>%  # <-- USANDO Ano.H
    summarise(Freq = n(), .groups = "drop") %>%
    filter(Freq > 0) %>%
    arrange(Ano.H)
  
  titulo <- "Relação entre Ano da História e Tempo (Todos os Dados)"
  subtitulo <- "Sem filtro aplicado"
  
} else {
  
  if (!(TIPO_FILTRO %in% names(rede_base))) {
    stop(paste("Coluna", TIPO_FILTRO, "não encontrada."))
  }
  
  rede <- rede_base %>%
    filter(!!sym(TIPO_FILTRO) == FILTRO_VALOR) %>%
    group_by(Ano.H, Tempo) %>%  # <-- USANDO Ano.H
    summarise(Freq = n(), .groups = "drop") %>%
    filter(Freq > 0) %>%
    arrange(Ano.H)
  
  titulo <- paste("Relação entre Ano da História e Tempo -", FILTRO_VALOR)
  subtitulo <- paste("Filtro por", TIPO_FILTRO)
}

# ===== ORDENAÇÃO DO EIXO Y =====

ordem_tempo <- rede %>%
  arrange(Ano.H) %>%
  pull(Tempo) %>%
  unique()

rede$Tempo <- factor(rede$Tempo, levels = ordem_tempo)

# ===== GRÁFICO =====

g_rede <- ggplot(rede, aes(x = Ano.H,  # <-- USANDO Ano.H
                            y = Tempo, 
                            size = Freq, 
                            color = Freq)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(1, 10)) +
  scale_x_continuous(
    breaks = seq(-3000, 2024, 500)  # Ajuste conforme seus dados
  ) +
  labs(title = titulo,
       subtitle = subtitulo,
       x = "Ano da História", 
       y = "Tempo (Gênero/Subgênero)") +
  tema_musica() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g_rede)

# ============================================================================
# 4. ANÁLISE GEOGRÁFICA (Países, Continentes)
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("📊 ANÁLISE GEOGRÁFICA\n")
cat(rep("=", 70), "\n\n", sep = "")

# ===== 4.1 CLUSTER E HEATMAP - SUBGÊNEROS POR PAÍS =====
cat("📊 07. Cluster e Heatmap - Subgêneros por País...\n")

p2_heat <- p2 %>% filter(!is.na(País), !is.na(Subgênero))

matriz_heat <- dcast(p2_heat, País ~ Subgênero, 
                     value.var = "Página_Total", 
                     fun.aggregate = function(x) ifelse(sum(x) > 0, 1, 0))

paises <- matriz_heat$País
matriz_heat_numeric <- as.matrix(matriz_heat[, -1])
rownames(matriz_heat_numeric) <- paises

riqueza <- rowSums(matriz_heat_numeric)
matriz_filtrada <- matriz_heat_numeric[riqueza >= 2, , drop = FALSE]

freq_sub <- colSums(matriz_filtrada)
matriz_filtrada <- matriz_filtrada[, freq_sub >= 2, drop = FALSE]

cat("  📊", nrow(matriz_filtrada), "países x", ncol(matriz_filtrada), "subgêneros\n")

# Cluster
dist_jaccard <- vegdist(matriz_filtrada, method = "jaccard", binary = TRUE)
hc <- hclust(dist_jaccard, method = "ward.D2")

# Dendrograma
dend <- as.dendrogram(hc) %>%
  set("branches_k_color", k = 4) %>%
  set("branches_lwd", 1.5) %>%
  set("labels_cex", 0.8)

ggd <- as.ggdend(dend)

g_dend <- ggplot(ggd, horiz = TRUE) +
  labs(title = "Cluster de Países por Subgênero Musical",
       subtitle = paste(nrow(matriz_filtrada), "países |", ncol(matriz_filtrada), "subgêneros"),
       x = "Distância de Jaccard") +
  tema_musica() +
  theme(axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 9))

print(g_dend)
salvar_pdf(g_dend, "dendrograma_subgeneros", largura = 14, altura = 10)

# Heatmap
matriz_long <- melt(matriz_filtrada)
names(matriz_long) <- c("País", "Subgênero", "Presenca")

ordem_paises <- rownames(matriz_filtrada)[hc$order]
ordem_subgeneros <- colnames(matriz_filtrada)[order(colSums(matriz_filtrada), decreasing = TRUE)]

matriz_long$País <- factor(matriz_long$País, levels = ordem_paises)
matriz_long$Subgênero <- factor(matriz_long$Subgênero, levels = ordem_subgeneros)

g_heatmap <- ggplot(matriz_long, aes(x = Subgênero, y = País, fill = as.factor(Presenca))) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_manual(values = c("0" = "#ECF0F1", "1" = "#2C3E50"),
                    name = "Presença", labels = c("Ausente", "Presente")) +
  labs(title = "Presença de Subgêneros por País",
       subtitle = paste(nrow(matriz_filtrada), "países |", ncol(matriz_filtrada), "subgêneros"),
       x = "Subgênero", y = "País") +
  tema_musica() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 9),
        legend.position = "bottom", legend.key.size = unit(0.8, "cm")) +
  guides(fill = guide_legend(title = "Presença", nrow = 1))

print(g_heatmap)
salvar_pdf(g_heatmap, "heatmap_subgeneros_simples", largura = 18, altura = 12)
salvar_png(g_heatmap, "heatmap_subgeneros_simples", largura = 18, altura = 12, dpi = 300)


# ============================================================================
# GRÁFICO DE COORDENADAS - MAPA CARTESIANO
# ============================================================================
# ============================================================================
# MAPA CARTESIANO - COORDENADAS (COM SELEÇÃO DE PAÍS/ESTADO)
# ============================================================================
cat("\n", rep("=", 70), "\n", sep = "")
cat("📊 MAPA CARTESIANO - COORDENADAS\n")
cat(rep("=", 70), "\n\n", sep = "")

# ===========================================
# ===== CONFIGURAÇÃO DA SELEÇÃO =====
#   "TOTAL", "País.H" ou "Estado.H"
# ===========================================

SELECAO <- "País.H"           # <-- MUDE AQUI: "TOTAL"/"País.H"/"Estado.H"/"Municipio.H"
FILTRO_VALOR <- "Brasil"      # <-- MUDE AQUI (se não for "TOTAL")

# ===== CONVERTER COORDENADAS =====
p2 <- p2 %>%
  mutate(
    Latitude = as.numeric(gsub(",", ".", Latitude)),
    Longitude = as.numeric(gsub(",", ".", Longitude))
  )

# ===== DADOS =====
dados_base <- p2 %>%
  filter(!is.na(Latitude), !is.na(Longitude), !is.na(País.H), !is.na(Estado.H), !is.na(Preciso)) %>%
  filter(Preciso %in% c("Preciso", "Muito Preciso", "Impreciso"))

# Aplica filtro ou mantém tudo
if (SELECAO == "TOTAL") {
  
  dados <- dados_base
  
  titulo <- "Mapa Cartesiano - Coordenadas (Todos os Dados)"
  subtitulo <- "Sem filtro aplicado"
  
} else {
  
  if (!(SELECAO %in% names(dados_base))) {
    stop(paste("Coluna", SELECAO, "não encontrada."))
  }
  
  dados <- dados_base %>%
    filter(!!sym(SELECAO) == FILTRO_VALOR)
  
  titulo <- paste("Mapa Cartesiano -", FILTRO_VALOR)
  subtitulo <- paste("Filtro por", SELECAO)
}

# ===== CONTAGEM PARA ELIPSES =====
contagem_estados <- dados %>%
  group_by(Estado.H) %>%
  summarise(N = n(), .groups = "drop") %>%
  filter(N >= 3)

dados_elipse <- dados %>% filter(Estado.H %in% contagem_estados$Estado.H)

# ===== RÓTULOS ÚNICOS =====
labels_unicos <- dados %>%
  group_by(Cenário) %>%
  slice(1) %>%
  ungroup()

# ===== GRÁFICO =====
g_coordenadas <- ggplot(dados, aes(x = Longitude, y = Latitude)) +
  
  # Pontos com tamanho baseado em Página_Total
  geom_point(aes(color = Estado.H, shape = Preciso, size = Página_Total), alpha = 0.5) +
  
  # Rótulos ÚNICOS (Cenário)
  geom_text_repel(data = labels_unicos, 
                  aes(label = Cenário, color = Estado.H), 
                  size = 2.5, max.overlaps = 10, show.legend = FALSE) +
                  
   geom_label_repel(data = labels_unicos, 
                  aes(label = Título, color = Estado.H), 
                  size = 2.5, max.overlaps = 10, show.legend = FALSE) +
                  
  # Elipses por Estado.H
  stat_ellipse(data = dados_elipse,
               aes(color = País.H, fill = País.H),
               type = "norm", level = 0.70,
               alpha = 0.15, show.legend = FALSE) +
  
  # Shapes para as 3 categorias de Preciso
  scale_shape_manual(values = c("Preciso" = 16, "Muito Preciso" = 17, "Impreciso" = 1)) +
  
  # Escala de tamanho (ajuste conforme seus dados)
  scale_size_continuous(
    range = c(1, 12),
    name = "Páginas"
  ) +
  
  labs(title = titulo,
       subtitle = subtitulo,
       x = "Longitude", y = "Latitude",
       shape = "Preciso") +  # Removeu color = "Estado.H"
  
  tema_musica() +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  
  # ===== REMOVER LEGENDA DE COR =====
  guides(color = "none")

print(g_coordenadas)

# ===== 4.2 PCA - PAÍSES POR ESFERA =====
cat("\n📊 08. PCA - Países por Esfera...\n")

p2_pca <- p2 %>%
  filter(!is.na(País), !is.na(Esfera), !is.na(Continente)) %>%
  separate_rows(Esfera, sep = "/")

local_pca <- dcast(p2_pca, País + Continente ~ Esfera, 
                   value.var = "Página_Total", fun.aggregate = sum)

identificadores <- local_pca[, c("País", "Continente")]
local_pca_num <- as.matrix(local_pca[, !(names(local_pca) %in% c("País", "Continente"))])
local_pca_num[is.na(local_pca_num)] <- 0
rownames(local_pca_num) <- identificadores$País

linhas_validas <- rowSums(local_pca_num) > 0
local_pca_num <- local_pca_num[linhas_validas, , drop = FALSE]
identificadores <- identificadores[linhas_validas, , drop = FALSE]
identificadores$label <- identificadores$País

pca_res <- prcomp(local_pca_num, scale. = TRUE)
scores <- as.data.frame(pca_res$x)
scores$País <- rownames(scores)
scores <- merge(scores, identificadores, by = "País")

contagem <- table(scores$Continente)
continentes_validos <- names(contagem[contagem >= 3])

g_pca <- ggplot(scores, aes(x = PC1, y = PC2, color = Continente)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(aes(label = País), size = 3.5, max.overlaps = 15) +
  stat_ellipse(data = scores[scores$Continente %in% continentes_validos, ],
               aes(fill = Continente), type = "norm", level = 0.95,
               alpha = 0.15, show.legend = FALSE) +
  geom_segment(data = as.data.frame(pca_res$rotation),
               aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5),
               color = "blue", alpha = 0.6, size = 0.8,
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_text(data = as.data.frame(pca_res$rotation),
            aes(x = PC1 * 5.5, y = PC2 * 5.5, 
                label = rownames(as.data.frame(pca_res$rotation))),
            color = "blue", size = 3, fontface = "bold") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
  scale_color_manual(values = cores_musica(length(unique(scores$Continente)))) +
  scale_fill_manual(values = cores_musica(length(unique(scores$Continente)))) +
  labs(title = "PCA - Países por Esfera Musical",
       subtitle = paste(nrow(scores), "países | Elipses:", paste(continentes_validos, collapse = ", ")),
       x = paste("PC1 (", round(summary(pca_res)$importance[2, 1] * 100, 1), "%)", sep = ""),
       y = paste("PC2 (", round(summary(pca_res)$importance[2, 2] * 100, 1), "%)", sep = ""),
       color = "Continente") +
  tema_musica() +
  theme(legend.position = "bottom")

print(g_pca)

# ============================================================================
# 5. ANÁLISE DE EDITORAS
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("📊 ANÁLISE DE EDITORAS\n")
cat(rep("=", 70), "\n\n", sep = "")

# ===== PCA - EDITORA POR SUBGÊNERO =====
cat("📊 09. PCA - Editora por Subgênero...\n")

p2_pca2 <- p2 %>%
  filter(!is.na(Editora), !is.na(Subgênero))

local_pca2 <- dcast(p2_pca2, Editora ~ Subgênero, value.var = "Página_Total", fun.aggregate = sum, fill = 0)
local_pca2 <- data.frame(local_pca2, row.names = 1)

S <- specnumber(local_pca2)
local_pca2 <- local_pca2[S > 3, ]

variancias <- apply(local_pca2, 2, var, na.rm = TRUE)
local_pca2 <- local_pca2[, variancias > 0 & !is.na(variancias), drop = FALSE]
local_pca2 <- local_pca2[, colSums(local_pca2) > 0, drop = FALSE]

if(nrow(local_pca2) >= 3 && ncol(local_pca2) >= 2) {
  
  pca_res2 <- prcomp(local_pca2, scale. = TRUE)
  df_editora <- data.frame(Editora = rownames(local_pca2))
  
  g_pca2 <- autoplot(pca_res2, data = df_editora,
                     label = TRUE, label.size = 4,
                     frame = TRUE, frame.type = 't',
                     loadings = TRUE, loadings.colour = 'blue',
                     loadings.label = TRUE, loadings.label.size = 3) +
    labs(title = "PCA - Editoras por Subgênero",
         x = paste("PC1 (", round(summary(pca_res2)$importance[2, 1] * 100, 1), "%)", sep = ""),
         y = paste("PC2 (", round(summary(pca_res2)$importance[2, 2] * 100, 1), "%)", sep = "")) +
    tema_musica() +
    theme(legend.position = "none")
  
  print(g_pca2)
}

# ============================================================================
# 6. ANÁLISE DE TEXTO (Títulos e Resenhas)
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("📊 ANÁLISE DE TEXTO\n")
cat(rep("=", 70), "\n\n", sep = "")

# ===== NUVEM DE PALAVRAS - TÍTULOS =====
cat("📊 10. Nuvem de Palavras - Títulos...\n")

words <- p2$Título %>% as.character()

word.corpus <- Corpus(VectorSource(words)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("SMART")) %>%
  tm_map(removeWords, c("the", "vol", "dos", "para", "das", "uma", "com", "seu", "sua"))

word.counts <- as.matrix(TermDocumentMatrix(word.corpus))
word.freq <- sort(rowSums(word.counts), decreasing = TRUE)

set.seed(32)
wordcloud(words = names(word.freq), freq = word.freq,
          scale = c(4, 0.3), max.words = 100,
          random.order = FALSE, color = wes_palette("Darjeeling1"), rot.per = 0.7)

png("resultados/nuvem_palavras_titulos.png", width = 800, height = 600)
set.seed(32)
wordcloud(words = names(word.freq), freq = word.freq,
          scale = c(4, 0.3), max.words = 100,
          random.order = FALSE, color = wes_palette("Darjeeling1"), rot.per = 0.7)
dev.off()
cat("  ✅ Salvo: resultados/nuvem_palavras_titulos.png\n")

# ===== NUVEM DE PALAVRAS - RESENHA =====
cat("\n📊 11. Nuvem de Palavras - Resenhas...\n")

if("Resenha" %in% names(p2)) {
  
  resenhas <- p2$Resenha %>% as.character() %>% na.omit()
  
  if(length(resenhas) > 0) {
    
    resenha.corpus <- Corpus(VectorSource(resenhas)) %>%
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(stripWhitespace) %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removeWords, stopwords("portuguese")) %>%
      tm_map(removeWords, c("que", "com", "uma", "para", "como", "mais", "por", "sem", "seu", "sua",
                           "ele", "ela", "eles", "elas", "isso", "aquele", "aquela", "quando",
                           "muito", "pouco", "todo", "ainda", "assim", "depois", "então", "onde",
                           "aos", "as", "ao", "à", "é", "ser", "está", "estão"))
    
    resenha.counts <- as.matrix(TermDocumentMatrix(resenha.corpus))
    resenha.freq <- sort(rowSums(resenha.counts), decreasing = TRUE)
    
    cat("  📊 Palavras mais frequentes:\n")
    top10 <- head(resenha.freq, 10)
    for(i in 1:length(top10)) {
      cat("    ", i, ".", names(top10)[i], ":", top10[i], "\n")
    }
    
    set.seed(42)
    wordcloud(words = names(resenha.freq), freq = resenha.freq,
              scale = c(4, 0.5), max.words = 120,
              random.order = FALSE, color = wes_palette("Darjeeling2"), rot.per = 0.3)
    
    png("resultados/nuvem_palavras_resenhas.png", width = 800, height = 600)
    set.seed(42)
    wordcloud(words = names(resenha.freq), freq = resenha.freq,
              scale = c(4, 0.5), max.words = 120,
              random.order = FALSE, color = wes_palette("Darjeeling2"), rot.per = 0.3)
    dev.off()
    cat("  ✅ Salvo: resultados/nuvem_palavras_resenhas.png\n")
  }
}

# ============================================================================
# 7. DASHBOARD E VISUALIZAÇÕES INTERATIVAS
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("📊 DASHBOARD E VISUALIZAÇÕES INTERATIVAS\n")
cat(rep("=", 70), "\n\n", sep = "")

# ===== DASHBOARD =====
cat("📊 12. Dashboard Completo...\n")

if("patchwork" %in% installed.packages()) {
  library(patchwork)
  
  g1 <- ggplot(p2, aes(x = Gênero)) + 
    geom_bar(aes(fill = Subgênero)) + 
    coord_flip() +
    tema_musica() +
    theme(legend.position = "none")
  
  g2 <- ggplot(p2, aes(x = Pontos)) + 
    geom_histogram(bins = 20, fill = "#3498DB", alpha = 0.7) +
    tema_musica()
  
  g3 <- ggplot(p2, aes(x = Ano_publicação)) + 
    geom_histogram(bins = 20, fill = "#2ECC71", alpha = 0.7) +
    tema_musica()
  
  dashboard <- (g1 | g2) / g3 +
    plot_annotation(title = "📊 Dashboard - Análise de Álbuns")
  
  print(dashboard)
}

# ===== SCATTERPLOT 3D =====
cat("\n📊 13. Scatterplot 3D - Interativo...\n")

if("plotly" %in% installed.packages()) {
  library(plotly)
  
  p3d <- plot_ly(p2, x = ~Página_Total, y = ~Ano_publicação, z = ~Pontos,
                 color = ~Gênero, type = "scatter3d", mode = "markers",
                 marker = list(size = 5))
  
  print(p3d)
}

