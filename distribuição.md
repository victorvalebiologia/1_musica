# INICIAR
## Música -> distribuição dos dados
#upgride de pacotes 'update.packages(repos='http://cran.rstudio.com/ ', ask=FALSE, checkBuilt=TRUE)'

## Pastas 
getwd()
setwd("/home/user/Área de Trabalho/Música") 
#Mudança de diretório para pasta do projeto
#p_opendir(path.expand("~")) #abrir pasta
#p_opendir(pacman:::p_basepath()) #abir pasta pacotes

### Pacotes e entradas de arquivos
if(!require(pacman, quietly = TRUE))(install.packages("pacman")) #agrupador de funções
#pacman::p_load(readODS, openxlsx) 
#setRepositories(ind=c(1:9))
##### Pacote de leitor de ods (reserva)
#caminho.do.arquivo <- "/home/user/Área de Trabalho/Serviços/ES - Rio Bananal/2021_03_06_grancol_rbananal.ods"
#planilhatotal <- read_ods(caminho.do.arquivo,sheet = 1,col_names = T,na = "")
#p_opendir(path.expand("~")) #abrir pasta
#p_opendir(pacman:::p_basepath()) #abir pasta pacotes
### Pacotes e entradas de arquivos
if(!require(pacman, quietly = TRUE))(install.packages("pacman")) #agrupador de funções
#pacman::p_load(readODS, openxlsx) 
#setRepositories(ind=c(1:9))
##### Pacote de leitor de ods (reserva)
#caminho.do.arquivo <- "/home/user/Área de Trabalho/Serviços/ES - Rio Bananal/2021_03_06_grancol_rbananal.ods"
#Xplanilhatotal <- read_ods(caminho.do.arquivo,sheet = 1,col_names = T,na = "")
## Pacote de análise e leitura de dados, gráficos
pacman::p_load(magrittr,dplyr) #magrittr para operações de pipe/dplyr para manipulador de dados
##### Operador Pipe - usar o valor resultante da expressão do lado esquerdo como primeiro argumento da função do lado direito
##### As medidas de dispersão são estatísticas descritivas, que quantificam de algum modo a variabilidade dos dados, geralmente utilizando como referência uma medida de posição.
## Pacotes gŕaficos
pacman::p_load(ggplot2, devtools, ggrepel, graphics) 
#scripta reservas
#installed.packages('ggplot2')
#install.packages("devtools")
#fazer gráficos e extensões para salvar e inserir imagem
#require(remotes)
#remotes::install_version
#install_version("ggplot2", version = "2.0.0", repos = "http://cran.us.r-project.org")
#Library(ggplot2)
## Pacote ecologia
pacman::p_load(vegan)  #vegan para estatística ecológica/graphics para os gráficos

# Caregar Planilha
pacman::p_load(openxlsx) 
caminho.do.arquivo <- "/home/user/Área de Trabalho/Música/musica_estatistica.xlsx"
planilhatotal <- read.xlsx(caminho.do.arquivo, #local do arquivo
                           sheet = 1, # em qual planilha estão os dados
                           colNames = T, # as colunas dos dados possuem nomes?
                           na.strings = "NA") # como estão identificados os dados omissos?
head(planilhatotal)

### Teste de plots
ts.plot(planilhatotal$Lançado)

boxplot(planilhatotal$Pontos)

### Corrigir planilha
##### Retirar os NA 
planilhatotal <- subset(planilhatotal, !is.na(Lançado)) #tirar n/a da ano
planilhatotal <- subset(planilhatotal, !is.na(Pontos)) #tirar n/a da pontos
##### Planilhas extras -> só álbum
planilhaalbum <- planilhatotal
planilhaalbum <- subset(planilhaalbum,Tipo!="Coletânea") #tirar n/a da espécies
planilhaalbum <- subset(planilhaalbum,Tipo!="Single") #tirar n/a da espécies
planilhaalbum <- subset(planilhaalbum,Tipo!="Bonus") #tirar n/a da espécies
head(planilhaalbum)

# Distribuição
## Distribuição dos Dados em gráficos
ggplot(planilhaalbum, aes(x = Pontos , y = Gênero)) + 
  geom_boxplot(aes(colour= Raiz)) +
  #geom_violin(aes(colour= Raiz)) +
  #facet_grid(Gravação~.) +
  theme_classic() 
#ggsave("1.Box_Gen_Pontos.png",width = 10, height = 6, dpi = 300)

### Pacote
library(ggbeeswarm)

### Gráfico
ymin <- min(planilhatotal$Lançado)
ymax <- max(planilhatotal$Lançado)

plot_points_ga <- ggplot() +
  geom_quasirandom(aes(x = factor(Gênero), y = Lançado),
                   data = planilhatotal) +
  xlab("Gênero") +
  ylab("Lançado") +
  theme_bw() +
  scale_y_continuous(limits = c(ymin, ymax))

plot_points_ga
#ggsave("7.distrib_gen_ano.png",width = 15, height = 8, dpi = 600)

## Continente/lançado
### Gráfico
ymin <- min(planilhatotal$Lançado)
ymax <- max(planilhatotal$Lançado)

plot_points_ga <- ggplot() +
  geom_quasirandom(aes(x = factor(Continente), y = Lançado),
                   data = planilhatotal) +
  #facet_grid(Continente~.) +
  xlab("Subcontinente") +
  ylab("Lançado") +
  theme_bw() +
  scale_y_continuous(limits = c(ymin, ymax))

plot_points_ga
#ggsave("7.distrib_gen_ano.png",width = 15, height = 8, dpi = 600)

# Diferença média de dois grupos 
#[site](https://www-r--bloggers-com.cdn.ampproject.org/v/s/www.r-bloggers.com/2021/05/showing-a-difference-in-mean-between-two-groups-take-2/amp/?amp_gsa=1&amp_js_v=a6&usqp=mq331AQFKAGwASA%3D#amp_tf=De%20%251%24s&aoh=16206389890747&csi=0&referrer=https%3A%2F%2Fwww.google.com&ampshare=https%3A%2F%2Fwww.r-bloggers.com%2F2021%2F05%2Fshowing-a-difference-in-mean-between-two-groups-take-2%2F)

## Pacotes
pacman::p_load(dabestr, ggbeeswarm)
library(dabestr)

## Tipo
bootstrap <- dabest(planilhatotal, #demora
                    Gravação,
                    Lançado,
                    idx = c("Estúdio", "Live"),
                    paired = FALSE)

bootstrap_diff <- mean_diff(bootstrap)
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Música/7.estud_live_pop.png",width=800,height=600) #local e tmamanho
plot(bootstrap_diff)
#dev.off()

# PCA 
#[site](https://www-r--bloggers-com.cdn.ampproject.org/v/s/www.r-bloggers.com/2021/05/principal-component-analysis-pca-in-r/amp/?amp_gsa=1&amp_js_v=a6&usqp=mq331AQFKAGwASA%3D#amp_tf=De%20%251%24s&aoh=16204247084008&csi=0&referrer=https%3A%2F%2Fwww.google.com&ampshare=https%3A%2F%2Fwww.r-bloggers.com%2F2021%2F05%2Fprincipal-component-analysis-pca-in-r%2F)
### Pacotes
pacman::p_load(psych)
library("devtools")
library("ggbiplot")
#install_github("vqv/ggbiplot")

### Gênero 
##### Selecionar Tabelas
local<-reshape2::dcast(planilhatotal, País ~ Gênero, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)
grupo<-reshape2::dcast(planilhatotal, Continente + País ~ Gênero, value.var = "Pontos", fun.aggregate = sum)
grupo2<-reshape2::dcast(planilhatotal, País ~ Gênero, value.var = "Pontos", fun.aggregate = sum)

##### Gráfico
wine.pca <- prcomp(local, scale. = TRUE)
ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         #groups = grupo$Continente, 
         ellipse = TRUE, circle = TRUE) +
  geom_label_repel(aes(label = grupo2$País), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top') +
  theme_classic()
#ggsave("5.PCA_pais_gen.png",width = 15, height = 8, dpi = 600)

##### Gráfico Resumo
summary(prcomp(local, scale = TRUE))
biplot(prcomp(local, scale = TRUE))

### Continente
##### Selecionar Tabelas
local<-reshape2::dcast(planilhatotal, Gênero ~ Continente, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)
grupo<-reshape2::dcast(planilhatotal, Raiz + Gênero ~ Continente, value.var = "Pontos", fun.aggregate = sum)
grupo2<-reshape2::dcast(planilhatotal, Gênero ~ Continente, value.var = "Pontos", fun.aggregate = sum)

##### Gráfico
wine.pca <- prcomp(local, scale. = TRUE)
ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         groups = grupo$Raiz, 
         ellipse = TRUE, circle = TRUE) +
  geom_label_repel(aes(label = grupo2$Gênero), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
    scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top') +
  theme_classic()
#ggsave("5.PCA_gen_cont.png",width = 15, height = 8, dpi = 600)

### Subontinente
##### Selecionar Tabelas
local<-reshape2::dcast(planilhatotal, Gênero ~ Subcontinente, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)
grupo<-reshape2::dcast(planilhatotal, Raiz + Gênero ~ Subcontinente, value.var = "Pontos", fun.aggregate = sum)
grupo2<-reshape2::dcast(planilhatotal, Gênero ~ Subcontinente, value.var = "Pontos", fun.aggregate = sum)

##### Gráfico
wine.pca <- prcomp(local, scale. = TRUE)
ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         groups = grupo$Raiz, 
         ellipse = TRUE, circle = TRUE) +
  geom_label_repel(aes(label = grupo2$Gênero), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top') +
  theme_classic()
#ggsave("5.PCA_gen_subcont.png",width = 15, height = 8, dpi = 600)

##### Resumo
#plot(prcomp(local, scale. = TRUE))
summary(prcomp(local, scale = TRUE))
biplot(prcomp(local, scale = TRUE))

### País
##### Selecionar Tabelas
p2 <- planilhatotal <- subset(planilhatotal,Continente!="Varios") #tirar Vários de Contintete
p2 <- planilhatotal <- subset(p2,Subcontinente!="Varios") #tirar Vários de subcontintete
p2 <- p2 %>%  subset(Pontos > 8) 
local<-reshape2::dcast(p2, Gênero ~ País, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)
grupo<-reshape2::dcast(p2, Raiz + Gênero ~ Continente, value.var = "Pontos", fun.aggregate = sum)

##### Gráfico
wine.pca <- prcomp(local, scale. = TRUE)
ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         groups = grupo$Raiz, 
         ellipse = TRUE, circle = TRUE) +
  geom_label_repel(aes(label = grupo$Gênero), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top') +
  theme_classic()
#ggsave("5.PCA_gen_pais.png",width = 15, height = 8, dpi = 600)


##### Resumo
summary(prcomp(local, scale = TRUE))
biplot(prcomp(local, scale = TRUE))

# Distribuição tempo
### Pacotes
pacman::p_load(ggridges, forcat)

## Tipo
### Gráficos
## Tipo1
gg_tx_timeseries <- planilhaalbum %>%
  ggplot(aes(Lançado, Pontos, group = Gênero)) +
  geom_line(aes(colour = Continente), alpha = 0.25) +
  geom_smooth(
    aes(group = NULL),
    method = "loess",
    span = 0.1,
    se = FALSE,
    size = 2.5,
    color = "black") +
  theme_minimal() +
  labs(y = "", x = "", title = "Pontos no tempo")
gg_tx_timeseries

## Tipo2 - gênero
gg_tx_ridge <- planilhatotal %>%
  ggplot(aes(x = Lançado, y = Gênero)) +
  geom_density_ridges(
    color="gray20",
    fill="gray10",
    alpha=0.75,
    size=1) +
  theme_minimal() +
  labs(y = "Gênero", x = "Ano", title = "Distribuição dos pontos")
gg_tx_ridge
#ggsave("8.ridge_gener_ano.png",width = 15, height = 8, dpi = 600)

# Tipo2 - tipo
gg_tx_ridge <- planilhatotal %>%
  ggplot(aes(x = Pontos, y = Tipo)) +
  geom_density_ridges(
    color="gray20",
    fill="gray10",
    alpha=0.75,
    size=1) +
  theme_minimal() +
  labs(y = "Gênero", x = "Pontos", title = "Distribuição dos pontos")
gg_tx_ridge
#ggsave("8.ridge_tipo_pontos.png",width = 15, height = 8, dpi = 600)


# Pontos e gráficos laterais
#[site](https://www-r--bloggers-com.cdn.ampproject.org/v/s/www.r-bloggers.com/2021/05/visualization-graphs-ggside-with-ggplot/amp/?amp_gsa=1&amp_js_v=a6&usqp=mq331AQFKAGwASA%3D#amp_tf=De%20%251%24s&aoh=16208562938944&csi=0&referrer=https%3A%2F%2Fwww.google.com&ampshare=https%3A%2F%2Fwww.r-bloggers.com%2F2021%2F05%2Fvisualization-graphs-ggside-with-ggplot%2F)
## Pacotes
pacman::p_load(ggside, tidyverse,tidyquant)

### Continente
p2<-planilhaalbum %>%
  ggplot(aes(Lançado, Pontos, color = Continente)) +
  geom_point(size = 2, alpha = 0.3) +
  geom_smooth(aes(color = NULL), se=TRUE) +
  geom_xsidedensity(
    aes(
      y    = after_stat(count),
      fill = Continente
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  geom_ysidedensity(
    aes(
      x    = after_stat(count), #density para densidade
      fill = Continente
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  labs(title = "Distribuição dos pontos por gênero" ,
       subtitle = "Densidade",
       x = "Ano", y = "Pontos") +  theme(
         ggside.panel.scale.x = 0.4,
         ggside.panel.scale.y = 0.4
       )
plot(p2)
#ggsave("9.Distr_Ponto_ano_Cont.png",width = 15, height = 8, dpi = 600)
#p2 + ggside(x.pos = "bottom", y.pos = "left") + labs(title = "Distribuição dos pontos dos continentes por ano", subtitle = " ")

### Continente+subcontinente
p2 + facet_wrap(Subcontinente~.) + #facet_wrap(Continente~Gênero)
  labs(title = "Pontos para raiz musical", subtitle = "Distribuição por gêneros") +
  ggside(collapse = "x")
#ggsave("9.Distr_Ponto_ano_Subcont.png",width = 18, height = 8, dpi = 600)

### Raiz 
p2<-planilhaalbum %>%
  ggplot(aes(Lançado, Pontos, color = Raiz)) +
  geom_point(size = 2, alpha = 0.3) +
  geom_smooth(aes(color = NULL), se=TRUE) +
  geom_xsidedensity(
    aes(
      y    = after_stat(count),
      fill = Raiz
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  geom_ysidedensity(
    aes(
      x    = after_stat(count),
      fill = Raiz
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  labs(title = "Distribuição dos pontos por gênero" ,
       subtitle = "Densidade",
       x = "Ano", y = "Pontos") +  theme(
         ggside.panel.scale.x = 0.4,
         ggside.panel.scale.y = 0.4
       )
plot(p2)
#ggsave("9.Distr_Ponto_ano_Raiz.png",width = 18, height = 8, dpi = 600)

### Raiz + Genero
p2 + facet_wrap(Gênero~.) + #facet_wrap(Continente~Gênero)
  labs(title = "Pontos para raiz musical", subtitle = "Distribuição por gêneros") +
  ggside(collapse = "x")
#ggsave("9.Distr_Ponto_ano_gen.png",width = 18, height = 8, dpi = 600)

### Continente + raiz
p2 <- subset(planilhatotal,Continente!="Vários") 
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies

p2 <- p2 %>%
  ggplot(aes(Lançado, Pontos, color = Raiz)) +
  geom_point(size = 2, alpha = 0.3) +
  geom_smooth(aes(color = NULL), se=TRUE) +
  geom_xsidedensity(
    aes(
      y    = after_stat(count),
      fill = Raiz
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  geom_ysidedensity(
    aes(
      x    = after_stat(count),
      fill = Raiz
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  labs(title = "Distribuição dos pontos por gênero" ,
       subtitle = "Densidade",
       x = "Ano", y = "Pontos") +  theme(
         ggside.panel.scale.x = 0.4,
         ggside.panel.scale.y = 0.4
       )
p2 + facet_grid(Continente~Raiz, space = "free", scales = "free") +
  labs(title = "Relação continente raiz musical", subtitle = "") +
  ggside(collapse = "all")
#ggsave("9.Distr_Ponto_ano_contin_raiz.png",width = 18, height = 8, dpi = 600)

### Raiz + continente
p2 <- subset(planilhatotal,Continente!="Vários") 
p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Raiz)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  facet_grid(rows = vars(Continente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_raiz_contin_asax.png",width = 15, height = 8, dpi = 600)

## Região
### América anglosaxônica
p2 <- subset(planilhatotal, Subcontinente == "A. Anglo-Saxônica")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  facet_grid(rows = vars(País), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_gen_subcontin_AméricaSax.png",width = 15, height = 8, dpi = 600)

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = País)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  geom_xsidedensity(
    aes(
      y    = after_stat(count),
      color = País
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_pais_subcontin_Américasax.png",width = 15, height = 8, dpi = 600)

### América latina
p2 <- subset(planilhatotal, Subcontinente == "A. Latina")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_gen_subcontin_AméricaLat.png",width = 15, height = 8, dpi = 600)

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = País)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  geom_xsidedensity(
    aes(
      y    = after_stat(count),
      color = País
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_pais_subcontin_AméricaLat.png",width = 15, height = 8, dpi = 600)

### Caribe
p2 <- subset(planilhatotal, Subcontinente == "Caribe")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_gen_subcontin_Américacar.png",width = 15, height = 8, dpi = 600)

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = País)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  geom_xsidedensity(
    aes(
      y    = after_stat(count),
      color = País
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_pais_subcontin_Américacar.png",width = 15, height = 8, dpi = 600)

## Europa
### Ilhas Britânicas
p2 <- subset(planilhatotal, Subcontinente == "Ilhas Britânicas")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_gen_subcontin_EuropaIB.png",width = 15, height = 8, dpi = 600)

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = País)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  geom_xsidedensity(
    aes(
      y    = after_stat(count),
      color = País
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_pais_subcontin_EuropaIB.png",width = 15, height = 8, dpi = 600)

### Europa Ocidental
p2 <- subset(planilhatotal, Subcontinente == "E. Ocidental")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_gen_subcontin_EuropaEOci.png",width = 15, height = 8, dpi = 600)

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = País)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  geom_xsidedensity(
    aes(
      y    = after_stat(count),
      color = País
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_pais_subcontin_EuropaE.Oci.png",width = 15, height = 8, dpi = 600)

### Europa Meridional
p2 <- subset(planilhatotal, Subcontinente == "E. Meridional")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_gen_subcontin_EuropaEmer.png",width = 15, height = 8, dpi = 600)

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = País)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  geom_xsidedensity(
    aes(
      y    = after_stat(count),
      color = País
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_pais_subcontin_EuropaEMer.png",width = 15, height = 8, dpi = 600)

### Europa Setetrional
p2 <- subset(planilhatotal, Subcontinente == "E. Setentrional")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_gen_subcontin_EuropaESet.png",width = 15, height = 8, dpi = 600)

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = País)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  geom_xsidedensity(
    aes(
      y    = after_stat(count),
      color = País
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_pais_subcontin_EuropaE.Set.png",width = 15, height = 8, dpi = 600)

## Ásia
p2 <- subset(planilhatotal, Continente == "Ásia")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_gen_subcontin_Asia.png",width = 15, height = 8, dpi = 600)

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = País)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  geom_xsidedensity(
    aes(
      y    = after_stat(count),
      color = País
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_pais_subcontin_Asia.png",width = 15, height = 8, dpi = 600)

## África
p2 <- subset(planilhatotal, Continente == "África")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_gen_subcontin_África.png",width = 15, height = 8, dpi = 600)

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = País)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  geom_xsidedensity(
    aes(
      y    = after_stat(count),
      color = País
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_pais_subcontin_África.png",width = 15, height = 8, dpi = 600)

## Oceania
p2 <- subset(planilhatotal, Continente == "Oceania")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_gen_subcontin_Oceania.png",width = 15, height = 8, dpi = 600)

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = País)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  geom_xsidedensity(
    aes(
      y    = after_stat(count),
      color = País
    ),
    alpha    = 0.5,
    size     = 1,
    position = "stack"
  ) +
  facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_pais_subcontin_Oceania.png",width = 15, height = 8, dpi = 600)

## País
### Brasil
p2 <- subset(planilhatotal, País == "Brasil")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Região!="Vários") 
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  facet_grid(rows = vars(Região), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_gen_regi_BR.png",width = 15, height = 8, dpi = 600)

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Região)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  #facet_grid(rows = vars(Região), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_regi_BR.png",width = 15, height = 8, dpi = 600)

### EUA
p2 <- subset(planilhatotal, País == "EUA")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Região!="Vários") 
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  #facet_grid(rows = vars(Região), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_gen_regi_USA.png",width = 15, height = 8, dpi = 600)

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Região)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  #facet_grid(rows = vars(Região), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_regi_USA.png",width = 15, height = 8, dpi = 600)

### Inglaterra
p2 <- subset(planilhatotal, País == "Inglaterra")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Região!="Vários") 
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  #facet_grid(rows = vars(Região), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_gen_regi_England.png",width = 15, height = 8, dpi = 600)

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Região)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  #facet_grid(rows = vars(Região), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_regi_England.png",width = 15, height = 8, dpi = 600)

### Canada
p2 <- subset(planilhatotal, País == "Canadá")
p3 <- subset(planilhatotal, País == "Canadá/USA")
p2 <- rbind(p2,p3)
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Região!="Vários") 
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  #facet_grid(rows = vars(Região), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_gen_regi_Canada.png",width = 15, height = 8, dpi = 600)

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Região)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  #facet_grid(rows = vars(Região), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_regi_Canada.png",width = 15, height = 8, dpi = 600)

# GIF
#[site](https://www-r--bloggers-com.cdn.ampproject.org/v/s/www.r-bloggers.com/2021/05/animated-graph-gif-with-gganimate-ggplot/amp/?amp_gsa=1&amp_js_v=a6&usqp=mq331AQFKAGwASA%3D#amp_tf=De%20%251%24s&aoh=16210958981586&csi=0&referrer=https%3A%2F%2Fwww.google.com&ampshare=https%3A%2F%2Fwww.r-bloggers.com%2F2021%2F05%2Fanimated-graph-gif-with-gganimate-ggplot%2F)

## Pacotes
pacman::p_load(gganimat,ggplot2,dplyr,gapminder,ggthemes,gifski,readr,tidyr,cargo, rustc)
devtools::install_github('thomasp85/gganimate', force = TRUE)
### Baxixar cargo antes
devtools::install_github("r-rust/gifski", force = TRUE)
#install.packages("gifski", type = "source")

## Gráfico tipo 1
##### Gráfico estático - Lançado
graph1 = planilhatotal %>%
  ggplot(aes(x=Ranking, y=Pontos, color=Continente, size=Avaliação)) +
  geom_point(alpha = 0.7, stroke = 0) +
  theme_fivethirtyeight() +
  scale_size(range=c(2,12), guide="none") +
  scale_x_log10() +
  labs(title = "Pontos e Ranking",
       x = "Pontos",
       y = "Ranking",
       color = "Continente",
       caption = "Fonte: Planilahtotal") +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        legend.text=element_text(size=10)) +
  scale_color_brewer(palette = "Set2")
#graph1

##### Animação
graph1.animation = graph1 +
  transition_time(Lançado) +
  labs(subtitle = "Ano: {frame_time}") +
  shadow_wake(wake_length = 0.1) 

##### Criar animação
animate(graph1.animation, height = 500, width = 800, fps = 30, duration = 20,
        end_pause = 60, res = 100, renderer = gifski_renderer()) 

#anim_save("11.aniamtion_ranking.gif") # ou criar em site como https://gifmaker.me/
#anim_save("out.gif",animation = graph1.animation)

##### Gráfico estático - Ranking
graph1 = planilhatotal %>%
  ggplot(aes(x=Lançado, y=Pontos, color=Continente, size=Avaliação)) +
  geom_point(alpha = 0.7, stroke = 0) +
  theme_fivethirtyeight() +
  scale_size(range=c(2,12), guide="none") +
  scale_x_log10() +
  labs(title = "Evolução dos pontos de álbuns",
       x = "Ano",
       y = "Pontos",
       color = "Continente",
       caption = "Fonte: Planilahtotal") +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        legend.text=element_text(size=10)) +
  scale_color_brewer(palette = "Set2")
#graph1

##### Animação
graph1.animation = graph1 +
  transition_time(Faixa) +
  labs(subtitle = "Faixa: {frame_time}") +
  shadow_wake(wake_length = 0.1) 

##### Criar animação
animate(graph1.animation, height = 500, width = 800, fps = 30, duration = 20,
        end_pause = 60, res = 100, renderer = gifski_renderer()) 

#anim_save("11.aniamtion_faixa.gif") # ou criar em site como https://gifmaker.me/
#anim_save("out.gif",animation = graph1.animation)

## Gráfico tipo 2
### Tranforma planilha
game_sales = planilhatotal %>%
  mutate(Lançado = as.numeric(Lançado)) %>%
#filter(Raiz == 'Latin Music', Gênero %in% c("Samba", "MPB")) %>% 
#drop_na() %>%
  group_by(Lançado, Continente) %>%
  summarise(Pontos = sum(Pontos), n = n(), .groups = 'drop')
  
##### Criar gráfico
graph2<- game_sales %>%
  ggplot(aes(x=Lançado, y=Pontos, color=Continente)) +
  geom_line(size = 2, alpha = 0.75) +
  theme_solarized_2(light = FALSE) +
  labs(title = "Continentes por década",
       y = "Pontos") +
  theme(text = element_text(family = "DM Sans Medium", colour = "#EEEEEE"),
        title = element_text(color = "#EEEEEE"),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "#111111"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Pastel1") +
  geom_point() 
#+ scale_x_continuous(breaks = 0:100)

##### Animação
graph2.animation<-graph2 +
  transition_reveal(Lançado) +
  view_follow(fixed_y = TRUE)

##### Criar animação
animate(graph2.animation, height = 500, width = 800, fps = 30, duration = 10,
        end_pause = 60, res = 100)
#anim_save("11.aniamtion_acuml.pontos.gif")

## Seleção de determinado tipo em colunas
#[site](https://www-r--bloggers-com.cdn.ampproject.org/v/s/www.r-bloggers.com/2021/05/ggplot-the-placing-and-order-of-aesthetics-matters/amp/?amp_gsa=1&amp_js_v=a6&usqp=mq331AQFKAGwASA%3D#amp_tf=De%20%251%24s&aoh=16211563531349&csi=0&referrer=https%3A%2F%2Fwww.google.com&ampshare=https%3A%2F%2Fwww.r-bloggers.com%2F2021%2F05%2Fggplot-the-placing-and-order-of-aesthetics-matters%2F)

### Tipo
#### Pacotes
pacman::p_load(ggplot2)
theme_set(theme_bw())
pacman::p_load(reshape2)

#### Tranforma planilha
p2 <- reshape2::dcast(planilhatotal, Classificação + Tipo + Gravação ~ Classificação, value.var = "Pontos", fun.aggregate = sum)
Pontos <- apply(p2[, 4:5], 1, sum)
p2 <- data.frame (p2, Pontos)

#### Gráfico
ggplot(p2, aes(Gravação, Pontos, fill = Classificação)) + 
  geom_col(position = "fill", width = 0.85, 
    color = "black", size = 1,
    mapping = aes(linetype = Tipo == "Álbum")) 
#ggsave("12.Barra_tipo_album.png",width = 14, height = 6, dpi = 300)

### País
#### Brasil
#### Tranforma planilha
p2 <- reshape2::dcast(planilhatotal, Continente + País + Raiz ~ Classificação, value.var = "Pontos", fun.aggregate = sum)
Pontos <- apply(p2[, 4:5], 1, sum)
p2 <- data.frame (p2, Pontos)

#### Gráfico
ggplot(p2, aes(Raiz, Pontos, fill = Continente)) + 
  geom_col(position = "fill", width = 0.85, 
    color = "black", size = 1,
    mapping = aes(linetype = País == "Brasil")) 
#ggsave("12.Barra_pais_Br.png",width = 14, height = 6, dpi = 300)


# Criar Tabela de dados

### Pacotes
pacman::p_load(tidyverse, tidyverse.quiet, dplyr)
options(tidyverse.quiet = TRUE)
library(tidyverse)
options(dplyr.summarise.inform = FALSE)

#### Continete por Classificação em diferença de média de ano de lançamento
p2 <- planilhatotal
p2 %>% 
  mutate(
   mean_height = mean(Lançado, na.rm = TRUE)
  ) %>% 
  group_by(Continente, Classificação) %>% 
  summarize(
    mean_height_species_gender = mean(Lançado, na.rm = TRUE),
    mean_height = first(mean_height)
  ) %>% 
  mutate(
    diff_mean_height = mean_height_species_gender - mean_height
  ) %>% 
  dplyr::select(Classificação, Continente, diff_mean_height) %>%
  pivot_wider(names_from = 'Classificação', values_from = 'diff_mean_height', values_fill = NA)   
##### identity() para separar os termos

#### Continente por Raiz em diferença de média de ano de Pontos
p2 <- planilhatotal
p2 <- p2 %>% 
  mutate(
   mean_height = mean(Pontos, na.rm = TRUE)
  ) %>% 
  group_by(Continente, Raiz) %>% 
  summarize(
    mean_height_species_gender = mean(Pontos, na.rm = TRUE),
    mean_height = first(mean_height)
  ) %>% 
  mutate(
    diff_mean_height = mean_height_species_gender - mean_height
  ) %>% 
  dplyr::select(Raiz, Continente, diff_mean_height) %>%
  pivot_wider(names_from = 'Raiz', values_from = 'diff_mean_height', values_fill = NA)   
p2

plot(p2)



