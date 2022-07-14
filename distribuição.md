# Apresentação
Repositório para testes de scripts em uma tabela pessoal de álbuns. O intuito são gráficos de distribuição das variáveis. Será dividido da seguinte forma:
- Início;
- Gráficos investigatórios;
- PCA;
- Distribuição de série temporal;
- Gráficos laterais;
- GIFs;
- Gráfico de barars.

## Início
Primeiro, vamos indicar as pastas corretas.
```
getwd()
setwd("/home/user/Área de Trabalho/Música") 
```

Agora baixar e ler alguns pacotes básicos.

```
if(!require(pacman, quietly = TRUE))(install.packages("pacman")) #agrupador de funções
pacman::p_load(magrittr,dplyr,reshape2) #magrittr para operações de pipe/dplyr para manipulador de dados
pacman::p_load(ggplot2, devtools, ggrepel, graphics,paletteer)
pacman::p_load(vegan)  #vegan para estatística ecológica/graphics para os gráficos
```
Agora vamos adicionar a planilha.
```
pacman::p_load(openxlsx) 
caminho.do.arquivo <- "/home/user/Área de Trabalho/musica.xlsx"
planilhatotal <- read.xlsx(caminho.do.arquivo, #local do arquivo
                           sheet = 1, # em qual planilha estão os dados
                           colNames = T, # as colunas dos dados possuem nomes?
                           na.strings = "NA") # como estão identificados os dados omissos?

head(planilhatotal)
```
E filtrar ela.
```
planilhatotal <- subset(planilhatotal, !is.na(Lançado)) #tirar n/a da ano
planilhatotal <- subset(planilhatotal, !is.na(Pontos)) #tirar n/a da pontos
```
Agora testaremos antes de começar as análises com alguns gráficos simples
```
ts.plot(planilhatotal$Lançado)

boxplot(planilhatotal$Pontos)
```

## Gráficos investigatórios.
Fazer gráficos pode ser uma ótima forma de entender o conjunto de dados que temos em mãos. Por exemplo, quais são os gêneros mais comuns que eu registrei? Vamos ver um boxplot deles.

```
p2 <- planilhatotal
p2 <- subset(p2,Classificação!="Extra")
p2 <- subset(p2,Classificação!="Outros") 
p2 <- subset(p2,Coletivo!="Vários")

ggplot(p2, aes(x = Pontos, y = Gênero)) + 
  geom_boxplot(aes(colour= Raiz)) +
  #geom_violin(aes(colour= Raiz)) +
  #facet_grid(Gravação~.) +
  theme_classic() 
#ggsave("1.Box_Gen_Pontos.png",width = 10, height = 6, dpi = 300)
```
Notamos que a distribuição dos pontos são similares, mas a média pode variar um pouco, tendo o rock com a média mas alta. Mas é R&B o outline de maior destaque. Se consideramor o período de lançamento, a média de gẽnero mais recente é o pop, apesar de oitline bem antigos. Já o jazz apresenta a média de anos mais antiga.

Há outra forma de ver isso, agora vamos focar em ano de lançamento e vamos ver para os gêneros o período de maior concentração de álbum. Primeiro o pacote.
`library(ggbeeswarm)`

Agora o gráfico.
```
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
```
Fica mais claro a constância de albuns de rock a partir de meados dos anos 60 e seu surgimento nos anos 50. Também percebemos melhor a concetração de álbuns de pop a partir de 200 e de jazz entre os anos 50 e 60. Se fizermos por continente perebe-se a predominância da América e Europa. 

Agora, vamos comparar média de dois grupos, seguindo o seguinte [site](https://www-r--bloggers-com.cdn.ampproject.org/v/s/www.r-bloggers.com/2021/05/showing-a-difference-in-mean-between-two-groups-take-2/amp/?amp_gsa=1&amp_js_v=a6&usqp=mq331AQFKAGwASA%3D#amp_tf=De%20%251%24s&aoh=16206389890747&csi=0&referrer=https%3A%2F%2Fwww.google.com&ampshare=https%3A%2F%2Fwww.r-bloggers.com%2F2021%2F05%2Fshowing-a-difference-in-mean-between-two-groups-take-2%2F)

Primeiro os pacotes.
```
pacman::p_load(dabestr, ggbeeswarm)
library(dabestr)
```
Agora vamos ver se há diferença entre álbuns de estúdio e lives.
```
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
```
Com esse gráfico fica evidente que existem bem mais álbuns de estúdio do que live e que na média, os albuns de estúdio foram lançado em um ano posterior a live. Se trocarmos o ano de lançamento pelos pontos, as médias agra são mais próximas. Esperado quando os dados apresentam uma distribuição perto da curva normla.

## PCA 
O PCA ou Análise de Componentes Principais ou PCA (Principal Component Analysis) é uma técnica de análise multivariada que pode ser usada para analisar inter-relações entre um grande número de variáveis e explicar essas variáveis em termos de suas dimensões inerentes (Componentes). O objetivo é encontrar um meio de condensar a informação contida em várias variáveis originais em um conjunto menor de variáveis estatísticas (componentes) com uma perda mínima de informação.Os scripts seguirão o seguinte [site](https://www-r--bloggers-com.cdn.ampproject.org/v/s/www.r-bloggers.com/2021/05/principal-component-analysis-pca-in-r/amp/?amp_gsa=1&amp_js_v=a6&usqp=mq331AQFKAGwASA%3D#amp_tf=De%20%251%24s&aoh=16204247084008&csi=0&referrer=https%3A%2F%2Fwww.google.com&ampshare=https%3A%2F%2Fwww.r-bloggers.com%2F2021%2F05%2Fprincipal-component-analysis-pca-in-r%2F)

Primeiro os pacotes:
```
pacman::p_load(psych)
library("devtools")
library("ggbiplot")
#install_github("vqv/ggbiplot")
```

Agora vamos ver como os países explicam os gêneros. Quais países apresentam o maior poder de explicação para os gêneros catalogados. Primeiro vamos separar as tabelas.

```
p2 <- planilhatotal
p2 <- subset(p2, !is.na(Gênero))
p2 <- subset(p2,Classificação!="Extra") 
p2 <- subset(p2,Coletivo!="Vários")
p2 <- subset(p2, !is.na(Continente))

local<-reshape2::dcast(p2, Subcontinente ~ Gênero, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)
grupo<-reshape2::dcast(p2, Subcontinente + Continente ~ Gênero, value.var = "Pontos", fun.aggregate = sum)
grupo2<-reshape2::dcast(p2, Subcontinente ~ Gênero, value.var = "Pontos", fun.aggregate = sum)
```
Agora vamos verm em gráfico:
```
wine.pca <- prcomp(local, scale. = TRUE)
ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         groups = grupo$Continente, 
         ellipse = TRUE, circle = TRUE) +
  geom_label_repel(aes(label = grupo2$Subcontinente), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50',
                   max.overlaps = 10) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top') +
  theme_classic()
#ggsave("5.PCA_pais_gen.png",width = 15, height = 8, dpi = 600)
```
Com cerca de 50% o rock explicaria quase todos os subcontinentes, exceto para o caribe. E a diferença entre eles seria pela proporção d e Rock e Pop, com 15%, onde quanto maios presença de música pop melhor expliacria regiões da Europa e a ausência do pop expliacria melhor as Américas e Ilhas Britânicas. Fica claro que a maior porporção de rock e de pop são a chave para entender o gráfico.

Um gráfico resumido e o sumário podem ser visto também. 
```
summary(prcomp(local, scale = TRUE))
biplot(prcomp(local, scale = TRUE))
```

## Distribuição de série temporal
Outra forma de enteder nossos dados é por uma série temporal. Vamos ver quais gêneros tipos de registros foram mais comuns com o passar dos anos. Primeiro o pacote.
`pacman::p_load(ggridges, forcat)`
Agora o gráfico.
```
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
```
Nesse gráfico fica evidente novamente como o jazz e um gênero com mais álbuns dos anos 50 e 60 e o pop mais contemporâneo. O mesmo dá para fazer com o tipo de registro por pontos, onde Single são grandes fornecedires de pontos e outras versões são fracos agregadores.

## Gráficos laterais
Gráficos laterias são excelente para ver quais componentes são mais imprtantes de determinados cenários. Os scripts seguirão o seguinte site [site](https://www-r--bloggers-com.cdn.ampproject.org/v/s/www.r-bloggers.com/2021/05/visualization-graphs-ggside-with-ggplot/amp/?amp_gsa=1&amp_js_v=a6&usqp=mq331AQFKAGwASA%3D#amp_tf=De%20%251%24s&aoh=16208562938944&csi=0&referrer=https%3A%2F%2Fwww.google.com&ampshare=https%3A%2F%2Fwww.r-bloggers.com%2F2021%2F05%2Fvisualization-graphs-ggside-with-ggplot%2F)

Primeiro o pacote.
`pacman::p_load(ggside, tidyverse,tidyquant)`

### Escala 1
Agora vamos ver quais continentes são mais presentes em uma escala temporal(eixo x) e na média de pontos (eixo y).
```
p2 <- planilhatotal
p2 <- subset(p2,Classificação!="Extra")
p2 <- subset(p2,Classificação!="Outros") 
p2 <- subset(p2,Coletivo!="Vários")

p3 <-p2 %>%
  ggplot(aes(Lançado, Pontos, color = Continente)) +
  geom_point(size = 2, alpha = 0.3) +
  geom_smooth(aes(color = NULL), se=TRUE) +
  geom_xsidedensity(aes(y = after_stat(count),
    fill = Continente),alpha = 0.5,size = 1, position = "stack") +
  geom_ysidedensity(aes(x = after_stat(count), #density para densidade
      fill = Continente),alpha = 0.5, size = 1, position = "stack") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  labs(title = "Distribuição dos pontos por gênero", subtitle = "Densidade",
       x = "Ano", y = "Pontos") +  theme(ggside.panel.scale.x = 0.4, ggside.panel.scale.y = 0.4)
plot(p3)
#ggsave("9.Distr_Ponto_ano_Cont.png",width = 15, height = 8, dpi = 600)
#p3 + ggside(x.pos = "bottom", y.pos = "left") + labs(title = "Distribuição dos pontos dos continentes por ano", subtitle = " ")
```
Fica claro que a América é a região com maios proporção, seguido da Europa. O mesmo pode ser feito com a raiz de gênero musical que mostra que a música anglo-americana é a mais comum, seguida por música latina nos registros mais antigos e música afro-americana mais recentemente.Ou com tipo, quase todo por álbuns.

Agora, vamos ver como ver os subtipos, como de subcontinente, por exemplo.
```
p3 + facet_wrap(Subcontinente~.) + #facet_wrap(Continente~Subcontinente)
  labs(title = "Pontos para raiz musical", subtitle = "Distribuição por gêneros") +
  ggside(collapse = "x")
#ggsave("9.Distr_Ponto_ano_Subcont.png",width = 18, height = 8, dpi = 600)
```
Assim, vê que a. Anglo-saxônica, Latina e Ilhas Britânicas são os grandes contribuidores destes pontos. O mesmo pode ser visto em raiz e gênero musical tendo rock como grande força. 

Dados cruzados também podem ser facilitados em compreensão com esse tipo de gráfico. Como continente e raiz musical. Vamos filtrar nossa tabela antes.
```
p2 <- planilhatotal
p2 <- subset(p2,Classificação!="Extra")
p2 <- subset(p2,Classificação!="Outros") 
p2 <- subset(p2,Coletivo!="Vários")
```
E vamos plotar:
```
p2 <- p2 %>%
  ggplot(aes(Lançado, Pontos, color = Raiz)) +
  geom_point(size = 2, alpha = 0.3) +
  geom_smooth(aes(color = NULL), se=TRUE) +
  geom_xsidedensity(aes(y = after_stat(count),
      fill = Raiz), alpha = 0.5,size = 1, position = "stack") +
  geom_ysidedensity(aes(x = after_stat(count),
      fill = Raiz), alpha = 0.5, size = 1,position = "stack") +
  #scale_color_tq() +
  #scale_fill_tq() +
  #theme_tq() +
  labs(title = "Distribuição dos pontos por gênero" , subtitle = "Densidade",
       x = "Ano", y = "Pontos") +  theme(ggside.panel.scale.x = 0.4, ggside.panel.scale.y = 0.4)

p2 + facet_grid(Continente~Raiz, space = "free", scales = "free") +
  labs(title = "Relação continente raiz musical", subtitle = "") +
  ggside(collapse = "all")
#ggsave("9.Distr_Ponto_ano_contin_raiz.png",width = 18, height = 8, dpi = 600)
```
Assim vemos que na América músicas de raizes americanas são maioria e que na europa a música anglo-americana e europeia predomina. O mesmo dá para se fazer cruzando subcontinentes, tipos de registros e outros.

Outra forma de ver nossos dados são temporalmente vendo quas raízes musicais são distribuídos por continente.
```
p2 <- planilhatotal
p2 <- subset(p2,Classificação!="Extra")
p2 <- subset(p2,Classificação!="Outros") 
p2 <- subset(p2,Coletivo!="Vários")

p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Raiz)) +
  geom_point() +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(
    alpha    = 0.5,
    size     = 1  ) +
  #scale_color_tq() +
  #scale_fill_tq() +
  #theme_tq() +
  facet_grid(rows = vars(Continente), scales = "free_x") + 
  labs(title = "Distibuição dos pontos por raíz musical e continente") +  theme(ggside.panel.scale.x = 0.4, ggside.panel.scale.y = 0.4)
#ggsave("9.Distr_Ponto_ano_raiz_contin_asax.png",width = 15, height = 8, dpi = 600)
```
Com esse gráfico fica evidente tanto os períodos musicais quanto as médias mais altas por continente. 

### Escala 2
Outra forma de ver o dado e diminuindo a escala e vendo cada região. As principais serão listadas abaixo:
- A. Anglo Saxônica;
- A. Latina;
- Caribe;
- Ilhas Britânicas.
- E. Ocidental;
- E. Setentrional;
- E. Meridional;
- E. Centro-Oriental;
- Oceania;
- África;
- Ásia.

Os scripts a seguir são filtros. deve-se subsituir o termos e prestar atenção nas legendas.
```
p2 <- planilhatotal

p2 <- subset(p2, Classificação == "Principal")
p2 <- subset(p2, Classificação!="Extra") 
p2 <- subset(p2, Coletivo!="Vários") 

p2 <- subset(planilhatotal, Subcontinente == "E. Ocidental")
p2 <- subset(planilhatotal, Continente == "América")

p2 <- subset(p2,País!="Brasil")
p2 <- subset(p2,Subcontinente!="Ilhas Britânicas") 
```
Agora o gráfico seprando os países por gênero musical ao longo do tempo. O gráfico lateral superior mostra a proporção de gênero e laterial a direita um boxplot dos pontos:
```
p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point(aes(size = Soma), alpha = 0.7) +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(alpha = 0.5,size = 1) +
  geom_xsidedensity(aes(y = after_stat(count),
      color = Gênero),alpha = 0.5,size = 1, position = "stack") +
  #theme_tq() +
  #facet_grid(rows = vars(País), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_gen_subcontin_AméricaSax.png",width = 15, height = 8, dpi = 600)
```
E um gráfico mostrando os países por subcontinente.
```
p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = País)) +
  geom_point(aes(size = Soma), alpha = 0.7) +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(alpha = 0.5,size = 1  ) +
  #theme_tq() +
  geom_xsidedensity(aes(y = after_stat(count),
      color = País),alpha = 0.5,size = 1, position = "stack") +
  #facet_grid(rows = vars(Subcontinente), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_pais_subcontin_Américasax.png",width = 15, height = 8, dpi = 600)
```

### Escala 3
Também podemos ver em uma escala de país. Os principais serão listadas abaixo:
- Brasil;
- EUA;
- Inglaterra;
- Canadá.
- Austrália.

Os scripts a seguir são filtros. deve-se subsituir o termos e prestar atenção nas legendas.
```
p2 <- planilhatotal

p2 <- subset(p2, Classificação == "Principal")
p2 <- subset(p2, Classificação!="Extra") 
p2 <- subset(p2, Coletivo!="Vários") 

p2 <- subset(p2, País == "Brasil")

```
Primeiro, vamos ver os gêneros:
```
p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point(aes(size = Soma), alpha = 0.7) +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(alpha = 0.5,size = 1) +
  geom_xsidedensity(aes(y = after_stat(count),
      color = Gênero),alpha = 0.5,size = 1, position = "stack") +
  theme_tq() +
  #facet_grid(rows = vars(Raiz), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_gen_Br.png",width = 15, height = 8, dpi = 600)
```
Agora vamos ver a contribuição dos estados por região ou estado (como Austrália).
```
p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Estado)) +
  geom_point(aes(size = Soma), alpha = 0.7) +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(alpha = 0.5,size = 1) +
  theme_tq() +
  geom_xsidedensity(aes(y = after_stat(count),
      color = Estado),alpha = 0.5,size = 1, position = "stack") +
  #facet_grid(rows = vars(Região), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")
#ggsave("9.Distr_Ponto_ano_gen_Br.png",width = 15, height = 8, dpi = 600)
```

### Escala 4
Também podemos ver em uma escala de Gênero. Os principais serão listadas abaixo:
- Raíz;
- Gênero;
- Categoria;
- Subgênero.

Os scripts a seguir são filtros. deve-se subsituir o termos e prestar atenção nas legendas.
```
p2 <- planilhatotal

p2 <- subset(p2, Classificação == "Principal")
p2 <- subset(p2, Classificação!="Extra") 
p2 <- subset(p2, Coletivo!="Vários") 

p2 <- subset(p2, Gênero == "Pop Music")
```
Primeiro, vamos ver os gêneros:
```
p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = País)) +
  geom_point(aes(size = Soma), alpha = 0.7) +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(alpha = 0.5,size = 1) +
  geom_xsidedensity(aes(y = after_stat(count),
      color = País),alpha = 0.5,size = 1, position = "stack") +
  theme_tq() +
  #facet_grid(rows = vars(País), scales = "free_x") +
  labs(title = "Distibuição dos pontos por gênero e países")
#ggsave("9.Distr_Ponto_ano_gen_Br.png",width = 15, height = 8, dpi = 600)
```
Agora vamos ver a contribuição dos estados por região ou estado (como Austrália).
```
p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Categoria)) +
  geom_point(aes(size = Soma), alpha = 0.7) +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(alpha = 0.5,size = 1  ) +
  theme_tq() +
  geom_xsidedensity(aes(y = after_stat(count),
      color = Categoria),alpha = 0.5,size = 1, position = "stack") +
  #facet_grid(rows = vars(Categoria), scales = "free_x") +
  labs(title = "Distibuição dos pontos por gênero e categoria")
#ggsave("9.Distr_Ponto_ano_gen_Br.png",width = 15, height = 8, dpi = 600)
```
## GIF
Outra forma de ver seus dados e gerando GIFs, gráficos animados. Vamos seguir o seguinte [site](https://www-r--bloggers-com.cdn.ampproject.org/v/s/www.r-bloggers.com/2021/05/animated-graph-gif-with-gganimate-ggplot/amp/?amp_gsa=1&amp_js_v=a6&usqp=mq331AQFKAGwASA%3D#amp_tf=De%20%251%24s&aoh=16210958981586&csi=0&referrer=https%3A%2F%2Fwww.google.com&ampshare=https%3A%2F%2Fwww.r-bloggers.com%2F2021%2F05%2Fanimated-graph-gif-with-gganimate-ggplot%2F)

Primeiro os pacotes:
```
pacman::p_load(gganimat,ggplot2,dplyr,gapminder,ggthemes,gifski,readr,tidyr,cargo, rustc)
devtools::install_github('thomasp85/gganimate', force = TRUE)
### Baxixar cargo antes
devtools::install_github("r-rust/gifski", force = TRUE)
#install.packages("gifski", type = "source")
```
### Gráfico de pontos
Vamos selecionar apenas álbuns principais:
`p2 <- subset(planilhatotal, Classificação == "Principal")`

Dois gráficos podem ser gerados, vamos tentar o primeiro uma animação, mas primeiro o gráfico onde queremos ver a fistribuição de pontos por álbuns dos continentes ao longo dos anos. 
```
graph1 = p2 %>%
  ggplot(aes(x=Lançado, y=Pontos, color=Continente, size=Soma)) +
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
```
Agora vamos adicionar os frames por ano e lançamento:
```
graph1.animation = graph1 +
  transition_time(Faixa) +
  labs(subtitle = "Faixa: {frame_time}") +
  shadow_wake(wake_length = 0.1) 
```
Agora a animação:
```
animate(graph1.animation, height = 500, width = 800, fps = 30, duration = 20,
        end_pause = 60, res = 100, renderer = gifski_renderer()) 
#anim_save("11.aniamtion_faixa.gif") # ou criar em site como https://gifmaker.me/
#anim_save("out.gif",animation = graph1.animation)
```
Com essa animação fica evidente que a maioria dos álbuns  melhores avaliados estão entre os anos 70 e 80, mas que o mais bem avalaido está no começo da década de 70 e que é americano, além da abundância de álbuns americanos.

### Gráfico de pontos
Agora um gráfico de linhas. Primeiro, precisamos transformar a planilha. 
```
game_sales = p2 %>%
  mutate(Lançado = as.numeric(Lançado)) %>%
#filter(Raiz == 'Latin Music', Gênero %in% c("Samba", "MPB")) %>% 
#drop_na() %>%
  group_by(Lançado, Continente) %>%
  summarise(Pontos = sum(Pontos), n = n(), .groups = 'drop')
```
Depois gerar vamos gerar o gráficos:
```
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
```
Adicionar a esvala temporal:
```
graph2.animation<-graph2 +
  transition_reveal(Lançado) +
  view_follow(fixed_y = TRUE)
```
E fazer a animação:
```
animate(graph2.animation, height = 500, width = 800, fps = 30, duration = 10,
        end_pause = 60, res = 100)
#anim_save("11.aniamtion_acuml.pontos.gif")
```
Nesse gráfico fica bem evidente que a américa é a região com mais pontos acumulados.

## Gráfico de barras
Gráfico de barras podem ser simplificadores. Vamos fazer um onde é possível selecionar alguma informação, principalemense que quisermos pensar sobre proporção. Vamos seguir o [site](https://www-r--bloggers-com.cdn.ampproject.org/v/s/www.r-bloggers.com/2021/05/ggplot-the-placing-and-order-of-aesthetics-matters/amp/?amp_gsa=1&amp_js_v=a6&usqp=mq331AQFKAGwASA%3D#amp_tf=De%20%251%24s&aoh=16211563531349&csi=0&referrer=https%3A%2F%2Fwww.google.com&ampshare=https%3A%2F%2Fwww.r-bloggers.com%2F2021%2F05%2Fggplot-the-placing-and-order-of-aesthetics-matters%2F)

Primeiro os pacotes:
```
pacman::p_load(ggplot2)
theme_set(theme_bw())
pacman::p_load(reshape2)
```
Agora vamos transformar nossa planilha para selecionar o tipo de disco e evidenciar o que for álbum.
```
p2 <- reshape2::dcast(planilhatotal, Classificação + Tipo + Gravação ~ Classificação, value.var = "Pontos", fun.aggregate = sum)
Pontos <- apply(p2[, 4:5], 1, sum)
p2 <- data.frame (p2, Pontos)
```
E vamos plotar em gráfico:
```
ggplot(p2, aes(Gravação, Pontos, fill = Classificação)) + 
  geom_col(position = "fill", width = 0.85, 
    color = "black", size = 1,
    mapping = aes(linetype = Tipo == "Álbum")) 
#ggsave("12.Barra_tipo_album.png",width = 14, height = 6, dpi = 300)
```
Fica bem fácil ver que em a proporção de álbuns da categria principal é maior para álbuns de estúdio, enquanto para lives a categiria princioal é extras. E que álbum é o tipo de disco mais comum quando olha-se disco de estúdios. Se trocarmos para Show, percena que é a forma principal para Live. 

O mesmo pode ser feito para evidenciar algum país ou gênero dentro de um todo. Vamos selecionar uma planilha com continente, país e raiz de gênero musical.
```
p2 <- reshape2::dcast(planilhatotal, Continente + País + Raiz ~ Classificação, value.var = "Pontos", fun.aggregate = sum)
Pontos <- apply(p2[, 4:5], 1, sum)
p2 <- data.frame (p2, Pontos)
```
E vamos plotar em gráfico e pedir para evidenciar o Brasil:
```
ggplot(p2, aes(Raiz, Pontos, fill = Continente)) + 
  geom_col(position = "fill", width = 0.85, 
    color = "black", size = 1,
    mapping = aes(linetype = País == "Brasil")) 
#ggsave("12.Barra_pais_Br.png",width = 14, height = 6, dpi = 300)
```
Com esse gráfico fica evidente que na música latina o Brasil é o maior contribuidor e que este ritmo é muito associado ao continente americano, por exemplo.

