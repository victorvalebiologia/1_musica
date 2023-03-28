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
pacman::p_load(ggplot2, ggrepel, graphics,lubridate, gghighlight) #devtools, 
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

#head(planilhatotal)
```
E filtrar ela.
```
planilhatotal <- subset(planilhatotal, !is.na(Lançado)) #tirar n/a da ano
planilhatotal <- subset(planilhatotal, !is.na(Pontos)) #tirar n/a da pontos
p2 <- planilhatotal
p2 <- subset(p2, !is.na(Lançado))
p2[is.na(p2)] <- 6

p3 <- subset(p2, !is.na(Mês))
p3 <- subset(p3, !is.na(Dia))

Data <- p3 %>% 
  select(Lançado,Mês,Dia) %>% 
  mutate(Data = make_date(Lançado,Mês,Dia))
Data <- data.frame(p3,Data)

```

## Gráfico exploratório artistas

Primeiro uma visão do artista ao longo do tempo.
``` 
pacman::p_load(ggside, stringr) #, tidyverse,tidyquant)

p2 <- Data
p3 <- p2 %>% filter(str_detect(Artista.Tag, "Caetano Veloso"))
p4 <- p2 %>% filter(str_detect(Artista.Tag, "Iron Maiden")) 
p3 <- rbind(p3,p4)
#p3 <- subset(p3, Artista == "Nick Drake") #escolher artista
#p3 <- subset(p3, Artista!="Princess Chelsea") #retirar artista
p3<-unique(p3)

p4 <- p3
p4 <- subset(p4,Classificação!="Extra") 
p4 <- subset(p4,Classificação!="Outros") 

p4 <- p4 %>%  subset(Nota > 0.55)

ggplot(p4, aes(x = Data, y = Pontos)) + 
  geom_point(aes(colour = Artista_principal, size = Tocado, shape = Tipo), alpha = 0.6) + 
  geom_smooth(method = lm,se = FALSE, alpha = 0.6, aes(colour = Artista_principal)) +  #method = lm ou loess
  scale_shape_manual(values = 0:10) +
  geom_line(aes(colour = Artista_principal), linetype = 2, linejoin = "mitre", lineend = "butt", alpha = 0.3) +
  geom_hline(aes(yintercept = mean(Pontos)), linetype = "dashed", alpha = 0.4) + 
  #facet_grid(.~Década, scales = "free_x", space = "free_x") + #
  scale_size(range = c(5, 18), name = "Número de audições") +
  geom_label_repel(aes(label = Álbum, colour = Artista_principal), size=2.5, alpha= 1,box.padding = 0.35,point.padding = 0.75,segment.color = 'grey50') +
  labs(title=" ", subtitle="",y="Pontos",x="Ano de lançamento", caption="", shape = "Tipo de álbum", colour = "Artista_principal", size = "Número de audições") +
  geom_xsideboxplot(aes(fill = Artista_principal),alpha = 0.5) +
  geom_ysideboxplot(aes(fill = Artista_principal),alpha = 0.5) + #
  stat_ellipse(geom="polygon", aes(fill = Artista_principal), alpha = 0.2, show.legend = TRUE,level = 0.1) +        
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()

``` 
Detalhando por tipo de lançamento.

``` 
p4 <- p3
#p4 <- subset(p4, Gravação == "Live") #escolher artista
#p3 <- subset(p3, Artista!="Princess Chelsea") #retirar artista
#p4 <- subset(p4,Classificação!="Extra") 
#p4 <- subset(p4,Classificação!="Outros") 


#p4 <- p4 %>%  subset(Nota > 0.6)

ggplot(p4, aes(x = Data, y = Total)) + 
  geom_point(aes(colour = Tipo, size = Tocado, shape = Artista_principal)) + 
  scale_shape_manual(values = 0:10) +
  geom_smooth(method = lm,se = FALSE, alpha = 0.6, aes(colour = Tipo)) + 
  geom_line(aes(colour = Tipo), linetype = 2, linejoin = "mitre", lineend = "butt", alpha = 0.3) +
  geom_hline(aes(yintercept = mean(Total)), linetype = "dashed", alpha = 0.4) + 
  #facet_grid(Classificação~Gravação) +
  scale_size(range = c(5, 18), name = "Número de audições") +
  geom_label_repel(aes(label = Álbum, colour = Tipo), size=2.5, alpha= 1,box.padding = 0.35,point.padding = 0.75,segment.color = 'grey50') +
  labs(title="", subtitle="",y="Total",x="Ano de lançamento", caption="",size = "Número de audições", shape = "Artista", colour = "Tipo de Álbum", fill = "Tipo de Álbum") +
  geom_ysideboxplot(aes(fill = Tipo), alpha = 0.5,size = 0.5) +
  geom_xsideboxplot(aes(fill = Tipo), alpha = 0.5,size = 0.5) + 
  #geom_xsidedensity(aes(fill = Tipo, y = after_stat(count)), position = "stack") +
  stat_ellipse(geom="polygon", aes(fill = Tipo), alpha = 0.2, show.legend = TRUE, level = 0.1) +        
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14)) +
  theme_classic()
  
#ggsave("ano.png",width = 15, height = 8, dpi = 600)

``` 
Agora, considerando a localização ou o gênero relacionado ao artista com outros do mesmo tipo ao longo do tempo.

``` 
pacman::p_load(ggside, stringr) #, tidyverse,tidyquant)

p2 <- Data
p3 <- p2 %>% filter(str_detect(Estado, "Paraná"))
#p3 <- p2 %>% filter(str_detect(Gênero, "MPB")) 

#p3 <- p3 %>% filter(str_detect(Gênero, "MPB")) 
#p3 <- rbind(p3,p4)
#p3 <- p3 %>% filter(str_detect(Categoria, "Alternative Rock")) #escolher artista
#p3 <- subset(p3, Região!="Mid-Atlantic") #retirar artista
p3<-unique(p3)

p4 <- p3
p4 <- subset(p4,Classificação!="Extra") 
p4 <- subset(p4,Classificação!="Outros") 

p4 <- p4 %>%  subset(Nota > 0.6)
#p4 <- p4 %>%  subset(Tocado > 3)

ggplot(p4, aes(x = Data, y = Pontos)) + 
  geom_point(aes(colour = Artista_principal, size = Tocado, shape = Tipo), alpha = 0.6) + 
  geom_smooth(method = lm,se = FALSE, alpha = 0.6, aes(colour = Artista_principal)) +  #method = lm ou loess
  scale_shape_manual(values = 0:10) +
  geom_line(aes(colour = Artista_principal), linetype = 2, linejoin = "mitre", lineend = "butt", alpha = 0.3) +
  geom_hline(aes(yintercept = mean(Pontos)), linetype = "dashed", alpha = 0.4) + 
  #facet_grid(.~Década, scales = "free_x", space = "free_x") + #
  scale_size(range = c(5, 18), name = "Número de audições") +
  geom_label_repel(aes(label = Álbum, colour = Artista_principal), size=2.5, alpha= 1,box.padding = 0.35,point.padding = 0.75,segment.color = 'grey50') +
  labs(title=" ", subtitle="",y="Pontos",x="Ano de lançamento", caption="", shape = "Tipo de álbum", colour = "Artista_principal", size = "Número de audições") +
  geom_xsideboxplot(aes(fill = Artista_principal),alpha = 0.5) +
  geom_ysideboxplot(aes(fill = Artista_principal),alpha = 0.5) + #
  stat_ellipse(geom="polygon", aes(fill = Artista_principal), alpha = 0.2, show.legend = TRUE,level = 0.1) + 
  theme_classic() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), legend.position = "none") 

``` 
Cosiderando os subtipos de música por local relacionado ao artista com ou não recorte de tempo.

``` 
p2 <- Data
p3 <- p2 %>% filter(str_detect(Categoria, "Neo Soul")) 
#p3 <- subset(p3, Coletivo!="Vários") #retirar artista
#p3 <- subset(p3, Estado!="Não Sei") #retirar artista

#p3 <- p3 %>% filter(str_detect(País, "Estados Unidos")) 
#p3 <- rbind(p3,p4)
#p3 <- p3 %>% filter(str_detect(Década, "1960")) #escolher artista
#p3 <- subset(p3, Gênero!="MPB") #retirar artista
p3<-unique(p3)

p4 <- p3
p4 <- subset(p4,Classificação!="Extra") 
p4 <- subset(p4,Classificação!="Outros") 

p4 <- p4 %>%  subset(Nota > 0.6)
#p4 <- p4 %>%  subset(Ranking < 500)

ggplot(p4, aes(x = Data, y = Pontos)) + 
  geom_point(aes(colour = Subgênero, size = Tocado, shape = Tipo), alpha = 0.6) + 
  geom_smooth(method = lm,se = FALSE, alpha = 0.6, aes(colour = Subgênero)) +  #method = lm ou loess
  scale_shape_manual(values = 0:10) +
  geom_line(aes(colour = Subgênero), linetype = 2, linejoin = "mitre", lineend = "butt", alpha = 0.3) +
  geom_hline(aes(yintercept = mean(Pontos)), linetype = "dashed", alpha = 0.4) + 
  #facet_grid(Categoria~., scales = "free_x", space = "free_x") + #
  scale_size(range = c(5, 18), name = "Número de audições") +
  geom_label_repel(aes(label = Álbum, colour = Subgênero), size=2.5, alpha= 1,box.padding = 0.35,point.padding = 0.75,segment.color = 'grey50') +
  labs(title=" ", subtitle="",y="Pontos",x="Ano de lançamento", caption="", shape = "Tipo de álbum", colour = "Subgênero", fill = "Subgênero", size = "Número de audições") +
  geom_xsideboxplot(aes(fill = Subgênero),alpha = 0.5) +
  geom_ysideboxplot(aes(fill = Subgênero),alpha = 0.5) + #
  stat_ellipse(geom="polygon", aes(fill = Subgênero), alpha = 0.2, show.legend = TRUE,level = 0.1) + 
  theme_classic() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14)) #, legend.position = "none") 

``` 
Agora, uma visão mais global da média de gênero por lugar ou tempo.

``` 
p2 <- Data
p3 <- p2 %>% filter(str_detect(País, "Canadá")) 
#p3 <- subset(p3, Coletivo!="Vários") #retirar artista
#p3 <- subset(p3, Estado!="Não Sei") #retirar artista

#p3 <- p3 %>% filter(str_detect(Gênero, "Rock")) 
#p3 <- rbind(p3,p4)
#p3 <- p3 %>% filter(str_detect(Região, "Sul")) #escolher artista
#p3 <- subset(p3, Região!="Mid-Atlantic") #retirar artista
p3<-unique(p3)

p4 <- p3
#p4 <- subset(p4,Classificação!="Extra") 
#p4 <- subset(p4,Classificação!="Outros") 

#p4 <- p4 %>%  subset(Nota > 0.6)
#p4 <- p4 %>%  subset(Ranking < 500)

ggplot(p4, aes(x = Data, y = Categoria)) + 
  geom_point(aes(colour = Tipo, shape = Tipo, size = Tocado), alpha = 0.6, stroke = 2) + 
  geom_boxplot(alpha = 0.5) + 
  scale_shape_manual(values = 0:12) +
  #gghighlight(Artista == "EPMD", label_key = Álbum, unhighlighted_colour = "black") +  
  facet_grid(Gênero~., scales = "free_y", space = "free_y") + 
  scale_size(range = c(5, 18), name = "Número de audições") +
  labs(title=" ", subtitle="",y="Categoria",x="Data", caption="", colour = "Tipo", shape = "Tipo", size = "Número de audições") +
  #geom_xsideboxplot(aes(fill = Categoria),alpha = 0.5) +
  #geom_ysideboxplot(aes(fill = Categoria),alpha = 0.5) + 
  #stat_ellipse(geom="polygon", aes(fill = Categoria), alpha = 0.2, show.legend = TRUE,level = 0.1) + 
  theme_classic() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14)) #, legend.position = "none") 

``` 
E das regiões

``` 
p2 <- Data
p3 <- p2 %>% filter(str_detect(País, "França")) 
p3 <- subset(p3, Coletivo!="Vários") #retirar artista
#p3 <- subset(p3, Estado!="Não Sei") #retirar artista

#p3 <- p3 %>% filter(str_detect(Raiz, "Anglo")) 
#p3 <- rbind(p3,p4)
#p3 <- p3 %>% filter(str_detect(Região, "Pacif")) #escolher artista
#p3 <- subset(p3, Região!="Mid-Atlantic") #retirar artista
p3<-unique(p3)

p4 <- p3
#p4 <- subset(p4,Classificação!="Extra") 
#p4 <- subset(p4,Classificação!="Outros") 

#p4 <- p4 %>%  subset(Nota > 0.6)
#p4 <- p4 %>%  subset(Ranking < 500)

ggplot(p4, aes(x = Data, y = Estado)) + 
  geom_point(aes(colour = Gênero, shape = Gênero, size = Tocado), alpha = 0.6, stroke = 2) + 
  geom_boxplot(alpha = 0.5) + 
  scale_shape_manual(values = 0:15) +
  #gghighlight(Artista == "EPMD", label_key = Álbum, unhighlighted_colour = "black") +  
  facet_grid(Região~., scales = "free_y", space = "free_y") + 
  scale_size(range = c(5, 18), name = "Número de audições") +
  labs(title=" ", subtitle="",y="Estado",x="Data", caption="", colour = "Gênero", shape = "Gênero", size = "Número de audições") +
  #geom_xsideboxplot(aes(fill = Estado),alpha = 0.5) +
  #geom_ysideboxplot(aes(fill = Estado),alpha = 0.5) + 
  #stat_ellipse(geom="polygon", aes(fill = Estado), alpha = 0.2, show.legend = TRUE,level = 0.1) + 
  theme_classic() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14)) #, legend.position = "none") 

``` 
Uma visão mais macro com o ano de lançamento e os gêneros.

``` 
p2 <- Data
p3 <- p2 %>% filter(str_detect(Lançado, "2019")) 
#p3 <- p2 %>% filter(str_detect(Continente, "E. Meridional")) 

#p3 <- p3 %>% filter(str_detect(Gênero, "Rock")) 
#p3 <- rbind(p3,p4)
#p3 <- p3 %>% filter(str_detect(Estilo, "Funk")) #escolher artista
#p3 <- subset(p3, Categoria!="Washington DC") #retirar artista
p3<-unique(p3)

p4 <- p3
p4 <- subset(p4,Classificação!="Extra") 
p4 <- subset(p4,Classificação!="Outros") 

p4 <- p4 %>%  subset(Nota > 0.6)
#p4 <- p4 %>%  subset(Tocado > 3)

ggplot(p4, aes(x = Data, y = Pontos)) + 
  geom_point(aes(colour = Gênero, size = Tocado, shape = Tipo), alpha = 0.6) + 
  geom_smooth(method = lm,se = FALSE, alpha = 0.6, aes(colour = Gênero)) +  #method = lm ou loess
  scale_shape_manual(values = 0:10) +
  geom_line(aes(colour = Gênero), linetype = 2, linejoin = "mitre", lineend = "butt", alpha = 0.3) +
  geom_hline(aes(yintercept = mean(Pontos)), linetype = "dashed", alpha = 0.4) + 
  #facet_grid(.~Década, scales = "free_x", space = "free_x") + #
  scale_size(range = c(5, 18), name = "Número de audições") +
  geom_label_repel(aes(label = Álbum, colour = Gênero), size=2.5, alpha= 1,box.padding = 0.35,point.padding = 0.75,segment.color = 'grey50') +
  labs(title=" ", subtitle="",y="Pontos",x="Data", caption="", shape = "Tipo de álbum", colour = "Gênero", fill = "Gênero", size = "Número de audições") +
  geom_xsideboxplot(aes(fill = Gênero),alpha = 0.5) +
  geom_ysideboxplot(aes(fill = Gênero),alpha = 0.5) + #
  stat_ellipse(geom="polygon", aes(fill = Gênero), alpha = 0.2, show.legend = TRUE,level = 0.1) + 
  theme_classic() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14)) #, legend.position = "none") 
``` 
Ou ainda por país.

``` 
p2 <- Data
p3 <- p2 %>% filter(str_detect(Lançado, "2021")) 
#p3 <- p3 %>% filter(str_detect(Continente, "América")) 
#p3 <- subset(p3, Coletivo!="Vários") #retirar artista

#p3 <- p3 %>% filter(str_detect(País, "Rock")) 
#p3 <- rbind(p3,p4)
#p3 <- p3 %>% filter(str_detect(Estilo, "Funk")) #escolher artista
#p3 <- subset(p3, Categoria!="Washington DC") #retirar artista
#p3 <- subset(p3, Região!="1") #retirar artista
p3<-unique(p3)

p4 <- p3
p4 <- subset(p4,Classificação!="Extra") 
p4 <- subset(p4,Classificação!="Outros") 

p4 <- p4 %>%  subset(Nota > 0.6)
#p4 <- p4 %>%  subset(Tocado > 2)

ggplot(p4, aes(x = Data, y = Pontos)) + 
  geom_point(aes(colour = País, size = Tocado, shape = Tipo), alpha = 0.6) + 
  geom_smooth(method = lm,se = FALSE, alpha = 0.6, aes(colour = País)) +  #method = lm ou loess
  scale_shape_manual(values = 0:10) +
  geom_line(aes(colour = País), linetype = 2, linejoin = "mitre", lineend = "butt", alpha = 0.3) +
  geom_hline(aes(yintercept = mean(Pontos)), linetype = "dashed", alpha = 0.4) + 
  #facet_grid(.~Década, scales = "free_x", space = "free_x") + #
  scale_size(range = c(5, 18), name = "Número de audições") +
  geom_label_repel(aes(label = Álbum, colour = País), size=2.5, alpha= 1,box.padding = 0.35,point.padding = 0.75,segment.color = 'grey50') +
  labs(title=" ", subtitle="",y="Pontos",x="Data", caption="", shape = "Tipo de álbum", colour = "País", fill = "País", size = "Número de audições") +
  geom_xsideboxplot(aes(fill = País),alpha = 0.5) +
  geom_ysideboxplot(aes(fill = País),alpha = 0.5) + #
  stat_ellipse(geom="polygon", aes(fill = País), alpha = 0.2, show.legend = TRUE,level = 0.1) + 
  theme_classic() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14)) #, legend.position = "none") 

``` 
Voltando para ver o cenário local, agora com a média.

``` 
p2 <- Data
#p3 <- p2 %>% filter(str_detect(Continente, "África")) 
p3 <- p2 %>% filter(str_detect(Categoria, "Indie Rock"))
p3 <- subset(p3, Coletivo!="Vários") #retirar artista
#p3 <- subset(p3, Estado!="Não Sei") #retirar artista

#p3 <- p3 %>% filter(str_detect(Raiz, "African Music")) 
#p3 <- rbind(p3,p4)
#p3 <- p3 %>% filter(str_detect(País, "Sul")) #escolher artista
#p3 <- subset(p3, Região!="6") #retirar artista
p3<-unique(p3)

p4 <- p3
#p4 <- subset(p4,Classificação!="Extra") 
#p4 <- subset(p4,Classificação!="Outros") 

#p4 <- p4 %>%  subset(Nota > 0.6)
#p4 <- p4 %>%  subset(Ranking < 500)

ggplot(p4, aes(x = Data, y = País)) + 
  geom_point(aes(colour = Subgênero, shape = Subgênero, size = Tocado), alpha = 0.6, stroke = 2) + 
  geom_boxplot(alpha = 0.5) + 
  scale_shape_manual(values = 0:50) +
  #gghighlight(Artista == "The Beatles", label_key = Álbum, unhighlighted_colour = "black") +  
  facet_grid(Continente~., scales = "free_y", space = "free_y") + 
  scale_size(range = c(5, 18), name = "Número de audições") +
  labs(title=" ", subtitle="",y="País",x="Data", caption="", colour = "Subgênero", fill = "Subgênero", size = "Número de audições") +
  #geom_xsidedensity(aes(y = after_stat(count/max(count)*5), group = Subgênero, fill = Subgênero),alpha = 0.5, size = 1, position = "stack") +
  #stat_ellipse(geom="polygon", aes(fill = País), alpha = 0.2, show.legend = TRUE,level = 0.1) + 
  theme_classic() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14)) #, legend.position = "none") 
``` 
Ou gênero, ainda com a média.

``` 
p2 <- Data
#p3 <- p2
p3 <- p2 %>% filter(str_detect(Estado, "Bahia")) 
#p3 <- p2 %>% filter(str_detect(País, "Japão")) 

p3 <- p3 %>% filter(str_detect(Gênero, "MPB")) 
#p3 <- rbind(p3,p4)
#p3 <- p3 %>% filter(str_detect(Região, "Sul")) #escolher artista
#p3 <- subset(p3, Região!="6") #retirar artista
p3<-unique(p3)

p4 <- p3
#p4 <- subset(p4,Classificação!="Extra") 
#p4 <- subset(p4,Classificação!="Outros") 

#p4 <- p4 %>%  subset(Nota > 0.6)
#p4 <- p4 %>%  subset(Ranking < 500)

ggplot(p4, aes(x = Data, y = Subgênero)) + 
  geom_point(aes(colour = Língua, shape = Língua, size = Tocado), alpha = 0.6, stroke = 2) + 
  geom_boxplot(alpha = 0.2) + 
  scale_shape_manual(values = 0:50) +
  #gghighlight(Língua == "Italiano", label_key = Álbum, unhighlighted_colour = "black") +  
  facet_grid(Categoria~., scales = "free_y", space = "free_y") + 
  scale_size(range = c(5, 18), name = "Número de audições") +
  #geom_xsidedensity(aes(y = after_stat(count/max(count)*5), group = Língua, fill = Língua),alpha = 0.5, size = 1, position = "stack") +
  labs(title=" ", subtitle="",y="Subgênero",x="Data", caption="", colour = "Idioma", fill = "Idioma", shape = "Idioma", size = "Número de audições") +
  #stat_ellipse(geom="polygon", aes(fill = Subgênero), alpha = 0.2, show.legend = TRUE,level = 0.1) + 
  theme_classic() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14)) #, legend.position = "none") 
``` 
Ou artistas

``` 
p2 <- Data
#p3 <- p2
p3 <- p2 %>% filter(str_detect(Categoria, "Heavy Metal")) 
#p3 <- p2 %>% filter(str_detect(Tag, "10 Pop")) 

p3 <- p3 %>% filter(str_detect(Subcontinente, "Latin")) 
#p3 <- p3 %>% filter(str_detect(Sexo, "Feminino")) #escolhe
#p4 <- p3 %>% filter(str_detect(Sexo, "Ambos")) #escolhe
#p3 <- rbind(p3,p4)

p3<-unique(p3)
p3 <- subset(p3, Artista_principal!="6") #retirar artista

p4 <- p3
p4 <- subset(p4,Artista_principal!="Parceria") 
#p4 <- subset(p4,Classificação!="Extra") 
#p4 <- subset(p4,Classificação!="Outros") 

#p4 <- p4 %>%  subset(Nota > 0.6)
#p4 <- p4 %>%  subset(Lançado > 1998)

ggplot(p4, aes(x = Data, y = Artista_principal)) + 
  geom_point(aes(colour = Subgênero, shape = Subgênero, size = Tocado), alpha = 0.7, stroke = 2) + 
  geom_boxplot(alpha = 0.5) + 
  scale_shape_manual(values = 0:50) +
  #gghighlight(Língua == "Italiano", label_key = Álbum, unhighlighted_colour = "black") +  
  #facet_grid(.~Década, scales = "free_x", space = "free_x") + 
  scale_size(range = c(5, 18), name = "Número de audições") +
  #geom_xsidedensity(aes(y = after_stat(count/max(count)*3), group = Subgênero, fill = Subgênero),alpha = 0.5, size = 1, position = "stack") +
  #geom_ysideboxplot(aes(group = Subgênero, fill = Subgênero), alpha = 0.5, size = 0.5) +
  labs(title=" ", subtitle="",y="Artista",x="Data", caption="", colour = "Subgênero", fill = "Subgênero", shape = "Subgênero", size = "Número de audições") +
  theme_classic() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14)) #, legend.position = "none") 
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

Agora vamos ver como os Gêneroes explicam os gêneros. Quais Gêneroes apresentam o maior poder de explicação para os gêneros catalogados. Primeiro vamos separar as tabelas.

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

p2 <- subset(p2, Subcontinente == "Am. Latina")
#p2 <- subset(p2, Continente == "África")
p2 <- p2 %>%  subset(Nota > 0.5)

p2 <- subset(p2,Gênero!="Brasil")
#p2 <- subset(p2,Subcontinente!="Ilhas Britânicas") 
```
Agora o gráfico seprando os Gêneroes por gênero musical ao longo do tempo. O gráfico lateral superior mostra a proporção de gênero e laterial a direita um boxplot dos pontos:
```
p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point(aes(size = Tocado), alpha = 0.7) +
  #geom_smooth(aes(color = NULL)) + #, method = lm
  scale_size(range = c(5, 18), name = "Número de audições") +
  #geom_ysideboxplot(alpha = 0.5,size = 1) +
  geom_xsidedensity(aes(y = after_stat(count),
      color = Gênero),alpha = 0.5,size = 1, position = "stack") +
  #theme_tq() +
  facet_grid(cols = vars(Década), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente") + theme_classic()
#ggsave("9.Distr_Ponto_ano_gen_subcontin_AméricaSax.png",width = 15, height = 8, dpi = 600)
```
E um gráfico mostrando os Gêneroes por subcontinente.
```
p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point(aes(size = Tocado), alpha = 0.7) +
  #geom_smooth(aes(color = NULL)) +
  scale_size(range = c(5, 18), name = "Número de audições") +
  #geom_ysideboxplot(alpha = 0.5,size = 1) +
  #theme_tq() +
  geom_xsidedensity(aes(y = after_stat(count),
      color = Gênero),alpha = 0.5,size = 1, position = "stack") +
  facet_grid(cols = vars(Década), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente") + theme_classic()
#ggsave("9.Distr_Ponto_ano_pais_subcontin_Américasax.png",width = 15, height = 8, dpi = 600)
```

### Escala 3
Também podemos ver em uma escala de Gênero. Os principais serão listadas abaixo:
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

p2 <- subset(p2, Gênero == "Estados Unidos")
#p2 <- subset(p2, Estado == "California")
p2 <- subset(p2, Gênero == "Rock")

#p2 <- p2 %>%  subset(Nota > 0.5)
```
Primeiro, vamos ver os gêneros:
```
p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point(aes(size = Tocado), alpha = 0.7) +
  #geom_smooth(aes(color = NULL)) + #, method = lm
  scale_size(range = c(5, 18), name = "Número de audições") +
  #geom_ysideboxplot(alpha = 0.5,size = 1) +
  geom_xsidedensity(aes(y = after_stat(count), color = Gênero),alpha = 0.5,size = 1, position = "stack") +
  theme_tq() +
  facet_grid(cols = vars(Década), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente") + theme_classic()
#ggsave("9.Distr_Ponto_ano_gen_Br.png",width = 15, height = 8, dpi = 600)
```
Agora vamos ver a contribuição dos estados por região ou estado (como Austrália).
```
p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Categoria)) +
  geom_point(aes(size = Tocado), alpha = 0.7) +
  #geom_smooth(aes(color = NULL)) +
  scale_size(range = c(5, 18), name = "Número de audições") +
  #geom_ysideboxplot(alpha = 0.5,size = 1) +
  geom_xsidedensity(aes(y = after_stat(count),color = Categoria),alpha = 0.5,size = 1, position = "stack") +
  facet_grid(cols = vars(Década), scales = "free_x") +
  labs(title = "Distibuição dos pontos por raíz musical e continente")  + theme_classic()
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
p2 %>% ggplot(aes(x = Lançado, y = Pontos, color = Gênero)) +
  geom_point(aes(size = Soma), alpha = 0.7) +
  geom_smooth(aes(color = NULL)) +
  geom_ysideboxplot(alpha = 0.5,size = 1) +
  geom_xsidedensity(aes(y = after_stat(count),
      color = Gênero),alpha = 0.5,size = 1, position = "stack") +
  theme_tq() +
  #facet_grid(rows = vars(Gênero), scales = "free_x") +
  labs(title = "Distibuição dos pontos por gênero e Gêneroes")
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

O mesmo pode ser feito para evidenciar algum Gênero ou gênero dentro de um todo. Vamos selecionar uma planilha com continente, Gênero e raiz de gênero musical.
```
p2 <- reshape2::dcast(planilhatotal, Continente + Gênero + Raiz ~ Classificação, value.var = "Pontos", fun.aggregate = sum)
Pontos <- apply(p2[, 4:5], 1, sum)
p2 <- data.frame (p2, Pontos)
```
E vamos plotar em gráfico e pedir para evidenciar o Brasil:
```
ggplot(p2, aes(Raiz, Pontos, fill = Continente)) + 
  geom_col(position = "fill", width = 0.85, 
    color = "black", size = 1,
    mapping = aes(linetype = Gênero == "Brasil")) 
#ggsave("12.Barra_pais_Br.png",width = 14, height = 6, dpi = 300)
```
Com esse gráfico fica evidente que na música latina o Brasil é o maior contribuidor e que este ritmo é muito associado ao continente americano, por exemplo.

