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
#setwd("/home/valev/Área de Trabalho/R/musica/R") 
setwd("/home/kaetes/Área de trabalho/R") 

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
caminho.do.arquivo <- "/home/valev/Área de Trabalho/Planilhas/musica.xlsx"
planilhatotal <- read.xlsx(caminho.do.arquivo, #local do arquivo
                           sheet = 1, # em qual planilha estão os dados
                           colNames = T, # as colunas dos dados possuem nomes?
                           na.strings = "NA") # como estão identificados os dados omissos?

#head(planilhatotal)

```
Ou pelo Google drive.
```
pacman::p_load(googledrive, googlesheets4, readxl) 

drive_auth()
#drive_find(pattern = "musica.xlsx")


arquivo <- drive_get("musica.xlsx")
drive_download(file = arquivo$id, path = "musica.xlsx", type = "xlsx", overwrite = TRUE)

library(readxl)
planilhatotal <- read_excel("musica.xlsx")

#head(planilhatotal)
```
E filtrar ela.
```
planilhatotal <- subset(planilhatotal, !is.na(Lançado)) #tirar n/a da ano
planilhatotal <- subset(planilhatotal, !is.na(Pontos)) #tirar n/a da pontos
p2 <- planilhatotal
p2 <- subset(p2, !is.na(Lançado))

p2 <- p2 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 6, .)))
p2 <- p2 %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), "6", .)))  
  

p3 <- subset(p2, !is.na(Mês))
p3 <- subset(p3, !is.na(Dia))

Data <- p3 %>% 
  select(Lançado,Mês,Dia) %>% 
  mutate(Data = make_date(Lançado,Mês,Dia))

Data <- data.frame(p3,Data)

```

## Gráfico exploratório artistas

Agora, considerando a localização ou o gênero relacionado ao artista com outros do mesmo tipo ao longo do tempo.

``` 
pacman::p_load(ggside, stringr) #, tidyverse,tidyquant)

p2 <- Data %>%
    tidyr::separate_rows(Artista_principal, sep = "/")
p3 <- p2 %>% filter(País %in% "Estados Unidos")
p3 <- p3 %>% filter(str_detect(Categoria, "R&B"))
#p3 <- p3 %>% filter(Lançado > 2000)

#p3 <- rbind(p3, p4)

# Escolher artistas: p3 <- subset(p3, Artista_principal == "Caetano Veloso")
# Retirar artistas: p3 <- subset(p3, Artista_principal != "Iron Maiden")

p3 <- unique(p3)

p3 <- subset(p3, Classificação != "Extra")
p3 <- subset(p3, Classificação != "Outros")
p3 <- p3 %>% subset(Nota > 0.74)


# Criar gráfico
ggplot(p3, aes(x = Data, y = Pontos)) + 
  # Pontos e linhas
  geom_point(aes(colour = Artista_principal, size = Tocado, shape = Tipo), alpha = 0.6) + 
  geom_line(aes(colour = Artista_principal), linetype = 2, alpha = 0.3) +
  geom_smooth(method = lm, se = FALSE, aes(colour = Artista_principal), alpha = 0.6) +  

  # Elementos adicionais
  scale_shape_manual(values = 0:10) +
  scale_size(range = c(5, 18), name = "Número de audições") +
  geom_hline(aes(yintercept = mean(Pontos)), linetype = "dashed", alpha = 0.4) +
  geom_label_repel(aes(label = Álbum, colour = Artista_principal), 
                   size = 2.5, box.padding = 0.35, point.padding = 0.75, 
                   segment.color = 'grey50') +

  # Boxplots laterais
  geom_xsideboxplot(aes(fill = Artista_principal), alpha = 0.5) +
  geom_ysideboxplot(aes(fill = Artista_principal), alpha = 0.5) +

  # Elipses
  stat_ellipse(geom = "polygon", aes(fill = Artista_principal), alpha = 0.2, level = 0.1) + 

  # Personalização
  labs(title = " ", subtitle = "", y = "Pontos", x = "Ano de lançamento", caption = "",
       shape = "Tipo de álbum", colour = "Artista principal", 
       fill = "Artista principal", size = "Número de audições") +
  theme_minimal() +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14), 
        legend.position = "right") +
  guides(colour = guide_legend(ncol = 2), 
         fill = guide_legend(ncol = 2), 
         size = guide_legend(ncol = 2))


``` 
Uma visão mais macro com o ano de lançamento e os gêneros.

``` 
pacman::p_load(ggside, stringr) #, tidyverse,tidyquant)

p2 <- Data %>%
    tidyr::separate_rows(Gênero, sep = "/")
p3 <- p2 %>% filter(Lançado %in% "2013")
#p4 <- p3 %>% filter(str_detect(Gênero, "Jazz"))

#p3 <- rbind(p3, p4)

# Escolher artistas: p3 <- subset(p3, Artista_principal == "Caetano Veloso")
# Retirar artistas: p3 <- subset(p3, Artista_principal != "Iron Maiden")

p3 <- p3 %>% distinct(Álbum, Unidades, .keep_all = TRUE)

p3 <- subset(p3, Classificação != "Extra")
p3 <- subset(p3, Classificação != "Outros")
p3 <- p3 %>% subset(Nota > 0.74)

nrow(p3)  # Veja se retorna o número correto de registros

ggplot(p3, aes(x = Data, y = Pontos)) + 
  # Pontos e linhas
  geom_point(aes(colour = País, size = Tocado, shape = Tipo), alpha = 0.6) + 
  geom_line(aes(colour = País), linetype = 2, alpha = 0.3) +
  geom_smooth(method = lm, se = FALSE, aes(colour = País), alpha = 0.6) +  

  # Elementos adicionais
  scale_shape_manual(values = 0:10) +
  scale_size(range = c(5, 18), name = "Número de audições") +
  geom_hline(aes(yintercept = mean(Pontos)), linetype = "dashed", alpha = 0.4) +
  geom_label_repel(aes(label = Álbum, colour = País), 
                   size = 2.5, box.padding = 0.35, point.padding = 0.75, 
                   segment.color = 'grey50') +

  # Boxplots laterais
  geom_xsideboxplot(aes(fill = País), alpha = 0.5) +
  geom_ysideboxplot(aes(fill = País), alpha = 0.5) +

  # Elipses
  stat_ellipse(geom = "polygon", aes(fill = País), alpha = 0.2, level = 0.1) + 

  # Personalização
  labs(title = " ", subtitle = "", y = "Pontos", x = "Ano de lançamento", caption = "",
       shape = "Tipo de álbum", colour = "Artista principal", 
       fill = "Artista principal", size = "Número de audições") +
  theme_minimal() +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14), 
        legend.position = "right") +
  guides(colour = guide_legend(ncol = 2), 
         fill = guide_legend(ncol = 2), 
         size = guide_legend(ncol = 2))

```

Primeiro uma visão do artista ao longo do tempo.
``` 
pacman::p_load(ggside, stringr) #, tidyverse,tidyquant)

p2 <- Data %>%
    tidyr::separate_rows(Artista_principal, sep = "/")
p3 <- p2 %>% filter(str_detect(Artista.Tag, "Beyoncé"))

#p3 <- p3 %>% filter(str_detect(Gênero, "Bossa Nova"))

#p3 <- rbind(p3, p4)

# Escolher artistas: p3 <- subset(p3, Artista_principal == "Caetano Veloso")
# Retirar artistas: p3 <- subset(p3, Artista_principal != "Iron Maiden")

p3 <- unique(p3)

p3 <- subset(p3, Classificação != "Extra")
p3 <- subset(p3, Classificação != "Outros")
p3 <- p3 %>% subset(Nota > 0.74)


nrow(p3)  # Deve retornar o número correto de registros

ggplot(p3, aes(x = Data, y = Pontos)) + 
  geom_point(aes(colour = Artista_principal, size = Tocado, shape = Tipo), alpha = 0.6) + 
  geom_smooth(method = lm,se = FALSE, alpha = 0.6, aes(colour = Artista_principal)) +  #method = lm ou loess
  scale_shape_manual(values = 0:10) +
  geom_line(aes(colour = Artista_principal), linetype = 2, linejoin = "mitre", lineend = "butt", alpha = 0.3) +
  geom_hline(aes(yintercept = mean(Pontos)), linetype = "dashed", alpha = 0.4) + 
  #facet_grid(.~Década, scales = "free_x", space = "free_x") + 
  scale_size(range = c(5, 18), name = "Número de audições") +
  geom_label_repel(aes(label = Álbum, colour = Artista_principal), size=2.5, alpha= 1,
    box.padding = 0.35,point.padding = 0.75,segment.color = 'grey50') +
  labs(title=" ", subtitle="",y="Pontos",x="Ano de lançamento", caption="", shape = "Tipo de álbum", 
    colour = "Artista Principal", fill = "Artista Principal", size = "Número de audições") +
  geom_xsideboxplot(aes(fill = Artista_principal),alpha = 0.5) +
  geom_ysideboxplot(aes(fill = Artista_principal),alpha = 0.5) +
  stat_ellipse(geom="polygon", aes(fill = Artista_principal), alpha = 0.2, show.legend = TRUE,level = 0.25) +  
  #stat_ellipse(geom="path", aes(color = Artista_principal), level = 0.95) +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_minimal()




``` 
Detalhando por tipo de lançamento.

``` 
p2 <- Data %>%
    tidyr::separate_rows(Artista_principal, sep = "/")
p3 <- p2 %>% filter(str_detect(Artista.Tag, "Beyoncé"))

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
  stat_ellipse(geom="polygon", aes(fill = Tipo), alpha = 0.2, show.legend = TRUE, level = 0.25) +        
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14)) +
  theme_minimal()
  
#ggsave("ano.png",width = 15, height = 8, dpi = 600)
``` 
