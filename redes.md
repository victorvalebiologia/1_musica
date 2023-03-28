# Apresentação
Repositório para testes de scripts em uma tabela pessoal de álbuns. O intuito são análises de diversidade. Será dividido da seguinte forma:
- Início;
- Acumulação;
- Série Temporal;
- Diversidade;
- Cluster.

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
pacman::p_load(igraph,miniCRAN,magrittr,keyplayer,dplyr,visNetwork,DT )

p2 <- planilhatotal
p2 <- subset(p2, !is.na(Lançado))

p2 <- subset(p2,Classificação!="Extra") 
p2 <- subset(p2,Estado!="Não Sei") 
p2 <- subset(p2,Coletivo!="Vários")

p2 <- subset(p2, !is.na(Estado))
p2 <- subset(p2, !is.na(País))


p3 <- p2

p3 <- subset(p2, País == "Brasil")
p3 <- subset(p2, Estado == "Espírito Santo")
```

```
local<-reshape2::dcast(p3, Estado ~ Estado, value.var = "Soma", fun.aggregate = sum)
local <- data.frame(local, row.names=1)

g <- makeDepGraph(local, suggests = FALSE)

library(??feather)


























## Hierarchical cluster analysis on famous data sets - enhanced with the dendextend package
#Tal Galili
#2022-07-04
#Seleção de dados

```
pacman::p_load(ggplot2, ggrepel, graphics,lubridate, BiodiversityR)
pacman::p_load(igraph, circlize, gplots, heatmaply, dendlist, hclust)

p2 <- planilhatotal
p2 <- subset(p2, !is.na(Lançado))

p2 <- subset(p2,Classificação!="Extra") 
p2 <- subset(p2,Estado!="Não Sei") 
p2 <- subset(p2,Coletivo!="Vários")

p2 <- subset(p2, !is.na(Estado))
p2 <- subset(p2, !is.na(País))


p3 <- p2

p3 <- subset(p2, País == "Brasil")
p3 <- subset(p2, Estado == "Espírito Santo")
```

```
local<-reshape2::dcast(p3, Estado ~ Subgênero, value.var = "Soma", fun.aggregate = sum)
local <- data.frame(local, row.names=1)
#local$Estado <- NULL

abund<-rowSums(local) #abunância por faixa
abund<-log(abund)
S <- specnumber(local)
S <- log(S)
H <- diversity(local, index = "shannon")
Simp <- diversity(local, index = "simpson")


local<-reshape2::dcast(p2, Estado + Continente ~ Álbum, value.var = "Soma", fun.aggregate = sum) 
local<-data.frame(S, abund, H, Simp, local)
local <- select(local, S, abund, H, Simp, Continente)
local <- local %>%   subset(S > 1)

#local<- p3[, c("Pontos","Soma","Tocado","Nota","Raiz")]

species_labels <- local[,5] #número da coluna de nome
library(colorspace) # get nice colors
species_col <- rev(rainbow_hcl(5))

#local<- p3[, c("Pontos","Soma","Tocado","Nota","Álbum")]
local2 <- local[,-5]

pairs(local, col = species_col,
      lower.panel = NULL,
       cex.labels=2, pch=19, cex = 1.2)

par(xpd = TRUE)
legend("bottomleft", legend = as.vector(unique(species_labels)),  
    fill= unique(species_col))
```
Vemos aqui a interação dos dados
```
par(las = 1, mar = c(4.5, 3, 3, 2) + 0.1, cex = .8)
MASS::parcoord(local2, col = species_col, var.label = TRUE, lwd = 2)

par(xpd = TRUE)
legend("bottomleft", legend = as.vector(unique(species_labels)),  
    fill= unique(species_col))
par(xpd = NA)    

#dev.off()
```
Agora plotar.
```
d_iris <- dist(local2) # method="man" # is a bit better
hc_iris <- hclust(d_iris, method = "complete")
iris_species <- rev(levels(local[,5]))

library(dendextend)
dend <- as.dendrogram(hc_iris)
dend <- rotate(dend, 1:21) #número de colunas.

dend <- color_branches(dend, k=5) #, groupLabels=iris_species)

labels_colors(dend) <-
   rainbow_hcl(3)[sort_levels_values(
      as.numeric(iris[,5])[order.dendrogram(dend)]
   )]


labels(dend) <- paste(as.character(local[,5])[order.dendrogram(dend)],
                           "(",labels(dend),")", 
                           sep = "")

dend <- hang.dendrogram(dend,hang_height=0.1)

dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.5)

par(mar = c(3,3,3,7))
plot(dend, 
     main = "Clustered", 
     horiz =  TRUE,  nodePar = list(cex = .007))

legend("bottomleft", legend = iris_species),  
    fill = rainbow_hcl(3))

```
Mapa circular.
```

par(mar = rep(0,4))
circlize_dendrogram(dend)

```
Mapa de calor agregado
```
some_col_func <- function(n) rev(colorspace::heat_hcl(n, c = c(80, 30), l = c(30, 90), power = c(1/5, 1.5)))
gplots::heatmap.2(as.matrix(local2), 
          main = "Heatmap for the Iris data set",
          srtCol = 20,
          dendrogram = "row",
          Rowv = dend,
          Colv = "NA", # this to make sure the columns are not ordered
          trace="none",          
          margins =c(5,0.1),      
          key.xlab = "Cm",
          denscol = "grey",
          density.info = "density",
          RowSideColors = rev(labels_colors(dend)), # to add nice colored strips        
          col = some_col_func
         )
```
uma outra forma.
```
heatmaply::heatmaply(as.matrix(local2),
          dendrogram = "row",
          Rowv = dend)

```
Correlação das medidas de valores como média, single e outros.
```
hclust_methods <- c("ward.D", "single", "complete", "average", "mcquitty", 
        "median", "centroid", "ward.D2")
iris_dendlist <- dendlist()
for(i in seq_along(hclust_methods)) {
   hc_iris <- hclust(d_iris, method = hclust_methods[i])   
   iris_dendlist <- dendlist(iris_dendlist, as.dendrogram(hc_iris))
}
names(iris_dendlist) <- hclust_methods
iris_dendlist

iris_dendlist_cor <- cor.dendlist(iris_dendlist)
iris_dendlist_cor

corrplot::corrplot(iris_dendlist_cor, "pie", "lower")

iris_dendlist_cor_spearman <- cor.dendlist(iris_dendlist, method_coef = "spearman")
corrplot::corrplot(iris_dendlist_cor_spearman, "pie", "lower")

```
Relação dos dados.
```
iris_dendlist %>% dendlist(which = c(1,8)) %>% ladderize %>% 
   set("branches_k_color",) %>% 
   # untangle(method = "step1side", k_seq = 3:20) %>%
   # set("clear_branches") %>% #otherwise the single lines are not black, since they retain the previous color from the branches_k_color.
   tanglegram(faster = TRUE) # (common_subtrees_color_branches = TRUE)

```
Considerando média
```   
iris_dendlist %>% dendlist(which = c(1,4)) %>% ladderize %>% 
   set("branches_k_color", k=2) %>% 
   # untangle(method = "step1side", k_seq = 3:20) %>%
   tanglegram(faster = TRUE) # (common_subtrees_color_branches = TRUE)   
```
Relação com cores
```
iris_dendlist %>% dendlist(which = c(1,4)) %>% ladderize %>% 
   # untangle(method = "step1side", k_seq = 3:20) %>%
   set("rank_branches") %>%
   tanglegram(common_subtrees_color_branches = TRUE)   
```
Número de sub-árvores
```
length(unique(common_subtrees_clusters(iris_dendlist[[1]], iris_dendlist[[4]]))[-1])
```
completo por média
```
iris_dendlist %>% dendlist(which = c(3,4)) %>% ladderize %>% 
   untangle(method = "step1side", k_seq = 2:6) %>%
   set("branches_k_color", k=10) %>% 
   tanglegram(faster = TRUE) # (common_subtrees_color_branches = TRUE)
```
Seprando por análise
```
par(mfrow = c(4,2))
for(i in 1:8) {
   iris_dendlist[[i]] %>% set("branches_k_color", k=2) %>% plot(axes = FALSE, horiz = TRUE)
   title(names(iris_dendlist)[i])
}
```









#########################
Vamos começar com uma análise de acumualção de dados. Podemos investigar a acumulação de:
- Álbuns;
- Discos;
- Língua (Idiomas).
```
p2 <- planilhatotal
acum<-reshape2::dcast(planilhatotal, Lançado ~ Álbum)
acum=data.frame(acum, row.names=1)
spAbund<-rowSums(acum) 
```
Agora vamos ver isso em um gráfico simples de acumulação.
```
rarecurve(acum, col="blue",cex=1,xlab="Tamanho amostral",ylab="Álbuns",main="Curva de Abundância") #abundância espécie
acumt=t(acum)
acumplot<-specaccum(acum) #dados de acumulação
plot(acumplot) #curva simples
```
Vamos completar com alguns dados informativos.
```
plot(acumplot,ci.type="poly",col="black",lwd=2,ci.lty=0,ci.col="lightgrey",ylab="Álbuns",
     xlab="Anos",main="Curva de Acumulação de Discos",las=1,font=1.5,font.lab=1.5,cex.lab=1,cex.axis=1) #curva clássica
```
No nosso caso ainda há uma grande tendência de acumulação de discos por ano de lançamento.
Há agumas formas de fazer isso. Vamos ver com vários tipos de curvas de rarefação juntas:
- sp1 = Rarefação;
- sp2 = Riqueza esperada;
- sp3 = Sítios aleatórios;
- sp4 = Curva do coletor.
```
sp1<-specaccum(acum,method="rarefaction")
sp2<-specaccum(acum,method="exact")
sp3<-specaccum(acum,method="random")
sp4<-specaccum(acum,method="collector")
```
Agora vamos plotar todos juntos. Somado a eles os scripts de exportar a figura gerada.
```
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Música/2.Acumul_álbum.png",width=800,height=600) #local e tmamanho
par(mfrow=c(2,2)) 
plot(sp1, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Amostras",ylab="Rarefação")
plot(sp2, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightgrey",xlab="Amostras",ylab="Riqueza Esperada")
plot(sp3, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="yellow",xlab="Amostras",ylab="Sítios Aleatórios")
plot(sp4, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Amostras",ylab="Curva do Coletor")
par(mfrow=c(1,1)) #compilado de curvas
#dev.off()
```
Esses gráficos facilitam a comparação, principalmente se olharmos o comportamento da curva do coletor. Agora, vamos ver esse mesmo gráfico mais de perto. Primeiro vamos selecionar a tabela.
```
p2 <- planilhatotal
acum<-reshape2::dcast(planilhatotal, Lançado ~ Álbum)
acum=data.frame(acum, row.names=1)
spAbund<-rowSums(acum) 
sp4<-specaccum(acum,method="collector")

acum<-reshape2::dcast(planilhatotal, Lançado ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
#names(acum)[grep('Animalia', names(altitude))] <- 'Abundance'
shannon<-diversity(acum)
a<-data.frame(sp4$sites,sp4$richness,acum,shannon,spAbund)
names(a)[grep('shannon', names(a))] <- 'Diversidade de Álbuns'
```
Agora vamos plotar, destacando a abundância de álbuns por ano.
```
ggplot(a, aes(x = Lançado, y = sp4.richness)) + 
  geom_line(size=6, alpha=0.6, color="Gray") + #geom_line(aes(group = sp4.sites))
  geom_point(aes(size=spAbund, colour=`Diversidade de Álbuns`), alpha=0.3) +
    #geom_label_repel(aes(label = sp4$richness), size=4, alpha=0.8, #funciona no zoom
                   #box.padding   = 0.35, 
                   #point.padding = 0.75,
                   #segment.color = 'grey50') +
  scale_size(range = c(.1, 24), name="Pontos") +
  #geom_text(aes(label = a$sp4.richness),col = 'black',size = 5) +
  ggtitle("Discos x Ano") +
  xlab("Ano") +
  ylab("Discos") + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + theme_classic() 
#ggsave("2.Acumul_álbum_count.png",width = 14, height = 6, dpi = 300)
```
Com o data considerando mês e dias a partir da variável formada: Data:
```
acum<-reshape2::dcast(Data, Data ~ Álbum, value.var = "Tocado")
acum=data.frame(acum, row.names=1)
acum[is.na(acum)] <- 0
spAbund<-rowSums(acum) 
```

## Série Temporal
Vamos ver a riqueza de dados ao passar dos anos. Essa riqueza pode ser de:
- Álbuns;
- Artistas.

Primeiro vamos separar a tabela.
```
p2 <- planilhatotal

acum<-reshape2::dcast(p2, Lançado ~ Álbum, value.var = "Tocado")
acum=data.frame(acum, row.names=1)
```
Agora vamos ver alguns índices simples, como abundância e riqueza:
```
spAbund <- specnumber(acum)
S <-rowSums(acum) 
sp4<-specaccum(acum,method="collector")
```
E vamos plotar em gráfico:
```
acum<-reshape2::dcast(p2, Lançado ~ Álbum, value.var = "Tocado")
acum<-data.frame(S, sp4$sites,sp4$richness,spAbund, acum)

ggplot(acum, aes(x = Lançado, y = S)) + 
  geom_point(aes(size= spAbund, colour = S), alpha = 0.4) + 
  geom_line(aes(colour=S)) +
  scale_size(range = c(.1, 18), name = "Número de artistas") +
  geom_label_repel(aes(label = Lançado), size=4, alpha= 1,box.padding   = 0.35,point.padding = 0.75,segment.color = 'grey50') +
  labs(title="Número de álbuns por ano", subtitle="",y="Número de audições",x="Ano de lançamento", caption="",color = "Número de álbuns",size = "Número de audições") +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("ano.png",width = 15, height = 8, dpi = 600)
```


## Diversidade
Análises clássicas de diversidade podem ser realizados compreender a riqueza e a diversidade de:
- Continente;
- Subcontinente;
- País;
- Estado;
- Raíz Musical;
- Gênero;
- Categoria;
- Subgênero;
- Língua (Idioma);
- Artistas.
Para as seguintes variáveis:
- Álbuns;
- Artistas;
- País.

Agora vamos selecionar a tabela, podemos filtrá-la se necessário:
```
p2 <- planilhatotal

p2 <- subset(p2,!is.na(Lançado))
p2 <- subset(p2,Classificação!="Extra") 
p2 <- subset(p2,Estado!="Não Sei") 
p2 <- subset(p2,Coletivo!="Vários")

p2 <- subset(p2, !is.na(Estado))
p2 <- subset(p2, !is.na(País))
p2 <- subset(p2, !is.na(Gênero))
p2 <- subset(p2, !is.na(Categoria))
p2 <- subset(p2, !is.na(Subgênero))
p2 <- subset(p2, !is.na(Língua))

p2 <- subset(p2, País == "Brasil")
p2 <- subset(planilhatotal,Artista!="Various Artist") 
p2 <- subset(p2, Continente == "Europa")
p2 <- subset(p2, Subcontinente == "E. Meridional")
p3 <- subset(p2, Continente == "Oceania")
p4 <- subset(p2, Continente == "Ásia")
p2 <- rbind(p3,p4)

```
Agora a tabela a ser analisada:
```
local<-reshape2::dcast(p2, País ~ Álbum, value.var = "Soma", fun.aggregate = sum)
local=data.frame(local, row.names=1)
```
Agora vamos ver os índices de diversidade. São eles:
- Abundância;
- Riqueza;
- Diversidade de Shannon;
- Equibilidade de Pielou;
- Dominância de Simpson;
- Inverso de Simpson.
```
abund<-rowSums(local) #abunância por faixa
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
H <- diversity(local)
J <- H/log(S)
simp <- diversity(local, "simpson")
invsimp <- diversity(local, "inv")
```
Vamos colocar isso em gráfico para ficar mais fácil a visualização. Primeiro vamos retomar a planilha. Algumas detalhes devem ser verficados:
- Observar se a variável analisada está no reshape2 da planilha;
- Se existe algum filtro em subset.
```
local<-reshape2::dcast(p2, País + Subcontinente ~ Álbum, value.var = "Soma", fun.aggregate = sum) 
local<-data.frame(S, local, H, simp, J, abund)
local <- local %>%   subset(S > 29)

local <- local %>%  subset(S %in% 15:29)  

local <- local %>%  subset(S %in% 5:14)  
  
```
Agora vamos plotar, mas preste atenção em:
- Se a variável analisada está em colour do geom_point;
- Em label em geom_label_repel.

Outra coisa, prestar atenção nas legendas, então verificar:
- Se em Title está a variável verificada;
- Se o eixo x está certo, bom mencionar o fltro;
- se o eixo y está certo;
- se a legenda de cor está certa (deve ser a segunda variável da tabela)

Ainda, para idiomas:
- p2 com estados vazios;
- diminuir o limite de 20 para 5.

```
theplot <- ggplot(local, aes(x = reorder(País, S), y = S)) +
  #geom_point(aes(size=abund, colour = Subcontinente), alpha = 0.85) + scale_size_binned(range = c(.1, 16)) +
  geom_col(aes(fill = Subcontinente), alpha = 0.65) + 
  
  geom_label_repel(aes(y = S, x = País, label = S), position = position_stack(vjust = 0.99), size=3.5, alpha= 0.85) +
  #geom_text_repel(aes(y = S, x = Estado, label = abund), position = position_dodge(width = 0), vjust=1.75, size=3, colour = "red") +
  
  labs(title="Ranking de importância", subtitle="", y="Número de álbuns", x="País", caption="2022_09_06", size = "Número de faixas", fill = "Subcontinente") +
  
  #scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) 
  theme_classic() +
  theme(legend.position="bottom") + #axis.title = element_text(size = 15), axis.text = element_text(size = 12), legend.position="bottom") 
  coord_flip() 

theplot  
  
ggsave(path = "/home/user/Área de Trabalho/Música", width = 20, height = 10, 
       device = "png", filename = "2022_08_03_país3", plot = theplot)
     

```
site[https://rpubs.com/Bruno_Vilela/768254]
Um parâmetro de diversidade pode ser calculando usando o ìndice de Hill, um índice de Equidade. Quanto maior o valor de q (definido em scales), maior será o peso para a equabilidade. Quanto mais próximo de zero, maior o peso para riqueza (quando q = 0, o valor de Hill é igual a riqueza de espécies - q = é um parâmetro conhecido como ordem da diversidade e é usado para dar peso às espécies comuns ou raras. q = 0 não considera a frequência das espécies e representa a riqueza observada de espécies, q = 1 equivale a transformação do índice de Shannon-Wiener (i.e. exp(H’)) e atribui pesos às espécies com base na proporção das suas frequências e, q = 2 equivale à transformação do índice de Gini-Simpson (i.e. 1/(1-D)) e atribui peso às espécies mais comuns).
```
pacman::p_load(forcats, iNEXT,tidyr,tibble)
pacman::p_load(CRAN,iNEXT) #,devtools, tidyverse)

p2 <- planilhatotal
p2 <- subset(p2,Coletivo!="Vários") 
p2 <- subset(p2,Classificação!="Extra") 
p2 <- subset(p2, !is.na(Lançado))

p2 <- subset(p2, !is.na(Aglomerado))
p2 <- subset(p2, !is.na(Artista_principal))
p2 <- subset(p2, !is.na(Década))
p2 <- subset(p2, !is.na(Origem))
p2 <- subset(p2, !is.na(Fonte))

p2 <- subset(p2, !is.na(País))
p2 <- subset(p2, País == "Canadá")
p2 <- subset(p2, Continente == "Europa")
p2 <- subset(p2, Subcontinente == "E. Meridional")

local<-reshape2::dcast(p2, Artista_principal ~ Álbum, value.var = "Soma", fun.aggregate = sum)
local=data.frame(local, row.names=1)

S <- specnumber(local)
local<-data.frame(S, local) 

local <- local %>%
  subset(S > 14)

local <- local %>%  
  subset(S %in% 10:14)  

R <- renyi(local,hill = TRUE)

R <- R %>%  
  rownames_to_column() %>% 
  pivot_longer(-rowname) %>% 
  mutate(name = factor(name, name[1:length(R)])) %>% 
  ggplot(aes(x = name, y = value, group = rowname,
             col = rowname)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlab("Parâmetro de ordem de diversidade (q)") +
  ylab("Diversidade") +
  labs(col = "Tipos") +
  theme_classic() +
  theme(legend.position="bottom") 

R  

ggsave(path = "/home/user/Área de Trabalho/Música", width = 20, height = 10, 
       device = "png", filename = "2022_08_03_artista2", plot = R)
       

```
Também podemos ver a estimativa de riqueza por categoria.
```
p2 <- planilhatotal
p2 <- subset(p2, !is.na(Lançado))
p2 <- subset(p2,Classificação!="Extra") 

p2 <- subset(p2, !is.na(Origem))
p2 <- subset(p2, !is.na(Conjunto))
p2 <- subset(p2, !is.na(Aglomerado))

local<-reshape2::dcast(p2, Artista ~ Origem, value.var = "Soma", fun.aggregate = sum)
local=data.frame(local, row.names=1)

# Mude o q para 1 para comparar a diversidade de Shannon e para 2 para Simpson

out <- iNEXT(local, q = 0,
             datatype = "abundance",
             size = seq(0, 30500, length.out=20))

R <- ggiNEXT(out, type = 1) +
  theme_bw() +
  labs(fill = "Áreas") +
  xlab("Tempo") + 
  ylab("Riqueza de álbuns") +
  scale_shape_manual(values = 0:19) +
  theme_classic() +
  theme(legend.position="bottom")

R

ggsave(path = "/home/user/Área de Trabalho/Música", width = 20, height = 10, 
       device = "png", filename = "2022_08_03_fonte", plot = R)

```
## Cluster

Vamos ver se existe alguma similaridade entre os gêneros considerando os países que possuem álbuns nos diversos gêneros. Outra forma de ver cluster é assim, onde k = núero de categorias em "branches_k_color" e "labels_colors":

``` 
pacman::p_load("plotly", "ggdendro")
pacman::p_load("magrittr", "dendextend")

p2 <- planilhatotal
p2 <- subset(p2,Classificação!="Extra") 
p2 <- subset(p2,Coletivo!="Vários") 
p2 <- subset(p2,Estado!="Não Sei") 
p2 <- subset(p2, !is.na(Estado))
p2 <- subset(p2, !is.na(Subgênero))

#p2 <- subset(p2, Continente == "América")

local<-reshape2::dcast(p2, Estado ~ Subgênero, value.var = "Soma", fun.aggregate = NULL) #sum ou NULL
local=data.frame(local, row.names=1)
S <- specnumber(local)
#local[local>0]<-1 #tansformar em presença e ausência
local<-data.frame(S, local) 
local <- local %>%   subset(S > 9)

dend <- local %>% dist %>%
  hclust %>% as.dendrogram %>%
  set("branches_k_color") %>% set("branches_lwd", 0.7) %>%
  #set("branches_lty", c(1, 1, 3, 1, 1, 2)) %>%
  set("labels_cex", 0.6) %>% set("labels_colors") %>%
  set("leaves_pch", 19) %>% set("leaves_cex", 0.5) 
ggd1 <- as.ggdend(dend) #,type = "triangle")
R<-ggplot(ggd1, horiz = TRUE) #+ scale_y_reverse(expand = c(0.2, 0)) + coord_polar(theta = "x")
R

#ggsave(path = "/home/user/Área de Trabalho/Música", width = 20, height = 10, 
       device = "png", filename = "2022_08_3_clusterestasubg", plot = R)

``` 

