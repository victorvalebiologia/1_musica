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
setwd("/home/user/Área de Trabalho/Livros") 
```

Agora baixar e ler alguns pacotes básicos.

```
if(!require(pacman, quietly = TRUE))(install.packages("pacman")) #agrupador de funções
pacman::p_load(magrittr,dplyr,reshape2) #magrittr para operações de pipe/dplyr para manipulador de dados
pacman::p_load(ggplot2, ggrepel, graphics,lubridate) #devtools, 
pacman::p_load(vegan)  #vegan para estatística ecológica/graphics para os gráficos
```
Agora vamos adicionar a planilha.
```
pacman::p_load(openxlsx) 
caminho.do.arquivo <- "/home/user/Área de Trabalho/Livros/livro.xlsx"
planilhatotal <- read.xlsx(caminho.do.arquivo, #local do arquivo
                           sheet = 1, # em qual planilha estão os dados
                           colNames = T, # as colunas dos dados possuem nomes?
                           na.strings = "NA") # como estão identificados os dados omissos?

head(planilhatotal)
```
E filtrar ela.
```
planilhatotal <- subset(planilhatotal, !is.na(Ano_leitura)) #tirar n/a da ano
planilhatotal <- subset(planilhatotal, !is.na(Ano_publicação)) #tirar n/a da ano
planilhatotal <- subset(planilhatotal, !is.na(Pontos)) #tirar n/a da pontos
```

## Acumulação
Primeiro a data:

```
p2 <- planilhatotal
p3 <- subset(p2, !is.na(Mês))

p3 <- subset(p3, !is.na(Mês))
p3 <- subset(p3, !is.na(Dia))

Data <- p3 %>% 
  select(Ano_publicação,Mês,Dia) %>% 
  mutate(Data = make_date(Ano_publicação,Mês,Dia))
Data <- data.frame(p3,Data)

```

Vamos começar com uma análise de acumualção de dados. Podemos investigar a acumulação de:
- Álbuns;
- Discos;
- Língua (Idiomas).
```
acum<-reshape2::dcast(p2, Ano_publicação ~ Título,value.var = "Página_Total", fun.aggregate = sum)
acum=data.frame(acum, row.names=1)
spAbund<-rowSums(acum) 
```
Agora vamos ver isso em um gráfico simples de acumulação.
```
rarecurve(acum, col="blue",cex=1,xlab="Tamanho amostral",ylab="Livros",main="Curva de Abundância") #abundância espécie
acumt=t(acum)
acumplot<-specaccum(acum) #dados de acumulação
plot(acumplot) #curva simples
```
Vamos completar com alguns dados informativos.
```
plot(acumplot,ci.type="poly",col="black",lwd=2,ci.lty=0,ci.col="lightgrey",ylab="Livros",
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
#png(filename="/home/user/Área de Trabalho/Música/2.Acumul_Título.png",width=800,height=600) #local e tmamanho
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
acum<-reshape2::dcast(p2, Ano_publicação ~ Título,value.var = "Página_Total", fun.aggregate = sum)
acum=data.frame(acum, row.names=1)
spAbund<-rowSums(acum) 
sp4<-specaccum(acum,method="collector")

acum<-reshape2::dcast(p2, Ano_publicação ~ Título,value.var = "Página_Total", fun.aggregate = sum)
#names(acum)[grep('Animalia', names(altitude))] <- 'Abundance'
shannon<-diversity(acum)
a<-data.frame(sp4$sites,sp4$richness,acum,shannon,spAbund)
names(a)[grep('shannon', names(a))] <- 'Diversidade de Livros'
```
Agora vamos plotar, destacando a abundância de álbuns por ano.
```
ggplot(a, aes(x = Ano_publicação, y = sp4.richness)) + 
  geom_line(size=6, alpha=0.6, color="Gray") + #geom_line(aes(group = sp4.sites))
  geom_point(aes(size=spAbund, colour=`Diversidade de Livros`), alpha=0.3) +
    #geom_label_repel(aes(label = sp4$richness), size=4, alpha=0.8, #funciona no zoom
                   #box.padding   = 0.35, 
                   #point.padding = 0.75,
                   #segment.color = 'grey50') +
  scale_size(range = c(.1, 24), name="Pontos") +
  #geom_text(aes(label = a$sp4.richness),col = 'black',size = 5) +
  ggtitle("Livros x Ano") +
  xlab("Ano") +
  ylab("Livros") + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + theme_classic() 
#ggsave("2.Acumul_Título_count.png",width = 14, height = 6, dpi = 300)
```
Com o data considerando mês e dias a partir da variável formada: Data:
```
acum<-reshape2::dcast(p2, Ano_publicação ~ Título,value.var = "Página_Total", fun.aggregate = sum)
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
acum<-reshape2::dcast(p2, Ano_publicação ~ Título,value.var = "Página_Total", fun.aggregate = sum)
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
acum<-reshape2::dcast(p2, Ano_publicação ~ Título,value.var = "Página_Total", fun.aggregate = sum)
acum<-data.frame(S, sp4$sites,sp4$richness,spAbund, acum)

ggplot(acum, aes(x = Ano_publicação, y = S)) + 
  geom_point(aes(size= spAbund, colour = S), alpha = 0.4) + 
  geom_line(aes(colour=S)) +
  scale_size(range = c(.1, 18), name = "Número de escritores") +
  geom_label_repel(aes(label = Ano_publicação), size=4, alpha= 1,box.padding   = 0.35,point.padding = 0.75,segment.color = 'grey50') +
  labs(title="Número de livros por ano", subtitle="",y="Número de páginas",x="Ano de lançamento", caption="",color = "Número de livros",size = "Número de páginas") +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("ano.png",width = 15, height = 8, dpi = 600)

```
Outros gráficos exploratórios.
```

ggplot(p2, aes(x = Continente, y = Ano_publicação)) + 
  geom_boxplot(aes(fill = Região), alpha = 0.4) +
  labs(title="Boxplot", subtitle="",y="Ano de Publicação",x="Continente") +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()

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

p2 <- subset(p2,!is.na(Ano_publicação))

p2 <- subset(p2, !is.na(País))
p2 <- subset(p2, !is.na(Região))

#p2 <- subset(p2, País == "Brasil")

```
Agora a tabela a ser analisada:
```
local<-reshape2::dcast(p2, País ~ Título,value.var = "Página_Total", fun.aggregate = sum)
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
local<-reshape2::dcast(p2, País + Região ~ Título,value.var = "Página_Total", fun.aggregate = sum)
local<-data.frame(S, local, H, simp, J, abund)

local <- local %>%   subset(S > 2)


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
  #geom_point(aes(size=abund, colour = Região), alpha = 0.85) + scale_size_binned(range = c(.1, 16)) +
  geom_col(aes(fill = Região), alpha = 0.65) + 
  
  geom_label_repel(aes(y = S, x = País, label = S), position = position_stack(vjust = 0.99), size=3.5, alpha= 0.85) +
  #geom_text_repel(aes(y = S, x = Estado, label = abund), position = position_dodge(width = 0), vjust=1.75, size=3, colour = "red") +
  
  labs(title="Ranking de importância", subtitle="", y="Número de livros", x="País", caption="2022_10_01", size = "Número de faixas", fill = "Subcontinente") +
  
  #scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) 
  theme_classic() +
  theme(legend.position="bottom") + #axis.title = element_text(size = 15), axis.text = element_text(size = 12), legend.position="bottom") 
  coord_flip() 

theplot  
  
ggsave(path = "/home/user/Área de Trabalho/Livros", width = 20, height = 10, 
       device = "png", filename = "2022_10_15_pais", plot = theplot)
     

```
site[https://rpubs.com/Bruno_Vilela/768254]
Um parâmetro de diversidade pode ser calculando usando o ìndice de Hill, um índice de Equidade. Quanto maior o valor de q (definido em scales), maior será o peso para a equabilidade. Quanto mais próximo de zero, maior o peso para riqueza (quando q = 0, o valor de Hill é igual a riqueza de espécies - q = é um parâmetro conhecido como ordem da diversidade e é usado para dar peso às espécies comuns ou raras. q = 0 não considera a frequência das espécies e representa a riqueza observada de espécies, q = 1 equivale a transformação do índice de Shannon-Wiener (i.e. exp(H’)) e atribui pesos às espécies com base na proporção das suas frequências e, q = 2 equivale à transformação do índice de Gini-Simpson (i.e. 1/(1-D)) e atribui peso às espécies mais comuns).
```
pacman::p_load(forcats, iNEXT,tidyr,tibble)
pacman::p_load(CRAN,iNEXT) #,devtools, tidyverse)

p2 <- planilhatotal

p2 <- subset(p2,!is.na(Ano_publicação))

p2 <- subset(p2, !is.na(País))
p2 <- subset(p2, !is.na(Subgênero))

#p2 <- subset(p2, País == "Brasil")

local<-reshape2::dcast(p2, Gênero ~ Título,value.var = "Página_Total", fun.aggregate = sum)
local=data.frame(local, row.names=1)

S <- specnumber(local)
local<-data.frame(S, local) 

local <- local %>%  subset(S > 4)

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
       device = "png", filename = "2022_10_15_subg", plot = R)
       

```
Também podemos ver a estimativa de riqueza por categoria.
```
p2 <- planilhatotal

p2 <- subset(p2,!is.na(Ano_publicação))

p2 <- subset(p2, !is.na(País))
p2 <- subset(p2, !is.na(Subgênero))

#p2 <- subset(p2, País == "Brasil")

local<-reshape2::dcast(p2, Título ~ Sexo,value.var = "Página_Total", fun.aggregate = sum)
local=data.frame(local, row.names=1)

# Mude o q para 1 para comparar a diversidade de Shannon e para 2 para Simpson

out <- iNEXT(local, q = 0,
             datatype = "abundance",
             size = seq(0, 75000, length.out=20))

R <- ggiNEXT(out, type = 1) +
  theme_bw() +
  labs(fill = "Áreas") +
  xlab("Páginas") + 
  ylab("Número de livros") +
  scale_shape_manual(values = 0:19) +
  theme_classic() +
  theme(legend.position="bottom")

R

ggsave(path = "/home/user/Área de Trabalho/Música", width = 20, height = 10, 
       device = "png", filename = "2022_10_15_sex", plot = R)

```
## Cluster

Vamos ver se existe alguma similaridade entre os gêneros considerando os países que possuem álbuns nos diversos gêneros. Outra forma de ver cluster é assim, onde k = núero de categorias em "branches_k_color" e "labels_colors":

``` 
pacman::p_load("plotly", "ggdendro")
pacman::p_load("magrittr", "dendextend")

p2 <- planilhatotal

p2 <- subset(p2,!is.na(Ano_publicação))

p2 <- subset(p2, !is.na(País))
p2 <- subset(p2, !is.na(Subgênero))

#p2 <- subset(p2, País == "Brasil")

local<-reshape2::dcast(p2, País ~ Subgênero,value.var = "Página_Total", fun.aggregate = sum)
local=data.frame(local, row.names=1)
S <- specnumber(local)
#local[local>0]<-1 #tansformar em presença e ausência
local<-data.frame(S, local) 
local <- local %>%   subset(S > 1)

dend <- local %>% dist %>%
  hclust %>% as.dendrogram %>%
  set("branches_k_color") %>% set("branches_lwd", 0.7) %>%
  #set("branches_lty", c(1, 1, 3, 1, 1, 2)) %>%
  set("labels_cex", 0.6) %>% set("labels_colors") %>%
  set("leaves_pch", 19) %>% set("leaves_cex", 0.5) 
ggd1 <- as.ggdend(dend) #,type = "triangle")
R<-ggplot(ggd1, horiz = TRUE) #+ scale_y_reverse(expand = c(0.2, 0)) + coord_polar(theta = "x")
R

as.hclust(dend)
as_hclust_fixed(dend, hc)

ggsave(path = "/home/user/Área de Trabalho/Música", width = 20, height = 10, 
       device = "png", filename = "2022_08_3_clusterestsubg", plot = R)

``` 
Outra forma de plotar. 
``` 
species_labels <- local #número da coluna de nome
library(colorspace) # get nice colors
species_col <- rev(rainbow_hcl(5))

plot(dend, 
     main = "Clustered", 
     horiz =  TRUE,  nodePar = list(cex = .007))

``` 
Incluindo mapas de calor.
``` 
some_col_func <- function(n) rev(colorspace::heat_hcl(n, c = c(80, 30), l = c(30, 90), power = c(1/5, 1.5)))
gplots::heatmap.2(as.matrix(local), 
          main = "Mapa de calor",
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


iris_dendlist %>% dendlist(which = c(1,8)) %>% ladderize %>% 
   set("branches_k_color", k=3) %>% 
   # untangle(method = "step1side", k_seq = 3:20) %>%
   # set("clear_branches") %>% #otherwise the single lines are not black, since they retain the previous color from the branches_k_color.
   tanglegram(faster = TRUE) # (common_subtrees_color_branches = TRUE)




``` 

## PCA




``` 
pacman::p_load(ggfortify, cluster)

p2 <- planilhatotal

p2 <- subset(p2,!is.na(Ano_publicação))

p2 <- subset(p2, !is.na(País))
p2 <- subset(p2, !is.na(Subgênero))

#p2 <- subset(p2, País == "Brasil")

local<-reshape2::dcast(p2, País ~ Subgênero,value.var = "Página_Total", fun.aggregate = sum)
local=data.frame(local, row.names=1)

pca_res <- prcomp(local, scale. = TRUE)
#autoplot(pca_res)

local<-reshape2::dcast(p2, País + Continente ~ Subgênero,value.var = "Página_Total", fun.aggregate = sum)
#autoplot(pca_res, data = local, colour = 'País', frame = TRUE)
#autoplot(pca_res, data = local, colour = 'Continente', label = TRUE, label.size = 3,  loadings = TRUE)
pca <-autoplot(pca_res, data = local, colour = 'Continente', label = TRUE, label.size = 4, 
         frame = TRUE, frame.type = 't', frame.color = 'Continente',
         loadings = TRUE, loadings.colour = 'blue',loadings.label = TRUE, loadings.label.size = 3) +                
         theme_classic() #+ 
         #stat_ellipse(geom="polygon", aes(fill = Região), alpha = 0.2, show.legend = FALSE,level = 0.95)        

pca

ggsave(path = "/home/user/Área de Trabalho/Livros", width = 20, height = 10, 
       device = "png", filename = "2022_10_15_PCA", plot = pca)
       
``` 
Um mais simples, sem a elipse
``` 
p2 <- planilhatotal

p2 <- subset(p2,!is.na(Ano_publicação))

p2 <- subset(p2, !is.na(Editora))
p2 <- subset(p2, !is.na(Subgênero))

#p2 <- subset(p2, País == "Brasil")

local<-reshape2::dcast(p2, Editora ~ Subgênero,value.var = "Página_Total", fun.aggregate = sum)
local=data.frame(local, row.names=1)
S <- specnumber(local)
#local[local>0]<-1 #tansformar em presença e ausência
local<-data.frame(S, local) 
local <- local %>%   subset(S > 3)
local$S <- NULL


pca_res <- prcomp(local)
#autoplot(pca_res)

pca <-autoplot(pca_res, data = local, label = TRUE, label.size = 4, frame = TRUE, frame.type = 't',
         loadings = TRUE, loadings.colour = 'blue',loadings.label = TRUE, loadings.label.size = 3) +                
         theme_classic() 

pca

ggsave(path = "/home/user/Área de Trabalho/Livros", width = 20, height = 10, 
       device = "png", filename = "2022_10_15_PCA2", plot = pca)
       
``` 

## Gráfico de núvens




``` 
pacman::p_load(readr, tm, dplyr, wordcloud, wesanderson)

words <- p2 %>% 
  select(Título)
str(words)  
words <- as.character(words)       

word.corpus <- Corpus(VectorSource(words)) 

word.corpus<-word.corpus %>%
  tm_map(removePunctuation) %>% ##eliminar pontuacao
  tm_map(removeNumbers) %>% #sem numeros
  tm_map(stripWhitespace) #sem espacos
  
word.corpus<-word.corpus %>%
  tm_map(content_transformer(tolower)) %>% ##make all words lowercase
  tm_map(removeWords, stopwords("SMART"))  #"por" para português "en" inglês

word.corpus <- tm_map(word.corpus, removeWords, c("the", "vol", "dos", "para", "das"))
    
word.corpus <- tm_map(word.corpus, stemDocument)  

word.counts <- as.matrix(TermDocumentMatrix(word.corpus))
word.freq <- sort(rowSums(word.counts), decreasing = TRUE)
head(word.freq)
         
set.seed(32)  #be sure to set the seed if you want to reproduce the same again

wordcloud(words = names(word.freq), freq = word.freq, scale = c(4, 0.3), max.words = 50, 
    random.order = FALSE, color = wes_palette("Darjeeling1"), rot.per = 0.7)    

#wordcloud(words = names(word.freq), freq = word.freq, scale = c(3, 0.5), max.words = 100, 
    random.order = TRUE)         
    
#wordcloud(words = names(word.freq), freq = word.freq, scale = c(4, 0.3), max.words = 100, 
    random.order = TRUE, color = wes_palette("Darjeeling1"))    

