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

# Diversidade
# Pacote - Criar tabelas para análise (vai ter mensagem, mas funciona)
pacman::p_load(reshape2)

## Acumulação
#### Acumulação de Discos
##### Riqueza
acum<-reshape2::dcast(planilhatotal, Lançado ~ Álbum) #, value.var = "Pontos", fun.aggregate = sum)
acum=data.frame(acum, row.names=1)
spAbund<-rowSums(acum) #abunância por espécie
spAbund

##### Curva
rarecurve(acum, col="blue",cex=1,xlab="Tamanho amostral",ylab="Álbuns",main="Curva de Abundância") #abundância espécie
acumt=t(acum)
acumplot<-specaccum(acum) #dados de acumulação
plot(acumplot) #curva simples
plot(acumplot,ci.type="poly",col="black",lwd=2,ci.lty=0,ci.col="lightgrey",ylab="Álbuns",
     xlab="Anos",main="Curva de Acumulação de Discos",las=1,font=1.5,font.lab=1.5,cex.lab=1,cex.axis=1) #curva clássica

### Acumulação Discos
##### Cálculos de rarefação
sp1<-specaccum(acum,method="rarefaction")

sp2<-specaccum(acum,method="exact")

sp3<-specaccum(acum,method="random")

sp4<-specaccum(acum,method="collector")

##### Rarefação Gráficos
###### Gráficos juntos
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Música/2.Acumul_álbum.png",width=800,height=600) #local e tmamanho
par(mfrow=c(2,2)) 
plot(sp1, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Amostras",ylab="Rarefação")
plot(sp2, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightgrey",xlab="Amostras",ylab="Riqueza Esperada")
plot(sp3, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="yellow",xlab="Amostras",ylab="Sítios Aleatórios")
plot(sp4, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Amostras",ylab="Curva do Coletor")
par(mfrow=c(1,1)) #compilado de curvas
#dev.off()

###### Gráficos de coletor
sp4<-specaccum(acum,method="collector")
acum<-reshape2::dcast(planilhatotal, Lançado ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
#names(acum)[grep('Animalia', names(altitude))] <- 'Abundance'
shannon<-diversity(acum)
a<-data.frame(sp4$sites,sp4$richness,acum,shannon,spAbund)
names(a)[grep('shannon', names(a))] <- 'Diversidade de Álbuns'

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

### Acumulação Artistas
##### Cálculos de rarefação

###### Riqueza
acum<-reshape2::dcast(planilhatotal, Lançado ~ Artista) # , value.var = "Pontos", fun.aggregate = sum)
acum=data.frame(acum, row.names=1)
spAbund<-rowSums(acum) #abunância por espécie
spAbund

##### Curva
rarecurve(acum, col="blue",cex=1,xlab="Tamanho amostral",ylab="Espécies",main="Curva de Abundância") #abundância espécie
acumt=t(acum)
acumplot<-specaccum(acum) #dados de acumulação
plot(acumplot) #curva simples
plot(acumplot,ci.type="poly",col="black",lwd=2,ci.lty=0,ci.col="lightgrey",ylab="Álbuns",
     xlab="Anos",main="Curva de Acumulação de Discos",las=1,font=1.5,font.lab=1.5,cex.lab=1,cex.axis=1) #curva clássica

### Acumulação Artistas
##### Cálculos de rarefação
sp1<-specaccum(acum,method="rarefaction")

sp2<-specaccum(acum,method="exact")

sp3<-specaccum(acum,method="random")

sp4<-specaccum(acum,method="collector")

##### Rarefação Gráficos
###### Gráficos juntos
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Música/2.Acumul_artista.png",width=800,height=600) #local e tmamanho
par(mfrow=c(2,2)) 
plot(sp1, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Amostras",ylab="Rarefação")
plot(sp2, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightgrey",xlab="Amostras",ylab="Riqueza Esperada")
plot(sp3, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="yellow",xlab="Amostras",ylab="Sítios Aleatórios")
plot(sp4, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Amostras",ylab="Curva do Coletor")
par(mfrow=c(1,1)) #compilado de curvas
#dev.off()

###### Gráficos de coletor
sp4<-specaccum(acum,method="collector")
acum<-reshape2::dcast(planilhatotal, Lançado ~ Artista, value.var = "Pontos", fun.aggregate = sum)
#names(acum)[grep('Animalia', names(altitude))] <- 'Abundance'
shannon<-diversity(acum)
a<-data.frame(sp4$sites,sp4$richness,acum,shannon,spAbund)
names(a)[grep('shannon', names(a))] <- 'Diversidade de Artista'

ggplot(a, aes(x = Lançado, y = sp4.richness)) + 
  geom_line(size=6, alpha=0.6, color="Gray") + #geom_line(aes(group = sp4.sites))
  geom_point(aes(size=spAbund, colour=`Diversidade de Artista`), alpha=0.3) +
  #geom_label_repel(aes(label = sp4$richness), size=4, alpha=0.8, #funciona no zoom
  #box.padding   = 0.35, 
  #point.padding = 0.75,
  #segment.color = 'grey50') +
  scale_size(range = c(.1, 24), name="Pontos") +
  #geom_text(aes(label = a$sp4.richness),col = 'black',size = 5) +
  ggtitle("Artista x Ano") +
  xlab("Ano") +
  ylab("Número de Artistas Novos") + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + theme_classic() 
#ggsave("2.Acumul_artist_count.png",width = 14, height = 6, dpi = 300)

## Ano
##### Tabela
local<-reshape2::dcast(planilhatotal, Lançado ~ Artista) 
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(planilhatotal, Lançado ~ Artista) #, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
#local <- local %>%
#subset(S > 10)
ggplot(local, aes(x = Lançado, y = S)) + 
  geom_point(aes(size=abund, colour = H), alpha = 0.4)+ #size=abund
  #scale_x_log10() + 
  geom_line() +
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Lançado), size=4, alpha= 1, #geom_label_repel
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  #geom_boxplot() +
  ggtitle("Número de Artista por Ano") +
  xlab("Ano") +
  ylab("Número de Artistas") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("2.Ano_count.png",width = 15, height = 8, dpi = 600)

## País
### País + artista
##### Tabela
local<-reshape2::dcast(planilhatotal, País ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(planilhatotal, País ~ Artista, value.var = "Pontos", fun.aggregate = sum) 
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%
  subset(S > 15)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = País), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = País), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de artista") +
  xlab("Número de artista > 15") +
  ylab("Diversidade de artista") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("3.Pais_artist.png",width = 15, height = 8, dpi = 600)

### País + álbum
##### Tabela
local<-reshape2::dcast(planilhatotal, País ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(planilhatotal, País ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%
  subset(S > 25)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = País), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = País), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de álbum") +
  xlab("Número de álbum > 25") +
  ylab("Diversidade de álbum") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("3.Pais_album.png",width = 15, height = 8, dpi = 600)

## Estado
### Estado + artista
##### Tabela
local<-reshape2::dcast(planilhatotal, Estado ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(planilhatotal, Estado ~ Artista, value.var = "Pontos", fun.aggregate = sum) 
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%  subset(S > 30)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Estado), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Estado), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de artista") +
  xlab("Número de artista > 30") +
  ylab("Diversidade de artista") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("3.Estado_artist.png",width = 15, height = 8, dpi = 600)

### Estado + álbum
##### Tabela
local<-reshape2::dcast(planilhatotal, Estado ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(planilhatotal, Estado ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%  subset(S > 60)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Estado), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Estado), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de álbum") +
  xlab("Número de álbum > 60") +
  ylab("Diversidade de álbum") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("3.Estado_album.png",width = 15, height = 8, dpi = 600)

## Gêneros
### Gênero + artista
##### Tabela
local<-reshape2::dcast(planilhatotal, Gênero ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(planilhatotal, Gênero ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Gênero), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Gênero), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de Gênero") +
  xlab("Número de artistas") +
  ylab("Diversidade de artistas") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("3.Gen_artist.png",width = 15, height = 10, dpi = 600)

### Gênero + album
##### Tabela
local<-reshape2::dcast(planilhatotal, Gênero ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(planilhatotal, Gênero ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Gênero), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Gênero), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de Gênero") +
  xlab("Número de álbuns") +
  ylab("Diversidade de álbuns") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("3.Gen_album.png",width = 15, height = 10, dpi = 600)

### Gênero + país
##### Tabela
local<-reshape2::dcast(planilhatotal, Gênero ~ País, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(planilhatotal, Gênero ~ País, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Gênero), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Gênero), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de Gênero") +
  xlab("Número de paises") +
  ylab("Diversidade de paises") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("3.Gen_pais.png",width = 15, height = 10, dpi = 600)

## Categoria
### Categoria + artista
##### Tabela
local<-reshape2::dcast(planilhatotal, Categoria ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(planilhatotal, Categoria ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>% subset(S > 50)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Categoria), alpha = 0.4)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Categoria), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de categoria") +
  xlab("Número de artistas > 50") +
  ylab("Diversidade de artistas") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("3.Categ_artist.png",width = 15, height = 8, dpi = 600)

### Categoria + álbum
##### Tabela
local<-reshape2::dcast(planilhatotal, Categoria ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(planilhatotal, Categoria ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>% subset(S > 100)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Categoria), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Categoria), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de categoria") +
  xlab("Número de álbuns > 100") +
  ylab("Diversidade de álbuns") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("3.Categ_album.png",width = 15, height = 8, dpi = 600)

### Categori + país
##### Tabela
local<-reshape2::dcast(planilhatotal, Categoria ~ País, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(planilhatotal, Categoria ~ País, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>% subset(S > 7)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Categoria), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Categoria), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de categoria") +
  xlab("Número de paises > 7") +
  ylab("Diversidade de países") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("3.Categ_pais.png",width = 15, height = 8, dpi = 600)

## Subgênero
### Subgênero + artista
##### Tabela
local<-reshape2::dcast(planilhatotal, Subgênero ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(planilhatotal, Subgênero ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>% subset(S > 30)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Subgênero), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Subgênero), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de Subgênero") +
  xlab("Número de artistas > 30") +
  ylab("Diversidade de artistas") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("3.Subg_artist.png",width = 15, height = 8, dpi = 600)

### Subgênero + álbum
##### Tabela
local<-reshape2::dcast(planilhatotal, Subgênero ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(planilhatotal, Subgênero ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>% subset(S > 60)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Subgênero), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Subgênero), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de Subgênero") +
  xlab("Número de álbum > 60") +
  ylab("Diversidade de álbum") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("3.Subg_album.png",width = 15, height = 8, dpi = 600)

### Subgênero + País
##### Tabela
local<-reshape2::dcast(planilhatotal, Subgênero ~ País, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(planilhatotal, Subgênero ~ País, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>% subset(S > 7)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Subgênero), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Subgênero), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de Subgênero") +
  xlab("Número de países > 7") +
  ylab("Diversidade de países") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("3.Subg_artist_pais.png",width = 15, height = 8, dpi = 600)

## Idioma
### Idioma + artista
##### Tabela
p2 <- subset(planilhatotal, !is.na(Língua)) #tirar n/a da ano
local<-reshape2::dcast(p2, Língua ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, Língua ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%   subset(S > 3)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Língua), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Língua), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de Idiomas") +
  xlab("Número de artista > 3") +
  ylab("Diversidade de artista") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("3.idiom_artist.png",width = 15, height = 8, dpi = 600)

### Idioma + álbum
##### Tabela
idioma <- subset(planilhatotal, !is.na(Língua)) #tirar n/a da ano
local<-reshape2::dcast(idioma, Língua ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, Língua ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%   subset(S > 3)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Língua), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Língua), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de Idiomas") +
  xlab("Número de álbum > 3") +
  ylab("Diversidade de álbum") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("3.idiom_album.png",width = 15, height = 8, dpi = 600)

### Idioma + país
##### Tabela
local<-reshape2::dcast(p2, Língua ~ País, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, Língua ~ País, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%   subset(S > 2)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Língua), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Língua), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de Idiomas") +
  xlab("Número de países > 2") +
  ylab("Diversidade de países") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("3.idiom_pais.png",width = 15, height = 8, dpi = 600)

## Artista
### Artista + álbum
##### Tabela
p2 <- subset(planilhatotal,Artista!="Various Artist") 
p2 <- subset(p2,Tipo!="Single") #tirar single
local<-reshape2::dcast(p2, Artista ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, Artista ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local,H, simp, S, J, abund)
local <- local %>% subset(S > 15)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Artista), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Artista), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de Artista") +
  xlab("Número de Álbuns > 15") +
  ylab("Pontos") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("3.artist_album.png",width = 15, height = 8, dpi = 600)

### Artista + Estilo
##### Tabela
local<-reshape2::dcast(p2, Artista ~ Tag, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, Artista ~ Tag, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local,H, simp, S, J, abund)
local <- local %>% subset(S > 4)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Artista), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Artista), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de artistas") +
  xlab("Número de estilos > 4") +
  ylab("Diversidade de estilos") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("3.artist_tag.png",width = 15, height = 8, dpi = 600)

## Continente
###América
#### Continente + artista
##### Tabela
p2 <- subset(planilhatotal, Continente == "América")
#p2 <- subset(p2,Continente!="Vários") 
#p2 <- subset(p2,Subcontinente!="Vários") 
#p2 <- subset(p2,Região!="Vários")
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies
local<-reshape2::dcast(p2, País ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, País ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%   subset(S > 10)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = País), alpha=0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = País), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de artistas na América") +
  xlab("Número de artistas > 10") +
  ylab("Diversidade nos países") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("4.América_pais_artist.png",width = 15, height = 8, dpi = 600)

#### Continente + álbum
##### Tabela
p2 <- subset(planilhatotal, Continente == "América")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Região!="Vários")
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies
local<-reshape2::dcast(p2, País ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, País ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%   subset(S > 10)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = País), alpha=0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = País), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de artistas a América") +
  xlab("Número de álbuns > 10") +
  ylab("Diversidade nos países") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("4.América_pais_artist.png",width = 15, height = 8, dpi = 600)

### Europa
#### Continente + artista
##### Tabela
p2 <- subset(planilhatotal, Continente == "Europa")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Região!="Vários")
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies
local<-reshape2::dcast(p2, País ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, País ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%   subset(S > 10)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = País), alpha=0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = País), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de artistas na Europa") +
  xlab("Número de artistas > 10") +
  ylab("Diversidade nos países") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("4.Europa_pais_artist.png",width = 15, height = 8, dpi = 600)

### Continente + álbum
##### Tabela
p2 <- subset(planilhatotal, Continente == "Europa")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Região!="Vários")
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies
local<-reshape2::dcast(p2, País ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, País ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%   subset(S > 10)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = País), alpha=0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = País), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de artistas na Europa") +
  xlab("Número de álbuns > 10") +
  ylab("Diversidade nos países") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("4.Europa_pais_artist.png",width = 15, height = 8, dpi = 600)

### Ásia
#### Continente + artista
##### Tabela
p2 <- subset(planilhatotal, Continente == "Ásia")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Região!="Vários")
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies
local<-reshape2::dcast(p2, País ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, País ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%   subset(S > 2)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = País), alpha=0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = País), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de artistas na Ásia") +
  xlab("Número de artistas > 2") +
  ylab("Diversidade nos países") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("4.Ásiaa_pais_artist.png",width = 15, height = 8, dpi = 600)

#### Continente + álbum
##### Tabela
p2 <- subset(planilhatotal, Continente == "Ásia")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Região!="Vários")
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies
local<-reshape2::dcast(p2, País ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, País ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%   subset(S > 2)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = País), alpha=0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = País), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de países na Ásia") +
  xlab("Número de álbuns > 2") +
  ylab("Diversidade nos álbuns") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("4.Ásia_pais_artist.png",width = 15, height = 8, dpi = 600)

### África
#### Continente + artista
##### Tabela
p2 <- subset(planilhatotal, Continente == "África")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Região!="Vários")
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies
local<-reshape2::dcast(p2, País ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, País ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
#local <- local %>%   subset(S > 2)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = País), alpha=0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = País), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de artistas") +
  xlab("Número de artistas") +
  ylab("Diversidade nos países") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("4.África_pais_artist.png",width = 15, height = 8, dpi = 600)

#### Continente + álbum
##### Tabela
p2 <- subset(planilhatotal, Continente == "África")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Região!="Vários")
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies
local<-reshape2::dcast(p2, País ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, País ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
#local <- local %>%   subset(S > 10)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = País), alpha=0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = País), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de artistas") +
  xlab("Número de álbuns") +
  ylab("Diversidade nos países") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("4.América_pais_artist.png",width = 15, height = 8, dpi = 600)

## Pais
### EUA
#### Estado + artista
##### Tabela
p2 <- subset(planilhatotal, País == "EUA")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Região!="Vários")
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies
local<-reshape2::dcast(p2, Estado ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, Estado ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%   subset(S > 10)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Estado), alpha=0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Estado), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de artistas") +
  xlab("Número de artistas > 10") +
  ylab("Diversidade nos estados") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("4.USA_estad_artist.png",width = 15, height = 8, dpi = 600)

#### Estado + álbum
##### Tabela
local<-reshape2::dcast(p2, Estado ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, Estado ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%   subset(S > 40)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Estado), alpha=0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Estado), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de álbum") +
  xlab("Número de álbum > 40") +
  ylab("Diversidade nos estados") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("4.USA_estad_album.png",width = 15, height = 8, dpi = 600)

### Brasil
#### Estado + artista
##### Tabela
p2 <- subset(planilhatotal, País == "Brasil")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Região!="Vários")
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies
local<-reshape2::dcast(p2, Estado ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, Estado ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%   subset(S > 10)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Estado), alpha=0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Estado), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de artistas") +
  xlab("Número de artistas > 10") +
  ylab("Diversidade nos estados") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("4.BR_estad_artist.png",width = 15, height = 8, dpi = 600)

#### Estado + álbum
##### Tabela
local<-reshape2::dcast(p2, Estado ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, Estado ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%   subset(S > 30)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Estado), alpha=0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Estado), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de álbum") +
  xlab("Número de álbum > 30") +
  ylab("Diversidade nos estados") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("4.BR_estad_album.png",width = 15, height = 8, dpi = 600)

### Inglaterra
#### Estado + artista
##### Tabela
p2 <- subset(planilhatotal, País == "Inglaterra")
p2 <- subset(p2,Continente!="Vários") 
p2 <- subset(p2,Subcontinente!="Vários") 
p2 <- subset(p2,Região!="Vários")
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies
local<-reshape2::dcast(p2, Estado ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, Estado ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%   subset(S > 3)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Estado), alpha=0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Estado), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de artistas") +
  xlab("Número de artistas > 3") +
  ylab("Diversidade nos estados") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("4.Engl_estad_artist.png",width = 15, height = 8, dpi = 600)

#### Estado/álbum
##### Tabela
local<-reshape2::dcast(p2, Estado ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, Estado ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%   subset(S > 10)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Estado), alpha=0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Estado), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de álbum") +
  xlab("Número de álbum > 10") +
  ylab("Diversidade nos estados") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("4.Engl_estad_album.png",width = 15, height = 8, dpi = 600)

### Canadá
#### Estado + artista
##### Tabela
p2 <- subset(planilhatotal, País == "Canadá")
p3 <- subset(planilhatotal, País == "Canadá/EUA")
p2 <- rbind(p2,p3)
#p2 <- subset(p2,Continente!="Vários") 
#p2 <- subset(p2,Subcontinente!="Vários") 
#p2 <- subset(p2,Região!="Vários")
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies
local<-reshape2::dcast(p2, Estado ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, Estado ~ Artista, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%   subset(S > 3)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Estado), alpha=0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Estado), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de artistas") +
  xlab("Número de artistas > 3") +
  ylab("Diversidade nos estados") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("4.Can_estad_artist.png",width = 15, height = 8, dpi = 600)

#### Estado + álbum
##### Tabela
local<-reshape2::dcast(p2, Estado ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)

##### Índices de diversidade
##### Abundância
abund<-rowSums(local) #abunância por faixa
abund
###### Diversidade de Shanon
H <- diversity(local)
H #shannon
###### Dominancia de Simpson (mede a probabilidade de 2 (dois) individuos, selecionados ao acaso na amostra, pertencer a mesma especie)
simp <- diversity(local, "simpson")
simp #simpson
###### Inverso do Simpson
invsimp <- diversity(local, "inv")
invsimp 
###### Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
unbias.simp <- rarefy(local, 2) - 1
unbias.simp
###### Alphade Fisher
alpha <- fisher.alpha(local)
alpha
##Plot all
#pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")
###### Riqueza
S <- specnumber(local) ## rowSums(BCI > 0) does the same...
S
##### Equabilidade de Pielou (J):
J <- H/log(S)
J

### Gráfico
local<-reshape2::dcast(p2, Estado ~ Álbum, value.var = "Pontos", fun.aggregate = sum)
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%   subset(S > 5)
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = Estado), alpha=0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Estado), size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #geom_boxplot() +
  ggtitle("Diversidade de álbum") +
  xlab("Número de álbum > 5") +
  ylab("Diversidade nos estados") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("4.Can_estad_album.png",width = 15, height = 8, dpi = 600)


