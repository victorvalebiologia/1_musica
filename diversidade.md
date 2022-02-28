# Apresentação
Repositório para testes de scripts em uma tabela pessoal de álbuns. O intuito são análises de diversidade. Será dividido da seguinte forma:
- Início;
- Acumulação;
- Série Temporal;
- Diversidade.

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
pacman::p_load(ggplot2, devtools, ggrepel, graphics,lubridate) 
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
## Acumulação
Primeiro a data:

```
p2 <- planilhatotal

p2 <- subset(p2, !is.na(Lançado))
p2 <- subset(p2, !is.na(Mês))
p2 <- subset(p2, !is.na(Dia))

p3 <- p2 %>% 
  select(Lançado,Mês,Dia) %>% 
  mutate(Data = make_date(Lançado,Mês,Dia))
Data <- data.frame(p3,p2)
```
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
Agora vamos ver isso em um gráfico simples de acumulação.
```
sp4<-specaccum(acum,method="collector")
spAbund<-rowSums(acum)

acum<-reshape2::dcast(Data, Data ~ Álbum, value.var = "Pontos", fun = length)
p3<-data.frame(spAbund,sp4$sites,sp4$richness,acum)

ggplot(p3, aes(x = Data, y = sp4.richness)) + 
  geom_line(size=6, alpha=0.6, color="Gray") + #geom_line(aes(group = sp4.sites))
  geom_point(aes(size=spAbund, colour=sp4.richness), alpha=0.3) +
    geom_label_repel(aes(label = sp4$richness), size=4, alpha=0.8, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  scale_size(range = c(.1, 24), name="Abundância de registros") +
  #geom_text(aes(label = a$sp4.richness),col = 'black',size = 5) +
  labs(title="Curva do coletor", subtitle="Total", y="Riqueza",x="Data", caption="",
       color = "Riqueza", size = "Abundância de registros") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) + theme_classic() 
#ggsave("2.Acumul_álbum_count.png",width = 14, height = 6, dpi = 300)
```


## Série Temporal
Vamos ver a riqueza de dados ao passar dos anos. Essa riqueza pode ser de:
- Álbuns;
- Artistas.

Primeiro vamos separar a tabela.
```
p2 <- planilhatotal
local<-reshape2::dcast(p2, Lançado ~ Álbum) 
local=data.frame(local, row.names=1)
```
Agora vamos ver alguns índices simples, como abundância e riqueza:
```
abund<-rowSums(local) #abunância por faixa
S <- specnumber(local) 
```
E vamos plotar em gráfico:
```
local<-reshape2::dcast(p2, Lançado ~ Artista)

local<-data.frame(S,abund,local)
p3 <- ggplot(local, aes(x = Lançado, y = S)) + 
  geom_point(aes(size=abund, colour = S), alpha = 0.4) + 
  geom_line(aes(colour=S)) +
  scale_size(range = c(.1, 18), name = "Número de álbuns") +
  geom_label_repel(aes(label = Lançado), size=4, alpha= 1,box.padding   = 0.35,point.padding = 0.75,segment.color = 'grey50') +
  labs(title="Número de álbuns por ano", subtitle="",y="Número de álbuns",x="Ano de lançamento", caption="",color = "Número de artistas",size = "") +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic()
#ggsave("2.Ano_count.png",width = 15, height = 8, dpi = 600)
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
p2 <- subset(p2,Classificação!="Extra") 
p2 <- subset(p2,Coletivo!="Vários")

p2 <- subset(p2, País == "Brasil")
p2 <- subset(planilhatotal,Artista!="Various Artist") 
p2 <- subset(p2, Continente == "Europa")
p2 <- subset(p2, Subcontinente == "E. Meridional")
p3 <- subset(p2, Continente == "Oceania")
p4 <- subset(p2, Continente == "Ásia")
p2 <- rbind(p3,p4)

p2 <- subset(p2, !is.na(Estado))
```
Agora a tabela a ser analisada:
```
local<-reshape2::dcast(p2, Gênero ~ Álbum, value.var = "Soma", fun.aggregate = sum)
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
local<-reshape2::dcast(p2, Gênero + Raiz ~ Álbum, value.var = "Soma", fun.aggregate = sum) 
local<-data.frame(local, H, simp, S, J, abund)
local <- local %>%
  subset(S > 20)
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
ggplot(local, aes(x = reorder(Raiz, S), y = S)) +
  #geom_point(aes(size=abund, colour = Gênero), alpha = 0.85) + scale_size_binned(range = c(.1, 16)) +
  geom_col(aes(fill = Gênero), alpha = 0.65) + 
  
  #geom_label(aes(y = S, x = Gênero, label = S), position = position_stack(vjust = 0.99), size=3.5, alpha= 0.85) +
  #geom_text_repel(aes(y = S, x = Gênero, label = abund), position = position_dodge(width = 0), vjust=1.75, size=3, colour = "red") +
  labs(title="Ranking de importância", subtitle="", y="Número de álbuns", x="Raiz", caption="2022_02_29", size = "Número de faixas", fill = "Gênero") +
  
  scale_y_continuous(breaks = c(0, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000, 2200)) + #0, 50, 100, 150, 200, 250, 300, 350, 400)) +
  theme_classic() +
  theme(legend.position="bottom") + #axis.title = element_text(size = 15), axis.text = element_text(size = 12), legend.position="bottom") 
        coord_flip() 
#ggsave("2022_02_28_gen2.png",width = 17, height = 8, dpi = 600)
```

Outro gráfico:
```
ggplot(local, aes(x = S, y = H)) + 
  geom_point(aes(size=abund, colour = País), alpha = 0.65)+ #size=abund
  scale_size(range = c(.1, 18), name = "Pontos") +
  geom_label_repel(aes(label = Estado, size=4, alpha= 1, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50')) +
  geom_text(aes(label = S), size=4, alpha= 1) +
  #labs(title="Número de artidtas por país > 20", subtitle="", y="Diversidade de artistas",x="Número de álbuns", caption="",
       #color = "Principais países", size = "Número de artistas") +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  theme_classic() 
#ggsave("3.Pais_artist.png",width = 15, height = 8, dpi = 600)
```
