# Início
Repositório para testes de scripts em uma tabela pessoal de álbuns. O intuito são análises de correlação.

Primeiro, vamos indicar as pastas corretas.
```
getwd()
setwd("/home/user/Área de Trabalho/Música") 
```

Agora baixar e ler alguns pacotes básicos.

```
if(!require(pacman, quietly = TRUE))(install.packages("pacman")) #agrupador de funções
pacman::p_load(magrittr,dplyr) #magrittr para operações de pipe/dplyr para manipulador de dados
pacman::p_load(ggplot2, devtools, ggrepel, graphics) 
pacman::p_load(vegan)  #vegan para estatística ecológica/graphics para os gráficos

```
Agora vamos adicionar a planilha.
```
pacman::p_load(openxlsx) 
caminho.do.arquivo <- "/home/user/Área de Trabalho/Música/musica_estatistica.xlsx"
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

# Testes de Normalidade
Primeiro o pacote para isso.

`pacman::p_load(fitdistrplus)` 

Primeiro é importante identificar as distribuções teórica dos dados, uma formaé o diagrama deCullen & Frey. Este gráfico é utilizado como uma alternativa possível para determinação da distribuição de probabilidade dos dados, considerando em um dos eixos a curtose e no outro o quadrado da assimetria (CULLEN e FREY, 1999).

Primeiro para a data de lançamento que estão distribuído entre 2,230 e 19,636, tendo mediana de 7,545 e média em 7,970.
`descdist(planilhatotal$Pontos, boot = 500, discrete = F, graph = T)` 
Também pode se fazer com o ano de lançamento que estão entre 1937 a 2021, com mediana em 1994 e média em 1992,2. Para ambos os dados foram considerados a distribuição Beta, Gama e Weibull.Também represeta que os dados estão próximos da normalidade. Vamos então ver se elas estão distruídos . 
```
fit.MF.normal <- fitdist(planilhatotal$Pontos, "norm") #gráfico de distribuição normal
plot(fit.MF.normal)
```
Fica bem clato a média em 7,5 e que elas estão bem próximos da distribuição normal. Já para o ano de lançamento existe uma diferença, tendo dois períodos de picos.
```
fit.MF.normal <- fitdist(planilhatotal$Lançado, "norm") #gráfico de distribuição normal
plot(fit.MF.normal)
```

Pensando em pontos, será que se excluir coletâneas e singles a distribuição mudaria? primeiro vamos selecionar os dados.
```
p2 <- planilhatotal
p2 <- subset(p2,Tipo!="Coletânea") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Single") #tirar n/a da espécies
p2 <- subset(p2,Tipo!="Bonus") #tirar n/a da espécies
```

# Análise de Variância
Primeiro vamos instalar o pacote.
`pacman::p_load(tidyverse, FSA, emmeans)`

Agora vamos ver os grupos por décadas:
`aggregate(Pontos ~ Década, planilhaalbum, var)` 

Nota-se que as médias de pontos nas décadas não são o mesmos.Um teste de homogeneidade de variância de Bartlett vai testar se há desigualdade. A hipótese nula é que são iguais, se o p for menos que 0,05 esta hipótese foi negada, que foi o caso.
`bartlett.test(Pontos ~ Década, planilhatotal)`

Agora, será que ocorre a mesma distribuição de pontos dos discos entre os continentes. Para responder isso vamos seguir a diretriz desse [site](http://derekogle.com/fishR/2021-05-25-fitPlot-replacement)
Primeiro vamos testar uma ANOVA. A Análise de Variância (ANOVA) trata-se de um método estatístico que permite realizar comparações simultâneas entre duas ou mais médias, ou seja, permite testar hipóteses sobre médias de distintas populações. Os pressuposto são: 
- Todas as observações devem ser independentes; 
- As observações em cada grupo devem possuir uma distribuição, aproximadamente normal;
- As variâncias em cada grupo devem ser aproximadamente iguais.

```
dicalt <- lm(Pontos ~ Continente, data = planilhatotal) 
anova(dicalt) 
```
Nesse teste a h0 significa não variável, o que foi estatisticamente não confirmado. Note forma considerados cinco continentes e que o F é diferença das médias. Uma outra forma de ver é pelo summary.
`summary(dicalt)`

Aqui, percebe-se que dentro da cada fator, noc aso continentes, nem todos apresentam variância, como no continente Americano. Porém, no conjunto entre os fatores existe uma diferença significativa entre elas. Ainda, outro parâmetro para se notar é o R². O R-quadrado (coeficiente de determinação) é uma medida estatística de quão próximos os dados estão da linha de regressão ajustada. Os valores são entre 0 e 1, onde 0 indica que o modelo não explica nada da variabilidade dos dados de resposta ao redor de sua média e 1 o contrário. Nesse caso os continentes não explicariam bem a diferença, apesar dela existir. Gênero, país e mesmo o artista tem um poder de explicação maior.

Também da para verificar o grau de liberdade dessa análise.
```
sumdata <- planilhatotal %>%
  dplyr::group_by(Continente) %>%
  dplyr::summarize(n=dplyr::n(),
                   mn=mean(Pontos),
                   se=FSA::se(Pontos)) %>%
  dplyr::mutate(lci=mn-qt(0.975,df=n-1)*se,
                uci=mn+qt(0.975,df=n-1)*se)
sumdata
```
Com essa tabela conseguimos ver quantos fatores foram contabilizados e o grau de liberdade superior (uci) e inferior (lci). Note que América, possuindo um n maior, apresentar os menores valores de uci e lci. O mesmo ocorre se trocarmos por Gênero ou Idioma. Uma boa forma de ver esses valores é em um gráfico, onde fica bem evidente a média e a grande diferença de grau de liberdade entre elas.
`FSA::fitPlot(dicalt)`

```
ggplot(data=sumdata,mapping=aes(x=Continente)) +
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.1) +
  geom_line(aes(y=mn,group=1)) +
  geom_point(aes(y=mn)) +
  theme_classic()
```


## Two-Way Anova
Quando são duas variáveis a serem comparadas. Para	conhecer	os	efeitos	isolados	de	cada	um	dos	factores	testam-se	as	
hipóteses:	
Para	o	factor	A:	
- H0 =	as	médias	dos	grupos	do	factor	A	são	iguais	
- H1 =	as	médias	dos	grupos	do	factor	A	são	diferentes	

Para	o	factor	B:	
- H0 =	as	médias	dos	grupos	do	factor	B	são	iguais	
- H1 =	as	médias	dos	grupos	do	factor	B	são	diferentes	

E para	conhecer	o	efeito	dos	2	factores	em	conjunto	testam-se	as	seguintes	hipóteses:	
- H0 = não	existe	interacção	entre	o	factor	A	e	o	factor	B	
- H1 =	existe	interacção	entre	o	factor	A	e	os	factor	B	

Dessa forma, quando	o	efeito	interativo	não	é	significativo,	passa-se	para	a	interpretação	dos efeitos	dos	fatores	isolados,	que	podem	ou	não	ser	estatisticamente	 significativos.	Quanto	o	efeito	da	interacção	é	significativo	os	efeitos	isolados	perdem	o	
significado	e	não	devem	ser	interpretados.	A	interacção	significa	que	existem	combinações	dos	dois	factores	que	produzem	efeitos	diferentes	na	variâvel dependente	do	que	aqueles	que	seria	de	esperar	se	os	factores	fossem	considerados	isoladamente.	
```
aov2 <- lm(Pontos~Lançado*Continente,data=planilhatotal)
anova(aov2)
```
No nosso caso, tanto continente quanto o ano de lançamento apresentam interação com os pontos. Uma forma de ver isso é por meio de gráfico simples ou por ggplot.
`FSA::fitPlot(aov2)`

```
pd <- position_dodge(width=0.2)
ggplot(data=planilhatotal,mapping=aes(x=Lançado,y=Pontos,color=Continente)) +  
  stat_summary(fun.data=mean_cl_normal,geom="errorbar",width=0.2,position=pd) + 
  stat_summary(fun=mean,geom="line",aes(group=Continente),position=pd) +  
  stat_summary(fun=mean,geom="point",position=pd) +
  theme_classic()
```

## Regressão Linear
### Regressão Linear simples
Regressão linear é o processo de traçar uma reta através dos dados em um diagrama de dispersão. A reta resume esses dados, o que é útil quando fazemos previsões. Primeiro calculamos a variação com um anova.
```
slr <- lm(Pontos~Lançado,data=planilhatotal)
anova(slr)
```
Em seguida plotamos em gráficos para ver se existe alguma relação positiva, negativa ou nenhuma relação. 

```
FSA::fitPlot(slr,interval="confidence")
slrdf <- dplyr::select(planilhatotal,Lançado,Pontos)
slrdf <- cbind(slrdf,predict(slr,newdata=slrdf,interval="confidence"))
FSA::peek(slrdf,n=6)
```
ou 
```
ggplot(planilhatotal,aes(x=Lançado,y=Pontos)) +
  geom_smooth(method="lm",alpha=0.2) +
  geom_point()+
  theme_classic()
```  
Nesse caso vimos que existe uma relação negativa, a média de pontos diminui com o passar do tempo. POdemos também observar a regressão por fator.
```  
ivr <- lm(Pontos~Lançado*Continente,data=planilhatotal)
anova(ivr)
FSA::fitPlot(ivr,interval="confidence")
```  
ou 
```
ggplot(planilhatotal,aes(x=Lançado,y=Pontos,color=Continente,fill=Continente)) +
  geom_smooth(method="lm",alpha=0.2) +
  geom_point() +
  theme_classic()
```  
## Regressão lógica (para dados binários - 0 e 1) - 
logreg <- glm(Binário~Pontos,data=planilhatotal,family="binomial")
summary(logreg)  

#### Gráfico
fitPlot(logreg)

### Manual
logregdf <- dplyr::select(planilhatotal,Binário,Pontos)
logregdf$fit <- predict(logreg,newdata=logregdf,type="response",interval="confidence")

#### Gráfico
FSA::peek(logregdf,n=6)

##### Normal
#ggplot(logregdf,aes(x=Pontos)) +
  geom_point(aes(y=Binário),alpha=0.25) +
  geom_line(aes(y=fit),size=1)
  
#### Sombra
ggplot(planilhatotal,aes(x=Pontos,y=Binário)) +
  geom_smooth(method="glm",alpha=0.2,
              method.args=list(family="binomial")) +
  geom_point(alpha=0.25)

#### Diferenciar fatores
logreg2 <- glm(Binário~Pontos*Continente,data=planilhatotal,family="binomial")

ggplot(planilhatotal,aes(x=Pontos,y=Binário,color=Continente,fill=Continente)) +
  geom_smooth(method="glm",alpha=0.2,
              method.args=list(family="binomial")) +
  geom_point(alpha=0.25)
  
## Regressão polinomial
poly2 <- lm(Pontos~Lançado+I(Lançado^2),data=planilhatotal)
summary(poly2)

### Gráficos
FSA::fitPlot(poly2,interval="confidence")

polydf <- dplyr::select(planilhatotal,Lançado,Pontos)
polydf <- cbind(polydf,predict(poly2,newdata=polydf,interval="confidence"))
#ggplot(polydf,aes(x=Lançado)) +
  geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.2) +
  geom_line(aes(y=fit),size=1) +
  geom_point(aes(y=Pontos))
  
ggplot(planilhatotal,aes(x=Lançado,y=Pontos)) +
  geom_smooth(method="lm",formula="y~x+I(x^2)",alpha=0.2) +
  geom_point()  
  
## Gráfico modelo final
### Pontos por Lançado
ggplot(planilhatotal,aes(x=Lançado,y=Pontos,color=Continente,fill=Continente)) +
  geom_smooth(method="lm",alpha=0.1,size=1.25) +
  geom_point(size=1.5) +
  scale_y_continuous(name="Pontos", #limits=c(0,0.5),
                     expand=expansion(mult=0)) +
  scale_x_continuous(name="Ano de Lançamento", #limits=c(0,15),
                     expand=expansion(mult=0)) +
  #scale_color_manual(values=c("#E69F00","#0072B2")) +
  #scale_fill_manual(values=c("#E69F00","#0072B2")) +
  theme_bw() +
  theme(panel.grid.major=element_line(color="gray90",linetype="dashed"),
        panel.grid.minor=element_blank(),
        axis.title=element_text(size=rel(1.25)),
        axis.text=element_text(size=rel(1.1)),
        legend.position=c(0,1),
        legend.justification=c(-0.05,1.02),
        legend.title=element_blank(),
        legend.text=element_text(size=rel(1.1)))

### Pontos por classificação
ggplot(planilhatotal,aes(x=Lançado,y=Pontos,color=Classificação,fill=Classificação)) +
  geom_smooth(method="lm",alpha=0.1,size=1.25) +
  geom_point(aes(size = Soma), alpha = 0.35) + #(size=1.5) +
  scale_y_continuous(name="Pontos", #limits=c(0,0.5),
                     expand=expansion(mult=0)) +
  scale_x_continuous(name="Ano de Lançamento", #limits=c(0,15),
                     expand=expansion(mult=0)) +
  #scale_color_manual(values=c("#E69F00","#0072B2")) +
  #scale_fill_manual(values=c("#E69F00","#0072B2")) +
  theme_bw() +
  theme(panel.grid.major=element_line(color="gray90",linetype="dashed"),
        panel.grid.minor=element_blank(),
        axis.title=element_text(size=rel(1.25)),
        axis.text=element_text(size=rel(1.1)),
        legend.position=c(0,1),
        legend.justification=c(-0.05,1.02),
        legend.title=element_blank(),
        legend.text=element_text(size=rel(1.1)))

### Limites outros
ggplot(planilhatotal, aes(x=Correção , y = Base, colour = Classificação)) + 
  geom_point(aes(shape = Tipo, size = Soma), alpha = 0.3) +
  #geom_line(aes(colour = Tipo), alpha = 0.3) +
  scale_shape_manual(values = 0:15) +
  geom_smooth(method = lm,se = FALSE, alpha = 0.6, aes(colour = Classificação)) + 
  scale_size(range = c(.1, 12), name="Música por álbum") +
  #geom_count(aes(x = Avaliação, y = Avaliação.1), colour = "black") +
  theme_classic() #correlação negativa
#ggsave("1.Point_Base_Corre.png",width = 10, height = 6, dpi = 300)

# Comparação de médias
##### Pacotes
pacman::p_load(jtools, sandwich, lme4, ggstance, vegan, rpart) 
require(jtools,rpart,vegan)

### Gráficos
summary(aov(planilhatotal$Pontos ~ factor(planilhatotal$Gênero))) 
a<-lm(planilhatotal$Pontos ~ factor(planilhatotal$Gênero)) 
#pacman::p_load(car)
#avPlots(a)
#plot(a, which=c(1,2,3,4,5,6))
summ(a)
#summ(a, robust = "HC1")
#summ(a, scale = TRUE, n.sd = 2)
#summ(a, center = TRUE)
#summ(a, confint = TRUE, digits = 3)
#glm(planilhatotal$Altitude ~ planilhatotal$Species)

##### Gênero
b<-glm(Pontos ~ Gênero, data = planilhatotal)
#summ(b)
#summ(b, exp = TRUE)
#c<-lmer(planilhatotal$Altitude ~ planilhatotal$Species) #randomização, não deu certo
#jtools::plot_summs(b)
#plot_summs(b)
#plot_summs(b, scale = TRUE)
#plot_summs(b, scale = TRUE, inner_ci_level = .9)
plot_summs(b, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)
#pacman::p_load(car)
#avPlots(b)

##### Tipo
fit <- lm(Pontos ~ Tipo, data = planilhatotal) #some os primeiros de cada grupo
summ(fit)
effect_plot(fit, pred = Order, interval = TRUE, plot.points = TRUE) #não faz sentido
plot_summs(fit, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)


# Significância
### Teste de significância CRD (CV, Shapiro-Wilk, Homogenety, Tykey)
##### Pacotes - Expdes - pacote de análise, normalidade, Turkey, shapiro-wilk e afins
pacman::p_load(ExpDes) 

##### CRD
crd(planilhatotal$Década, planilhatotal$Pontos, mcomp = "tukey", sigF = 0.01, sigT = 0.01) 
##### Variância + Shapiro + homogeneidade + agrupamento

crd(planilhaalbum$Subcontinente, planilhaalbum$Pontos, mcomp = "tukey", sigF = 0.01, sigT #as tabelas tem que ser iguais
    = 0.01)

# Teste de Wilcoxon
#[site](https://www.r-bloggers.com/2021/05/wilcoxon-signed-rank-test-in-r/)
##### Usado para dados não paramétricos com grande diferença de magnitude para usado para comparar amostras relacionadas, amostras combinadas ou medições repetidas em uma única amostra para avaliar se suas classificações de médias populacionais diferem

### Pacotes
pacman::p_load(ggpubr,reshape, psych, exactRankTests)
#install.packages(exactRankTests)

#### Tipo
Principal <- planilhatotal %>% subset(planilhatotal$Classificação == "Principal")
Principal <- Principal$Pontos
Extra <- planilhatotal %>% subset(planilhatotal$Classificação == "Extra")
Extra <- Extra$Pontos
Tipo <- cbind(Principal,Extra)
Tipo <- melt(Tipo)

#### Gráfico
ggboxplot(Tipo, x = "X2", y = "value",
          color = "X2", palette = c("#00AFBB", "#E7B800"), fill = "X2", 
          order = c("Principal", "Extra"),
          ylab = "Valor", xlab = "Groupos")

#### Dados
describeBy(Tipo,Tipo$X2)          

###### 100 de 500 amostras
Principal <- sample(1:500, 100, replace = T)
Extra <- sample(1:500, 100, replace = T)
###### Testar se o segundo valor é maior que o primeiro (, alternative = "less")  para testar se é menor)
res <- wilcox.exact(Principal,Extra, paired = TRUE)
res          
###### Hipótese negada, o primeiro é maior

##### Valor exato
wilcox.exact(Principal, Extra, data = data, paired = TRUE, alternative = "greater")
###### Com base no teste, foi negado o aumento significativa entre Principal para Extra - houve diminuição.          

#### Dados
Live <- planilhatotal %>% subset(planilhatotal$Gravação == "Live")
Live <- Live$Pontos
Estúdio <- planilhatotal %>% subset(planilhatotal$Gravação == "Estúdio")
Estúdio <- Estúdio$Pontos
Tipo <- cbind(Live,Estúdio)
Tipo <- melt(Tipo)

#### Gráfico
ggboxplot(Tipo, x = "X2", y = "value",
          color = "X2", palette = c("#00AFBB", "#E7B800"), fill = "X2", 
          order = c("Live", "Estúdio"),
          ylab = "Valor", xlab = "Groupos")

#### Dados
describeBy(Tipo,Tipo$X2)          

###### 100 de 500 amostras
Live <- sample(1:500, 100, replace = T)
Estúdio <- sample(1:500, 100, replace = T)
###### Testar se o segundo valor é maior que o primeiro (, alternative = "less")  para testar se é menor)
res <- wilcox.exact(Live,Estúdio, paired = TRUE)
res          
###### Hipótese negada, o primeiro é maior (??)

##### Valor exato
wilcox.exact(Live, Estúdio, data = data, paired = TRUE, alternative = "greater")
###### Com base no teste, foi aceito (>0,95) o aumento significativa entre Live para Eestúdio - houve diminuição.         

# Cluster
### Pacotes
pacman::p_load("ade4")

##### Cluster Gênero + país 
local<-reshape2::dcast(planilhatotal, Gênero ~ País, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) 
#method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Música/5.clust_gen_pais.png",width=800,height=600) #local e tmamanho
plot(hc, labels=local$ID)    # plot the dendrogram
#dev.off()

##### Cluster Lançado + Estilo
local<-reshape2::dcast(planilhatotal, Lançado ~ Estilo) #, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) 
#method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Música/5.clust_ano_genr.png",width=800,height=600) #local e tmamanho
plot(hc, labels=local$ID)    # plot the dendrogram
#dev.off()

##### Cluster Década + Gênero
local<-reshape2::dcast(planilhatotal, Década ~ Gênero, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) 
#method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Música/5.clust_decd_gen.png",width=800,height=600) #local e tmamanho
plot(hc, labels=local$ID)    # plot the dendrogram
#dev.off()

##### Cluster País + Categoria
###### Filtro
p2 <- planilhatotal <- subset(planilhatotal,Subcontinente!="Varios") #tirar Vários de subcontintete
p2 <- p2 %>%  subset(Pontos > 8) 
#planilhaalbum <- subset(planilhaalbum,Tipo!="Coletânea") #tirar n/a da espécies

###### Gráfico
pacman::p_load("ade4")
local<-reshape2::dcast(p2, País ~ Categoria, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) #method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Música/5.clus_pais_categ.png",width=800,height=600) #local e tmamanho
plot(hc, labels=local$ID)    # plot the dendrogram
#dev.off()

# Correlação
planilhatotal %>% 
  subset(Lançado >= 0) %$% #escolher a partir dos dados (relação de alt com as anos)
  cor(Pontos, Lançado) #correlação dos dados selecionados, numéricos
  #correlação negativa entre os pontos e as datas
  
#[site](https://www-r--bloggers-com.cdn.ampproject.org/v/s/www.r-bloggers.com/2021/05/correlation-analysis-different-types-of-plots-in-r/amp/?amp_gsa=1&amp_js_v=a6&usqp=mq331AQFKAGwASA%3D#amp_tf=De%20%251%24s&aoh=16209293593076&csi=0&referrer=https%3A%2F%2Fwww.google.com&ampshare=https%3A%2F%2Fwww.r-bloggers.com%2F2021%2F05%2Fcorrelation-analysis-different-types-of-plots-in-r%2F)

##### Pacotes
remotes::install_github("r-link/corrmorant")
pacman::p_load(corrmorant,tidyverse,dplyr)

##### Raiz + país
###### Cálculo
cor<-reshape2::dcast(planilhatotal, País ~ Raiz, value.var = "Pontos", fun.aggregate = sum)
cor=data.frame(cor, row.names=1)
###### Gráfico
corrmorant(cor, style = "binned") +
  theme_dark() +
  labs(title = "Correlations")
#1 max correlação positiva
#-1 max correlação negativa

###### Gráfico 2
ggcorrm(data = cor) +
  lotri(geom_point(alpha = 0.5)) +
  lotri(geom_smooth()) +
  utri_heatmap() +
  utri_corrtext() +
  dia_names(y_pos = 0.15, size = 3) +
  dia_histogram(lower = 0.3, fill = "grey80", color = 1) +
  scale_fill_corr() +
  labs(title = "Correlação Continentes por gênero musical")
#ggsave("10.correl_pais_raiz_cor.png",width = 15, height = 8, dpi = 600)

###### Gráfico 3
library(PerformanceAnalytics)
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Música/10.correl_pais_rais_stat.png",width=800,height=600) #local e tmamanho
chart.Correlation(cor, histogram=TRUE, pch="+")
#dev.off()

##### Continente + Gênero
###### Cálculo
cor<-reshape2::dcast(planilhatotal, Gênero ~ Continente, value.var = "Pontos", fun.aggregate = sum)
cor=data.frame(cor, row.names=1)

###### Gráfico
corrmorant(cor, style = "binned") +
  theme_dark() +
  labs(title = "Correlations")

#1 max correlação positiva
#-1 max correlação negativa

###### Gráfico 2
ggcorrm(data = cor) +
  lotri(geom_point(alpha = 0.5)) +
  lotri(geom_smooth()) +
  utri_heatmap() +
  utri_corrtext() +
  dia_names(y_pos = 0.15, size = 3) +
  dia_histogram(lower = 0.3, fill = "grey80", color = 1) +
  scale_fill_corr() +
  labs(title = "Correlação Continentes por gênero musical")
#ggsave("10.correl_gen_contin_cor.png",width = 15, height = 8, dpi = 600)

###### Gráfico 3
library(PerformanceAnalytics)
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Música/10.correl_gen_contin_stat.png",width=800,height=600) #local e tmamanho
chart.Correlation(cor, histogram=TRUE, pch="+")
#dev.off()



