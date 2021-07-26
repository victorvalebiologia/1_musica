# Apresentação
Repositório para testes de scripts em uma tabela pessoal de álbuns. O intuito são análises de correlação. Será dividido da seguinte forma:
- Início;
- Teste de Normalidade;
- Análise de Variância (one-way, wilcox, two-way);
- Regressão linear (simples, polinominal);
- Comparação de médias;
- Cluster;
- Correlação;
- Cálculos.

## Início
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

## Testes de Normalidade
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
## Análise de Variância
Primeiro vamos instalar o pacote.
`pacman::p_load(tidyverse, FSA, emmeans)`

Agora vamos ver os grupos por décadas:
`aggregate(Pontos ~ Década, planilhatotal, var)` 

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
### Outra análises de correlação
O pacote Expdes `pacman::p_load(ExpDes)` possui um conjunto de teste de significância CRD que calcula vários testes:
- A variância (Analysis of Variance Table), onde h0 corresponde a não possuir variância;
- Teste de normalidad de Shapiro-Wilk, onde h0 significa dados normal;
- Homogeneidade do teste de variância, onde h0 significa que são homogêneos;
- teste Tukey para comparar as médias e verificar aquelas que diferem. 

Vamos ver para pontos por décadas.
`crd(planilhatotal$Década, planilhatotal$Pontos, mcomp = "tukey", sigF = 0.01, sigT = 0.01)`

Fica claro que os dados variam, não sao normal, que não são homogenos e que 1930 e 2010 se isolam, sendo o resto dos grupos em estados intermediários. O mesmo ocorre para subcontinente, gênero e outros.

### Teste de Wilcoxon
Agora, como comparar as médias em dados não paramétricos, como substituir o anova. Uma forma é o teste de Wilcoxon, que é usado para dados não paramétricos com grande diferença de magnitude para usado para comparar amostras relacionadas, amostras combinadas ou medições repetidas em uma única amostra para avaliar se suas classificações de médias populacionais diferem. Vamos seguir o seguinte [site](https://www.r-bloggers.com/2021/05/wilcoxon-signed-rank-test-in-r/)
Primeiro o apcote. 
`pacman::p_load(ggpubr,reshape, psych, exactRankTests)`

Agora,vamos isolar o conjunto de dados que iremos comparar. No caso vai ser o tipo de álbum, entre principais e extras.
``` 
Principal <- planilhatotal %>% subset(planilhatotal$Classificação == "Principal")
Principal <- Principal$Pontos
Extra <- planilhatotal %>% subset(planilhatotal$Classificação == "Extra")
Extra <- Extra$Pontos
Tipo <- cbind(Principal,Extra)
Tipo <- melt(Tipo)
``` 
Feito isso, vamos ver no gráfico:
``` 
ggboxplot(Tipo, x = "X2", y = "value", alpha = 0.5,
          color = "X2", palette = c("#00AFBB", "#E7B800"), fill = "X2", 
          order = c("Principal", "Extra"),
          ylab = "Valor", xlab = "Groupos")
``` 
Fica mais cláro que a média de álbuns principais é um pouco maior que de extras, mas esses apresenta os maiores outliers. Mas será significativamente diferentes?

Separando os dados, vamos testá-los. Primeiro vamos aleatorizar as amostras e pegar 100 de 500 amostras.
``` 
Principal <- sample(1:500, 100, replace = T)
Extra <- sample(1:500, 100, replace = T)
``` 
Agora vamos testar:
``` 
res <- wilcox.exact(Principal,Extra, paired = TRUE)
res 
``` 
A hipótese nula é que eles são iguais e isso foi confirmado, já que o p não foi menor que 0,05. Ou seja, apesar da pequena diferença eles podem ser considerados iguais.

### Two-Way Anova
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
repare que é o mesmo do anova two-away. Outro gráfico: 
```
ggplot(planilhatotal,aes(x=Lançado,y=Pontos,color=Continente,fill=Continente)) +
  geom_smooth(method="lm",alpha=0.2) +
  geom_point() +
  theme_classic()
```  

Dados binários também podem ser analisados para vê se ocorre diferença entre eles. 
```  
logreg <- glm(Binário~Pontos,data=planilhatotal,family="binomial")
summary(logreg)  
```  
Que foi o caso, existe mais o dado 1 (que equivale a álbuns principais) do que 0 (que seria o extras). Uma forma d ever isso é por gráfico. 
`fitPlot(logreg)`

Ou um gráfico ggplot
```  
logregdf <- dplyr::select(planilhatotal,Binário,Pontos)
logregdf$fit <- predict(logreg,newdata=logregdf,type="response",interval="confidence")

ggplot(planilhatotal,aes(x=Pontos,y=Binário)) +
  geom_smooth(method="glm",alpha=0.2,
              method.args=list(family="binomial")) +
  geom_point(alpha=0.25) +
  theme_classic ()
```  
Como antes também é possível fazer a diferenciação dos fatores.No caso escolhemos os continentes.
``` 
logreg2 <- glm(Binário~Pontos*Continente,data=planilhatotal,family="binomial")

ggplot(planilhatotal,aes(x=Pontos,y=Binário,color=Continente,fill=Continente)) +
  geom_smooth(method="glm",alpha=0.2,
              method.args=list(family="binomial")) +
  geom_point(alpha=0.25) +
  theme_classic()
``` 

### Regressão polinomial
Em casos que a regressão não é uma reta pode-se fazer um modelo de regressão para duas variáveis, sendo uma dependente e uma independente. A variável independente é expandida num polinômio com geração de novas variáveis. No nosso caso foi o ano de lançamento.
```  
poly2 <- lm(Pontos~Lançado+I(Lançado^2),data=planilhatotal)
summary(poly2)
``` 

Note o gráfico a forte tendência de queda na média de pontos nos anos mais recentes.
`FSA::fitPlot(poly2,interval="confidence")`

O mesmo para o gráfico em ggplot que pode ser desmembrado em fatores. 
``` 
polydf <- dplyr::select(planilhatotal,Lançado,Pontos)
polydf <- cbind(polydf,predict(poly2,newdata=polydf,interval="confidence"))

ggplot(planilhatotal,aes(x=Lançado,y=Pontos)) +
  geom_smooth(method="lm",formula="y~x+I(x^2)",alpha=0.2) +
  geom_point() +
  theme_classic()
``` 
### Gráficos
Um exemplo de gráfico mais elaborado que mede a regressão linear de pontos por ano de lançamento por continente. 
``` 
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
``` 
Ou um gráfico entre os tipos de álbum. 
``` 
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
``` 

Agora uma correlação entre a base de ponto com o sistema de correção dos tipos de gráficos:
``` 
ggplot(planilhatotal, aes(x=Correção , y = Base, colour = Classificação)) + 
  geom_point(aes(shape = Tipo, size = Soma), alpha = 0.3) +
  #geom_line(aes(colour = Tipo), alpha = 0.3) +
  scale_shape_manual(values = 0:15) +
  geom_smooth(method = lm,se = FALSE, alpha = 0.6, aes(colour = Classificação)) + 
  scale_size(range = c(.1, 12), name="Música por álbum") +
  #geom_count(aes(x = Avaliação, y = Avaliação.1), colour = "black") +
  theme_classic() #correlação negativa
#ggsave("1.Point_Base_Corre.png",width = 10, height = 6, dpi = 300)
``` 

## Comparação de médias
Visto as anãlises de normalidade, variância e regressão, passamos para comparações de médias. Primeiro os pacotes:
``` 
pacman::p_load(jtools, sandwich, lme4, ggstance, vegan, rpart) 
require(jtools,rpart,vegan)
``` 
Primeiro é necessário ver se essas médias diferem. Escolhemos agora pontos por gênero. Primeiro um total com uma ANOVA simples, como foi para continentes.
`summary(aov(planilhatotal$Pontos ~ factor(planilhatotal$Gênero)))`
E agora um por fator:
``` 
a<-lm(planilhatotal$Pontos ~ factor(planilhatotal$Gênero)) 
summ(a)
``` 
Uma boa forma de ver se as médias de fatores são diferentes são jogando em um gráfico. 
``` 
b<-glm(Pontos ~ Gênero, data = planilhatotal)
plot_summs(b, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)
``` 
Note que rock está mais perto da média, é o esperado por ser o ritmo com a maior quantidade de pontos e, consequentemente, mais responsável pela aferição da média. Experimental e Folklore, por possuirem poucos álbuns, apresenta uma grande variância. Agora, vamos ver por tipo de álbum. 
``` 
b <- lm(Pontos ~ Tipo, data = planilhatotal) #some os primeiros de cada grupo
summ(b)
``` 
Primeiro verificamos em tabela e agora o gráfico.
``` 
effect_plot(b, pred = Order, interval = TRUE, plot.points = TRUE) #não faz sentido
plot_summs(b, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)
``` 

## Cluster
Outra forma de ver os dados é fazer uma análise de similaridade de Jaccard e plotar em um cladograma.
`pacman::p_load("ade4")`

Vamos ver se existe alguma similaridade entre os gêneros considerando os países que possuem álbuns nos diversos gêneros. 
``` 
local<-reshape2::dcast(planilhatotal, Gênero ~ País, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) 
#method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Música/5.clust_gen_pais.png",width=800,height=600) #local e tmamanho
plot(hc, labels=local$ID)    # plot the dendrogram
#dev.off()
``` 
Nota-se que alguns grupos foram criados. Um com gêneros mais mainstream sendo irmão de um de música americana. Outro com ritmos brasileiros. O mesmo pode ser realizado com décadas, subcontinente e outros. Vamos testar agora um agrupamento de países por categoria de gêneros. Primeira vamos filtrar os dados retirando vários.
``` 
p2 <- planilhatotal <- subset(planilhatotal,Subcontinente!="Varios") #tirar Vários de subcontintete
p2 <- p2 %>%  subset(Pontos > 8) 
#planilhaalbum <- subset(planilhaalbum,Tipo!="Coletânea") #tirar n/a da espécies
``` 
Agora o cluster:
``` 
pacman::p_load("ade4")
local<-reshape2::dcast(p2, País ~ Gênero, value.var = "Pontos", fun.aggregate = sum)
local=data.frame(local, row.names=1)
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) #method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Música/5.clus_pais_categ.png",width=800,height=600) #local e tmamanho
plot(hc, labels=local$ID)    # plot the dendrogram
#dev.off()
``` 
USA e Inglaterra ficaram próximos, por serem os países com mais dados, sendo a maior proporção de rock aioria de categorias de rock.

## Correlação
Análises de correlação podem ser positivas ou negativas. Aqui, usando apenas variáveis numéricas, vamos comparar pontps e ano.
``` 
planilhatotal %>% 
  subset(Lançado >= 0) %$% #escolher a partir dos dados 
  cor(Pontos, Lançado) #correlação dos dados selecionados, numéricos
```   
Percebe-se uma correlação negativa entre os pontos e as datas. Agora, graficamente isso pode ser mai fácil de ser entendido, seguiremos segido o site a seguir. [site](https://www-r--bloggers-com.cdn.ampproject.org/v/s/www.r-bloggers.com/2021/05/correlation-analysis-different-types-of-plots-in-r/amp/?amp_gsa=1&amp_js_v=a6&usqp=mq331AQFKAGwASA%3D#amp_tf=De%20%251%24s&aoh=16209293593076&csi=0&referrer=https%3A%2F%2Fwww.google.com&ampshare=https%3A%2F%2Fwww.r-bloggers.com%2F2021%2F05%2Fcorrelation-analysis-different-types-of-plots-in-r%2F)

Primeiro os pacotes
``` 
remotes::install_github("r-link/corrmorant")
pacman::p_load(corrmorant,tidyverse,dplyr)
``` 
Vamos ver se a raiz musical são correlacionados pelos países. 
``` 
cor<-reshape2::dcast(planilhatotal, Gênero ~ Continente, value.var = "Pontos", fun.aggregate = sum)
cor=data.frame(cor, row.names=1)
ggcorrm(data = cor) +
  lotri(geom_point(alpha = 0.5)) +
  lotri(geom_smooth()) +
  utri_heatmap() +
  utri_corrtext() +
  dia_names(y_pos = 0.15, size = 3) +
  dia_histogram(lower = 0.3, fill = "grey80", color = 1) +
  scale_fill_corr() +
  labs(title = "Correlação Continentes por gênero musical")
#ggsave("10.correl_cont_gênero_cor.png",width = 15, height = 8, dpi = 600)
``` 
O 1 é o máximo de correlação positiva e o -1 o máximo correlação negativa. Nota-se que vários são os que apresentam maios correlação com América, Eurpa e Oceania, por agrupar geralmente países desse continente. Europa e América e Europa e Oceania apresentam alta correlação de gêneros também. 

Um outro modelo de gráfico a seguir, mas agora iremos ver a correlação das raízes musicais de álbuns por país.
``` 
cor<-reshape2::dcast(planilhatotal, País ~ Raiz, value.var = "Pontos", fun.aggregate = sum)
cor=data.frame(cor, row.names=1)
library(PerformanceAnalytics)
#par(mgp=c(1,1,0)) #exportar a imagem
#png(filename="/home/user/Área de Trabalho/Música/10.correl_pais_rais_stat.png",width=800,height=600) #local e tmamanho
chart.Correlation(cor, histogram=TRUE, pch="+")
#dev.off()
``` 
Músicas americanas são bastantes correlacionadas, como esperado.

## Cálculos
Também existe uma forma de calcular a diferença de valores dentro da tabela de dados, no caso, a diferença da média de pontos em diferentes categorias, primeiro por tipo de álbuns e segundo por raiz musical. Vamo seguir o seguinte [site](https://www.r-bloggers.com/2021/06/simple-tricks-for-debugging-pipes-within-magrittr-base-r-or-ggplot2/)
Primeiro os pacotes.
``` 
pacman::p_load(tidyverse, tidyverse.quiet, dplyr)
options(tidyverse.quiet = TRUE)
library(tidyverse)
options(dplyr.summarise.inform = FALSE)
``` 
Agora, vamos construir uma tabela com essa diferença entre os tipos de álbum por continente.
``` 
p2 <- planilhatotal
p2 %>% 
  mutate(mean_height = mean(Lançado, na.rm = TRUE)) %>% 
  group_by(Continente, Classificação) %>% 
  summarize(mean_height_species_gender = mean(Lançado, na.rm = TRUE),
    mean_height = first(mean_height)) %>% 
  mutate(diff_mean_height = mean_height_species_gender - mean_height) %>% 
  dplyr::select(Classificação, Continente, diff_mean_height) %>%
  pivot_wider(names_from = 'Classificação', values_from = 'diff_mean_height', values_fill = NA)   
##### identity() para separar os termos
``` 
Nessa tabela percebemos que existe diferença entre álbum principal e extra em todos os continentes, principalmente aqueles bem amostrados. Para América, por exemplo, a média de álbuns extra e menor que a média geral, já para principal é ligeiramente maior.

Vamos ver agora considerando os continentes e a raiz musical. 
``` 
p2 <- planilhatotal
p3 <- p2 %>% 
  mutate(mean_height = mean(Pontos, na.rm = TRUE)) %>% 
  group_by(Continente, Raiz) %>% 
  summarize(mean_height_species_gender = mean(Pontos, na.rm = TRUE),
    mean_height = first(mean_height)) %>% 
  mutate(diff_mean_height = mean_height_species_gender - mean_height) %>% 
  dplyr::select(Raiz, Continente, diff_mean_height) %>%
  pivot_wider(names_from = 'Raiz', values_from = 'diff_mean_height', values_fill = NA)   
p3
``` 
O mesmo se dá com música anglo-ameriana, onde para quase todos os continentes apresentam a média de pontos maior. Já musica européia apenas na Ásia a sua média de pontos é maior que o restante (este principalmente composto por música anglo-americana).

Vamos ver no gráfico de correlação usado anteriormente:
``` 
ggcorrm(data = p3) +
  lotri(geom_point(alpha = 0.5)) +
  lotri(geom_smooth()) +
  utri_heatmap() +
  utri_corrtext() +
  dia_names(y_pos = 0.15, size = 3) +
  dia_histogram(lower = 0.3, fill = "grey80", color = 1) +
  scale_fill_corr() +
  labs(title = "Correlação Raiz Musical")
``` 
Nesse gráfico fica mais fácil perceber que apesar das diferenças de média de pontos, música anglo-americana está mais próximo de música altina e europeis do que música afro-americana.
