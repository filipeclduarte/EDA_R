#### Estatística Descritiva ####

# Importando os dados
dados <- read.csv("dados.csv", header = TRUE, sep = "")

# visualizando as 6 primeiras linhas
head(dados)

# visualizando as 6 últimas linhas
tail(head)

# criando novo data frame com algumas variáveis 
vars <- c("id", "sexo", "EstadoCivil", "carreira", "QntDependentes", "idade","temposervico","BaseCalculoMensal")
dados_estat <- dados[, vars]

# vendo as 6 primeiras linhas
head(dados_estat)

# vendo as carreiras - valores únicos
unique(dados_estat$carreira)

##### Exercício: mostre quais são estados civis presentes na base

unique(dados_estat$EstadoCivil)

#####

# sumarizando os dados
summary(dados_estat)

##### Exercício: Mostre o sumário da variável Base de Cálculo Mensal

summary(dados_estat$BaseCalculoMensal)

# Vamos encontrar o NA
which(is.na(dados_estat$temposervico))
which(is.na(dados_estat$BaseCalculoMensal))
dados_estat[c(51, 564, 619, 686, 741),]

# Repetindo o processo de identificação das linhas com NA's
na_ts <-  which(is.na(dados_estat$temposervico))
na_bcm <- which(is.na(dados_estat$BaseCalculoMensal))

# Vamos juntá-los, isto é, concatenaremos os vetores na_ts e na_bcm
na <- c(na_ts, na_bcm)
# Veja que 51 aparece duas vezes
print(na)

# Agora vamos selecionar os valores sem repetições
na_unique <- unique(na)
# Agora sim!
print(na_unique)

# Selecionando as pessoas que possuem NA (informação faltante)
dados_estat[na_unique,]


#####

# média - mean()
mean(dados_estat$idade)
mean(dados_estat$QntDependentes)
mean(dados_estat$temposervico)
mean(dados_estat$temposervico, na.rm = TRUE)
mean(dados_estat$BaseCalculoMensal)

# desvio padrão - sd()
sd(dados_estat$idade)
sd(dados_estat$QntDependentes)
sd(dados_estat$temposervico, na.rm = TRUE)
sd(dados_estat$BaseCalculoMensal)
sd(dados_estat$BaseCalculoMensal, na.rm = TRUE)

# mínimo - min()
min(dados_estat$idade)
min(dados_estat$temposervico, na.rm = TRUE)
min(dados_estat$BaseCalculoMensal)
min(dados_estat$BaseCalculoMensal, na.rm = TRUE)

# máximo - max()
max(dados_estat$idade)
max(dados_estat$temposervico, na.rm = TRUE)
max(dados_estat$BaseCalculoMensal)
max(dados_estat$BaseCalculoMensal, na.rm = TRUE)

# quantidade - se vetor, length(); se for data frame, use nrow() ou str()
length(dados_estat$idade)
nrow(dados_estat)
str(dados_estat)

# quantis - quantile(dados, probs = c(...))
quantile(dados_estat$idade, probs = seq(0.25, 1, 0.25))
quantile(dados_estat$idade, probs = c(0.1, 0.5, 0.9))
quantile(dados_estat$idade, probs = c(0.01, 0.5, 0.99))

##### Exerício: Mostre os mesmos quantis para a Base de Cálculo Mensal 
quantile(dados_estat$BaseCalculoMensal, probs = seq(0.25, 1, 0.25), na.rm = TRUE)
quantile(dados_estat$BaseCalculoMensal, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)
quantile(dados_estat$BaseCalculoMensal, probs = c(0.01, 0.5, 0.99), na.rm = TRUE)
#####

##### Exercício: Calcule a variância para a. Tempo de serviço; b. Base de Cálculo Mensal.
# a.
var(dados_estat$temposervico, na.rm = TRUE)
# b. 
var(dados_estat$BaseCalculoMensal, na.rm = TRUE)
#####

# Vamos criar uma nova tabela onde BaseCalculoMensal é diferente de 0
quantile(dados_estat$BaseCalculoMensal[dados_estat$BaseCalculoMensal != 0], probs = seq(0.25, 1, 0.25), na.rm = TRUE)

# criando um vetor lógico com TRUE quando a Base de Cálculo Mensal for igual a 0.
base0 <- dados_estat$BaseCalculoMensal == 0
base0

# Vamos fazer a seleção das linhas diferentes de base0 - selecionar quem tem base diferente de 0 
quantile(dados_estat$BaseCalculoMensal[!base0], probs = seq(0.25, 1, 0.25), na.rm = TRUE)
quantile(dados_estat$BaseCalculoMensal[!base0], probs = c(0.01, 0.5, 0.99), na.rm = TRUE)

# Exercício: Apresente as pessoas que possuem a Base de Cálculo igual a 0.
base_zero <- which(dados_estat$BaseCalculoMensal == 0)
dados_estat[base_zero,]

# Agora vamos construir uma função que calcula as estatísticas utilizadas acima de uma única vez para todas as variáveis.
# Inicialmente, criaremos uma função com as estatísticas desejadas e usaremos a função `sapply` para aplicá-la em todas as variáveis desejadas.
estatisticas <- function(x){
  x <- x[!is.na(x)] # isso serve para que ele omita as observações com NA
  me <- mean(x)
  med <- median(x)
  n <- length(x)
  s <- sd(x)
  mi <- min(x)
  ma <- max(x)
  q25 <- quantile(x, probs = 0.25)
  q75 <- quantile(x, probs = 0.75)
  return(c(n = n, media = me, mediana = med, desvio = s, Q25 = q25, Q75 = q75, min = mi, max = ma))
}

# variáveis desejadas (quantitativas)
variaveis_estat <- c("idade", "QntDependentes", "temposervico", "BaseCalculoMensal")
# Vamos usar a função sapply - ela aplica uma função em uma base de dados (cada coluna) e retorna uma matriz. 
sapply(dados_estat[variaveis_estat], estatisticas)

# podemos salvar essa tabela
estatistica_descritiva <- sapply(dados_estat[variaveis_estat], estatisticas)
class(estatistica_descritiva)

# transformar em um data frame
estatistica_descritiva <- as.data.frame(estatistica_descritiva)
class(estatistica_descritiva)

# e exportar para uma planilha em csv - lembre de usar csv2 se for usar a vígula como separador decimal
write.csv2(estatistica_descritiva, "estatistica_descritiva.csv")

# vamos criar uma nova tabela onde não existe base (salário) = 0
base_d_zero <- dados_estat$BaseCalculoMensal != 0
table(base_d_zero)
dados_estat_f <- dados_estat[base_d_zero, ]
str(dados_estat_f)

# calculando estatistica por grupos - usaremos a função aggregate
aggregate(dados_estat_f[variaveis_estat], by=list(sexo=dados_estat_f$sexo), mean)
aggregate(dados_estat_f[variaveis_estat], by=list(sexo=dados_estat_f$sexo), sd)

# Existe alguma diferença entre os sexos? 

##### Exercício: Apresente a media, o desvio-padrão e a mediana das variáveis pelos estados civis. 
# Qual 

aggregate(dados_estat_f[variaveis_estat], by=list(estado_civil=dados_estat_f$EstadoCivil), median)
aggregate(dados_estat_f[variaveis_estat], by=list(estado_civil=dados_estat_f$EstadoCivil), sd)
aggregate(dados_estat_f[variaveis_estat], by=list(estado_civil=dados_estat_f$EstadoCivil), median)

#####

# Vamos omitir os dados faltantes, mas antes vamos descobrir quem é que está com dados omissos. 
# Mostrando outra forma de detectar os dados NA
which(is.na(dados_estat_f$temposervico))
which(is.na(dados_estat_f$BaseCalculoMensal))

# Agora vamos omití-los
dados_estat_f <- na.omit(dados_estat_f)
head(dados_estat_f)

# verificando que não existe NA
which(is.na(dados_estat_f$temposervico))
which(is.na(dados_estat_f$BaseCalculoMensal))

# criando a tabela como um arquivo em csv
write.csv(x = dados_estat_f, file = "dados_estatisticas_final.csv")

# calculando várias estatísticas por grupos com a função by()
destats <- function(x){
  sapply(x, estatisticas)
}
by(dados_estat_f[variaveis_estat], dados_estat_f$sexo, destats)
# Você vai colocar os dados, o grupo e a função com as estatísticas

##### Exercício: Faça o mesmo procedimento passado para os estados civis

estat_estados_civis <- by(dados_estat_f[variaveis_estat], dados_estat_f$EstadoCivil, destats)

#####

# Vamos agora visualizar as tabelas de frequência 
table(dados_estat_f$sexo)
table(dados_estat_f$EstadoCivil)
table(dados_estat_f$carreira)
table(dados_estat_f$sexo, dados_estat_f$EstadoCivil)

# com as frequências
prop.table(table(dados_estat_f$sexo))
prop.table(table(dados_estat_f$sexo, dados_estat_f$EstadoCivil))
prop.table(table(dados_estat_f$sexo, dados_estat_f$EstadoCivil))*100

# fica mais fácil dando nomes as tabelas
minha_tabela <- table(dados_estat_f$sexo, dados_estat_f$EstadoCivil)
prop.table(minha_tabela)
prop.table(minha_tabela)*100

#### Correlação/Covariância #####
# correlação de pearson
cor(dados_estat_f[variaveis_estat])
# matriz de variância-covariância
cov(dados_estat_f[variaveis_estat])
# testando a significância da correlação
cor.test(dados_estat_f$idade, dados_estat_f$temposervico)
cor.test(dados_estat_f$idade, dados_estat_f$BaseCalculoMensal)
cor.test(dados_estat_f$temposervico, dados_estat_f$BaseCalculoMensal)

