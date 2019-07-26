# Visualização com ggplot2

library(ggplot2)
library(dplyr)

# importando a tabela
base <- read.csv("dados_estatisticas_final.csv", header = TRUE, sep = ",") # ver dados - criou com separador ","
head(base)

# ver estrutura dos dados
str(base)

# O pacote ggplot2 utiliza a gramática de gráficos, conceito desenvolvido por ....
# esse pacote possui um conjunto de ferramentas que permite a criação de gráficos que capture relações complexas.
# é uma biblioteca muito flexível... 
# essa gramática de gráficos é sistematizada da seguinte forma:
# você inicializa a função ggplot() com os dados e pode adicionar diversos elementos de forma encadeada
# ex.: ggplot(data = dados, aes(x = idade, y = BaseCalculoMensal)) 
# Essa funcão cria um espaço para você adicionar alguma estrutura. 
# Se eu quiser adicionar outro elemento, como pontos entre essas variáveis, devemos adicioná-los à p (objeto salvo anteriormente):
# ggplot(data = dados, aes(x = idade, y = BaseCalculoMensal))  + geom_point() 
# Melhoramos o gráfico. 
# É possível modificá-lo? Sim, podemos adicionar um título e alterar o texto dos eixos por meio da função labs(). 
# ggplot(data = dados, aes(x = idade, y = BaseCalculoMensal)) + geom_point() + labs(title = "Gráfico de dispersão", x = "Idade", y = "Base de Cálculo Mensal")

# Vamos visualizar as distribuições das variáveis
# Tudo dependerá do tipo da variável, se ela é qualitativa ou quantitativa
# Se ela for qualitativa, vamos usar o gráfico de barras

# Vamos visualizar um gráfico de barras par o sexo
ggplot(data = base) + 
  geom_bar(mapping = aes(x = sexo))
# mesmo grafico com cores
ggplot(data = base) + 
  geom_bar(mapping = aes(x = sexo, fill = sexo))


# Queremos ver agora o gráfico de barras para o estado civil
ggplot(data = base) + 
  geom_bar(mapping = aes(x = EstadoCivil))
# com cores
ggplot(data = base) + 
  geom_bar(mapping = aes(x = EstadoCivil, fill = EstadoCivil))

##### Exercício: Faça um gráfico de barras para a carreira

# dica: veja quantas pessoas por carreira de forma decrescente: 
base %>% 
  count(carreira) %>% 
  arrange(desc(n))
#

ggplot(data = base) + 
  geom_bar(mapping = aes(x = carreira))

##### Não ficou legal, pois temos muitas carreiras. Selecione 5 carreias e faça o mesmo gráfico colorido. 

base_carreiras <-base %>% 
  filter(carreira == "PROFESSOR" | carreira == "ENFERMEIRO" | carreira == "MOTORISTA" | carreira == "AG. COM. SAÚDE" | carreira == "AGENTE ADM.")

ggplot(base_carreiras) +
  geom_bar(mapping = aes(x = carreira))

ggplot(base_carreiras) +
  geom_bar(mapping = aes(x = carreira, fill = carreira))


#####

# Quando a variável é quantitativa, usamos o histograma
## Vamos visualizar o histograma da variável idade 
ggplot(base) +
  geom_histogram(mapping = aes(x = idade))

# Podemos modificar o tamanho dos intervalos - bin
ggplot(base) +
  geom_histogram(mapping = aes(x = idade), binwidth = 30)

ggplot(base) +
  geom_histogram(mapping = aes(x = idade), binwidth = 20)

ggplot(base) +
  geom_histogram(mapping = aes(x = idade), binwidth = 10)

ggplot(base) +
  geom_histogram(mapping = aes(x = idade), binwidth = 5)

ggplot(base) +
  geom_histogram(mapping = aes(x = idade), binwidth = 2)

# visualizando os intervalos
base %>% 
  count(cut_width(idade, 20))

base %>% 
  count(cut_width(idade, 10))

base %>% 
  count(cut_width(idade, 5))

base %>% 
  count(cut_width(idade, 2))

# Se for de interesse plotar o histograma da idade pelo sexo
# vamos usar o argumento fill para preencher a área com a cor e o alpha para tornar mais transparente
ggplot(base) + 
  geom_histogram(mapping = aes(x = idade, fill = sexo, color = sexo) , binwidth = 5, alpha=0.6)

ggplot(base) + 
  geom_histogram(mapping = aes(x = idade, fill = sexo, color = sexo) , binwidth = 5, alpha=0.6, position="identity")  # vamos usar a position = "identity" para que a distribuição do sexo masculino não fique sobreposta

##### Exercício: Crie um histograma para a base de cálculo para os estados civis solteiro e casado usando a cor e transparência
sol_cas <- base %>% 
  filter(EstadoCivil == "solteir" | EstadoCivil == "casad")

ggplot(sol_cas) + 
  geom_histogram(mapping = aes(x = BaseCalculoMensal, fill = EstadoCivil, color = EstadoCivil) , binwidth = 400, alpha=0.6, position="identity") 

# Verificamos que temos uma distribuição bimodal
##### Exercício: Crie um histograma para a base de cálculo para os estados civis solteiro e casado com base de cálculo menor do que R$ 3.000 usando a cor e transparência
sol_cas <- base %>% 
  filter(EstadoCivil == "solteir" | EstadoCivil == "casad", BaseCalculoMensal < 3000)

ggplot(sol_cas) + 
  geom_histogram(mapping = aes(x = BaseCalculoMensal, fill = EstadoCivil, color = EstadoCivil) , binwidth = 400, alpha=0.6, position="identity") 

####

# Verificamos que não há diferenças aberrantes. 
# Mas será que existe diferença da bases de cálculos entre os sexos?
# Vamos investigar:
ggplot(base) +
  geom_histogram(mapping = aes(x = BaseCalculoMensal, fill = sexo, color = sexo))

ggplot(base) +
  geom_histogram(mapping = aes(x = BaseCalculoMensal, fill = sexo, color = sexo), binwidth = 400, alpha=0.6, position="identity")

##### Exercício: Faça um histograma por sexo para quem possui Base de Cálculo maior do que 3500. 

base2000 <- base %>% 
  filter(BaseCalculoMensal > 3500)

ggplot(base2000) +
  geom_histogram(mapping = aes(x = BaseCalculoMensal, fill = sexo, color = sexo), binwidth = 200, alpha=0.6, position="identity")


#####

# Podemos usar o gráfico de colunas para nos ajudar quando tentamos analisar uma variável quantitativa juntamente com uma qualitativa
## A base de cálculo é diferente entre os estados civis? 
ggplot(base) + 
  geom_col(aes(x = EstadoCivil, y = BaseCalculoMensal))

ggplot(base) + 
  geom_col(aes(x = EstadoCivil, y = BaseCalculoMensal, fill = EstadoCivil))

#### Exercício: 
ggplot(base) + 
  geom_col(aes(x = EstadoCivil, y = BaseCalculoMensal, fill = EstadoCivil))


# Quando desejamos criar um gráfico a partir de duas variáveis quantitativas, usamos o gráfico de dispersão
# Vamos analisar o salario em funcao da idade
ggplot(base) +
  geom_point(aes(x = idade, y = BaseCalculoMensal))

# Percebmos que há dois grupos distintos. Será que é o sexo?
ggplot(base) +
  geom_point(aes(x = idade, y = BaseCalculoMensal, color = sexo))

# Parece que não, vamos investigar com outro grupo, o estado civil
ggplot(base) +
  geom_point(aes(x = idade, y = BaseCalculoMensal, color = EstadoCivil))

# Ainda não. Que tal a carreira
ggplot(base) +
  geom_point(aes(x = idade, y = BaseCalculoMensal, color = carreira))
# Ops, muitas categorias
# vamos selecionar algumas

base_carreiras <-base %>% 
  filter(carreira == "PROFESSOR" | carreira == "ENFERMEIRO" | carreira == "MOTORISTA" | carreira == "AG. COM. SAÚDE" | carreira == "AGENTE ADM.")

# Parece que agora conseguimos identificar a diferença entre os grupos. 
ggplot(base_carreiras) +
  geom_point(aes(x = idade, y = BaseCalculoMensal, color = carreira))

# Perceba também que não há uma correlação entre idade e base de cálculo

# Por fim, vamos visualizar o box-plot
## box_plot base por sexo
ggplot(base) +
  geom_boxplot(mapping = aes(x = sexo, y = BaseCalculoMensal))

# Box-plot base por estado civil
ggplot(base) +
  geom_boxplot(mapping = aes(x = EstadoCivil, y = BaseCalculoMensal))

# Box-plot base por estado civil e sexo
ggplot(base) +
  geom_boxplot(mapping = aes(x = EstadoCivil, y = BaseCalculoMensal, color = sexo))
# Perceba que há diferenças entre os sexos para alguns estados civis

##### Exercício: Faça um gráfico de box-plot da Base de cálculo para as 3 carreiras com as maiores e as 3 carreiras menores médias salariais (use profissões com n > 10), e faça a distinção por sexo.

maiores_medias <- base %>% 
  group_by(carreira) %>% 
  summarize(n = n(), media = mean(BaseCalculoMensal)) %>% 
  filter(n > 10) %>% 
  arrange(desc(media))

menores_medias <- base %>% 
  group_by(carreira) %>% 
  summarize(n = n(), media = mean(BaseCalculoMensal)) %>% 
  filter(n > 10) %>% 
  arrange(media)

maiores_medias
menores_medias


base_boxplot <- base %>% 
  filter(carreira == "PROFESSORA" | carreira == "PROFESSOR" | carreira == "AGENTE ADM." | carreira == "GARI" | carreira == "AGENTE DE ENDEMIAS" | carreira == "MOTORISTA")

base_boxplot

ggplot(base_boxplot) +
  geom_boxplot(mapping = aes(x = carreira, y = BaseCalculoMensal, color = sexo))

#####