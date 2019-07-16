# Manipulação de Dados e Estatística Descritiva
library(dplyr) # manipulação de dados
library(magrittr) # operador pipe - %>% 

# importando os dados
dados <- read.csv("dados.csv", header = TRUE, sep = "")

# visualizando a tabela - dados
dados[1:10,]

# O dplyr é focado em alguns verbos
# mutate() cria variáveis a partir das variáveis existentes
# select() seleciona variáveis a partir dos nomes
# filter() seleciona as observações com base nos valores
# summarise() reduz os múltiplos valores em summary.
# arrange() reordena as observações.
# Todos essas funções combinam com a função group_by()

# Vamos selecionar - select() apenas uma variável, a DataNascimento
dados %>% 
  select(DataNascimento)

# O operador pipe, %>%, serve para facilitar a leitura do código.
# Ele serve para você não incluir os objetos ou funções dentro de outras funções. 
# Vamos fazer o código acima pelo jeito tradicional:
select(dados, DataNascimento)
# O operador pipe vai ajudar quando você precisar usar várias funções aninhadas

# Agora, selecionaremos duas variáveis, sexo e idade
dados %>% 
  select(sexo, idade)

##### Exercício: Selecione as colunas referentes ao sexo, idade, carreira e a base de cálculo mensal

dados %>% 
  select(sexo, idade, carreira, BaseCalculoMensal)

#####

# Faremos a filtragem com base em condições
# Filtrando os solteiros
dados %>% 
  filter(EstadoCivil == "solteir")

# Filtrando quem não é divorciado
dados %>% 
  filter(EstadoCivil != "divociad")

# Filtrando os soteiros e Professores
dados %>% 
  filter(EstadoCivil == "solteir", carreira == "PROFESSOR")

# Filtrando os casados com mais de 30 anos 
dados %>% 
  filter(EstadoCivil == "casad", idade > 30)

##### Ex.: Filtre os casados ou solteiros que têm entre 30 e 40 anos e são professores

dados %>% 
  filter(EstadoCivil == "casad" | EstadoCivil == "solteir", idade >= 30 & idade <= 40, carreira == "PROFESSOR")

#####

# Podemos também realizar a operação de filtro e selecionar colunas
# Filtrando sexo feminino e EstadoCivil solteiro, ao passo que seleciona as carreira e BaseCalculoMensal
dados %>% 
  filter(sexo == "feminino", EstadoCivil == "solteir") %>% 
  select(carreira, BaseCalculoMensal)

# Agora - filtrando EstadoCivil divorciado ou separado judicialmente com menos 50 anos ou menos de idade, e selecionando sexo, EstadoCivil, carreira, idade e Base de Cálculo
dados %>% 
  filter(EstadoCivil == "divorciad" | EstadoCivil == "sepjudicialmente", idade <= 50) %>% 
  select(sexo, EstadoCivil, carreira, idade, BaseCalculoMensal)

# Filtrando quem não é casado que possuem mais de 40 anos de idade e selecionando sexo, estado civil, idade, carreira e base 
dados %>% 
  filter(EstadoCivil != "casad", idade > 40) %>% 
  select(sexo, EstadoCivil, carreira, idade, BaseCalculoMensal)

##### Exercício: Faça uma tabela que filtra as pessoas com Estado Civil casado, união estável ou  solteiro que têm mais de 50 anos de idade e que recebem entre de R$2.000 e R$3.000 mensais, e imprime o sexo, idade, estado civil, carreira e base de calculo mensal

dados %>% 
  filter(EstadoCivil == "casad" | EstadoCivil == "uniaoestavel" | EstadoCivil == "solteir", idade > 50, BaseCalculoMensal >= 2000 & BaseCalculoMensal <= 3000) %>% 
  select(sexo, idade, EstadoCivil, carreira, BaseCalculoMensal)

###### Exercício: Repita o exercício anterior para as pessoas que recebem mais de R$3.000

dados %>% 
  filter(EstadoCivil == "casad" | EstadoCivil == "uniaoestavel" | EstadoCivil == "solteir", idade > 50, BaseCalculoMensal >= 3000) %>% 
  select(sexo, idade, EstadoCivil, carreira, BaseCalculoMensal)

#######

# Ordenando
# Vamos ordenar a tabela pela idade
dados %>% 
  arrange(idade)

# ordenar pela idade e pela base de cálculo
dados %>% 
  arrange(idade, BaseCalculoMensal)

# Adicionando novas colunas
# Vamos criar uma coluna com a base de cálculo anual. obs.: lembre-se de incluir o 13º.
dados %>% 
  mutate(BaseCalculoAnual = BaseCalculoMensal * 13)

# Vamos agora, selecionar apenas a profissão, a base de cálculo mensal e vamos criar a coluna base de cálculo anual novamente
dados %>% 
  mutate(BaseCalculoAnual = BaseCalculoMensal * 13) %>% 
  select(idade, BaseCalculoMensal, BaseCalculoAnual)

##### Exercício: Filtre os solteiros com idade entre 30 e 50 anos, crie a coluna Base de Cálculo Anual e mostre o sexo, a idade, a carreira e a Base de Cálculo Anual

dados %>% 
  filter(EstadoCivil == "solteir", idade >= 30 & idade <= 50) %>% 
  mutate(BaseCalculoAnual = BaseCalculoMensal * 13) %>% 
  select(sexo, idade, carreira, BaseCalculoAnual)
      
#####

# vamos sumarizar com estatísticas
# apresentando a média da BaseCálculoMensal
dados %>% 
  summarize(media_Base = mean(BaseCalculoMensal))
dados %>% 
  na.omit() %>% 
  summarize(media_Base = mean(BaseCalculoMensal))


# a função summarize se torna muito útil quando se utiliza a função group_by()
# apresentando a média da base de cálculo mensal por sexo
dados %>% 
  group_by(sexo) %>% 
  summarize(media = mean(BaseCalculoMensal))
dados %>% 
  na.omit() %>% 
  group_by(sexo) %>% 
  summarize(media = mean(BaseCalculoMensal))

# Vamos trabalhar com a base sem os NA
dados <- na.omit(dados)
which(is.na(dados))

# Posso adicionar outras estatísticas, como por ex. o desvio-padrão e a quantidade
dados %>% 
  group_by(sexo) %>% 
  summarize(quantidade = n(), media = mean(BaseCalculoMensal), desvio_padrao = sd(BaseCalculoMensal))

# Vamos usar outro grupo, o Estado Civil
dados %>% 
  group_by(EstadoCivil) %>% 
  summarize(quantidade = n(), media = mean(BaseCalculoMensal), desvio_padrao = sd(BaseCalculoMensal))

# Posso usar mais de um grupo, sexo e Estado Civil
dados %>% 
  group_by(sexo, EstadoCivil) %>% 
  summarize(quantidade = n(), media = mean(BaseCalculoMensal), desvio_padrao = sd(BaseCalculoMensal))

# Vamos fazer por carreira
dados %>% 
  group_by(carreira) %>% 
  summarize(quantidade = n(), media = mean(BaseCalculoMensal), desvio_padrao = sd(BaseCalculoMensal))

# São muitas linhas. Vamos salvar como carreiras_base
carreiras_base <- dados %>% 
  group_by(carreira) %>% 
  summarize(quantidade = n(), media = mean(BaseCalculoMensal), desvio_padrao = sd(BaseCalculoMensal))

# Vamos fazer uma ordenação
arrange(carreiras_base, media)
# Pela ordem descrescente
arrange(carreiras_base, desc(media))
  
###### Faça uma tabela com a quantidade, a média, mediana, desvio-padrão, mínimo, máximo da idade e da Base de Cálculo Mensal

dados %>% 
  summarize(quantidade = n(), media_idade = mean(idade), mediana_idade = median(idade), desvio_idade = sd(idade), min_idade = min(idade), max_idade = max(idade))

##### Repita o ex. anterior sendo que apresente por sexo
dados %>% 
  group_by(sexo) %>% 
  summarize(quantidade = n(), media_idade = mean(idade), mediana_idade = median(idade), desvio_idade = sd(idade), min_idade = min(idade), max_idade = max(idade))

##### Exercício: Faça o mesmo, sendo que com os divorciados ou viuvos
dados %>% 
  group_by(sexo) %>% 
  filter(EstadoCivil == "divorciad" | EstadoCivil == "viuv") %>% 
  summarize(quantidade = n(), media_idade = mean(idade), mediana_idade = median(idade), desvio_idade = sd(idade), min_idade = min(idade), max_idade = max(idade))

##### Exercício: faça uma tabela ordenada que separa por sexo e estado civil e apresenta a quantidade, a média da base de cálculo mensal e da idade e o desvio-padrão da base e da idade

dados %>% 
  group_by(sexo, EstadoCivil) %>% 
  select(sexo, EstadoCivil, idade, BaseCalculoMensal) %>% 
  summarize(n = n(), media_base = mean(BaseCalculoMensal), media_idade = mean(idade), desvio_base = sd(BaseCalculoMensal), desvio_idade = sd(idade)) %>% 
  arrange()

#####