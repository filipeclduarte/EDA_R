##### Início do curso #####

#### Preparação dos dados #### 

# É o processo de organização e estruturação dos dados

# Primeiro vamos limpar o ambiente
rm(list = ls())

# Agora podemos importar os dados
# Se a base estiver com a vírgula como separador decimal, utilize a função read.csv2.  
dados <- read.csv2("BasePens.csv", sep = ";", header = TRUE)

# estrutura dos dados
str(dados)

# vendo as 6 primeiras linhas 
head(dados)

# vendo os nomes das variáveis
names(dados)

# vamos renomear as variáveis
names(dados) <- c("id", "sexo", "EstadoCivil", "DataNascimento", "DataIngresso", "carreira", "BaseCalculoMensal", "QntDependentes")

# vendo novamente as variáveis
names(dados)
head(dados)
str(dados)

# Vamos transformar a variável sexo para factor (qualitativa) 
dados$sexo <- ifelse(dados$sexo == 1, "feminino", "masculino")
str(dados)
dados$sexo <- as.factor(dados$sexo)
str(dados)

# Vamos transformar a variável estado civil para factor
dados$EstadoCivil <- ifelse(dados$EstadoCivil == 1, "solteir", 
                     ifelse(dados$EstadoCivil == 2, "casad", 
                     ifelse(dados$EstadoCivil == 3, "viuv",
                     ifelse(dados$EstadoCivil == 4, "sepjudicialmente", 
                     ifelse(dados$EstadoCivil == 5, "divorciad", 
                     ifelse(dados$EstadoCivil == 6, "uniaoestavel", "outros"))))))
str(dados)
head(dados)

# Transformando a variável estado civil para factor
dados$EstadoCivil <- as.factor(dados$EstadoCivil)

# Quantidade de pessoas por estado civil
table(dados$EstadoCivil)

# visualizando a estrutura dos dados
str(dados)

# Datas de referência no R e data de hoje
data_hoje <- Sys.Date()
data_hoje

# Ajustando a data de ingresso e de nascimento
str(dados)
dados$DataNascimento
dados$DataNascimento <- as.Date(dados$DataNascimento, format = "%d/%m/%Y")
class(dados$DataNascimento)

dados$DataIngresso
dados$DataIngresso <- as.Date(dados$DataIngresso, "%d/%m/%Y")
class(dados$DataIngresso)

# Calculando a idade dos participantes do plano
dados$idade <- as.numeric(round((data_hoje - dados$DataNascimento)/365,0))
str(dados)

##### Exercício: Calcule a quantidade de tempo de serviço passado para os participantes
dados$temposervico <- as.numeric(round((data_hoje - dados$DataIngresso)/365,0))
str(dados)
#####

##### Exercício: Crie uma coluna (variável) representando a Base de Cálculo Anual, considerando o 13º salário

dados$BaseCalculoAnual <- dados$BaseCalculoMensal * 13

#####

# Vamos salvar essa planilha final 
write.table(x = dados, file = "dados.csv")
