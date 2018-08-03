library(RODBC) #Para banco de dados
library(data.table)
library(tidyverse)
library(readxl)
library(Hmisc)
#Importar arquivos

#PDAD2018
pdad_moradores<-read.csv(file = "dados/Base-de-Moradores-PDAD.csv", sep=";",header = TRUE, encoding = "auto", stringsAsFactors = FALSE)
pdad_domicilios<-read.csv(file="dados/Base-Domicílios-PDAD-2.csv", sep=";", header=TRUE, stringsAsFactors = FALSE)
#Leitura do site
Base_Moradores_PDAD_2015 <- read_delim("http://www.codeplan.df.gov.br/wp-content/uploads/2018/02/Base_Moradores_PDAD_2015.csv", 
                                       ";", escape_double = FALSE, trim_ws = TRUE)

head(pdad_domicilios)

#define o ambiente de trabalho
getwd()
setwd(getwd())

#Usando Fread - # dec diz respeito ao operador decimal - encoding para consertar characteres
pdad_moradores_fread<-data.table::fread(input="dados/Base-de-Moradores-PDAD.csv", 
                                        sep = ";", dec=",", encoding = "Latin-1")
#FREAD online
pdad_moradores <- data.table::fread("http://www.codeplan.df.gov.br/wp-content/uploads/2018/02/Base_Moradores_PDAD_2015.csv",
                                    dec = ",",encoding = "Latin-1")


#Agora veremos carga pelo banco de dados - Dados para matrícula
matricula<-readline(prompt = "Escreva sua matrícula:")
senha<-readline(prompt = "Escreva sua senha:")
#Acesso ao banco de dados
db<-RODBC::odbcConnect("db_codeplan", uid=matricula, pwd=senha)

#remova as senhas
rm(pwd,matricula, senha)

#acesse o banco de dados
tabelas_db<-RODBC::sqlTables(db)
head(tabelas_db) #primeiros dados
tail(tabelas_db) #ultimos dados

class(tabelas_db)
str(tabelas_db) #tipos das colunas
names(tabelas_db)
dplyr::glimpse(tabelas_db)
#Para seleção
tabelas_db[tabelas_db[,2]=='pdad',]$TABLE_NAME


#use dplyr
RODBC::sqlTables(db) %>%
  dplyr::filter(TABLE_SCHEM=="pdad") %>% #aplica filtro
  dplyr::select(TABLE_NAME) #seleciona

RODBC::sqlTables(db) %>%
  dplyr::filter(TABLE_SCHEM=="pdad") %>% #aplica filtro
  dplyr::rename(Nome=TABLE_NAME, Banco=TABLE_CAT,
                Esquema=TABLE_SCHEM, Tipo=TABLE_TYPE,
                Observa=REMARKS) #seleciona

# Consultar as tabelas disponíveis
RODBC::sqlTables(db) %>%
  # Filtrar apenas linhas do esquema PDAD
  dplyr::filter(stringr::str_detect(TABLE_SCHEM, pattern =  "pdad")) %>%
  # Selecionar apenas a coluna com as tabelas
  dplyr::select(TABLE_NAME)

colunas<-RODBC::sqlColumns(db,"pdad.pdad_2015_mor") #consultamos as tabelas. Column_NAME informa as variáveis disponíveis neste arquivo

# Consultar as colunas da tabela pdad_2015_mor
RODBC::sqlColumns(db,"pdad.pdad_2015_mor") %>%
  # Selecionar a coluna "COLUMN_NAME"
  dplyr::select(COLUMN_NAME)


#usamos a coluna sqlQuery para acessar a base de dados
#Faz a consulta, de modo a copiar os dados
pdad_moradores<-RODBC::sqlQuery(db,"select A01_DOM_RA,A05_DOM_MORADORES from pdad.pdad_2015_mor")

#lista os moradores entrevistados por RA
table(pdad_moradores$A01_DOM_RA)


#procura usando * para selecionar todas colunas
pdad_moradores<-RODBC::sqlQuery(db, "select * from pdad.pdad_2015_mor")
#liste todos os nomes
list.files("dados")
dic_mora<-read_excel("dados/Dicionário-Base-Moradores-PDAD-2.xlsx")

#Vamos colocar o nome das colunas como nomes no dicionário
#Cria um objeto com os rótulos
var.labels<-dic_mora$`RÓTULO DA VARIÁVEL`
#nomeia os rótulos com o nome das variáveis do banco de dados
names(var.labels)<-names(pdad_moradores)
#adiciona os rótulos ao banco de dados
pdad_moradores<-Hmisc::upData(pdad_moradores,labels=var.labels)
Hmisc::describe(pdad_moradores)


#Veremos agora como fica a questão de fatores.
#cria a RA, usa o dplyr para acessá-la
#Transmute altera as colunas que só mantém a 
#coluna que se está alterando.
#Mutate adiciona novas variáveis e preserva as existentes
RA <- pdad_moradores %>%
  dplyr::transmute(RA=factor(A01_DOM_RA,
                             levels=1:31,
                             labels=c('Brasília/Plano Piloto',      
                                      'Gama',
                                      'Taguatinga',
                                      'Brazlândia',
                                      'Sobradinho',
                                      'Planaltina',
                                      'Paranoá',
                                      'Núcleo Bandeirante',
                                      'Ceilândia',
                                      'Guará',
                                      'Cruzeiro',
                                      'Samambaia',
                                      'Santa Maria',
                                      'São Sebastião',
                                      'Recanto das Emas',
                                      'Lago Sul',
                                      'Riacho Fundo',
                                      'Lago Norte',
                                      'Candangolândia',
                                      'Águas Claras',
                                      'Riacho Fundo II',
                                      'Sudoeste/Octogonal',
                                      'Varjão',
                                      'Park Way',
                                      'SCIA-Estrutural',
                                      'Sobradinho II',
                                      'Jardim Botânico',
                                      'Itapoã',
                                      'SIA',
                                      'Vicente Pires',
                                      'Fercal')))
# Tabular os resultados
table(RA$RA)


# Filtrar somente as informações da coluna RA
ra <- dic_mora[dic_mora$`NOME DA VARIÁVEL`=="A01_DOM_RA",
                    ]$VALORES

# Separar o vetor de string, baseado no argumento "\n"
ra <- base::strsplit(ra, "\n")

# Transformar a lista de caracteres em data.frame
ra <- plyr::ldply(ra, data.frame, stringsAsFactors=F)

# Renomear a coluna de RA
colnames(ra) <- "RA"

# Realizar os tratamentos finais
ra <- ra %>%
  # Separar as colunas, baseado no espaço em branco ""
  tidyr::separate(col = RA, into = c("cod", "RA"), 
                  sep = " ",
                  extra="merge") %>%
  # Retirar o caracter "'" do campo RA e deixar somente a primeira letra maiúscula
  dplyr::mutate(RA= stringr::str_to_title(gsub("[']","",RA))) %>%
  # Criar uma coluna, unindo os valores com um sinal de "="
  tidyr::unite(col = "Arg",c("cod","RA"),sep="=",remove=F) %>%
  dplyr::mutate(RA=gsub("\r","",RA))


# Recodificar os nomes
ra_codificada <- pdad_moradores %>%
  dplyr::transmute(ra=factor(A01_DOM_RA,
                             levels = ra$cod,
                             labels = ra$RA)) %>%
  dplyr::select(ra)

# Tabular os resultados
table(ra_codificada$ra)


RA %>%
  # Criar a área de plotagem, com o eixo X
  ggplot(aes(x=RA)) +
  # Inserir a geometrica do tipo "Barra", com a opção de contagem (gerada automaticamente no eixo y)
  geom_bar(stat = "count") +
  # Inverter os eixos
  coord_flip() +
  labs(y="Amostra",x="RA")

# Contar quantas pessoas foram amostradas em cada RA
RA %>%
  # Contar quantas observações temos em cada RA
  dplyr::count(RA) %>%
  # Plotar o gráfico, ajustando as categorias de acordo com o total amostrado, em ordem
  #Forcats é usado para dados categóricos
  ggplot(aes(x=forcats::fct_reorder(RA,n),y=n)) +
  # Desenhar a geometria de barras
  #Identity plota exatamente como está informado
  geom_bar(stat = "identity") +
  # Inverter os eixos
  coord_flip() +
  # Rotular os eixos
  labs(y="Amostra",
       x="Região Administrativa")+
  ggtitle("Amostra por Região Administrativa")
  


#Temas com ggthemr
ggthemr::ggthemr()


# Tratamento de strings
## Usa expressões regulares


#aqui veremos o uso da PDAD Domicios
#Extrai da base
pdad_domicilios <- RODBC::sqlQuery(db,
                  "select * from pdad.pdad_2015_dom", stringsAsFactor=F)

#Importa o dicionário
dic_dom<-read_excel('dados/Dicionário-Base-Domicílios-PDAD-2.xlsx')
var.labels<-dic_dom$`RÓTULO DA VARIÁVEL`
#nomeia os rótulos com o nome das variáveis do banco de dados
names(var.labels)<-names(pdad_domicilios)
#adiciona os rótulos ao banco de dados
pdad_domicilios<-Hmisc::upData(pdad_domicilios,labels=var.labels)
Hmisc::describe(pdad_domicilios)


#Precisamos juntar duas bases 
#Precisamos achar os colunas de nome em comum entre duas bases
nome_comum<-which((names(pdad_domicilios) %in% names(pdad_moradores)))
#Aqui vai mostrar quais variáveis têm nomes em comum
names(pdad_domicilios)[nome_comum]
nome_comum_sem1<-names(pdad_domicilios)[nome_comum[-1]]
#Vamos juntar as informações usando o left_join

pdad <- pdad_moradores %>%
  dplyr::left_join(
    #aqui será informada a base que queremos unir,
    #filtrando para colunas repetidas
    dplyr::select(-c(A02_DOM_SETOR,
                     A04_DOM_SETOR_CENSITARIO,
                     A05_DOM_MORADORES,
                     NFATOR_RA, PESO_PRE,
                     POPULACAO_AJUSTADA,
                     ESTRATO))
    by=c("CD_SEQ_DOM"="CD_SEQ_DOM")
  )

pdad <- pdad_moradores %>%
  # Entrar com a função para left join
  dplyr::left_join(
    # Informar a base que iremos unir, filtrando para colunas repetidas
    pdad_domicilios %>%
      # Filtrar as colunas repetidas
      dplyr::select(-c(A01_DOM_RA,A02_DOM_SETOR,A04_DOM_SETOR_CENSITARIO,
                       A05_DOM_MORADORES,NFATOR_RA,PESO_PRE,POPULACAO_AJUSTADA,
                       ESTRATO)),
    by=c("CD_SEQ_DOM"="CD_SEQ_DOM"))


#Aqui analisaremos alguns dados
#importaremos o survey e srvyr
library(survey)
library(srvyr)

#O Desenho inicial da pesquisa
sample.pdad<-
  # Declarar a base a ser utilizada
  pdad %>%
  srvyr::as_survey_design(id=CD_SEQ_DOM,  # Identificador único da unidade amostrada
                          strata=ESTRATO, # Identificação do estrato
                          weights=PESO_PRE, # Probabilidade da unidade ser sorteada
                          nest=T) # Parâmetro de tratamento para dos IDs dos estratos

#Crie um objeto para pós estrato
post.pop <- unique(subset(pdad,select=c(POPULACAO_AJUSTADA)))

# Criar a variável de frequência (a mesma variável de pós-estrato, 
#para funcionar como id e peso)
post.pop$Freq <- post.pop$POPULACAO_AJUSTADA

# Outra maneira de criar o mesmo objeto
post.pop <- pdad %>%
  dplyr::select(POPULACAO_AJUSTADA) %>%
  dplyr::distinct() %>%
  dplyr::mutate(Freq=POPULACAO_AJUSTADA)

# Declarar o objeto de pós-estrato
sample.pdad <- survey::postStratify(sample.pdad,~POPULACAO_AJUSTADA,post.pop)


# Criar objeto para calcular os erros por bootstrap (Rao and Wu’s(n − 1) bootstrap)
# J. N. K. Rao and C. F. J. Wu - Journal of the American Statistical Association
# Vol. 83, No. 401 (Mar., 1988), pp. 231-241
amostra <- survey::as.svrepdesign(sample.pdad, type = "subbootstrap")

# Ajustar estratos com apenas uma UPA (adjust=centered)
# Imagine uma única pessoa com uma característica, não tem como ter variância com um valor apenas
options( survey.lonely.psu = "adjust")

# Ajustar objeto de amostra, para uso com o pacote srvyr
amostra <- srvyr::as_survey(amostra)


# #Vamos testar estimando o total de possas com 18 anos ou mais 
# de idade no Distrito Federal. O nosso objeto base agora será o 
# amostra, com o qual utilizaremos o pacote  srvyr. 
# Com a parâmetro vartype='ci', obtemos as estimativas dos 
# intervalos de confiança.

# População DF com mais de 18 anos
pop18 <- data.frame(
  amostra %>%
    # Filtrar somente a população com 18 anos ou mais de idade, retirando os códigos de não informação
    srvyr::filter(D06_MOR_IDADE>=18&D06_MOR_IDADE!=999) %>%
    # Criar uma variável auxiliar para contagem
    srvyr::mutate(count=1) %>%
    # Calcular o total da população, com seu intervalo de confiança
    srvyr::summarise(n=survey_total(count, vartype = "ci")))

# #existiam entre 2.206.956 e 2.237.671 pessoas com mais de 18 anos no DF em 2015,
# sendo o valor médio de 2.222.313 pessoas. 
# Caso o desejo fosse estimar esse mesmo total por sexo, isso poderia ser 
# feito com a função group_by(). 
# Vamos aproveitar e calcular o percentual de cada grupo no total.



#Agora vamos mexer com datas 
glimpse(pdad_domicilios)
# Utilizar a base de domicílios
pdad_domicilios %>%
  # Selecionar apenas a variável de data
  dplyr::select(DT_DATA_PESQUISA) %>%
  # Contar o número de caracteres do campo data
  dplyr::mutate(n_char=stringr::str_length(DT_DATA_PESQUISA)) %>%
  # Contar a quantidade de caracteres
  dplyr::count(n_char) 

#O resultado mostra que todos es

# Utilizar a base de domicílios
pdad_domicilios %>%
  # Selecionar apenas a variável de data
  dplyr::select(A01_DOM_RA,DT_DATA_PESQUISA) %>%
  # Contar o número de caracteres do campo data
  dplyr::mutate(ano_pesquisa=stringr::str_sub(DT_DATA_PESQUISA,7,10)) %>%
  # Contar a quantidade de caracteres
  dplyr::count(A01_DOM_RA,ano_pesquisa) tão com 10 caracteres.

#Tivemos um erro com a quantidade de caracteres. Alguns estão com espaços em branco e outros problemas

# Utilizar a base de domicílios
pdad_domicilios %>%
  # Selecionar apenas a variável de data
  dplyr::select(DT_DATA_PESQUISA) %>%
  # Contar o número de caracteres do campo data
  # o str_trim retira todo espaço em branco
  #Nisso o dplyr faz a contagem
  dplyr::mutate(n_char=stringr::str_length(stringr::str_trim(DT_DATA_PESQUISA))) %>%
  # Contar a quantidade de caracteres
  dplyr::count(n_char) 

# o resultado mostra que há 5 casos onde não há preenchimento algum, 16036 onde não há 8 chars, etc.


# Os dias não estão com zero à esquerda também, por isso alguns campos variam o tamanho
# Amrzenar resultado no objeto x
x<-
  # Utilizar a base de domicílios
  pdad_domicilios %>%
  # Selecionar a data da pesquisa
  dplyr::select(DT_DATA_PESQUISA) %>%
  # Retirar os espaços em branco
  dplyr::mutate(DT_DATA_PESQUISA=stringr::str_trim(DT_DATA_PESQUISA)) %>%
  # Separar a data, em MM, DD, AAAA, utilizando como separador "/" e mantendo a variável original
  tidyr::separate(DT_DATA_PESQUISA,
                  into = c("MM","DD","AAAA"),sep="/",
                  remove=F) %>%
  # Ajustar o mês e o ano, adicionando um zero à esquerda quando houver somente 1 caracter, 
  #criado a variável de data no formato DD/MM/AAAA
  #se str_length(MM)==1, ele cola "0" e depois MM, dentro da variável MM, que passa a ser 01, 02, ... 31, 01.
  #Data ajustada cola as variáveis dia, mês e ano e usa "/" como separador, no formato de data
  dplyr::mutate(MM=case_when(stringr::str_length(MM)==1~paste0("0",MM),
                             TRUE~MM),
                DD=case_when(stringr::str_length(DD)==1~paste0("0",DD),
                             TRUE~DD),
                DATA_AJUSTADA=paste(DD,MM,AAAA,sep = "/"),
                ANO=stringr::str_sub(DATA_AJUSTADA,7,10))

table(x$ANO)
table(x$MM)
table(x$DD)


# Utilizar a base de domicílios
pdad_domicilios %>%
  # Selecionar a data da pesquisa
  dplyr::select(DT_DATA_PESQUISA) %>%
  # Transformar o campo de data (em caracter) em data (formato data)
  dplyr::mutate(DATA_CORRIGIDA=lubridate::mdy(DT_DATA_PESQUISA),
                # Extrair o valor do ano
                ANO=lubridate::year(DATA_CORRIGIDA))

# Utilizar a base de domicílios
#Vamos usar o pacote lubridate para corrigir a data
pdad_domicilios %>%
  # Selecionar a data da pesquisa
  dplyr::select(DT_DATA_PESQUISA) %>%
  # Transformar o campo de data (em caracter) em data (formato data)
  # O parâmetro mdy é month/day/year
  dplyr::mutate(DATA_CORRIGIDA=lubridate::mdy(DT_DATA_PESQUISA),
                # Extrair o valor do ano
                # A variável de ano era a que possuia mais problemas e agora foi corrigida
                ANO=lubridate::year(DATA_CORRIGIDA))

# Utilizar a base de domicílios
pdad_domicilios %>%
  # Selecionar a data da pesquisa
  dplyr::select(CD_SEQ_DOM,A01_DOM_RA,DT_DATA_PESQUISA) %>%
  # Transformar o campo de data (em caracter) em data (formato data)
  dplyr::mutate(DATA_CORRIGIDA=lubridate::mdy(DT_DATA_PESQUISA),
                # Extrair o valor do ano
                ANO=lubridate::year(DATA_CORRIGIDA)) %>%
  # Verificar o total de obsevações por ano
  dplyr::count(A01_DOM_RA,ANO)

inconsistencia <-
  # Utilizar a base de domicílios
  pdad_domicilios %>%
  # Selecionar a data da pesquisa
  dplyr::select(CD_SEQ_DOM,RA,DT_DATA_PESQUISA) %>%
  # Transformar o campo de data (em caracter) em data (formato data)
  dplyr::mutate(DATA_CORRIGIDA=lubridate::mdy(DT_DATA_PESQUISA),
                # Extrair o valor do ano
                ANO=lubridate::year(DATA_CORRIGIDA),
                RA=factor(A01_DOM_RA,
                          levels=1:31,
                          labels=c('Brasília/Plano Piloto',      
                                   'Gama',
                                   'Taguatinga',
                                   'Brazlândia',
                                   'Sobradinho',
                                   'Planaltina',
                                   'Paranoá',
                                   'Núcleo Bandeirante',
                                   'Ceilândia',
                                   'Guará',
                                   'Cruzeiro',
                                   'Samambaia',
                                   'Santa Maria',
                                   'São Sebastião',
                                   'Recanto das Emas',
                                   'Lago Sul',
                                   'Riacho Fundo',
                                   'Lago Norte',
                                   'Candangolândia',
                                   'Águas Claras',
                                   'Riacho Fundo II',
                                   'Sudoeste/Octogonal',
                                   'Varjão',
                                   'Park Way',
                                   'SCIA-Estrutural',
                                   'Sobradinho II',
                                   'Jardim Botânico',
                                   'Itapoã',
                                   'SIA',
                                   'Vicente Pires',
                                   'Fercal'))) %>%
  # Verificar o total de obsevações por ano
  dplyr::filter(!ANO %in% c(2015,2016)) %>%
  # Organizar os resultados por ordem crescente de RA e Data
  dplyr::arrange(RA,CD_SEQ_DOM)

View(RA)
