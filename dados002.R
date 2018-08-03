# Vamos agora elencar uma série de variáveis para construirmos um pequeno relatório. 
# Vamos construir gráficos e tabelas para: população, por faixa etária e sexo; distribuição do rendimento do trabalho, 
# por faixas; e tipo de esgotamento sanitário do domicílio. E montar um mini relatório com essas informações.

# Criar um objeto com o salário mínimo
sm <- 788

# Criar um objeto com as variáveis de interesse
x <- amostra %>%
  # Criar variável de sexo
  # Classifca de acordo com o dicionário
  # Vejamos que 
  srvyr::mutate(sexo=case_when(D04_MOR_SEXO==1~"Masculino",
                               D04_MOR_SEXO==2~"Feminino"),
                # Criar variável de esgotamento sanitário
                # O True~OUTROS quer dizer que todas que não forem as de cima, serão considerados "OUTROS"
                esgotamento=case_when(B07_DOM_ESGOTO_SANITARIO==1~"Rede Geral (Caesb)",
                                      B07_DOM_ESGOTO_SANITARIO==2~"Fossa séptica",
                                      B07_DOM_ESGOTO_SANITARIO==3~"Fossa rudimentar",
                                      B07_DOM_ESGOTO_SANITARIO==4~"Esgoto a céu aberto",
                                      TRUE~"Outros"),
                # Criar variável de faixas de idade
                # Ordenar por faixa
                idade_faixas=cut(D06_MOR_IDADE,
                                 #A primeira faixa de idade é de 4. O código vai crescer a partir de 4, de 5 em 5 anos, 
                                 # até chegar a 79
                                 # Tudo que tiver abaixo de 4 (-Inf) é considerado 0 a 4. 
                                 # Acima de 79 (Inf), é tudo de Mais de 80 anos
                                 breaks = c(-Inf,seq(4,79,by=5),Inf),
                                 #Os labels dizem respeito aos cortes que eu farei, de modo que é um vetor de classificação
                                 labels = c("0 a 4 anos","5 a 9 anos","10 a 14 anos",
                                            "15 a 19 anos","20 a 24 anos",
                                            "25 a 29 anos","30 a 34 anos",
                                            "35 a 39 anos","40 a 44 anos",
                                            "45 a 49 anos","50 a 54 anos",
                                            "55 a 59 anos","60 a 64 anos",
                                            "65 a 69 anos","70 a 74 anos",
                                            "75 a 79 anos","Mais de 80 anos"),
                                 ordered_result = T),
                # Criar variável de faixas de salário
                # As sequencias de números 777777, 88888888, 999999999, dizem respeito a quando não se informa o valor
                # Seja por não informar, por não saber ou querer informar
                # Quando tiver esses valores 777777,888888,99999, os valores serão trocados para Missing
                # NA_real é um NA real
                faixas_salario=cut(case_when(E14_MOR_PRINC_REND_BRUTO %in% c(77777,88888,99999)~NA_real_,
                                             
                                             TRUE~as.numeric(E14_MOR_PRINC_REND_BRUTO)),
                                   #O tratamento vai de menos d e1 salário, dois salários até chegar ao vários salários mínimos
                                   breaks = c(-Inf,sm,2*sm,4*sm,10*sm,Inf),
                                   labels = c("Até 1 salário","Mais de 1 até 2 salários",
                                              "Mais de 2 até 4 salários",
                                              "Mais de 4 até 10 salários",
                                              "Mais de 10 salários")),
                # Criar variável para as RAs
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
  # Transformar em fator variáveis do tipo character
  srvyr::mutate_if(is.character,funs(factor(.))) %>%
  # Selecionar as variáveis criadas e algumas variáveis auxiliares
  srvyr::select(RA,D03_MOR_CONDICAO_UNID,D06_MOR_IDADE,E08_MOR_ATIVIDADE,sexo,esgotamento,idade_faixas,faixas_salario)









# Construir um objeto com as idades calculadas, por faixas de idade e sexo
piramide <-
  x %>%
  # Retirar as idades indeterminadas
  srvyr::filter(D06_MOR_IDADE!=999) %>%
  # Agrupar por faixas de idade e sexo
  srvyr::group_by(idade_faixas,sexo) %>%
  # Calcular os totais
  # Sumarise faz o cálculo dos totais, recebe número de lementos e o tipo de variável
  # na.rm retira os missings
  srvyr::summarise(n=survey_total(na.rm = T, vartype = "ci"))

# Fazer o gráfico com a pirâmide
piramide_grafico <-
  piramide %>%
  # Construir um plot com as idades no eixo x, as quantidades no eixo y,
  #  preenchimento com a variável sexo, e os intervalos de confiança
  # inferiores e superiores
  # Valores máximo e mínimo para os valores de Y e Min
  ggplot(aes(x=idade_faixas,y=n, fill=sexo, ymin=n_low,ymax=n_upp))+
  # Fazer o gráfico de barras para o sexo Feminino
  # Uma barra é feminina, usando os dados exatos
  # Os dados são exatamente os listados
  geom_bar(data = dplyr::filter(piramide, sexo == "Feminino"),
           stat = "identity") +
  # Fazer o gráfico de barras para o sexo Masculino
  geom_bar(data = dplyr::filter(piramide, sexo == "Masculino"),
           stat = "identity",
           position = "identity",
           # Negativar os valores para espelhar no eixo
           mapping = aes(y = -n))+
  # Plotar os erros para o sexo Masculino, negativando os valores para espelhar o eixo
  # Deste modo, o valor está negativado para ficar do outro lado
  geom_errorbar(data = dplyr::filter(piramide, sexo == "Masculino"),
                mapping = aes(ymin = -n_low,ymax=-n_upp),
                width=0,
                color="black")+
  # Plotar os erros para o sexo Feminino
  geom_errorbar(data = dplyr::filter(piramide, sexo == "Feminino"),
                width=0,
                color="black")+
  # Inverter os eixos, fazendo com que o gráfico de colunas verticais fique
  # horizontal
  coord_flip() + 
  # Ajustar as configurações de escala
  scale_y_continuous(labels = function(x) format(abs(x), 
                                                 big.mark = ".",
                                                 scientific = FALSE,
                                                 decimal.mark=",")) +
  # Suprimir os nomes dos eixos
  labs(x="",y="", title="Pirâmide etária para os sexos masculino e feminino") +
  # Suprimir o nome da legenda
  scale_fill_discrete(name = "")

# Plotar gráfico
piramide_grafico



# Construir um objeto com as informações de salario
salario <- x %>%
  # Filtar pessoas sem atividade
  srvyr::filter(E08_MOR_ATIVIDADE!=0) %>%
  # Agrupar por faixas de salário
  srvyr::group_by(faixas_salario) %>%
  # Calcular os totais para cada grupo de salário
  srvyr::summarise(n=survey_total(na.rm=T,vartype = "ci"))

# Construir um objeto com o gráfico
salario_grafico <-
  salario %>%
  # Plotar os eixos x e y
  ggplot(aes(x=faixas_salario, y=n, fill="orange"))+
  # Construir o gráfico de barras
  geom_bar(stat = "identity") +
  # Construir as barras de erro
  geom_errorbar(aes(ymin=n_low,ymax=n_upp,size=4, width=0), color="darkred")+
  # Inverter os eixos
  coord_flip()+
  # Suprimir o nome dos eixos
  labs(x="",y="",title="Salário por Faixa Etária", subtitle="A situação de Brasília", colour="Cylinders")+
  # Retirar o título da legenda
  theme(legend.position="none")+
  # Ajustar as formatações de escala
  scale_y_continuous(labels = function(x) format(abs(x), 
                                                 big.mark = ".",
                                                 scientific = FALSE,
                                                 decimal.mark=","))
# Plotar gráfico
salario_grafico


# Carregar o pacote Scales
library(scales)
library(stringr)

# Construir o objeto com os valores
#Para saber os salários por RA
salario2 <- x %>%
  # Retirar pessoas sem atividade
  srvyr::filter(E08_MOR_ATIVIDADE!=0) %>%
  # Agrupar por RA e faixas de salário
  srvyr::group_by(RA,faixas_salario) %>%
  # Calcular as proporções por faixa de salário
  srvyr::summarise(n=survey_mean(na.rm=T,vartype = "ci"))

# Construir o gráfico
salario_grafico2<-
  salario2 %>%
  # Plotar os eixos x e y
  ggplot(aes(x=faixas_salario, y=n, fill="orange"))+
  # Construir o gráfico de barras
  geom_bar(stat = "identity") +
  # Construir o gráfico com os erros
  geom_errorbar(aes(ymin=n_low,ymax=n_upp,size=4, width=0,group=RA), color="darkred")+
  # Inverter os eixos
  coord_flip()+
  # Suprimir o nome dos eixos
  labs(x="",y="", title="Faixas salariais por RA")+
  # Suprimir o nome da legenda
  theme(legend.position="none")+
  # Ajustar as formatações de escala
  scale_y_continuous(labels = scales::percent)+
  # Plotar o gráfico para cada uma das RAs, divididas em 4 colunas
  facet_wrap(.~RA, ncol=4)

salario_grafico2



# Consultar as tabelas do esquema inflação
RODBC::sqlTables(db) %>%
  dplyr::filter(str_detect(TABLE_SCHEM,"inflacao"))

# Consular as colunas da tabela do IPCA mensal
# Consulta as colunas do ipca_mensal
RODBC::sqlColumns(db,"inflacao.ipca_mensal")$COLUMN_NAME

# Puxar as colunas desejadas
#Escolhemos a coluna do DF
#codigo 0 é o índice geral da inflação
inflacao_df <- RODBC::sqlQuery(db,"select referencia,codigo,descricao,
                               distritofederal from inflacao.ipca_mensal where codigo=0",stringsAsFactors=F) %>%
  # Ajustar a variável de data
  # Filtro para os valres de refer}encia como o ano-mes-dia
  dplyr::mutate(referencia=lubridate::ymd_hms(referencia)) %>%
  # Filtrar datas superiores ao período de referência da PDAD 2015
  dplyr::filter(referencia>"2015-07-01")

# Calcular a inflação acumulada
# roda o produtório da inflação divido por 100 + uma unidade
inflacao_acum <- prod(inflacao_df$distritofederal/100+1)

View(inflacao_acum)
# Calcular o salário médio nominal e ajustado da PDAD 2015
amostra %>%
  # Retirar pessoas sem atividade
  srvyr::filter(E08_MOR_ATIVIDADE!=0) %>%
  # Criar uma variável com o salário nominal e o salário real
  srvyr::mutate(salario_nominal=case_when(E14_MOR_PRINC_REND_BRUTO %in%
                                            c(77777,88888,99999)~NA_real_,
                                          TRUE~as.numeric(E14_MOR_PRINC_REND_BRUTO)),
                salario_real=salario_nominal*inflacao_acum) %>%
  # Calcular os valores médios
  #survey_mean calcula a média de uma variável continua 
  srvyr::summarise(salario_nominal=survey_mean(salario_nominal, na.rm=T,vartype = "ci"),
                   salario_real=survey_mean(salario_real, na.rm=T,vartype = "ci"))



##
# O formato long é útil para variáveis
#
# Carreguemos a inflação geral do DF, RJ e SP do IPA acumulado por mês
inflacao <- RODBC::sqlQuery(db,"select referencia,descricao,distritofederal,riodejaneiro,
                            saopaulo,brasil from inflacao.ipca_acum12m where codigo=0",stringsAsFactors=F) %>%
  # Ajustar a variável de data
  dplyr::mutate(referencia=lubridate::ymd_hms(referencia)) %>%
  # Filtrar datas superiores ao período de referência da PDAD 2015
  dplyr::filter(referencia>"2017-06-01")

#inflacao está no formato wide
# vamos transformar no formato long, usando um pacote do tidyr

inflacao_long <-
  inflacao %>%
  # Passar as colunas de cada localidade para o formato long, sendo
  # a localidade armazenada na variável "Local" e os valores na variável
  # "Inflação"
  tidyr::gather("Local","Inflação",c(3:6))

teste_data<-as.Date(inflacao_long$referencia)

#O novo objeto agora tem o local como uma tabela. Essa forma é como fazer um painel
# A função inversa desta função long é a função spread
inflacao_wide <-
  inflacao_long %>%
  # Passar as colunas de cada localidade para o formato long, sendo
  # a localidade armazenada na variável "Local" e os valores na variável
  # "Inflação"
  tidyr::spread(Local,`Inflação`,c(3:6))

library(ggthemr)
library(ggthemes)
## Faremos o plot do gráfico de inflação
inflacao %>%
  # Passar as colunas de cada localidade para o formato long, sendo
  # a localidade armazenada na variável "Local" e os valores na variável
  # "Inflação"
  tidyr::gather("Local","Inflação",c(3:6)) %>%
  # Construir o gráfico, com a referencia no eixo x (mudando para o formato data),
  # a inflação no eixo y e o Local na cor
  ggplot(aes(x=lubridate::ymd(referencia),y=`Inflação`/100,colour=Local))+
  # Construir as linhas, variando o tipo de linha conforme o local
  geom_line(aes(linetype = Local))+
  # Ajustar os rótudos dos meses do eixo x
  # quebas de 2 meses em 2 meses
  scale_x_date(date_breaks = "2 month")+
  # Ajustar o rótulo do eixo y
  scale_y_continuous(labels = scales::percent)+
  # Ajustar a legenda das cores, atribuindo cores específicas para as linhas
  scale_colour_manual(labels=c("Brasil","Distrito Federal",
                               "Rio de Janeiro","São Paulo"),
                      values=c("cadetblue4","coral4",
                               "darkgoldenrod","chartreuse4"))+
  # Ajustar a legenda das linhas, combinando com a legenda anterior
  scale_linetype_manual(labels=c("Brasil","Distrito Federal",
                                 "Rio de Janeiro","São Paulo"), 
                        values=c(1:4))+
  # Ajustar o rótulo dos eixos
  labs(y="Inflação acumulada (12 meses)",
       x="Período", title="Inflação acumulada em 12 meses de 2017 a 2018")+
  # Alterar a posição da legenda
  theme(legend.position = "bottom")+
  theme_economist_white()




# Construir o objeto com o esgotamento sanitário
esgotamento <- x %>%
  # Filtrar para as informações somente do responsável (1 obs. por domicílio)
  srvyr::filter(D03_MOR_CONDICAO_UNID==1) %>%
  # Agrupar por situação de esgotamento sanitário
  srvyr::group_by(esgotamento) %>%
  # Calcular a proporção de cada grupo
  srvyr::summarise(n=survey_mean(na.rm = T,vartype = "ci"))


library(ggthemr)
# Construir o objeto com o gráfico
esgotamento_grafico <-
  esgotamento %>%
  # Plotar os eixos x e y, reordenando os fatores, do maior para o menor resultado
  # fct_reorder faz o reordenamento 
  ggplot(aes(x=forcats::fct_reorder(esgotamento,-n),y=n,ymin=n_low,ymax=n_upp, fill=esgotamento))+
  # Construir o gráfico de barras
  geom_bar(stat = "identity")+
  # Construir os erros
  geom_errorbar(size=4, width=0,
                color="black")+
  # Ajustar os nomes dos eixos
  labs(x="",y="Nº Domicílios", title="Perfil do esgotamento no DF", subtitle="Nível percentual por perfil de esgotamento")+
  # Retirar o nome da legenda
  theme(legend.position="none")+
  # Ajustar a formatação dos rótulos
  scale_y_continuous(labels = scales::percent)+
  # Inserir informações dos resultados no gráfico
  geom_text(aes(label = paste0(round(100*n,0),"%")),
            size=3, fontface = "bold", 
            vjust = -0.25,hjust=1.25)+
  theme_fivethirtyeight()


# Plotar grafico
esgotamento_grafico


#Agora para fazer um gráfico de pizza

# Carregar o pacote ggrepel
library(ggrepel)

# Construir o objeto com as informações de esgotamento sanitário
esgotamento2 <- x %>%
  # Filtar para o responsável
  srvyr::filter(D03_MOR_CONDICAO_UNID==1) %>%
  # Agrupar por tipo de esgotamento
  srvyr::group_by(esgotamento) %>%
  # Calcular as proporções
  srvyr::summarise(n=survey_mean(na.rm = T,vartype = "ci")) %>%
  # Deixar as informações em ordem decrescente
  dplyr::arrange(-n) %>%
  # Construir uma variável auxiliar, com a posição do label
  dplyr::mutate(pos=cumsum(n)-n/8)

# Criar o tema branco, eliminando todos os elementos gráficos padrões
tema_branco <- theme_minimal()+
  theme(
    # Retirar título do eixo x
    axis.title.x = element_blank(),
    # Retirar título do eixo y
    axis.title.y = element_blank(),
    # Retirar as bordas no painel
    panel.border = element_blank(),
    # Retirar elementos textuais do eixo y
    axis.text.y = element_blank(),
    # Retirar demais elementos textuais dos eixos
    axis.text = element_blank(),
    # Retirar as linhas de grade
    panel.grid=element_blank(),
    # Retirar os ticks
    axis.ticks = element_blank())

# Construir o gráfico de pizza
esgotamento2 %>%
  # Plotar o gráfico, com as quantidades no eixo y, o preenchimento com as categorias,
  # reordenando as quabtudades, e o valor 1 para travar o eixo x
  ggplot(aes(x=1,y=n,fill=forcats::fct_reorder(esgotamento,n)))+
  # Construir as "barras"
  geom_bar(stat="identity")+
  # Transformar em coordenada polar o eixo y, com início em 0
  coord_polar("y", start=0)+
  # Retirar os nomes dos eixos
  labs(x="",y="", title="Gráfico de pizza maroto:", subtitle="O retorno") +
  # Adicionar o tema branco
  tema_branco+
  # Retirar o nome da legenda
  scale_fill_discrete(name="")+
  # Adicionar o label com os valores, usando a função repel para evitar
  # sobreposições
  geom_text_repel(aes(label = percent(n), y=pos), size=4, color="white",
                  fontface="bold")


# Salvar um arquivo com todos os objetos
save.image("objetos", compress = T)
# Remover todos os objetos do ambiente
rm(list = ls())
# Carregar os objetos salvos
load("objetos")
