#_______________________________________________________________________________________________________________________
#
# Script para gerar as series a serem mostradas no app
#
#_______________________________________________________________________________________________________________________
#
#_______________________________________________________________________________________________________________________
# inicializacao ----
#_______________________________________________________________________________________________________________________
library(jsonlite)
library(curl)
library(dplyr)
library(uuid)
#_______________________________________________________________________________________________________________________
# carregar os dados do INPC ----
#_______________________________________________________________________________________________________________________
# limpar a area de trabalho
rm(list=ls())
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/7063/metadados"
# carregar todos os metadados
inpc <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- inpc$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- inpc$variaveis
# remover o termo INPC das variaveis
variaveis$nome <- gsub("INPC - ","", variaveis$nome)
variaveis <- variaveis[variaveis$id!=45,]
# filtrar as categorias
categorias <- inpc$classificacoes$categorias[[1]]
# categorias <- categorias[categorias$id%in%c(7169,7170,7445,7486,7558,7625,7660,7712,7766,7786),]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/7063/localidades/N7%7CN6%7CN1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar coluna com os estados
localidades$estados <- c("(PA)","(CE)","(PE)","(BA)","(MG)","(ES)","(RJ)","(SP)","(PR)","(RS)","(AC)","(MA)","(SE)","(MS)","(GO)","(DF)","")
# unir as cidades e estados
localidades$nome_loc <- paste(localidades$nome_loc, localidades$estados, sep=" ")
# remover o espaco no nome das localidades
localidades[localidades$id_loc==1,"nome_loc"] <- gsub(" ","",localidades[localidades$id_loc==1,"nome_loc"])
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
               "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
base_df <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in 1:nrow(categorias)){
        codigo <- UUIDgenerate()
        lista_dados[['numero']] <- codigo
        lista_dados[['nome']] <- "Índice Nacional de Preços ao Consumidor (INPC)"
        lista_dados[['nomeCompleto']] <- "'Índice Nacional de Preços ao Consumidor (INPC)'"
        lista_dados[['descricao']] <-"O INPC tem por objetivo a correção do poder de compra dos salários, através da mensuração das variações de preços da cesta de consumo da população assalariada com mais baixo rendimento. Atualmente, a população-objetivo do INPC abrange as famílias com rendimentos de 1 a 5 salários mínimos, cuja pessoa de referência é assalariada, residentes nas regiões metropolitanas de Belém, Fortaleza, Recife, Salvador, Belo Horizonte, Vitória, Rio de Janeiro, São Paulo, Curitiba, Porto Alegre, além do Distrito Federal e dos municípios de Goiânia, Campo Grande, Rio Branco, São Luís e Aracaju."
        lista_dados[['formato']] <- "%"
        lista_dados[['fonte']] <- "IBGE"
        lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/7063/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=315[",categorias[w,"id"],"]")
        lista_dados[['idAssunto']] <- 1
        lista_dados[['periodicidade']] <- "mensal"
        lista_dados[['metrica']] <-variaveis[i,"nome"]
        lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
        lista_dados[['localidades']] <- localidades[j,"nome_loc"]
        lista_dados[['categoria']] <- categorias[w,"nome"]
        
        teste <- do.call("cbind",lista_dados)
        teste2 <- as.data.frame(teste)
        
        base_df <- bind_rows(base_df, teste2)
    }
  }
}
# apagar o numero das linhas
row.names(base_df) <- NULL
# exportar como csv
write.csv(base_df, file="C:/Users/Kleber/Documents/inpc_completo2.csv", row.names = F)
#_______________________________________________________________________________________________________________________
# carregar os dados do IPCA ----
#_______________________________________________________________________________________________________________________
# metadados
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/7060/metadados"
# carregar todos os metadados
ipca <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- ipca$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- ipca$variaveis
# remover o termo INPC das variaveis
variaveis$nome <- gsub("IPCA - ","", variaveis$nome)
variaveis <- variaveis[!variaveis$id%in%c(45,66),]
# filtrar as categorias
categorias <- ipca$classificacoes$categorias[[1]]
# categorias <- categorias[categorias$id%in%c(7169,7170,7445,7486,7558,7625,7660,7712,7766,7786),]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/7060/localidades/N7%7CN6%7CN1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar coluna com os estados
localidades$estados <- c("(PA)","(CE)","(PE)","(BA)","(MG)","(ES)","(RJ)","(SP)","(PR)","(RS)","(AC)","(MA)","(SE)","(MS)","(GO)","(DF)","")
# unir as cidades e estados
localidades$nome_loc <- paste(localidades$nome_loc, localidades$estados, sep=" ")
# remover o espaco no nome das localidades
localidades[localidades$id_loc==1,"nome_loc"] <- gsub(" ","",localidades[localidades$id_loc==1,"nome_loc"])
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
base_df <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in 1:nrow(categorias)){
      codigo <- UUIDgenerate()
      lista_dados[['numero']] <- codigo
      lista_dados[['nome']] <- "Índice Nacional de Preços ao Cons. Amplo (IPCA)"
      lista_dados[['nomeCompleto']] <- "Índice Nacional de Preços ao Consumidor Amplo (IPCA)"
      lista_dados[['descricao']] <-"O IPCA tem por objetivo medir a inflação de um conjunto de produtos e serviços comercializados no varejo, referentes ao consumo pessoal das famílias. Atualmente, a população-objetivo do IPCA abrange as famílias com rendimentos de 1 a 40 salários mínimos, qualquer que seja a fonte, residentes nas regiões metropolitanas de Belém, Fortaleza, Recife, Salvador, Belo Horizonte, Vitória, Rio de Janeiro, São Paulo, Curitiba, Porto Alegre, além do Distrito Federal e dos municípios de Goiânia, Campo Grande, Rio Branco, São Luís e Aracaju."
      lista_dados[['formato']] <- "%"
      lista_dados[['fonte']] <- "IBGE"
      lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/7060/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=315[",categorias[w,"id"],"]")
      lista_dados[['idAssunto']] <- 1
      lista_dados[['periodicidade']] <- "mensal"
      lista_dados[['metrica']] <-variaveis[i,"nome"]
      lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
      lista_dados[['localidades']] <- localidades[j,"nome_loc"]
      lista_dados[['categoria']] <- categorias[w,"nome"]
      
      teste <- do.call("cbind",lista_dados)
      teste2 <- as.data.frame(teste)
      
      base_df <- bind_rows(base_df, teste2)
    }
  }
}
# apagar o numero das linhas
row.names(base_df) <- NULL
# exportar como csv
write.csv(base_df, file="C:/Users/Kleber/Documents/ipca_completo2.csv", row.names = F)
#_______________________________________________________________________________________________________________________
# carregar os dados do IPCA-15 ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/7062/metadados"
# carregar todos os metadados
ipca15 <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- ipca15$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- ipca15$variaveis
# remover o termo INPC das variaveis
variaveis$nome <- gsub("IPCA15 - ","", variaveis$nome)
variaveis <- variaveis[!variaveis$id%in%c(357),]
# filtrar as categorias
categorias <- ipca15$classificacoes$categorias[[1]]
# categorias <- categorias[categorias$id%in%c(7169,7170,7445,7486,7558,7625,7660,7712,7766,7786),]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/7062/localidades/N7%7CN6%7CN1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar coluna com os estados
localidades$estados <- c("(PA)","(CE)","(PE)","(BA)","(MG)","(RJ)","(SP)","(PR)","(RS)","(GO)","(DF)","")
# unir as cidades e estados
localidades$nome_loc <- paste(localidades$nome_loc, localidades$estados, sep=" ")
# remover o espaco no nome das localidades
localidades[localidades$id_loc==1,"nome_loc"] <- gsub(" ","",localidades[localidades$id_loc==1,"nome_loc"])
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# formato da lista
formatoLista <- "<String, dynamic>"
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
base_df <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in 1:nrow(categorias)){
      codigo <- UUIDgenerate()
      lista_dados[['numero']] <- codigo
      lista_dados[['nome']] <- "Índice Nac. de Preços ao Cons. Amplo 15 (IPCA-15)"
      lista_dados[['nomeCompleto']] <- "Índice Nacional de Preços ao Consumidor Amplo 15 (IPCA-15)"
      lista_dados[['descricao']] <-"O IPCA-15 difere do IPCA apenas no período de coleta que abrange, em geral, do dia 16 do mês anterior ao 15 do mês de referência e na abrangência geográfica. Atualmente, a população-objetivo do IPCA-15 abrange as famílias com rendimentos de 1 a 40 salários mínimos, qualquer que seja a fonte, residentes nas regiões metropolitanas de Belém, Fortaleza, Recife, Salvador, Belo Horizonte, Rio de Janeiro, São Paulo, Curitiba, Porto Alegre, além do Distrito Federal e do município de Goiânia."
      lista_dados[['formato']] <- "%"
      lista_dados[['fonte']] <- "IBGE"
      lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/7062/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=315[",categorias[w,"id"],"]")
      lista_dados[['idAssunto']] <- 1
      lista_dados[['periodicidade']] <- "mensal"
      lista_dados[['metrica']] <-variaveis[i,"nome"]
      lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
      lista_dados[['localidades']] <- localidades[j,"nome_loc"]
      lista_dados[['categoria']] <- categorias[w,"nome"]
      teste <- do.call("cbind",lista_dados)
      teste2 <- as.data.frame(teste)
      base_df <- bind_rows(base_df, teste2)
    }
  }
}
# apagar o numero das linhas
row.names(base_df) <- NULL
# exportar como csv
write.csv(base_df, file="C:/Users/Kleber/Documents/ipca15_completo2.csv", row.names = F)
#_______________________________________________________________________________________________________________________
# carregar os dados do Indice de Precos ao Produtor - agregado 6723 ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/6723/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# remover o termo INPC das variaveis
variaveis$nome <- gsub("IPP - ","", variaveis$nome)
variaveis <- variaveis[!variaveis$id%in%c(10008),]
variaveis[variaveis$id==1396,"nome"] <- "Variação mensal"
variaveis[variaveis$id==1395,"nome"] <- "Variação acumulada no ano"
variaveis[variaveis$id==1394,"nome"] <- "Variação acumulada em 12 anos"
# filtrar as categorias
categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/6723/localidades/N1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
base_df <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in 1:nrow(categorias)){
      codigo <- UUIDgenerate()
      lista_dados[['numero']] <- codigo
      lista_dados[['nome']] <- "Índice de Preços ao Produtor (IPP)"
      lista_dados[['nomeCompleto']] <- "Índice de Preços ao Produtor (IPP)"
      lista_dados[['descricao']] <-"O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente."
      lista_dados[['formato']] <- "%"
      lista_dados[['fonte']] <- "IBGE"
      lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=844[",categorias[w,"id"],"]")
      lista_dados[['idAssunto']] <- 1
      lista_dados[['periodicidade']] <- "mensal"
      lista_dados[['metrica']] <-variaveis[i,"nome"]
      lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
      lista_dados[['localidades']] <- localidades[j,"nome_loc"]
      lista_dados[['categoria']] <- categorias[w,"nome"]
      teste <- do.call("cbind",lista_dados)
      teste2 <- as.data.frame(teste)
      base_df <- bind_rows(base_df, teste2)
    }
  }
}
# apagar o numero das linhas
row.names(base_df) <- NULL
# exportar como csv
write.csv(base_df, file="C:/Users/Kleber/Documents/ipp6723_2.csv", row.names = F)
#_______________________________________________________________________________________________________________________
# carregar os dados do Indice de Precos ao Produtor - agregado 6903 ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/6903/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# remover o termo INPC das variaveis
variaveis$nome <- gsub("IPP - ","", variaveis$nome)
variaveis <- variaveis[!variaveis$id%in%c(10008),]
variaveis[variaveis$id==1396,"nome"] <- "Variação mensal"
variaveis[variaveis$id==1395,"nome"] <- "Variação acumulada no ano"
variaveis[variaveis$id==1394,"nome"] <- "Variação acumulada em 12 anos"
# filtrar as categorias
categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/6903/localidades/N1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar variavel com o ultimo numero de serie inserido manualmente no app
numero_ini <- 101454
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
base_df <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in 1:nrow(categorias)){
      codigo <- UUIDgenerate()
      lista_dados[['numero']] <- codigo
      lista_dados[['nome']] <- "Índice de Preços ao Produtor (IPP)"
      lista_dados[['nomeCompleto']] <- "Índice de Preços ao Produtor (IPP)"
      lista_dados[['descricao']] <-"O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente."
      lista_dados[['formato']] <- "%"
      lista_dados[['fonte']] <- "IBGE"
      lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=844[",categorias[w,"id"],"]")
      lista_dados[['idAssunto']] <- 1
      lista_dados[['periodicidade']] <- "mensal"
      lista_dados[['metrica']] <-variaveis[i,"nome"]
      lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
      lista_dados[['localidades']] <- localidades[j,"nome_loc"]
      lista_dados[['categoria']] <- categorias[w,"nome"]
      teste <- do.call("cbind",lista_dados)
      teste2 <- as.data.frame(teste)
      base_df <- bind_rows(base_df, teste2)
    }
  }
}
# apagar o numero das linhas
row.names(base_df) <- NULL
# exportar como csv
write.csv(base_df, file="C:/Users/Kleber/Documents/ipp6903_2.csv", row.names = F)
#_______________________________________________________________________________________________________________________
# carregar os dados do Indice de Precos ao Produtor - agregado 6904 ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/6904/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# remover o termo INPC das variaveis
variaveis$nome <- gsub("IPP - ","", variaveis$nome)
variaveis <- variaveis[!variaveis$id%in%c(10008),]
variaveis[variaveis$id==1396,"nome"] <- "Variação mensal"
variaveis[variaveis$id==1395,"nome"] <- "Variação acumulada no ano"
variaveis[variaveis$id==1394,"nome"] <- "Variação acumulada em 12 anos"
# filtrar as categorias
categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/6904/localidades/N1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar variavel com o ultimo numero de serie inserido manualmente no app
numero_ini <- 101532
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
base_df <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in 1:nrow(categorias)){
      codigo <- UUIDgenerate()
      lista_dados[['numero']] <- codigo
      lista_dados[['nome']] <- "Índice de Preços ao Produtor (IPP)"
      lista_dados[['nomeCompleto']] <- "Índice de Preços ao Produtor (IPP)"
      lista_dados[['descricao']] <-"O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente."
      lista_dados[['formato']] <- "%"
      lista_dados[['fonte']] <- "IBGE"
      lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=844[",categorias[w,"id"],"]")
      lista_dados[['idAssunto']] <- 1
      lista_dados[['periodicidade']] <- "mensal"
      lista_dados[['metrica']] <-variaveis[i,"nome"]
      lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
      lista_dados[['localidades']] <- localidades[j,"nome_loc"]
      lista_dados[['categoria']] <- categorias[w,"nome"]
      teste <- do.call("cbind",lista_dados)
      teste2 <- as.data.frame(teste)
      base_df <- bind_rows(base_df, teste2)
    }
  }
}
# apagar o numero das linhas
row.names(base_df) <- NULL
# exportar como csv
write.csv(base_df, file="C:/Users/Kleber/Documents/ipp6904_2.csv", row.names = F)
#_______________________________________________________________________________________________________________________
# carregar os dados das Contas Nacionais Trimestrais - agregado 1620 ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/1620/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# filtrar as categorias
categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/1620/localidades/N1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar variavel com o ultimo numero de serie inserido manualmente no app
numero_ini <- 199999
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
scnt1620 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in 1:nrow(categorias)){
      codigo <- UUIDgenerate()
      lista_dados[['numero']] <- codigo
      lista_dados[['nome']] <- "PIB trimestral observado"
      lista_dados[['nomeCompleto']] <- "PIB trimestral observado (Base: média 1995 = 100)"
      lista_dados[['descricao']] <-"Número-índice de volume com base de comparação em 1990; calculado pelo encadeamento da série base móvel trimestral."
      lista_dados[['formato']] <- "Número-índice"
      lista_dados[['fonte']] <- "IBGE"
      lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/1620/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=11255[",categorias[w,"id"],"]")
      lista_dados[['idAssunto']] <- 2
      lista_dados[['periodicidade']] <- "trimestral"
      lista_dados[['metrica']] <-variaveis[i,"nome"]
      lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
      lista_dados[['localidades']] <- localidades[j,"nome_loc"]
      lista_dados[['categoria']] <- categorias[w,"nome"]
      teste <- do.call("cbind",lista_dados)
      teste2 <- as.data.frame(teste)
      scnt1620 <- bind_rows(scnt1620, teste2)
    }
  }
}
# apagar o numero das linhas
row.names(scnt1620) <- NULL
#_______________________________________________________________________________________________________________________
# carregar os dados das Contas Nacionais Trimestrais - agregado 1621 ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/1621/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# filtrar as categorias
categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/1621/localidades/N1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
scnt1621 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in 1:nrow(categorias)){
      codigo <- UUIDgenerate()
      lista_dados[['numero']] <- codigo
      lista_dados[['nome']] <- "PIB trimestral com ajuste sazonal"
      lista_dados[['nomeCompleto']] <- "PIB trimestral com ajuste sazonal (Base: média 1995 = 100)"
      lista_dados[['descricao']] <-"Número-índice com base de comparação em 1990, calculada por encadeamento da série anterior. O ajuste sazonal foi realizado apenas nas séries onde foi identificado um componente sazonal significante utilizando-se o método X-13 ARIMA."
      lista_dados[['formato']] <- "Número-índice"
      lista_dados[['fonte']] <- "IBGE"
      lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/1621/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=11255[",categorias[w,"id"],"]")
      lista_dados[['idAssunto']] <- 2
      lista_dados[['periodicidade']] <- "trimestral"
      lista_dados[['metrica']] <-variaveis[i,"nome"]
      lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
      lista_dados[['localidades']] <- localidades[j,"nome_loc"]
      lista_dados[['categoria']] <- categorias[w,"nome"]
      teste <- do.call("cbind",lista_dados)
      teste2 <- as.data.frame(teste)
      scnt1621 <- bind_rows(scnt1621, teste2)
    }
  }
}
# apagar o numero das linhas
row.names(scnt1621) <- NULL
#_______________________________________________________________________________________________________________________
# carregar os dados das Contas Nacionais Trimestrais - agregado 1846 ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/1846/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# filtrar as categorias
categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/1846/localidades/N1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
scnt1846 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in 1:nrow(categorias)){
      codigo <- UUIDgenerate()
      lista_dados[['numero']] <- codigo
      lista_dados[['nome']] <- "PIB a preços correntes"
      lista_dados[['nomeCompleto']] <- "PIB a preços correntes"
      lista_dados[['descricao']] <-"PIB a preços correntes da produção (R\\$ milhões)."
      lista_dados[['formato']] <- "R\\$ milhões"
      lista_dados[['fonte']] <- "IBGE"
      lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/1846/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=11255[",categorias[w,"id"],"]")
      lista_dados[['idAssunto']] <- 2
      lista_dados[['periodicidade']] <- "trimestral"
      lista_dados[['metrica']] <-variaveis[i,"nome"]
      lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
      lista_dados[['localidades']] <- localidades[j,"nome_loc"]
      lista_dados[['categoria']] <- categorias[w,"nome"]
      teste <- do.call("cbind",lista_dados)
      teste2 <- as.data.frame(teste)
      scnt1846 <- bind_rows(scnt1846, teste2)
    }
  }
}
# apagar o numero das linhas
row.names(scnt1846) <- NULL
#_______________________________________________________________________________________________________________________
# carregar os dados das Contas Nacionais Trimestrais - agregado 2072 ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/2072/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# filtrar as categorias
# categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/2072/localidades/N1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
scnt2072 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
      codigo <- UUIDgenerate()
      lista_dados[['numero']] <- codigo
      lista_dados[['nome']] <- "Contas econômicas trimestrais"
      lista_dados[['nomeCompleto']] <- "Contas econômicas trimestrais"
      lista_dados[['descricao']] <-"Valor das Contas econômicas trimestrais (R\\$ milhões)."
      lista_dados[['formato']] <- "R\\$ milhões"
      lista_dados[['fonte']] <- "IBGE"
      lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/2072/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]")
      lista_dados[['idAssunto']] <- 2
      lista_dados[['periodicidade']] <- "trimestral"
      lista_dados[['metrica']] <-variaveis[i,"nome"]
      lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
      lista_dados[['localidades']] <- localidades[j,"nome_loc"]
      lista_dados[['categoria']] <- "Valor"
      teste <- do.call("cbind",lista_dados)
      teste2 <- as.data.frame(teste)
      scnt2072 <- bind_rows(scnt2072, teste2)
  }
}
# apagar o numero das linhas
row.names(scnt2072) <- NULL
#_______________________________________________________________________________________________________________________
# carregar os dados das Contas Nacionais Trimestrais - agregado 2205 ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/2205/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# filtrar as categorias
categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/2205/localidades/N1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
scnt2205 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in 1:nrow(categorias)){
      codigo <- UUIDgenerate()
      lista_dados[['numero']] <- codigo
      lista_dados[['nome']] <- "Conta financeira trimestral consolidada"
      lista_dados[['nomeCompleto']] <- "Conta financeira trimestral consolidada"
      lista_dados[['descricao']] <-"Valor da conta financeira trimestral consolidada (R\\$ milhões)."
      lista_dados[['formato']] <- "R\\$ milhões"
      lista_dados[['fonte']] <- "IBGE"
      lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/2205/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=12116[",categorias[w,"id"],"]")
      lista_dados[['idAssunto']] <- 2
      lista_dados[['periodicidade']] <- "trimestral"
      lista_dados[['metrica']] <-variaveis[i,"nome"]
      lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
      lista_dados[['localidades']] <- localidades[j,"nome_loc"]
      lista_dados[['categoria']] <- categorias[w,"nome"]
      teste <- do.call("cbind",lista_dados)
      teste2 <- as.data.frame(teste)
      scnt2205 <- bind_rows(scnt2205, teste2)
    }
  }
}
# apagar o numero das linhas
row.names(scnt2205) <- NULL
#_______________________________________________________________________________________________________________________
# carregar os dados das Contas Nacionais Trimestrais - agregado 5932 ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/5932/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# filtrar as categorias
categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/5932/localidades/N1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
scnt5932 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in 1:nrow(categorias)){
      codigo <- UUIDgenerate()
      lista_dados[['numero']] <- codigo
      lista_dados[['nome']] <- "Taxa de variação do PIB trimestral"
      lista_dados[['nomeCompleto']] <- "Taxa de variação do PIB trimestral"
      lista_dados[['descricao']] <-"Representa a taxa de variação do índice de volume trimestral do PIB."
      lista_dados[['formato']] <- "%"
      lista_dados[['fonte']] <- "IBGE"
      lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/5932/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=11255[",categorias[w,"id"],"]")
      lista_dados[['idAssunto']] <- 2
      lista_dados[['periodicidade']] <- "trimestral"
      lista_dados[['metrica']] <-variaveis[i,"nome"]
      lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
      lista_dados[['localidades']] <- localidades[j,"nome_loc"]
      lista_dados[['categoria']] <- categorias[w,"nome"]
      teste <- do.call("cbind",lista_dados)
      teste2 <- as.data.frame(teste)
      scnt5932 <- bind_rows(scnt5932, teste2)
    }
  }
}
# apagar o numero das linhas
row.names(scnt5932) <- NULL
#_______________________________________________________________________________________________________________________
# carregar os dados das Contas Nacionais Trimestrais - agregado 6612 ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/6612/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# filtrar as categorias
categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/6612/localidades/N1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
scnt6612 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in 1:nrow(categorias)){
      codigo <- UUIDgenerate()
      lista_dados[['numero']] <- codigo
      lista_dados[['nome']] <- "PIB - valores encadeados a preços de 1995"
      lista_dados[['nomeCompleto']] <- "PIB - valores encadeados a preços de 1995"
      lista_dados[['descricao']] <-"PIB calculado pelo encadeamento da série base móvel trimestral."
      lista_dados[['formato']] <- "R\\$ milhões"
      lista_dados[['fonte']] <- "IBGE"
      lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/6612/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=11255[",categorias[w,"id"],"]")
      lista_dados[['idAssunto']] <- 2
      lista_dados[['periodicidade']] <- "trimestral"
      lista_dados[['metrica']] <-variaveis[i,"nome"]
      lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
      lista_dados[['localidades']] <- localidades[j,"nome_loc"]
      lista_dados[['categoria']] <- categorias[w,"nome"]
      teste <- do.call("cbind",lista_dados)
      teste2 <- as.data.frame(teste)
      scnt6612 <- bind_rows(scnt6612, teste2)
    }
  }
}
# apagar o numero das linhas
row.names(scnt6612) <- NULL
#_______________________________________________________________________________________________________________________
# carregar os dados das Contas Nacionais Trimestrais - agregado 6613 ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/6613/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# filtrar as categorias
categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/6613/localidades/N1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
scnt6613 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in 1:nrow(categorias)){
      codigo <- UUIDgenerate()
      lista_dados[['numero']] <- codigo
      lista_dados[['nome']] <- "PIB - valores encadeados a preços de 1995 com ajuste sazonal"
      lista_dados[['nomeCompleto']] <- "PIB - valores encadeados a preços de 1995 com ajuste sazonal"
      lista_dados[['descricao']] <-"PIB calculado pelos valores encadeados a preços de 1995. O ajuste sazonal foi realizado apenas nas séries onde foi identificado um componente sazonal significante utilizando-se o método X-13 ARIMA."
      lista_dados[['formato']] <- "R\\$ milhões"
      lista_dados[['fonte']] <- "IBGE"
      lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/6613/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=11255[",categorias[w,"id"],"]")
      lista_dados[['idAssunto']] <- 2
      lista_dados[['periodicidade']] <- "trimestral"
      lista_dados[['metrica']] <-variaveis[i,"nome"]
      lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
      lista_dados[['localidades']] <- localidades[j,"nome_loc"]
      lista_dados[['categoria']] <- categorias[w,"nome"]
      teste <- do.call("cbind",lista_dados)
      teste2 <- as.data.frame(teste)
      scnt6613 <- bind_rows(scnt6613, teste2)
    }
  }
}
# apagar o numero das linhas
row.names(scnt6613) <- NULL
#_______________________________________________________________________________________________________________________
# carregar os dados das Contas Nacionais Trimestrais - agregado 6726 ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/6726/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# filtrar as categorias
# categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/6726/localidades/N1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
scnt6726 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
      codigo <- UUIDgenerate()
      lista_dados[['numero']] <- codigo
      lista_dados[['nome']] <- "Taxa de poupança"
      lista_dados[['nomeCompleto']] <- "Taxa de poupança"
      lista_dados[['descricao']] <-"Representa o montante que deixa de ser consumido pela população e acaba sendo usada para investimento."
      lista_dados[['formato']] <- "% do PIB"
      lista_dados[['fonte']] <- "IBGE"
      lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/6726/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]")
      lista_dados[['idAssunto']] <- 2
      lista_dados[['periodicidade']] <- "trimestral"
      lista_dados[['metrica']] <-variaveis[i,"nome"]
      lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
      lista_dados[['localidades']] <- localidades[j,"nome_loc"]
      lista_dados[['categoria']] <- "Valor"
      teste <- do.call("cbind",lista_dados)
      teste2 <- as.data.frame(teste)
      scnt6726 <- bind_rows(scnt6726, teste2)
    
  }
}
# apagar o numero das linhas
row.names(scnt6726) <- NULL
#_______________________________________________________________________________________________________________________
# carregar os dados das Contas Nacionais Trimestrais - agregado 6727 ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/6727/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# filtrar as categorias
# categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/6727/localidades/N1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
scnt6727 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
      codigo <- UUIDgenerate()
      lista_dados[['numero']] <- codigo
      lista_dados[['nome']] <- "Taxa de investimento"
      lista_dados[['nomeCompleto']] <- "Taxa de investimento"
      lista_dados[['descricao']] <-"Representa o montante destinado ao investimento."
      lista_dados[['formato']] <- "% do PIB"
      lista_dados[['fonte']] <- "IBGE"
      lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/6727/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]")
      lista_dados[['idAssunto']] <- 2
      lista_dados[['periodicidade']] <- "trimestral"
      lista_dados[['metrica']] <-variaveis[i,"nome"]
      lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
      lista_dados[['localidades']] <- localidades[j,"nome_loc"]
      lista_dados[['categoria']] <- categorias[w,"nome"]
      teste <- do.call("cbind",lista_dados)
      teste2 <- as.data.frame(teste)
      scnt6727 <- bind_rows(scnt6727, teste2)
  }
}
# apagar o numero das linhas
row.names(scnt6727) <- NULL
# unir as tabelas
scnt_total <- bind_rows(scnt1620, scnt1621, scnt1846, scnt2072, scnt2205, scnt5932, scnt6612, scnt6613, scnt6726,
                        scnt6727)

# exportar como csv
write.csv(scnt_total, file="C:/Users/Kleber/Documents/scnt_total_2.csv", row.names = F)
#_______________________________________________________________________________________________________________________
# carregar os dados da Pesquisa Industrial Mensal - Producao Fisica (agregado 8885) ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/8885/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# ajustar o nome das variaveis
variaveis$nome <- gsub("PIMPF - ","",variaveis$nome)
# filtrar as categorias
categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/8885/localidades/N1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar variavel com o ultimo numero de serie inserido manualmente no app
# numero_ini <- 200265
# criar lista vazia
lista_dados <- list()
# data frame vazio para se preenchido
pimpf <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    codigo <- UUIDgenerate()
    lista_dados[['numero']] <- codigo
    lista_dados[['nome']] <- "Pesquisa Industrial Mensal - Produção Física"
    lista_dados[['nomeCompleto']] <- "Pesquisa Industrial Mensal - Produção Física"
    lista_dados[['descricao']] <-"Produz indicadores de curto prazo relativos ao comportamento do produto real da indústria, tendo como unidade de investigação a empresa formalmente constituída cuja principal fonte de receita seja a atividade industrial."
    lista_dados[['formato']] <- "%"
    lista_dados[['fonte']] <- "IBGE"
    lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/8885/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=542[",categorias[w,"id"],"]")
    lista_dados[['idAssunto']] <- 2
    lista_dados[['periodicidade']] <- "mensal"
    lista_dados[['metrica']] <-variaveis[i,"nome"]
    lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
    lista_dados[['localidades']] <- localidades[j,"nome_loc"]
    lista_dados[['categoria']] <- categorias[w,"nome"]
    teste <- do.call("cbind",lista_dados)
    teste2 <- as.data.frame(teste)
    pimpf <- bind_rows(pimpf, teste2)
  }
}
# apagar o numero das linhas
row.names(pimpf) <- NULL
# exportar como csv
write.csv(pimpf, file="C:/Users/Kleber/Documents/pimpf8885.csv", row.names = F)
#_______________________________________________________________________________________________________________________
# carregar os dados da Pesquisa Industrial Mensal - Producao Fisica (agregado 8886) ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/8886/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# ajustar o nome das variaveis
variaveis$nome <- gsub("PIMPF - ","",variaveis$nome)
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/8886/localidades/N1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar variavel com o ultimo numero de serie inserido manualmente no app
# numero_ini <- 200685
# criar lista vazia
lista_dados <- list()
# data frame vazio para se preenchido
pimpf <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    codigo <- UUIDgenerate()
    lista_dados[['numero']] <- codigo
    lista_dados[['nome']] <- "Pesquisa Industrial Mensal - Produção Física"
    lista_dados[['nomeCompleto']] <- "Pesquisa Industrial Mensal - Produção Física"
    lista_dados[['descricao']] <-"Produz indicadores de curto prazo relativos ao comportamento do produto real da indústria, tendo como unidade de investigação a empresa formalmente constituída cuja principal fonte de receita seja a atividade industrial."
    lista_dados[['formato']] <- "%"
    lista_dados[['fonte']] <- "IBGE"
    lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/8886/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=542[",categorias[w,"id"],"]")
    lista_dados[['idAssunto']] <- 2
    lista_dados[['periodicidade']] <- "mensal"
    lista_dados[['metrica']] <-variaveis[i,"nome"]
    lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
    lista_dados[['localidades']] <- localidades[j,"nome_loc"]
    lista_dados[['categoria']] <- categorias[w,"nome"]
    teste <- do.call("cbind",lista_dados)
    teste2 <- as.data.frame(teste)
    pimpf <- bind_rows(pimpf, teste2)
  }
}
# apagar o numero das linhas
row.names(pimpf) <- NULL
# exportar como csv
write.csv(pimpf, file="C:/Users/Kleber/Documents/pimpf8886.csv", row.names = F)
#_______________________________________________________________________________________________________________________
# carregar os dados da Pesquisa Industrial Mensal - Producao Fisica (agregado 8887) ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/8887/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# ajustar o nome das variaveis
variaveis$nome <- gsub("PIMPF - ","",variaveis$nome)
# filtrar as categorias
categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/8887/localidades/N1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar variavel com o ultimo numero de serie inserido manualmente no app
# numero_ini <- 200689
# criar lista vazia
lista_dados <- list()
# data frame vazio para se preenchido
pimpf <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    codigo <- UUIDgenerate()
    lista_dados[['numero']] <- codigo
    lista_dados[['nome']] <- "Pesquisa Industrial Mensal - Produção Física"
    lista_dados[['nomeCompleto']] <- "Pesquisa Industrial Mensal - Produção Física"
    lista_dados[['descricao']] <-"Produz indicadores de curto prazo relativos ao comportamento do produto real da indústria, tendo como unidade de investigação a empresa formalmente constituída cuja principal fonte de receita seja a atividade industrial."
    lista_dados[['formato']] <- "%"
    lista_dados[['fonte']] <- "IBGE"
    lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/8887/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=543[",categorias[w,"id"],"]")
    lista_dados[['idAssunto']] <- 2
    lista_dados[['periodicidade']] <- "mensal"
    lista_dados[['metrica']] <-variaveis[i,"nome"]
    lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
    lista_dados[['localidades']] <- localidades[j,"nome_loc"]
    lista_dados[['categoria']] <- categorias[w,"nome"]
    teste <- do.call("cbind",lista_dados)
    teste2 <- as.data.frame(teste)
    pimpf <- bind_rows(pimpf, teste2)
  }
}
# apagar o numero das linhas
row.names(pimpf) <- NULL
# exportar como csv
write.csv(pimpf, file="C:/Users/Kleber/Documents/pimpf8887.csv", row.names = F)
#_______________________________________________________________________________________________________________________
# carregar os dados da Pesquisa Industrial Mensal - Producao Fisica (agregado 8888) ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/8888/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# ajustar o nome das variaveis
variaveis$nome <- gsub("PIMPF - ","",variaveis$nome)
# filtrar as categorias
categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/8888/localidades/N1|N2|N3"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar variavel com o ultimo numero de serie inserido manualmente no app
# numero_ini <- 200833
# criar lista vazia
lista_dados <- list()
# data frame vazio para se preenchido
pimpf <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    codigo <- UUIDgenerate()
    lista_dados[['numero']] <- codigo
    lista_dados[['nome']] <- "Pesquisa Industrial Mensal - Produção Física"
    lista_dados[['nomeCompleto']] <- "Pesquisa Industrial Mensal - Produção Física"
    lista_dados[['descricao']] <-"Produz indicadores de curto prazo relativos ao comportamento do produto real da indústria, tendo como unidade de investigação a empresa formalmente constituída cuja principal fonte de receita seja a atividade industrial."
    lista_dados[['formato']] <- "%"
    lista_dados[['fonte']] <- "IBGE"
    lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/8888/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=543[",categorias[w,"id"],"]")
    lista_dados[['idAssunto']] <- 2
    lista_dados[['periodicidade']] <- "mensal"
    lista_dados[['metrica']] <-variaveis[i,"nome"]
    lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
    lista_dados[['localidades']] <- localidades[j,"nome_loc"]
    lista_dados[['categoria']] <- categorias[w,"nome"]
    teste <- do.call("cbind",lista_dados)
    teste2 <- as.data.frame(teste)
    pimpf <- bind_rows(pimpf, teste2)
  }
}
# apagar o numero das linhas
row.names(pimpf) <- NULL
# exportar como csv
write.csv(pimpf, file="C:/Users/Kleber/Documents/pimpf8888.csv", row.names = F)
#_______________________________________________________________________________________________________________________
# carregar os dados da Pesquisa Industrial Mensal - Producao Fisica  (agregado 8889) ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/8889/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# ajustar o nome das variaveis
variaveis$nome <- gsub("PIMPF - ","",variaveis$nome)
# filtrar as categorias
categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/8889/localidades/N1"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar variavel com o ultimo numero de serie inserido manualmente no app
# numero_ini <- 203911
# criar lista vazia
lista_dados <- list()
# data frame vazio para se preenchido
pimpf <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    codigo <- UUIDgenerate()
    lista_dados[['numero']] <- codigo
    lista_dados[['nome']] <- "Pesquisa Industrial Mensal - Produção Física"
    lista_dados[['nomeCompleto']] <- "Pesquisa Industrial Mensal - Produção Física"
    lista_dados[['descricao']] <-"Produz indicadores de curto prazo relativos ao comportamento do produto real da indústria, tendo como unidade de investigação a empresa formalmente constituída cuja principal fonte de receita seja a atividade industrial."
    lista_dados[['formato']] <- "%"
    lista_dados[['fonte']] <- "IBGE"
    lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/8889/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=543[",categorias[w,"id"],"]")
    lista_dados[['idAssunto']] <- 2
    lista_dados[['periodicidade']] <- "mensal"
    lista_dados[['metrica']] <-variaveis[i,"nome"]
    lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
    lista_dados[['localidades']] <- localidades[j,"nome_loc"]
    lista_dados[['categoria']] <- categorias[w,"nome"]
    teste <- do.call("cbind",lista_dados)
    teste2 <- as.data.frame(teste)
    pimpf <- bind_rows(pimpf, teste2)
  }
}
# apagar o numero das linhas
row.names(pimpf) <- NULL
# exportar como csv
write.csv(pimpf, file="C:/Users/Kleber/Documents/pimpf8889.csv", row.names = F)
#_______________________________________________________________________________________________________________________
# carregar os dados da Pesquisa Mensal de Comercio  (agregado 8190) ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/8190/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# ajustar o nome das variaveis
variaveis$nome <- gsub("PIMPF - ","",variaveis$nome)
# filtrar as categorias
categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/8190/localidades/N1|N3"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar variavel com o ultimo numero de serie inserido manualmente no app
# numero_ini <- 204187
# criar lista vazia
lista_dados <- list()
# data frame vazio para se preenchido
pimpf <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    codigo <- UUIDgenerate()
    if(i%in%grep("índice",variaveis$nome)){
      unidade <- variaveis[i,"nome"]
    } else {
      unidade <- "%"
    }
    lista_dados[['numero']] <- codigo
    lista_dados[['nome']] <- "Pesquisa Mensal de Comércio"
    lista_dados[['nomeCompleto']] <- "Pesquisa Mensal de Comércio"
    lista_dados[['descricao']] <-"A Pesquisa Mensal de Comércio produz indicadores que permitem acompanhar o comportamento conjuntural do comércio varejista no País, investigando a receita bruta de revenda nas empresas formalmente constituídas, com 20 ou mais pessoas ocupadas, e cuja atividade principal é o comércio varejista."
    lista_dados[['formato']] <- unidade
    lista_dados[['fonte']] <- "IBGE"
    lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/8190/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=11046[",categorias[w,"id"],"]")
    lista_dados[['idAssunto']] <- 2
    lista_dados[['periodicidade']] <- "mensal"
    lista_dados[['metrica']] <-variaveis[i,"nome"]
    lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
    lista_dados[['localidades']] <- localidades[j,"nome_loc"]
    lista_dados[['categoria']] <- categorias[w,"nome"]
    teste <- do.call("cbind",lista_dados)
    teste2 <- as.data.frame(teste)
    pimpf <- bind_rows(pimpf, teste2)
  }
}
# apagar o numero das linhas
row.names(pimpf) <- NULL
# exportar como csv
write.csv(pimpf, file="C:/Users/Kleber/Documents/pmc8190.csv", row.names = F)
#_______________________________________________________________________________________________________________________
# carregar os dados da Pesquisa Mensal de Comercio  (agregado 8757) ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/8757/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# ajustar o nome das variaveis
variaveis$nome <- gsub("PMC - ","",variaveis$nome)
# filtrar as categorias
categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/8757/localidades/N1|N3"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar variavel com o ultimo numero de serie inserido manualmente no app
numero_ini <- 204265
# criar lista vazia
lista_dados <- list()
# data frame vazio para se preenchido
pimpf <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    codigo <- UUIDgenerate()
    if(i%in%grep("índice",variaveis$nome)){
      unidade <- variaveis[i,"nome"]
    } else {
      unidade <- "%"
    }
    lista_dados[['numero']] <- codigo
    lista_dados[['nome']] <- "Pesquisa Mensal de Comércio"
    lista_dados[['nomeCompleto']] <- "Pesquisa Mensal de Comércio"
    lista_dados[['descricao']] <-"A Pesquisa Mensal de Comércio produz indicadores que permitem acompanhar o comportamento conjuntural do comércio varejista no País, investigando a receita bruta de revenda nas empresas formalmente constituídas, com 20 ou mais pessoas ocupadas, e cuja atividade principal é o comércio varejista."
    lista_dados[['formato']] <- unidade
    lista_dados[['fonte']] <- "IBGE"
    lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/8757/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=11046[",categorias[w,"id"],"]")
    lista_dados[['idAssunto']] <- 2
    lista_dados[['periodicidade']] <- "mensal"
    lista_dados[['metrica']] <-variaveis[i,"nome"]
    lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
    lista_dados[['localidades']] <- localidades[j,"nome_loc"]
    lista_dados[['categoria']] <- categorias[w,"nome"]
    teste <- do.call("cbind",lista_dados)
    teste2 <- as.data.frame(teste)
    pimpf <- bind_rows(pimpf, teste2)
  }
}
# apagar o numero das linhas
row.names(pimpf) <- NULL
# exportar como csv
write.csv(pimpf, file="C:/Users/Kleber/Documents/pmc8757.csv", row.names = F)
#_______________________________________________________________________________________________________________________
# carregar os dados da Pesquisa Mensal de Comercio  (agregado 8880) ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/8880/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# ajustar o nome das variaveis
variaveis$nome <- gsub("PMC - ","",variaveis$nome)
# filtrar as categorias
categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/8880/localidades/N1|N3"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar variavel com o ultimo numero de serie inserido manualmente no app
numero_ini <- 204421
# criar lista vazia
lista_dados <- list()
# data frame vazio para se preenchido
pimpf <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    codigo <- UUIDgenerate()
    if(i%in%grep("índice",variaveis$nome)){
      unidade <- variaveis[i,"nome"]
    } else {
      unidade <- "%"
    }
    lista_dados[['numero']] <- codigo
    lista_dados[['nome']] <- "Pesquisa Mensal de Comércio"
    lista_dados[['nomeCompleto']] <- "Pesquisa Mensal de Comércio"
    lista_dados[['descricao']] <-"A Pesquisa Mensal de Comércio produz indicadores que permitem acompanhar o comportamento conjuntural do comércio varejista no País, investigando a receita bruta de revenda nas empresas formalmente constituídas, com 20 ou mais pessoas ocupadas, e cuja atividade principal é o comércio varejista."
    lista_dados[['formato']] <- unidade
    lista_dados[['fonte']] <- "IBGE"
    lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/8880/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=11046[",categorias[w,"id"],"]")
    lista_dados[['idAssunto']] <- 2
    lista_dados[['periodicidade']] <- "mensal"
    lista_dados[['metrica']] <-variaveis[i,"nome"]
    lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
    lista_dados[['localidades']] <- localidades[j,"nome_loc"]
    lista_dados[['categoria']] <- categorias[w,"nome"]
    teste <- do.call("cbind",lista_dados)
    teste2 <- as.data.frame(teste)
    pimpf <- bind_rows(pimpf, teste2)
  }
}
# apagar o numero das linhas
row.names(pimpf) <- NULL
# exportar como csv
write.csv(pimpf, file="C:/Users/Kleber/Documents/pmc8880.csv", row.names = F)
#_______________________________________________________________________________________________________________________
# carregar os dados da Pesquisa Mensal de Comercio  (agregado 8881) ----
#_______________________________________________________________________________________________________________________
# metadados 
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/8881/metadados"
# carregar todos os metadados
serie <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- serie$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- serie$variaveis
# ajustar o nome das variaveis
variaveis$nome <- gsub("PMC - ","",variaveis$nome)
# filtrar as categorias
categorias <- serie$classificacoes$categorias[[1]]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/8881/localidades/N1|N3"
# carregar as localidades
localidades <- fromJSON(url_loc)
# dividir as bases
localidades1 <- localidades[,c("id","nome")]
# renomear as colunas
colnames(localidades1) <- c("id_loc","nome_loc")
# criar a segunda base
localidades2 <- localidades$nivel
# renomear a base de localidades 2
colnames(localidades2) <- c("id_nivel","nome_nivel")
# unir as bases novamente
localidades <- cbind(localidades1, localidades2)
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}
# criar variavel com o ultimo numero de serie inserido manualmente no app
numero_ini <- 204757
# criar lista vazia
lista_dados <- list()
# data frame vazio para se preenchido
pimpf <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    codigo <- UUIDgenerate()
    if(i%in%grep("índice",variaveis$nome)){
      unidade <- variaveis[i,"nome"]
    } else {
      unidade <- "%"
    }
    lista_dados[['numero']] <- codigo
    lista_dados[['nome']] <- "Pesquisa Mensal de Comércio"
    lista_dados[['nomeCompleto']] <- "Pesquisa Mensal de Comércio"
    lista_dados[['descricao']] <-"A Pesquisa Mensal de Comércio produz indicadores que permitem acompanhar o comportamento conjuntural do comércio varejista no País, investigando a receita bruta de revenda nas empresas formalmente constituídas, com 20 ou mais pessoas ocupadas, e cuja atividade principal é o comércio varejista."
    lista_dados[['formato']] <- unidade
    lista_dados[['fonte']] <- "IBGE"
    lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/8881/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=11046[",categorias[w,"id"],"]")
    lista_dados[['idAssunto']] <- 2
    lista_dados[['periodicidade']] <- "mensal"
    lista_dados[['metrica']] <-variaveis[i,"nome"]
    lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
    lista_dados[['localidades']] <- localidades[j,"nome_loc"]
    lista_dados[['categoria']] <- categorias[w,"nome"]
    teste <- do.call("cbind",lista_dados)
    teste2 <- as.data.frame(teste)
    pimpf <- bind_rows(pimpf, teste2)
  }
}
# apagar o numero das linhas
row.names(pimpf) <- NULL
# exportar como csv
write.csv(pimpf, file="C:/Users/Kleber/Documents/pmc8881.csv", row.names = F)

# exportar como csv
write.csv(base_df, file="C:/Users/Kleber/Documents/inpc_completo2.csv", row.names = F)
