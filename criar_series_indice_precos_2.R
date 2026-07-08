library(uuid)
library(jsonlite)
library(curl)
library(dplyr)

# limpar a area de trabalho
rm(list=ls())
#_______________________________________________________________________________________________________________________
# carregar os dados do INPC ----
#_______________________________________________________________________________________________________________________
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
base_inpc <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in 1:nrow(categorias)){
      codigo <- UUIDgenerate()
      lista_dados[['numero']] <- codigo
      lista_dados[['nome']] <- "Índice Nacional de Preços ao Consumidor (INPC)"
      lista_dados[['nomeCompleto']] <- "Índice Nacional de Preços ao Consumidor (INPC)"
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
      base_inpc <- bind_rows(base_inpc, teste2)
    }
  }
}
# apagar o numero das linhas
row.names(base_inpc) <- NULL
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
base_ipca <- data.frame()
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
      base_ipca <- bind_rows(base_ipca, teste2)
    }
  }
}
# apagar o numero das linhas
row.names(base_ipca) <- NULL
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
base_ipca15 <- data.frame()
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
      base_ipca15 <- bind_rows(base_ipca15, teste2)
    }
  }
}
# apagar o numero das linhas
row.names(base_ipca15) <- NULL
#_______________________________________________________________________________________________________________________
# carregar os dados do SINAPI - 647 ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/647/metadados"
# carregar todos os metadados
inpc <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- inpc$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- inpc$variaveis
# filtrar as categorias
categorias <- inpc$classificacoes$categorias[[1]]
# categorias <- categorias[categorias$id%in%c(7169,7170,7445,7486,7558,7625,7660,7712,7766,7786),]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/647/localidades/N3"
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

# 
acabamento <- data.frame(codigo = c(786,787,788), desc = c("Padrão Alto", "Padrão Normal", "Padrão Baixo"))
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
base_sinapi_647 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in 1:nrow(categorias)){
      for(k in c(1:nrow(acabamento))){
        codigo <- UUIDgenerate()
        lista_dados[['numero']] <- codigo
        lista_dados[['nome']] <- "Sistema Nac. de Pesq. de Custos e Índices da Construção Civil"
        lista_dados[['nomeCompleto']] <- "Sistema Nacional de Pesquisa de Custos e Índices da Construção Civil (SINAPI)"
        lista_dados[['descricao']] <-"O Sistema Nacional de Pesquisa de Custos e Índices da Construção Civil - SINAPI tem por objetivo a produção de séries mensais de custos e índices para o setor habitacional, e de séries mensais de salários medianos de mão de obra e preços medianos de materiais, máquinas e equipamentos e serviços da construção para os setores de saneamento básico, infraestrutura e habitação. O Sistema é uma produção conjunta do IBGE e da Caixa Econômica Federal - Caixa, realizada por meio de acordo de cooperação técnica, cabendo ao Instituto a responsabilidade da coleta, apuração e cálculo, enquanto à CAIXA, a definição e manutenção dos aspectos de engenharia, tais como projetos, composições de serviços etc. As estatísticas do SINAPI são fundamentais na programação de investimentos, sobretudo para o setor público. Os preços e custos auxiliam na elaboração, análise e avaliação de orçamentos, enquanto os índices possibilitam a atualização dos valores das despesas nos contratos e orçamentos."
        lista_dados[['formato']] <- "Reais"
        lista_dados[['fonte']] <- "IBGE"
        lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/647/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=314[",categorias[w,"id"],"]|41[",acabamento[k,"codigo"],"]")
        lista_dados[['idAssunto']] <- 1
        lista_dados[['periodicidade']] <- "mensal"
        lista_dados[['metrica']] <- paste0(variaveis[i,"nome"], " - ", acabamento[k,"desc"])
        lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
        lista_dados[['localidades']] <- localidades[j,"nome_loc"]
        lista_dados[['categoria']] <- categorias[w,"nome"]
        
        teste <- do.call("cbind",lista_dados)
        teste2 <- as.data.frame(teste)
        
        base_sinapi_647 <- bind_rows(base_sinapi_647, teste2)
      }
    }
  }
}
# apagar o numero das linhas
row.names(base_sinapi_647) <- NULL
#_______________________________________________________________________________________________________________________
# carregar os dados do SINAPI - 2296 ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/2296/metadados"
# carregar todos os metadados
inpc <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- inpc$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- inpc$variaveis
# categorias <- categorias[categorias$id%in%c(7169,7170,7445,7486,7558,7625,7660,7712,7766,7786),]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/2296/localidades/N1|N2|N3"
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
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
base_sinapi_2296 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
        codigo <- UUIDgenerate()
        lista_dados[['numero']] <- codigo
        lista_dados[['nome']] <- "Sistema Nac. de Pesq. de Custos e Índices da Construção Civil"
        lista_dados[['nomeCompleto']] <- "Sistema Nacional de Pesquisa de Custos e Índices da Construção Civil (SINAPI)"
        lista_dados[['descricao']] <-"O Sistema Nacional de Pesquisa de Custos e Índices da Construção Civil - SINAPI tem por objetivo a produção de séries mensais de custos e índices para o setor habitacional, e de séries mensais de salários medianos de mão de obra e preços medianos de materiais, máquinas e equipamentos e serviços da construção para os setores de saneamento básico, infraestrutura e habitação. O Sistema é uma produção conjunta do IBGE e da Caixa Econômica Federal - Caixa, realizada por meio de acordo de cooperação técnica, cabendo ao Instituto a responsabilidade da coleta, apuração e cálculo, enquanto à CAIXA, a definição e manutenção dos aspectos de engenharia, tais como projetos, composições de serviços etc. As estatísticas do SINAPI são fundamentais na programação de investimentos, sobretudo para o setor público. Os preços e custos auxiliam na elaboração, análise e avaliação de orçamentos, enquanto os índices possibilitam a atualização dos valores das despesas nos contratos e orçamentos."
        lista_dados[['formato']] <- variaveis[i,"unidade"]
        lista_dados[['fonte']] <- "IBGE"
        lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/2296/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]")
        lista_dados[['idAssunto']] <- 1
        lista_dados[['periodicidade']] <- inpc$periodicidade$frequencia
        lista_dados[['metrica']] <- variaveis[i,"nome"]
        lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
        lista_dados[['localidades']] <- localidades[j,"nome_loc"]
        lista_dados[['categoria']] <- "Valor"
        
        teste <- do.call("cbind",lista_dados)
        teste2 <- as.data.frame(teste)
        
        base_sinapi_2296 <- bind_rows(base_sinapi_2296, teste2)
  }
}
# apagar o numero das linhas
row.names(base_sinapi_2296) <- NULL
#_______________________________________________________________________________________________________________________
# carregar os dados do SINAPI - 6586 ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/6586/metadados"
# carregar todos os metadados
inpc <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- inpc$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- inpc$variaveis
# categorias <- categorias[categorias$id%in%c(7169,7170,7445,7486,7558,7625,7660,7712,7766,7786),]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/6586/localidades/N1|N2|N3"
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
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
base_sinapi_6586 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    codigo <- UUIDgenerate()
    lista_dados[['numero']] <- codigo
    lista_dados[['nome']] <- "Sistema Nac. de Pesq. de Custos e Índices da Construção Civil"
    lista_dados[['nomeCompleto']] <- "Sistema Nacional de Pesquisa de Custos e Índices da Construção Civil (SINAPI)"
    lista_dados[['descricao']] <-"O Sistema Nacional de Pesquisa de Custos e Índices da Construção Civil - SINAPI tem por objetivo a produção de séries mensais de custos e índices para o setor habitacional, e de séries mensais de salários medianos de mão de obra e preços medianos de materiais, máquinas e equipamentos e serviços da construção para os setores de saneamento básico, infraestrutura e habitação. O Sistema é uma produção conjunta do IBGE e da Caixa Econômica Federal - Caixa, realizada por meio de acordo de cooperação técnica, cabendo ao Instituto a responsabilidade da coleta, apuração e cálculo, enquanto à CAIXA, a definição e manutenção dos aspectos de engenharia, tais como projetos, composições de serviços etc. As estatísticas do SINAPI são fundamentais na programação de investimentos, sobretudo para o setor público. Os preços e custos auxiliam na elaboração, análise e avaliação de orçamentos, enquanto os índices possibilitam a atualização dos valores das despesas nos contratos e orçamentos."
    lista_dados[['formato']] <- variaveis[i,"unidade"]
    lista_dados[['fonte']] <- "IBGE"
    lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/6586/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]")
    lista_dados[['idAssunto']] <- 1
    lista_dados[['periodicidade']] <- inpc$periodicidade$frequencia
    lista_dados[['metrica']] <- variaveis[i,"nome"]
    lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
    lista_dados[['localidades']] <- localidades[j,"nome_loc"]
    lista_dados[['categoria']] <- "Valor"
    
    teste <- do.call("cbind",lista_dados)
    teste2 <- as.data.frame(teste)
    
    base_sinapi_6586 <- bind_rows(base_sinapi_6586, teste2)
  }
}
# apagar o numero das linhas
row.names(base_sinapi_6586) <- NULL
#_______________________________________________________________________________________________________________________
# criar a funcao que simula a classe cadastroSeries existente no app
cadastroSeries <- function(numero, nome, nomeCompleto, descricao, formato, fonte, urlAPI, idAssunto, periodicidade, metrica, nivelGeografico, localidades,
                           categoria){
  
  return(paste0("cadastroSeries(",numero,",'",nome,"','",nomeCompleto,"','",descricao,"','",formato,"','",fonte,"','",urlAPI,"',",idAssunto,",'",periodicidade,"','",metrica,
                "','",nivelGeografico,"','",localidades,"','",categoria,"'),"))
  
}


listaSeries <- list(
cadastroSeries(101381,
               'Índice de Preços ao Consumidor - Brasil (IPC-Br)',
               'Índice de Preços ao Consumidor - Brasil (IPC-Br)',
               'O Índice de Preços ao Consumidor (IPC) mede a variação de preços de um conjunto fixo de bens e serviços componentes de despesas habituais de famílias com nível de renda situado entre 1 e 33 salários mínimos mensais. Sua pesquisa de preços se desenvolve diariamente, cobrindo sete das principais capitais do país: São Paulo, Rio de Janeiro, Belo Horizonte, Salvador, Recife, Porto Alegre e Brasília. Este indicador representa a variação de preços entre o dia primeiro e o último dia do mês.',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.191/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               'Índice Geral'),
cadastroSeries(101382,
               'Índice de Preços ao Consumidor - Brasil (IPC-Br)',
               'Índice de Preços ao Consumidor - Brasil (IPC-Br)',
               'O Índice de Preços ao Consumidor (IPC) mede a variação de preços de um conjunto fixo de bens e serviços componentes de despesas habituais de famílias com nível de renda situado entre 1 e 33 salários mínimos mensais. Sua pesquisa de preços se desenvolve diariamente, cobrindo sete das principais capitais do país: São Paulo, Rio de Janeiro, Belo Horizonte, Salvador, Recife, Porto Alegre e Brasília. Este indicador representa a variação de preços entre o dia primeiro e o último dia do mês. O núcleo de inflação, também denominado de inflação subjacente, é uma medida que procuracaptar a tendência dos preços, desconsiderando distúrbios resultantes de choques temporários',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.4467/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               'Núcleo'),
cadastroSeries(101383,
               'Índice de Preços ao Consumidor - Mercado (IPC-M)',
               'Índice de Preços ao Consumidor - Mercado (IPC-M)',
               'O Índice de Preços ao Consumidor (IPC) mede a variação de preços de um conjunto fixo de bens e serviços componentes de despesas habituais de famílias com nível de renda situado entre 1 e 33 salários mínimos mensais. Sua pesquisa de preços se desenvolve diariamente, cobrindo sete das principais capitais do país: São Paulo, Rio de Janeiro, Belo Horizonte, Salvador, Recife, Porto Alegre e Brasília. A diferença deste índice em relação ao IPC-Br é que o IPC-M representa a variação dos preços entre os dias 21 do mês anterior e 20 do mês de referência',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7453/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               'Índice geral'),
cadastroSeries(101384,
               'Índice de Preços ao Consumidor - Mercado (IPC-M)',
               'Índice de Preços ao Consumidor - Mercado (IPC-M)',
               'O Índice de Preços ao Consumidor (IPC) mede a variação de preços de um conjunto fixo de bens e serviços componentes de despesas habituais de famílias com nível de renda situado entre 1 e 33 salários mínimos mensais. Sua pesquisa de preços se desenvolve diariamente, cobrindo sete das principais capitais do país: São Paulo, Rio de Janeiro, Belo Horizonte, Salvador, Recife, Porto Alegre e Brasília. A diferença deste índice em relação ao IPC-Br é que o IPC-M representa a variação dos preços entre os dias 21 do mês anterior e 20 do mês de referência',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7454/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               '1º decêndio'),
cadastroSeries(101385,
               'Índice de Preços ao Consumidor - Mercado (IPC-M)',
               'Índice de Preços ao Consumidor - Mercado (IPC-M)',
               'O Índice de Preços ao Consumidor (IPC) mede a variação de preços de um conjunto fixo de bens e serviços componentes de despesas habituais de famílias com nível de renda situado entre 1 e 33 salários mínimos mensais. Sua pesquisa de preços se desenvolve diariamente, cobrindo sete das principais capitais do país: São Paulo, Rio de Janeiro, Belo Horizonte, Salvador, Recife, Porto Alegre e Brasília. A diferença deste índice em relação ao IPC-Br é que o IPC-M representa a variação dos preços entre os dias 21 do mês anterior e 20 do mês de referência',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7455/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               '2º decêndio'),
cadastroSeries(101386,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47180]',1,'mensal','Variação mensal','Brasil','Brasil','101 Abate e fabricação de produtos de carne'),
cadastroSeries(101387,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47181]',1,'mensal','Variação mensal','Brasil','Brasil','104 Fabricação de óleos e gorduras vegetais e animais'),
cadastroSeries(101388,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47182]',1,'mensal','Variação mensal','Brasil','Brasil','105 Laticínios'),
cadastroSeries(101389,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47183]',1,'mensal','Variação mensal','Brasil','Brasil','106 Moagem, fabricação de produtos amiláceos e de alimentos para animais'),
cadastroSeries(101390,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47184]',1,'mensal','Variação mensal','Brasil','Brasil','107 Fabricação e refino de açúcar'),
cadastroSeries(101391,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47185]',1,'mensal','Variação mensal','Brasil','Brasil','108 Torrefação e moagem de café'),
cadastroSeries(101392,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47186]',1,'mensal','Variação mensal','Brasil','Brasil','151 Curtimento e outras preparações de couro'),
cadastroSeries(101393,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47187]',1,'mensal','Variação mensal','Brasil','Brasil','153 Fabricação de calçados'),
cadastroSeries(101394,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47188]',1,'mensal','Variação mensal','Brasil','Brasil','201 Fabricação de produtos químicos inorgânicos'),
cadastroSeries(101395,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47189]',1,'mensal','Variação mensal','Brasil','Brasil','203 Fabricação de resinas e elastômeros'),
cadastroSeries(101396,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47190]',1,'mensal','Variação mensal','Brasil','Brasil','205 Fabricação de defensivos agrícolas e desinfestantes domissanitários'),
cadastroSeries(101397,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47191]',1,'mensal','Variação mensal','Brasil','Brasil','221 Fabricação de produtos de borracha'),
cadastroSeries(101398,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47192]',1,'mensal','Variação mensal','Brasil','Brasil','222 Fabricação de produtos de material plástico'),
cadastroSeries(101399,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47193]',1,'mensal','Variação mensal','Brasil','Brasil','233 Fabricação de artefatos de concreto, cimento, fibrocimento, gesso e materiais semelhantes'),
cadastroSeries(101400,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47258]',1,'mensal','Variação mensal','Brasil','Brasil','234 Fabricação de produtos cerâmicos'),
cadastroSeries(101401,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47259]',1,'mensal','Variação mensal','Brasil','Brasil','242 Siderurgia'),
cadastroSeries(101402,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47260]',1,'mensal','Variação mensal','Brasil','Brasil','262 Fabricação de equipamentos de informática e periféricos'),
cadastroSeries(101403,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47261]',1,'mensal','Variação mensal','Brasil','Brasil','264 Fabricação de aparelhos de recepção, reprodução, gravação e amplificação de áudio e vídeo'),
cadastroSeries(101404,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47262]',1,'mensal','Variação mensal','Brasil','Brasil','271 Fabricação de geradores, transformadores e motores elétricos'),
cadastroSeries(101405,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47263]',1,'mensal','Variação mensal','Brasil','Brasil','275 Fabricação de eletrodomésticos'),
cadastroSeries(101406,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47264]',1,'mensal','Variação mensal','Brasil','Brasil','281 Fabricação de motores, bombas, compressores e equipamentos de transmissão'),
cadastroSeries(101407,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47265]',1,'mensal','Variação mensal','Brasil','Brasil','283 Fabricação de tratores e de máquinas e equipamentos para a agricultura e pecuária'),
cadastroSeries(101408,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=844[47266]',1,'mensal','Variação mensal','Brasil','Brasil','291 Fabricação de automóveis, camionetas e utilitários'),
cadastroSeries(101409,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47180]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','101 Abate e fabricação de produtos de carne'),
cadastroSeries(101410,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47181]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','104 Fabricação de óleos e gorduras vegetais e animais'),
cadastroSeries(101411,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47182]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','105 Laticínios'),
cadastroSeries(101412,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47183]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','106 Moagem, fabricação de produtos amiláceos e de alimentos para animais'),
cadastroSeries(101413,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47184]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','107 Fabricação e refino de açúcar'),
cadastroSeries(101414,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47185]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','108 Torrefação e moagem de café'),
cadastroSeries(101415,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47186]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','151 Curtimento e outras preparações de couro'),
cadastroSeries(101416,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47187]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','153 Fabricação de calçados'),
cadastroSeries(101417,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47188]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','201 Fabricação de produtos químicos inorgânicos'),
cadastroSeries(101418,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47189]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','203 Fabricação de resinas e elastômeros'),
cadastroSeries(101419,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47190]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','205 Fabricação de defensivos agrícolas e desinfestantes domissanitários'),
cadastroSeries(101420,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47191]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','221 Fabricação de produtos de borracha'),
cadastroSeries(101421,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47192]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','222 Fabricação de produtos de material plástico'),
cadastroSeries(101422,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47193]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','233 Fabricação de artefatos de concreto, cimento, fibrocimento, gesso e materiais semelhantes'),
cadastroSeries(101423,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47258]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','234 Fabricação de produtos cerâmicos'),
cadastroSeries(101424,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47259]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','242 Siderurgia'),
cadastroSeries(101425,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47260]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','262 Fabricação de equipamentos de informática e periféricos'),
cadastroSeries(101426,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47261]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','264 Fabricação de aparelhos de recepção, reprodução, gravação e amplificação de áudio e vídeo'),
cadastroSeries(101427,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47262]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','271 Fabricação de geradores, transformadores e motores elétricos'),
cadastroSeries(101428,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47263]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','275 Fabricação de eletrodomésticos'),
cadastroSeries(101429,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47264]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','281 Fabricação de motores, bombas, compressores e equipamentos de transmissão'),
cadastroSeries(101430,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47265]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','283 Fabricação de tratores e de máquinas e equipamentos para a agricultura e pecuária'),
cadastroSeries(101431,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=844[47266]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','291 Fabricação de automóveis, camionetas e utilitários'),
cadastroSeries(101432,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47180]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','101 Abate e fabricação de produtos de carne'),
cadastroSeries(101433,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47181]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','104 Fabricação de óleos e gorduras vegetais e animais'),
cadastroSeries(101434,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47182]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','105 Laticínios'),
cadastroSeries(101435,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47183]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','106 Moagem, fabricação de produtos amiláceos e de alimentos para animais'),
cadastroSeries(101436,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47184]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','107 Fabricação e refino de açúcar'),
cadastroSeries(101437,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47185]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','108 Torrefação e moagem de café'),
cadastroSeries(101438,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47186]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','151 Curtimento e outras preparações de couro'),
cadastroSeries(101439,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47187]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','153 Fabricação de calçados'),
cadastroSeries(101440,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47188]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','201 Fabricação de produtos químicos inorgânicos'),
cadastroSeries(101441,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47189]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','203 Fabricação de resinas e elastômeros'),
cadastroSeries(101442,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47190]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','205 Fabricação de defensivos agrícolas e desinfestantes domissanitários'),
cadastroSeries(101443,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47191]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','221 Fabricação de produtos de borracha'),
cadastroSeries(101444,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47192]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','222 Fabricação de produtos de material plástico'),
cadastroSeries(101445,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47193]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','233 Fabricação de artefatos de concreto, cimento, fibrocimento, gesso e materiais semelhantes'),
cadastroSeries(101446,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47258]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','234 Fabricação de produtos cerâmicos'),
cadastroSeries(101447,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47259]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','242 Siderurgia'),
cadastroSeries(101448,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47260]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','262 Fabricação de equipamentos de informática e periféricos'),
cadastroSeries(101449,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47261]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','264 Fabricação de aparelhos de recepção, reprodução, gravação e amplificação de áudio e vídeo'),
cadastroSeries(101450,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47262]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','271 Fabricação de geradores, transformadores e motores elétricos'),
cadastroSeries(101451,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47263]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','275 Fabricação de eletrodomésticos'),
cadastroSeries(101452,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47264]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','281 Fabricação de motores, bombas, compressores e equipamentos de transmissão'),
cadastroSeries(101453,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47265]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','283 Fabricação de tratores e de máquinas e equipamentos para a agricultura e pecuária'),
cadastroSeries(101454,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6723/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=844[47266]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','291 Fabricação de automóveis, camionetas e utilitários'),
cadastroSeries(101455,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46608]',1,'mensal','Variação mensal','Brasil','Brasil','indústria geral'),
cadastroSeries(101456,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46609]',1,'mensal','Variação mensal','Brasil','Brasil','b indústrias extrativas'),
cadastroSeries(101457,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46610]',1,'mensal','Variação mensal','Brasil','Brasil','c indústrias de transformação'),
cadastroSeries(101458,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46611]',1,'mensal','Variação mensal','Brasil','Brasil','10 fabricação de produtos alimentícios'),
cadastroSeries(101459,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46618]',1,'mensal','Variação mensal','Brasil','Brasil','11 fabricação de bebidas'),
cadastroSeries(101460,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46619]',1,'mensal','Variação mensal','Brasil','Brasil','12 fabricação de produtos do fumo'),
cadastroSeries(101461,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46620]',1,'mensal','Variação mensal','Brasil','Brasil','13 fabricação de produtos têxteis'),
cadastroSeries(101462,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46621]',1,'mensal','Variação mensal','Brasil','Brasil','14 confecção de artigos do vestuário e acessórios'),
cadastroSeries(101463,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46622]',1,'mensal','Variação mensal','Brasil','Brasil','15 preparação de couros e fabricação de artefatos de couro, artigos para viagem e calçados'),
cadastroSeries(101464,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46625]',1,'mensal','Variação mensal','Brasil','Brasil','16 fabricação de produtos de madeira'),
cadastroSeries(101465,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46626]',1,'mensal','Variação mensal','Brasil','Brasil','17 fabricação de celulose, papel e produtos de papel'),
cadastroSeries(101466,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46627]',1,'mensal','Variação mensal','Brasil','Brasil','18 impressão e reprodução de gravações'),
cadastroSeries(101467,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46628]',1,'mensal','Variação mensal','Brasil','Brasil','19 fabricação de coque, de produtos derivados do petróleo e de biocombustíveis'),
cadastroSeries(101468,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46633]',1,'mensal','Variação mensal','Brasil','Brasil','20b fabricação de sabões, detergentes, produtos de limpeza, cosméticos, produtos de perfumaria e de higiene pessoal'),
cadastroSeries(101469,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46629]',1,'mensal','Variação mensal','Brasil','Brasil','20c fabricação de outros produtos químicos'),
cadastroSeries(101470,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46634]',1,'mensal','Variação mensal','Brasil','Brasil','21 fabricação de produtos farmoquímicos e farmacêuticos'),
cadastroSeries(101471,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46635]',1,'mensal','Variação mensal','Brasil','Brasil','22 fabricação de produtos de borracha e de material plástico'),
cadastroSeries(101472,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46638]',1,'mensal','Variação mensal','Brasil','Brasil','23 fabricação de produtos de minerais não metálicos'),
cadastroSeries(101473,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46641]',1,'mensal','Variação mensal','Brasil','Brasil','24 metalurgia'),
cadastroSeries(101474,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46643]',1,'mensal','Variação mensal','Brasil','Brasil','25 fabricação de produtos de metal, exceto máquinas e equipamentos'),
cadastroSeries(101475,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46644]',1,'mensal','Variação mensal','Brasil','Brasil','26 fabricação de equipamentos de informática, produtos eletrônicos e ópticos'),
cadastroSeries(101476,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46647]',1,'mensal','Variação mensal','Brasil','Brasil','27 fabricação de máquinas, aparelhos e materiais elétricos'),
cadastroSeries(101477,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46650]',1,'mensal','Variação mensal','Brasil','Brasil','28 fabricação de máquinas e equipamentos'),
cadastroSeries(101478,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46653]',1,'mensal','Variação mensal','Brasil','Brasil','29 fabricação de veículos automotores, reboques e carrocerias'),
cadastroSeries(101479,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46655]',1,'mensal','Variação mensal','Brasil','Brasil','30 fabricação de outros equipamentos de transporte, exceto veículos automotores'),
cadastroSeries(101480,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=842[46656]',1,'mensal','Variação mensal','Brasil','Brasil','31 fabricação de móveis'),
cadastroSeries(101481,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46608]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','indústria geral'),
cadastroSeries(101482,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46609]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','b indústrias extrativas'),
cadastroSeries(101483,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46610]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','c indústrias de transformação'),
cadastroSeries(101484,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46611]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','10 fabricação de produtos alimentícios'),
cadastroSeries(101485,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46618]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','11 fabricação de bebidas'),
cadastroSeries(101486,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46619]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','12 fabricação de produtos do fumo'),
cadastroSeries(101487,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46620]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','13 fabricação de produtos têxteis'),
cadastroSeries(101488,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46621]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','14 confecção de artigos do vestuário e acessórios'),
cadastroSeries(101489,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46622]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','15 preparação de couros e fabricação de artefatos de couro, artigos para viagem e calçados'),
cadastroSeries(101490,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46625]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','16 fabricação de produtos de madeira'),
cadastroSeries(101491,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46626]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','17 fabricação de celulose, papel e produtos de papel'),
cadastroSeries(101492,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46627]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','18 impressão e reprodução de gravações'),
cadastroSeries(101493,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46628]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','19 fabricação de coque, de produtos derivados do petróleo e de biocombustíveis'),
cadastroSeries(101494,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46633]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','20b fabricação de sabões, detergentes, produtos de limpeza, cosméticos, produtos de perfumaria e de higiene pessoal'),
cadastroSeries(101495,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46629]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','20c fabricação de outros produtos químicos'),
cadastroSeries(101496,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46634]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','21 fabricação de produtos farmoquímicos e farmacêuticos'),
cadastroSeries(101497,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46635]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','22 fabricação de produtos de borracha e de material plástico'),
cadastroSeries(101498,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46638]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','23 fabricação de produtos de minerais não metálicos'),
cadastroSeries(101499,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46641]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','24 metalurgia'),
cadastroSeries(101500,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46643]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','25 fabricação de produtos de metal, exceto máquinas e equipamentos'),
cadastroSeries(101501,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46644]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','26 fabricação de equipamentos de informática, produtos eletrônicos e ópticos'),
cadastroSeries(101502,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46647]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','27 fabricação de máquinas, aparelhos e materiais elétricos'),
cadastroSeries(101503,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46650]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','28 fabricação de máquinas e equipamentos'),
cadastroSeries(101504,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46653]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','29 fabricação de veículos automotores, reboques e carrocerias'),
cadastroSeries(101505,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46655]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','30 fabricação de outros equipamentos de transporte, exceto veículos automotores'),
cadastroSeries(101506,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=842[46656]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','31 fabricação de móveis'),
cadastroSeries(101507,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46608]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','indústria geral'),
cadastroSeries(101508,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46609]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','b indústrias extrativas'),
cadastroSeries(101509,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46610]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','c indústrias de transformação'),
cadastroSeries(101510,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46611]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','10 fabricação de produtos alimentícios'),
cadastroSeries(101511,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46618]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','11 fabricação de bebidas'),
cadastroSeries(101512,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46619]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','12 fabricação de produtos do fumo'),
cadastroSeries(101513,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46620]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','13 fabricação de produtos têxteis'),
cadastroSeries(101514,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46621]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','14 confecção de artigos do vestuário e acessórios'),
cadastroSeries(101515,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46622]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','15 preparação de couros e fabricação de artefatos de couro, artigos para viagem e calçados'),
cadastroSeries(101516,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46625]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','16 fabricação de produtos de madeira'),
cadastroSeries(101517,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46626]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','17 fabricação de celulose, papel e produtos de papel'),
cadastroSeries(101518,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46627]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','18 impressão e reprodução de gravações'),
cadastroSeries(101519,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46628]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','19 fabricação de coque, de produtos derivados do petróleo e de biocombustíveis'),
cadastroSeries(101520,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46633]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','20b fabricação de sabões, detergentes, produtos de limpeza, cosméticos, produtos de perfumaria e de higiene pessoal'),
cadastroSeries(101521,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46629]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','20c fabricação de outros produtos químicos'),
cadastroSeries(101522,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46634]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','21 fabricação de produtos farmoquímicos e farmacêuticos'),
cadastroSeries(101523,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46635]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','22 fabricação de produtos de borracha e de material plástico'),
cadastroSeries(101524,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46638]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','23 fabricação de produtos de minerais não metálicos'),
cadastroSeries(101525,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46641]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','24 metalurgia'),
cadastroSeries(101526,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46643]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','25 fabricação de produtos de metal, exceto máquinas e equipamentos'),
cadastroSeries(101527,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46644]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','26 fabricação de equipamentos de informática, produtos eletrônicos e ópticos'),
cadastroSeries(101528,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46647]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','27 fabricação de máquinas, aparelhos e materiais elétricos'),
cadastroSeries(101529,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46650]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','28 fabricação de máquinas e equipamentos'),
cadastroSeries(101530,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46653]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','29 fabricação de veículos automotores, reboques e carrocerias'),
cadastroSeries(101531,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46655]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','30 fabricação de outros equipamentos de transporte, exceto veículos automotores'),
cadastroSeries(101532,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6903/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=842[46656]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','31 fabricação de móveis'),
cadastroSeries(101533,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=543[33586]',1,'mensal','Variação mensal','Brasil','Brasil','Indústria geral'),
cadastroSeries(101534,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=543[33583]',1,'mensal','Variação mensal','Brasil','Brasil','Bens de capital (BK)'),
cadastroSeries(101535,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=543[33585]',1,'mensal','Variação mensal','Brasil','Brasil','Bens intermediários (BI)'),
cadastroSeries(101536,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=543[33584]',1,'mensal','Variação mensal','Brasil','Brasil','Bens de consumo (BC)'),
cadastroSeries(101537,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=543[33580]',1,'mensal','Variação mensal','Brasil','Brasil','Bens de consumo duráveis (BCD)'),
cadastroSeries(101538,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/1396?localidades=N1[1]&classificacao=543[33579]',1,'mensal','Variação mensal','Brasil','Brasil','Bens de consumo semiduráveis e não duráveis (BCND)'),
cadastroSeries(101539,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=543[33586]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','Indústria geral'),
cadastroSeries(101540,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=543[33583]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','Bens de capital (BK)'),
cadastroSeries(101541,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=543[33585]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','Bens intermediários (BI)'),
cadastroSeries(101542,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=543[33584]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','Bens de consumo (BC)'),
cadastroSeries(101543,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=543[33580]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','Bens de consumo duráveis (BCD)'),
cadastroSeries(101544,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/1395?localidades=N1[1]&classificacao=543[33579]',1,'mensal','Variação acumulada no ano','Brasil','Brasil','Bens de consumo semiduráveis e não duráveis (BCND)'),
cadastroSeries(101545,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=543[33586]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','Indústria geral'),
cadastroSeries(101546,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=543[33583]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','Bens de capital (BK)'),
cadastroSeries(101547,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=543[33585]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','Bens intermediários (BI)'),
cadastroSeries(101548,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=543[33584]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','Bens de consumo (BC)'),
cadastroSeries(101549,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=543[33580]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','Bens de consumo duráveis (BCD)'),
cadastroSeries(101550,'Índice de Preços ao Produtor (IPP)','Índice de Preços ao Produtor (IPP)','O Índice de Preços ao Produtor - IPP, cujo âmbito são as indústrias extrativas e de transformação, tem como principal objetivo mensurar a mudança média dos preços de venda recebidos pelos produtores domésticos de bens e serviços, bem como sua evolução ao longo do tempo, sinalizando as tendências inflacionárias de curto prazo no País. O IPP investiga, em pouco mais de 2.100 empresas, os preços recebidos pelo produtor, isentos de impostos, tarifas e fretes e definidos segundo as práticas comerciais mais usuais. Os produtos coletados são especificados em detalhes (aspectos físicos e de transação), garantindo, dessa forma, que sejam comparados produtos homogêneos ao longo do tempo. Com isso, coletam-se cerca de 6.000 preços mensalmente.','%','IBGE','https://servicodados.ibge.gov.br/api/v3/agregados/6904/periodos/all/variaveis/1394?localidades=N1[1]&classificacao=543[33579]',1,'mensal','Variação acumulada em 12 meses','Brasil','Brasil','Bens de consumo semiduráveis e não duráveis (BCND)'),
cadastroSeries(101551,
               'Índice de Preços ao Produtor Amplo (IPA-DI)',
               'Índice de Preços ao Produtor Amplo (IPA-DI)',
               'O Índice de Preços ao Produtor Amplo (IPA) registra variações de preços de produtos agropecuários e industriais nas transações interempresariais, isto é, nos estágios de comercialização anteriores ao consumo final. A pesquisa de preços em que se baseia o cálculo do IPA é realizada continuamente, sendo feitas apurações a cada decêndio. O IPA está disponível nas mesmas versões do IGP (IPA-10, IPA-M e IPA-DI), que têm em comum a amostra de produtos e o cálculo, diferindo apenas no período de coleta de preços.',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.225/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               'Índice Geral'),
cadastroSeries(101551,
               'Índice de Preços ao Produtor Amplo (IPA-DI)',
               'Índice de Preços ao Produtor Amplo (IPA-DI)',
               'O Índice de Preços ao Produtor Amplo (IPA) registra variações de preços de produtos agropecuários e industriais nas transações interempresariais, isto é, nos estágios de comercialização anteriores ao consumo final. A pesquisa de preços em que se baseia o cálculo do IPA é realizada continuamente, sendo feitas apurações a cada decêndio. O IPA está disponível nas mesmas versões do IGP (IPA-10, IPA-M e IPA-DI), que têm em comum a amostra de produtos e o cálculo, diferindo apenas no período de coleta de preços.',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7459/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               'Produtos industriais'),
cadastroSeries(101551,
               'Índice de Preços ao Produtor Amplo (IPA-DI)',
               'Índice de Preços ao Produtor Amplo (IPA-DI)',
               'O Índice de Preços ao Produtor Amplo (IPA) registra variações de preços de produtos agropecuários e industriais nas transações interempresariais, isto é, nos estágios de comercialização anteriores ao consumo final. A pesquisa de preços em que se baseia o cálculo do IPA é realizada continuamente, sendo feitas apurações a cada decêndio. O IPA está disponível nas mesmas versões do IGP (IPA-10, IPA-M e IPA-DI), que têm em comum a amostra de produtos e o cálculo, diferindo apenas no período de coleta de preços.',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7460/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               'Produtos agrícolas'),
cadastroSeries(101552,
               'Índice de Preços ao Produtor - Mercado (IPA-M)',
               'Índice de Preços ao Produtor - Mercado (IPA-M)',
               'O Índice de Preços ao Produtor Amplo (IPA) registra variações de preços de produtos agropecuários e industriais nas transações interempresariais, isto é, nos estágios de comercialização anteriores ao consumo final. A pesquisa de preços em que se baseia o cálculo do IPA é realizada continuamente, sendo feitas apurações a cada decêndio. O IPA está disponível nas mesmas versões do IGP (IPA-10, IPA-M e IPA-DI), que têm em comum a amostra de produtos e o cálculo, diferindo apenas no período de coleta de preços.',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7450/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               'Índice Geral'),
cadastroSeries(101553,
               'Índice de Preços ao Produtor - Mercado (IPA-M)',
               'Índice de Preços ao Produtor - Mercado (IPA-M)',
               'O Índice de Preços ao Produtor Amplo (IPA) registra variações de preços de produtos agropecuários e industriais nas transações interempresariais, isto é, nos estágios de comercialização anteriores ao consumo final. A pesquisa de preços em que se baseia o cálculo do IPA é realizada continuamente, sendo feitas apurações a cada decêndio. O IPA está disponível nas mesmas versões do IGP (IPA-10, IPA-M e IPA-DI), que têm em comum a amostra de produtos e o cálculo, diferindo apenas no período de coleta de preços.',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7451/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               '1º decêndio'),
cadastroSeries(101554,
               'Índice de Preços ao Produtor - Mercado (IPA-M)',
               'Índice de Preços ao Produtor - Mercado (IPA-M)',
               'O Índice de Preços ao Produtor Amplo (IPA) registra variações de preços de produtos agropecuários e industriais nas transações interempresariais, isto é, nos estágios de comercialização anteriores ao consumo final. A pesquisa de preços em que se baseia o cálculo do IPA é realizada continuamente, sendo feitas apurações a cada decêndio. O IPA está disponível nas mesmas versões do IGP (IPA-10, IPA-M e IPA-DI), que têm em comum a amostra de produtos e o cálculo, diferindo apenas no período de coleta de preços.',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7452/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               '2º decêndio'),
cadastroSeries(101555,
               'Índice Nac. Custo da Construção (INCC)',
               'Índice Nacional de Custo da Construção (INCC)',
               'O INCC calcula a evolução do custo da construção em sete capitais brasileiras: três no Sudeste (Rio de Janeiro, São Paulo e Belo Horizonte), duas no Nordeste (Recife e Salvador), uma no Centro-Oeste (Brasília) e uma no Sul (Porto Alegre). As apurações são mensais e estão disponíveis nas versões 10, M e DI. A diferença entre elas é apenas o período de coleta, que ocorre sempre ao longo de 30 dias, mas com intervalos de 10 dias entre uma e outra.',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.192/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               'Índice Geral'),
cadastroSeries(101556,
               'Índice Nac. Custo da Construção - Mercado (INCC-M)',
               'Índice Nacional de Custo da Construção - Mercado (INCC-M)',
               'O INCC calcula a evolução do custo da construção em sete capitais brasileiras: três no Sudeste (Rio de Janeiro, São Paulo e Belo Horizonte), duas no Nordeste (Recife e Salvador), uma no Centro-Oeste (Brasília) e uma no Sul (Porto Alegre). As apurações são mensais e estão disponíveis nas versões 10, M e DI. A diferença entre elas é apenas o período de coleta, que ocorre sempre ao longo de 30 dias, mas com intervalos de 10 dias entre uma e outra.',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7456/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               'Índice Geral'),
cadastroSeries(101557,
               'Índice Nac. Custo da Construção - Mercado (INCC-M)',
               'Índice Nacional de Custo da Construção - Mercado (INCC-M)',
               'O INCC calcula a evolução do custo da construção em  sete capitais brasileiras: três no Sudeste (Rio de Janeiro, São Paulo e Belo Horizonte), duas no Nordeste (Recife e Salvador), uma no Centro-Oeste (Brasília) e uma no Sul (Porto Alegre). As apurações são mensais e estão disponíveis nas versões 10, M e DI. A diferença entre elas é apenas o período de coleta, que ocorre sempre ao longo de 30 dias, mas com intervalos de 10 dias entre uma e outra.',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7457/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               '1º decêndio'),
cadastroSeries(101558,
               'Índice Nac. Custo da Construção - Mercado (INCC-M)',
               'Índice Nacional de Custo da Construção - Mercado (INCC-M)',
               'O INCC calcula a evolução do custo da construção em  sete capitais brasileiras: três no Sudeste (Rio de Janeiro, São Paulo e Belo Horizonte), duas no Nordeste (Recife e Salvador), uma no Centro-Oeste (Brasília) e uma no Sul (Porto Alegre). As apurações são mensais e estão disponíveis nas versões 10, M e DI. A diferença entre elas é apenas o período de coleta, que ocorre sempre ao longo de 30 dias, mas com intervalos de 10 dias entre uma e outra.',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7458/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               '2º decêndio'),
cadastroSeries(101561,
               'Índice Nac. Custo da Construção - Disp. Interna (INCC-DI)',
               'Índice Nacional de Custo da Construção - Disponibilidade Interna (INCC-DI)',
               'O INCC calcula a evolução do custo da construção em  sete capitais brasileiras: três no Sudeste (Rio de Janeiro, São Paulo e Belo Horizonte), duas no Nordeste (Recife e Salvador), uma no Centro-Oeste (Brasília) e uma no Sul (Porto Alegre). As apurações são mensais e estão disponíveis nas versões 10, M e DI. A diferença entre elas é apenas o período de coleta, que ocorre sempre ao longo de 30 dias, mas com intervalos de 10 dias entre uma e outra.',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7461/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               'Mão de obra'),
cadastroSeries(101562,
               'Índice Nac. Custo da Construção - Disp. Interna (INCC-DI)',
               'Índice Nacional de Custo da Construção - Disponibilidade Interna (INCC-DI)',
               'O INCC calcula a evolução do custo da construção em  sete capitais brasileiras: três no Sudeste (Rio de Janeiro, São Paulo e Belo Horizonte), duas no Nordeste (Recife e Salvador), uma no Centro-Oeste (Brasília) e uma no Sul (Porto Alegre). As apurações são mensais e estão disponíveis nas versões 10, M e DI. A diferença entre elas é apenas o período de coleta, que ocorre sempre ao longo de 30 dias, mas com intervalos de 10 dias entre uma e outra.',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7462/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               'Materiais e serviços'),
cadastroSeries(101563,
               'Índice Geral de Preços do Mercado (IGP-M)',
               'Índice Geral de Preços do Mercado (IGP-M)',
               'O indicador foi criado no final dos anos de 1940 para ser uma medida abrangente do movimento de preços, que englobasse não apenas diferentes atividades como também etapas distintas do processo produtivo. Dessa forma, o IGP é um indicador mensal do nível de atividade econômica do país, englobando seus principais setores. O IGP possui três versões com coleta de preços encadeada: o IGP-10 (com base nos preços apurados dos dias 11 do mês anterior ao dia 10 do mês da coleta), IGP-DI (de 1 a 30) e o mais popular deles, o Índice Geral de Preços – Mercado, ou simplesmente IGP-M, que apura informações sobre a variação de preços do dia 21 do mês anterior ao dia 20 do mês de coleta.',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.189/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               'Índice Geral'),
cadastroSeries(101564,
               'Índice Geral de Preços do Mercado (IGP-M)',
               'Índice Geral de Preços do Mercado (IGP-M)',
               'O indicador foi criado no final dos anos de 1940 para ser uma medida abrangente do movimento de preços, que englobasse não apenas diferentes atividades como também etapas distintas do processo produtivo. Dessa forma, o IGP é um indicador mensal do nível de atividade econômica do país, englobando seus principais setores. O IGP possui três versões com coleta de preços encadeada: o IGP-10 (com base nos preços apurados dos dias 11 do mês anterior ao dia 10 do mês da coleta), IGP-DI (de 1 a 30) e o mais popular deles, o Índice Geral de Preços – Mercado, ou simplesmente IGP-M, que apura informações sobre a variação de preços do dia 21 do mês anterior ao dia 20 do mês de coleta.',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7448/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               '1º decêndio'),
cadastroSeries(101565,
               'Índice Geral de Preços do Mercado (IGP-M)',
               'Índice Geral de Preços do Mercado (IGP-M)',
               'O indicador foi criado no final dos anos de 1940 para ser uma medida abrangente do movimento de preços, que englobasse não apenas diferentes atividades como também etapas distintas do processo produtivo. Dessa forma, o IGP é um indicador mensal do nível de atividade econômica do país, englobando seus principais setores. O IGP possui três versões com coleta de preços encadeada: o IGP-10 (com base nos preços apurados dos dias 11 do mês anterior ao dia 10 do mês da coleta), IGP-DI (de 1 a 30) e o mais popular deles, o Índice Geral de Preços – Mercado, ou simplesmente IGP-M, que apura informações sobre a variação de preços do dia 21 do mês anterior ao dia 20 do mês de coleta.',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7449/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               '2º decêndio'),
cadastroSeries(101566,
               'Índice Geral de Preços - Disponib. Interna (IGP-DI)',
               'Índice Geral de Preços - Disponibilidade Interna (IGP-DI)',
               'O indicador foi criado no final dos anos de 1940 para ser uma medida abrangente do movimento de preços, que englobasse não apenas diferentes atividades como também etapas distintas do processo produtivo. Dessa forma, o IGP é um indicador mensal do nível de atividade econômica do país, englobando seus principais setores. O IGP possui três versões com coleta de preços encadeada: o IGP-10 (com base nos preços apurados dos dias 11 do mês anterior ao dia 10 do mês da coleta), IGP-DI (de 1 a 30) e o mais popular deles, o Índice Geral de Preços – Mercado, ou simplesmente IGP-M, que apura informações sobre a variação de preços do dia 21 do mês anterior ao dia 20 do mês de coleta.',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.190/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               'Índice Geral'),
cadastroSeries(101567,
               'Índice Geral de Preços 10 (IGP-10)',
               'Índice Geral de Preços 10 (IGP-10)',
               'O indicador foi criado no final dos anos de 1940 para ser uma medida abrangente do movimento de preços, que englobasse não apenas diferentes atividades como também etapas distintas do processo produtivo. Dessa forma, o IGP é um indicador mensal do nível de atividade econômica do país, englobando seus principais setores. O IGP possui três versões com coleta de preços encadeada: o IGP-10 (com base nos preços apurados dos dias 11 do mês anterior ao dia 10 do mês da coleta), IGP-DI (de 1 a 30) e o mais popular deles, o Índice Geral de Preços – Mercado, ou simplesmente IGP-M, que apura informações sobre a variação de preços do dia 21 do mês anterior ao dia 20 do mês de coleta.',
               '%',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7447/dados?formato=json',
               1,
               'mensal',
               'Variação mensal',
               'Brasil',
               'Brasil',
               'Índice Geral'),
cadastroSeries(101588,
               'Custo da cesta básica',
               'Custo da cesta básica',
               'Valor da cesta básica conforme estabelecido no Decreto nº 399, de 30.04.1938, composta de treze produtos de alimentação básica, definidos no mesmo Decreto.',
               'R$',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7479/dados?formato=json',
               1,
               'mensal',
               'Valor da cesta básica',
               'Município',
               'Aracaju (SE)',
               'Valor em reais'),
cadastroSeries(101589,
               'Custo da cesta básica',
               'Custo da cesta básica',
               'Valor da cesta básica conforme estabelecido no Decreto nº 399, de 30.04.1938, composta de treze produtos de alimentação básica, definidos no mesmo Decreto.',
               'R$',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7480/dados?formato=json',
               1,
               'mensal',
               'Valor da cesta básica',
               'Município',
               'Belém (PA)',
               'Valor em reais'),
cadastroSeries(101590,
               'Custo da cesta básica',
               'Custo da cesta básica',
               'Valor da cesta básica conforme estabelecido no Decreto nº 399, de 30.04.1938, composta de treze produtos de alimentação básica, definidos no mesmo Decreto.',
               'R$',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7481/dados?formato=json',
               1,
               'mensal',
               'Valor da cesta básica',
               'Município',
               'Belo Horizonte (MG)',
               'Valor em reais'),
cadastroSeries(101591,
               'Custo da cesta básica',
               'Custo da cesta básica',
               'Valor da cesta básica conforme estabelecido no Decreto nº 399, de 30.04.1938, composta de treze produtos de alimentação básica, definidos no mesmo Decreto.',
               'R$',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7482/dados?formato=json',
               1,
               'mensal',
               'Valor da cesta básica',
               'Município',
               'Brasília (DF)',
               'Valor em reais'),
cadastroSeries(101592,
               'Custo da cesta básica',
               'Custo da cesta básica',
               'Valor da cesta básica conforme estabelecido no Decreto nº 399, de 30.04.1938, composta de treze produtos de alimentação básica, definidos no mesmo Decreto.',
               'R$',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7483/dados?formato=json',
               1,
               'mensal',
               'Valor da cesta básica',
               'Município',
               'Curitiba (PR)',
               'Valor em reais'),
cadastroSeries(101593,
               'Custo da cesta básica',
               'Custo da cesta básica',
               'Valor da cesta básica conforme estabelecido no Decreto nº 399, de 30.04.1938, composta de treze produtos de alimentação básica, definidos no mesmo Decreto.',
               'R$',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7484/dados?formato=json',
               1,
               'mensal',
               'Valor da cesta básica',
               'Município',
               'Florianópolis (SC)',
               'Valor em reais'),
cadastroSeries(101594,
               'Custo da cesta básica',
               'Custo da cesta básica',
               'Valor da cesta básica conforme estabelecido no Decreto nº 399, de 30.04.1938, composta de treze produtos de alimentação básica, definidos no mesmo Decreto.',
               'R$',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7485/dados?formato=json',
               1,
               'mensal',
               'Valor da cesta básica',
               'Município',
               'Fortaleza (CE)',
               'Valor em reais'),
cadastroSeries(101595,
               'Custo da cesta básica',
               'Custo da cesta básica',
               'Valor da cesta básica conforme estabelecido no Decreto nº 399, de 30.04.1938, composta de treze produtos de alimentação básica, definidos no mesmo Decreto.',
               'R$',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7486/dados?formato=json',
               1,
               'mensal',
               'Valor da cesta básica',
               'Município',
               'Goiânia (GO)',
               'Valor em reais'),
cadastroSeries(101596,
               'Custo da cesta básica',
               'Custo da cesta básica',
               'Valor da cesta básica conforme estabelecido no Decreto nº 399, de 30.04.1938, composta de treze produtos de alimentação básica, definidos no mesmo Decreto.',
               'R$',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7487/dados?formato=json',
               1,
               'mensal',
               'Valor da cesta básica',
               'Município',
               'João Pessoa (PB)',
               'Valor em reais'),
cadastroSeries(101597,
               'Custo da cesta básica',
               'Custo da cesta básica',
               'Valor da cesta básica conforme estabelecido no Decreto nº 399, de 30.04.1938, composta de treze produtos de alimentação básica, definidos no mesmo Decreto.',
               'R$',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7488/dados?formato=json',
               1,
               'mensal',
               'Valor da cesta básica',
               'Município',
               'Natal (RN)',
               'Valor em reais'),
cadastroSeries(101598,
               'Custo da cesta básica',
               'Custo da cesta básica',
               'Valor da cesta básica conforme estabelecido no Decreto nº 399, de 30.04.1938, composta de treze produtos de alimentação básica, definidos no mesmo Decreto.',
               'R$',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7489/dados?formato=json',
               1,
               'mensal',
               'Valor da cesta básica',
               'Município',
               'Porto Alegre (RS)',
               'Valor em reais'),
cadastroSeries(101599,
               'Custo da cesta básica',
               'Custo da cesta básica',
               'Valor da cesta básica conforme estabelecido no Decreto nº 399, de 30.04.1938, composta de treze produtos de alimentação básica, definidos no mesmo Decreto.',
               'R$',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7490/dados?formato=json',
               1,
               'mensal',
               'Valor da cesta básica',
               'Município',
               'Recife (PE)',
               'Valor em reais'),
cadastroSeries(101600,
               'Custo da cesta básica',
               'Custo da cesta básica',
               'Valor da cesta básica conforme estabelecido no Decreto nº 399, de 30.04.1938, composta de treze produtos de alimentação básica, definidos no mesmo Decreto.',
               'R$',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7491/dados?formato=json',
               1,
               'mensal',
               'Valor da cesta básica',
               'Município',
               'Rio de Janeiro (RJ)',
               'Valor em reais'),
cadastroSeries(101601,
               'Custo da cesta básica',
               'Custo da cesta básica',
               'Valor da cesta básica conforme estabelecido no Decreto nº 399, de 30.04.1938, composta de treze produtos de alimentação básica, definidos no mesmo Decreto.',
               'R$',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7492/dados?formato=json',
               1,
               'mensal',
               'Valor da cesta básica',
               'Município',
               'Salvador (BA)',
               'Valor em reais'),
cadastroSeries(101602,
               'Custo da cesta básica',
               'Custo da cesta básica',
               'Valor da cesta básica conforme estabelecido no Decreto nº 399, de 30.04.1938, composta de treze produtos de alimentação básica, definidos no mesmo Decreto.',
               'R$',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7493/dados?formato=json',
               1,
               'mensal',
               'Valor da cesta básica',
               'Município',
               'São Paulo (SP)',
               'Valor em reais'),
cadastroSeries(101603,
               'Custo da cesta básica',
               'Custo da cesta básica',
               'Valor da cesta básica conforme estabelecido no Decreto nº 399, de 30.04.1938, composta de treze produtos de alimentação básica, definidos no mesmo Decreto.',
               'R$',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.7494/dados?formato=json',
               1,
               'mensal',
               'Valor da cesta básica',
               'Município',
               'Vitória (ES)',
               'Valor em reais'),
cadastroSeries(101605,
               'Índice de Commodities - Brasil (IC-Br)',
               'Índice de Commodities - Brasil (IC-Br)',
               'O Índice exprime média mensal ponderada dos preços em reais das commodities relevantes para a dinâmica da inflação brasileira.',
               'Índice',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.27574/dados?formato=json',
               1,
               'mensal',
               'Índice (dez/05 = 100)',
               'Brasil',
               'Brasil',
               'Índice geral'),
cadastroSeries(101612,
               'Índice de Commodities - Brasil (IC-Br)',
               'Índice de Commodities - Brasil (IC-Br)',
               'O Índice exprime média mensal ponderada dos preços em dólares dos EUA das commodities relevantes para a dinâmica da inflação brasileira.',
               'Índice',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.29042/dados?formato=json',
               1,
               'mensal',
               'Índice (dez/05 = 100)',
               'Brasil',
               'Brasil',
               'Índice geral (USD)'),
cadastroSeries(101606,
               'Índice de Commodities - Brasil (IC-Br)',
               'Índice de Commodities - Brasil (IC-Br)',
               'É uma média mensal ponderada dos preços em reais de boi gordo, algodão, óleo de soja, trigo, açúcar, milho, café, arroz, porco, suco de laranja e cacau.',
               'Índice',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.27575/dados?formato=json',
               1,
               'mensal',
               'Índice (dez/05 = 100)',
               'Brasil',
               'Brasil',
               'Agropecuária'),
cadastroSeries(101611,
               'Índice de Commodities - Brasil (IC-Br)',
               'Índice de Commodities - Brasil (IC-Br)',
               'O Índice exprime média mensal ponderada dos preços em dólares dos EUA de carne de boi gordo, algodão, óleo de soja, trigo, açúcar, milho, café, arroz, porco, suco de laranja e cacau.',
               'Índice',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.29041/dados?formato=json',
               1,
               'mensal',
               'Índice (dez/05 = 100)',
               'Brasil',
               'Brasil',
               'Agropecuária (USD)'),
cadastroSeries(101607,
               'Índice de Commodities - Brasil (IC-Br)',
               'Índice de Commodities - Brasil (IC-Br)',
               'É uma média mensal ponderada dos preços em reais de alumínio, minério de ferro, cobre, estanho, zinco, chumbo, níquel, ouro e prata.',
               'Índice',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.27576/dados?formato=json',
               1,
               'mensal',
               'Índice (dez/05 = 100)',
               'Brasil',
               'Brasil',
               'Metal'),
cadastroSeries(101610,
               'Índice de Commodities - Brasil (IC-Br)',
               'Índice de Commodities - Brasil (IC-Br)',
               'É uma média mensal ponderada dos preços em dólares do EUA de alumínio, minério de ferro, cobre, estanho, zinco, chumbo, níquel, ouro e prata.',
               'Índice',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.29040/dados?formato=json',
               1,
               'mensal',
               'Índice (dez/05 = 100)',
               'Brasil',
               'Brasil',
               'Metal (USD)'),
cadastroSeries(101608,
               'Índice de Commodities - Brasil (IC-Br)',
               'Índice de Commodities - Brasil (IC-Br)',
               'É uma média mensal ponderada dos preços em reais de petróleo Brent, gás natural e carvão.',
               'Índice',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.27577/dados?formato=json',
               1,
               'mensal',
               'Índice (dez/05 = 100)',
               'Brasil',
               'Brasil',
               'Energia'),
cadastroSeries(101609,
               'Índice de Commodities - Brasil (IC-Br)',
               'Índice de Commodities - Brasil (IC-Br)',
               'É uma média mensal ponderada dos preços em dólares dos EUA de petróleo Brent, gás natural e carvão.',
               'Índice',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.29039/dados?formato=json',
               1,
               'mensal',
               'Índice (dez/05 = 100)',
               'Brasil',
               'Brasil',
               'Energia (USD)'),
cadastroSeries(101613,
               'Índice de Valores de Garantia de Imóveis Res. Financ. (IVG-R)',
               'Índice de Valores de Garantia de Imóveis Residenciais Financiados (IVG-R)',
               'O Índice de Valor de Garantias Reais (IVG-R) estima a tendência de preço de longo prazo dos valores de imóveis residenciais no Brasil utilizando informações do Sistema de Informações de Crédito (SCR) do Banco Central do Brasil. Para isso, utiliza os valores de avaliação dos imóveis dados em garantia a financiamentos imobiliários residenciais para pessoas físicas nas modalidades de alienação fiduciária e hipoteca residencial. O cálculo é realizado considerando as mesmas regiões metropolitanas usadas no cálculo do IPCA pelo IBGE: 11 regiões até dezembro de 2013 (Belém, Belo Horizonte, Brasília, Curitiba, Fortaleza, Goiânia, Porto Alegre, Recife, Rio de Janeiro, Salvador e São Paulo), e 13 regiões a partir de janeiro de 2014 (inclui Campo Grande e Vitória, conforme NT/IBGE 03/2013).',
               'Índice',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.21340/dados?formato=json',
               1,
               'mensal',
               'Índice (mar/01 = 100)',
               'Brasil',
               'Brasil',
               'Índice geral'),
cadastroSeries(101614,
               'Mediana dos Valores de Garantia de Imóveis Res. Financ. (MVG-R)',
               'Mediana dos Valores de Garantia de Imóveis Residenciais Financiados (MVG-R)',
               'A Mediana dos Valores de Garantia de Imóveis Residenciais Financiados (MVG-R) é uma série temporal complementar ao IVG-R (série 21340). É calculada a partir dos valores de avaliação dos imóveis dados em garantia a financiamentos imobiliários residenciais para pessoas físicas nas modalidades de alienação fiduciária e hipoteca residencial, informados pelos bancos ao Sistema de Informações de Crédito (SCR) do Banco Central do Brasil.',
               'R$',
               'Banco Central do Brasil',
               'https://api.bcb.gov.br/dados/serie/bcdata.sgs.25419/dados?formato=json',
               1,
               'mensal',
               'Valor em reais',
               'Brasil',
               'Brasil',
               'Valor em reais')
)


lista_nova <- list()

base_df <- data.frame()
# alterar o codigo das series
for(i in c(1:length(listaSeries))){

  # Split the text at commas 
  split_text <- unlist(strsplit(listaSeries[[i]], "','", fixed = TRUE))
  
  # substituir o numero pela combinacao de data e hora
  # split_text[[1]] <- sub("\\b\\d{6}\\b", paste0("'numero': ", "'",UUIDgenerate(),"'"), split_text[[1]])
  lista_nova[['numero']] <- UUIDgenerate()
  lista_nova[['nome']] <- gsub("'","",sub(".*,\\s*", "", split_text[[1]]))
  lista_nova[['nomeCompleto']] <- split_text[[2]]
  lista_nova[['descricao']] <- split_text[[3]]
  lista_nova[['formato']] <- split_text[[4]]
  lista_nova[['fonte']] <- split_text[[5]]
  
  split_text_7 <- unlist(strsplit(split_text[[6]], ",", fixed = TRUE))
  
  lista_nova[['urlAPI']] <- gsub("'","",split_text_7[[1]])
  lista_nova[['idAssunto']] <- split_text_7[[2]]
  lista_nova[['periodicidade']] <- gsub("'","",split_text_7[[3]])
  lista_nova[['metrica']] <- split_text[[7]]
  lista_nova[['nivelGeografico']] <- split_text[[8]]
  lista_nova[['localidades']] <- split_text[[9]]
  lista_nova[['categoria']] <- gsub("'),","",split_text[[10]])
  
  
  teste <- do.call("cbind",lista_nova)
  teste2 <- as.data.frame(teste)
  base_df <- bind_rows(base_df, teste2)
}
# apagar o numero das linhas
row.names(base_df) <- NULL

# unir as bases

base_df_2 <- bind_rows(base_ipca, base_inpc, base_ipca15, base_df, base_sinapi_647, base_sinapi_2296, base_sinapi_6586)

# exportar como csv
write.csv(base_df_2, file="C:/Users/Kleber/Documents/indice_precos_2.csv", row.names = F)
     



