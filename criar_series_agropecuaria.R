library(uuid)
library(jsonlite)
library(curl)
library(dplyr)
library(readxl)
# limpar a area de trabalho
# rm(list=ls())
#_______________________________________________________________________________________________________________________
# carregar a tabela de municipios ----
#_______________________________________________________________________________________________________________________
rel_municipio <- read_excel("DTB_2022/RELATORIO_DTB_BRASIL_MUNICIPIO.xls")
rel_municipio[,"sigla_uf"] <- NA
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==12,"AC",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==27,"AL",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==16,"AP",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==13,"AM",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==29,"BA",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==23,"CE",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==53,"DF",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==32,"ES",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==52,"GO",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==21,"MA",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==51,"MT",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==50,"MS",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==31,"MG",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==15,"PA",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==25,"PB",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==41,"PR",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==26,"PE",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==22,"PI",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==24,"RN",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==43,"RS",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==33,"RJ",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==11,"RO",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==14,"RR",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==42,"SC",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==35,"SP",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==28,"SE",rel_municipio$sigla_uf)
rel_municipio$sigla_uf <- ifelse(rel_municipio$UF==17,"TO",rel_municipio$sigla_uf)

# 
colnames(rel_municipio)[which(colnames(rel_municipio)=='Código Município Completo')] <- "cod_mun_comp"
#_______________________________________________________________________________________________________________________
# carregar os dados dos estoques - 254 ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/254/metadados"
# carregar todos os metadados
inpc <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- inpc$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- inpc$variaveis[inpc$variaveis$id==150,]
# filtrar as categorias
categorias1 <- inpc$classificacoes$categorias[[1]]
categorias1[,"classif"] <- 162
categorias2 <- inpc$classificacoes$categorias[[2]]
categorias2 <- categorias2[categorias2$id==0,]
categorias2[,"classif"] <- 161
categorias3 <- inpc$classificacoes$categorias[[3]]
categorias3 <- categorias3[categorias3$id%in%c(3054),]
categorias3[,"classif"] <- 163
# categorias <- categorias[categorias$id%in%c(7169,7170,7445,7486,7558,7625,7660,7712,7766,7786),]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/254/localidades/N1|N2|N8|N9|N6|N3"
# carregar as localidades
localidades <- fromJSON(url_loc)
localidades <- localidades[localidades$nivel$id%in%c("N1","N2","N6"),]
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


localidades <- merge(localidades, rel_municipio[,c("cod_mun_comp","sigla_uf")], by.x="id_loc", by.y="cod_mun_comp", 
                     all.x=T)
# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
base_est_254 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in c(1:nrow(categorias1))){
      for(k in c(1:nrow(categorias2))){
        for(c in c(1:nrow(categorias3))){
          codigo <- UUIDgenerate()
          lista_dados[['numero']] <- codigo
          lista_dados[['nome']] <- "Pesquisa de Estoques"
          lista_dados[['nomeCompleto']] <- "Pesquisa de Estoques"
          lista_dados[['descricao']] <-"A Pesquisa de Estoques objetiva fornecer informações estatísticas conjunturais sobre o volume e distribuição espacial dos estoques de produtos agrícolas básicos, sobre as unidades onde é feita a sua guarda, e acompanhar a sua evolução ao longo do tempo."
          lista_dados[['formato']] <- variaveis[i,"unidade"]
          lista_dados[['fonte']] <- "IBGE"
          lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/254/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=",categorias1[w,"classif"],"[",categorias1[w,"id"],"]|",categorias2[k,"classif"],"[",categorias2[k,"id"],"]|",categorias3[c,"classif"],"[",categorias3[c,"id"],"]")
          lista_dados[['idAssunto']] <- 1
          lista_dados[['periodicidade']] <- inpc$periodicidade$frequencia
          lista_dados[['metrica']] <-variaveis[i,"nome"]
          lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
          
          if(localidades[j,"nome_nivel"]=="Município"){
            lista_dados[['localidades']] <- paste0(localidades[j,"nome_loc"]," (",localidades[j,"sigla_uf"],")")
          } else {
            lista_dados[['localidades']] <- localidades[j,"nome_loc"]
          }
          
          lista_dados[['categoria']] <- paste0("Produto: ", categorias1[w,"nome"], " - ", "Tipo de propriedade da empresa: ",  categorias2[k,"nome"], " - ", "Tipo de atividade do estabelecimento: ", categorias3[c,"nome"])
          teste <- do.call("cbind",lista_dados)
          teste2 <- as.data.frame(teste)
          base_est_254 <- bind_rows(base_est_254, teste2)
        }
      }
    }
  }
}
# apagar o numero das linhas
row.names(base_est_254) <- NULL
#_______________________________________________________________________________________________________________________
# carregar os dados dos estoques - 259 ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/259/metadados"
# carregar todos os metadados
inpc <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- inpc$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- inpc$variaveis[inpc$variaveis$id==153,]
# filtrar as categorias
categorias1 <- inpc$classificacoes$categorias[[1]]
categorias1[,"classif"] <- 12687
# categorias <- categorias[categorias$id%in%c(7169,7170,7445,7486,7558,7625,7660,7712,7766,7786),]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/259/localidades/N1|N2|N8|N9|N6|N3"
# carregar as localidades
localidades <- fromJSON(url_loc)
localidades <- localidades[localidades$nivel$id%in%c("N1","N2","N6"),]
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

localidades <- merge(localidades, rel_municipio[,c("cod_mun_comp","sigla_uf")], by.x="id_loc", by.y="cod_mun_comp", 
                     all.x=T)

# criar lista vazia para armazenar as series
lista_dados <- list()
# data frame vazio para se preenchido
base_est_259 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in c(1:nrow(categorias1))){
          codigo <- UUIDgenerate()
          lista_dados[['numero']] <- codigo
          lista_dados[['nome']] <- "Pesquisa de Estoques"
          lista_dados[['nomeCompleto']] <- "Pesquisa de Estoques"
          lista_dados[['descricao']] <-"A Pesquisa de Estoques objetiva fornecer informações estatísticas conjunturais sobre o volume e distribuição espacial dos estoques de produtos agrícolas básicos, sobre as unidades onde é feita a sua guarda, e acompanhar a sua evolução ao longo do tempo."
          lista_dados[['formato']] <- variaveis[i,"unidade"]
          lista_dados[['fonte']] <- "IBGE"
          lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/259/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=",categorias1[w,"classif"],"[",categorias1[w,"id"],"]")
          lista_dados[['idAssunto']] <- 1
          lista_dados[['periodicidade']] <- inpc$periodicidade$frequencia
          lista_dados[['metrica']] <- variaveis[i,"nome"]
          lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
          
          if(localidades[j,"nome_nivel"]=="Município"){
            lista_dados[['localidades']] <- paste0(localidades[j,"nome_loc"]," (",localidades[j,"sigla_uf"],")")
          } else {
            lista_dados[['localidades']] <- localidades[j,"nome_loc"]
          }
          
          lista_dados[['categoria']] <- categorias1[w,"nome"]
          teste <- do.call("cbind",lista_dados)
          teste2 <- as.data.frame(teste)
          base_est_259 <- bind_rows(base_est_259, teste2)
    }
  }
}
# apagar o numero das linhas
row.names(base_est_259) <- NULL
#_______________________________________________________________________________________________________________________
# levantamento de safra - 188 ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/188/metadados"
# carregar todos os metadados
inpc <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- inpc$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- inpc$variaveis
# filtrar as categorias
categorias1 <- inpc$classificacoes$categorias[[1]]
categorias1[,"classif"] <- 49
categorias2 <- inpc$classificacoes$categorias[[2]]
categorias2[,"classif"] <- 48
# categorias <- categorias[categorias$id%in%c(7169,7170,7445,7486,7558,7625,7660,7712,7766,7786),]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/188/localidades/N1|N2|N3"
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
base_est_188 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in c(1:nrow(categorias1))){
      for(k in c(1:nrow(categorias2))){
        codigo <- UUIDgenerate()
        lista_dados[['numero']] <- codigo
        lista_dados[['nome']] <- "Levantamento Sistemático da Produção Agrícola"
        lista_dados[['nomeCompleto']] <- "Levantamento Sistemático da Produção Agrícola"
        lista_dados[['descricao']] <-"O Levantamento Sistemático da Produção Agrícola tem por objetivo fornecer informações estatísticas sobre o plantio, colheita, produção e rendimento médio, de forma sistemática, para os principais produtos das lavouras permanentes e temporárias. É uma pesquisa de previsão e acompanhamento das variáveis área, produção e rendimento médio de 25 importantes produtos agrícolas, desde a fase de intenção de plantio até o final da colheita, de cada cultura investigada dentro do ano civil corrente e prognóstico da safra subsequente."
        lista_dados[['formato']] <- "Quilogramas por Hectare"
        lista_dados[['fonte']] <- "IBGE"
        lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/188/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=",categorias1[w,"classif"],"[",categorias1[w,"id"],"]|",categorias2[k,"classif"],"[",categorias2[k,"id"],"]")
        lista_dados[['idAssunto']] <- 1
        lista_dados[['periodicidade']] <- inpc$periodicidade$frequencia
        lista_dados[['metrica']] <-variaveis[i,"nome"]
        lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
        lista_dados[['localidades']] <- localidades[j,"nome_loc"]
        lista_dados[['categoria']] <- paste0(categorias1[w,"nome"], " - ", categorias2[k,"nome"])
        teste <- do.call("cbind",lista_dados)
        teste2 <- as.data.frame(teste)
        base_est_188 <- bind_rows(base_est_188, teste2)
      }

    }
  }
}
# apagar o numero das linhas
row.names(base_est_188) <- NULL
#_______________________________________________________________________________________________________________________
# levantamento de safra - 1618 ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/1618/metadados"
# carregar todos os metadados
inpc <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- inpc$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- inpc$variaveis
# filtrar as categorias
categorias1 <- inpc$classificacoes$categorias[[1]]
categorias1[,"classif"] <- 49
categorias2 <- inpc$classificacoes$categorias[[2]]
categorias2[,"classif"] <- 48
# categorias <- categorias[categorias$id%in%c(7169,7170,7445,7486,7558,7625,7660,7712,7766,7786),]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/1618/localidades/N1|N2|N3"
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
base_est_1618 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in c(1:nrow(categorias1))){
      for(k in c(1:nrow(categorias2))){
        codigo <- UUIDgenerate()
        lista_dados[['numero']] <- codigo
        lista_dados[['nome']] <- "Levantamento Sistemático da Produção Agrícola"
        lista_dados[['nomeCompleto']] <- "Levantamento Sistemático da Produção Agrícola"
        lista_dados[['descricao']] <-"O Levantamento Sistemático da Produção Agrícola tem por objetivo fornecer informações estatísticas sobre o plantio, colheita, produção e rendimento médio, de forma sistemática, para os principais produtos das lavouras permanentes e temporárias. É uma pesquisa de previsão e acompanhamento das variáveis área, produção e rendimento médio de 25 importantes produtos agrícolas, desde a fase de intenção de plantio até o final da colheita, de cada cultura investigada dentro do ano civil corrente e prognóstico da safra subsequente."
        
        if(variaveis[i,"id"]%in%c(109,216)){
          lista_dados[['formato']] <- "Hectares"
        } else {
          lista_dados[['formato']] <- "Toneladas"
        }
        lista_dados[['fonte']] <- "IBGE"
        lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/1618/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=",categorias1[w,"classif"],"[",categorias1[w,"id"],"]|",categorias2[k,"classif"],"[",categorias2[k,"id"],"]")
        lista_dados[['idAssunto']] <- 1
        lista_dados[['periodicidade']] <- inpc$periodicidade$frequencia
        lista_dados[['metrica']] <-variaveis[i,"nome"]
        lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
        lista_dados[['localidades']] <- localidades[j,"nome_loc"]
        lista_dados[['categoria']] <- paste0(categorias1[w,"nome"], " - ", categorias2[k,"nome"])
        teste <- do.call("cbind",lista_dados)
        teste2 <- as.data.frame(teste)
        base_est_1618 <- bind_rows(base_est_1618, teste2)
      }
      
    }
  }
}
# apagar o numero das linhas
row.names(base_est_1618) <- NULL
#_______________________________________________________________________________________________________________________
# pesquisa de ovos ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/7524/metadados"
# carregar todos os metadados
inpc <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- inpc$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- inpc$variaveis
# filtrar as categorias
categorias1 <- inpc$classificacoes$categorias[[1]]
categorias1[,"classif"] <- 12716
categorias2 <- inpc$classificacoes$categorias[[2]]
categorias2[,"classif"] <- 1835
# categorias <- categorias[categorias$id%in%c(7169,7170,7445,7486,7558,7625,7660,7712,7766,7786),]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/7524/localidades/N1|N3"
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
base_est_7524 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in c(1:nrow(categorias1))){
      for(k in c(1:nrow(categorias2))){
        codigo <- UUIDgenerate()
        lista_dados[['numero']] <- codigo
        lista_dados[['nome']] <- "Produção de Ovos de Galinha"
        lista_dados[['nomeCompleto']] <- "Produção de Ovos de Galinha"
        lista_dados[['descricao']] <-"Tem por objetivo fornecer indicadores da variação da produção física de ovos de galinha, de forma a incorporar, no cálculo do Produto Interno Bruto, o valor dessa produção. A produção de ovos desta pesquisa é utilizada como componente da estimativa da produção total de ovos municipal da Pesquisa da Pecuária Municipal (PPM), que inclui ainda a produção não comercial e aquela abaixo dos limites de corte definidos para esta pesquisa."
        lista_dados[['formato']] <- variaveis[i,"unidade"]
        lista_dados[['fonte']] <- "IBGE"
        lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/7524/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=",categorias1[w,"classif"],"[",categorias1[w,"id"],"]|",categorias2[k,"classif"],"[",categorias2[k,"id"],"]")
        lista_dados[['idAssunto']] <- 1
        lista_dados[['periodicidade']] <- inpc$periodicidade$frequencia
        lista_dados[['metrica']] <- variaveis[i,"nome"]
        lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
        lista_dados[['localidades']] <- localidades[j,"nome_loc"]
        lista_dados[['categoria']] <- paste0(categorias1[w,"nome"], " - ", categorias2[k,"nome"])
        teste <- do.call("cbind",lista_dados)
        teste2 <- as.data.frame(teste)
        base_est_7524 <- bind_rows(base_est_7524, teste2)
      }
      
    }
  }
}
# apagar o numero das linhas
row.names(base_est_7524) <- NULL
#_______________________________________________________________________________________________________________________
# pesquisa do couro - 1088 ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/1088/metadados"
# carregar todos os metadados
inpc <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- inpc$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- inpc$variaveis
# filtrar as categorias
categorias1 <- inpc$classificacoes$categorias[[1]]
categorias1[,"classif"] <- 12716
categorias2 <- inpc$classificacoes$categorias[[2]]
categorias2[,"classif"] <- 11531
# categorias <- categorias[categorias$id%in%c(7169,7170,7445,7486,7558,7625,7660,7712,7766,7786),]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/1088/localidades/N1|N3"
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
base_est_1088 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in c(1:nrow(categorias1))){
      for(k in c(1:nrow(categorias2))){
        codigo <- UUIDgenerate()
        lista_dados[['numero']] <- codigo
        lista_dados[['nome']] <- "Pesquisa Trimestral do Couro"
        lista_dados[['nomeCompleto']] <- "Pesquisa Trimestral do Couro"
        lista_dados[['descricao']] <-"Tem por objetivo obter informações estatísticas sobre a quantidade de couro cru de bovino adquirido e curtido. As informações produzidas fornecem aos órgãos do governo e entidades do setor privado subsídios para o acompanhamento e análise da evolução do setor coureiro. Permite ainda avaliar o abate bovino não captado pela Pesquisa Trimestral do Abate."
        lista_dados[['formato']] <- variaveis[i,"unidade"]
        lista_dados[['fonte']] <- "IBGE"
        lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/1088/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=",categorias1[w,"classif"],"[",categorias1[w,"id"],"]|",categorias2[k,"classif"],"[",categorias2[k,"id"],"]")
        lista_dados[['idAssunto']] <- 1
        lista_dados[['periodicidade']] <- inpc$periodicidade$frequencia
        lista_dados[['metrica']] <-variaveis[i,"nome"]
        lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
        lista_dados[['localidades']] <- localidades[j,"nome_loc"]
        lista_dados[['categoria']] <- paste0(categorias1[w,"nome"], " - ", categorias2[k,"nome"])
        teste <- do.call("cbind",lista_dados)
        teste2 <- as.data.frame(teste)
        base_est_1088 <- bind_rows(base_est_1088, teste2)
      }
      
    }
  }
}
# apagar o numero das linhas
row.names(base_est_1088) <- NULL
#_______________________________________________________________________________________________________________________
# pesquisa do couro - 1089 ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/1089/metadados"
# carregar todos os metadados
inpc <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- inpc$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- inpc$variaveis
# filtrar as categorias
categorias1 <- inpc$classificacoes$categorias[[1]]
categorias1[,"classif"] <- 12716
# categorias <- categorias[categorias$id%in%c(7169,7170,7445,7486,7558,7625,7660,7712,7766,7786),]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/1089/localidades/N1|N3"
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
base_est_1089 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in c(1:nrow(categorias1))){
        codigo <- UUIDgenerate()
        lista_dados[['numero']] <- codigo
        lista_dados[['nome']] <- "Pesquisa Trimestral do Couro"
        lista_dados[['nomeCompleto']] <- "Pesquisa Trimestral do Couro"
        lista_dados[['descricao']] <-"Tem por objetivo fornecer indicadores da variação da produção física de ovos de galinha, de forma a incorporar, no cálculo do Produto Interno Bruto, o valor dessa produção. A produção de ovos desta pesquisa é utilizada como componente da estimativa da produção total de ovos municipal da Pesquisa da Pecuária Municipal (PPM), que inclui ainda a produção não comercial e aquela abaixo dos limites de corte definidos para esta pesquisa."
        lista_dados[['formato']] <- variaveis[i,"unidade"]
        lista_dados[['fonte']] <- "IBGE"
        lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/1089/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=",categorias1[w,"classif"],"[",categorias1[w,"id"],"]")
        lista_dados[['idAssunto']] <- 1
        lista_dados[['periodicidade']] <- inpc$periodicidade$frequencia
        lista_dados[['metrica']] <- variaveis[i,"nome"]
        lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
        lista_dados[['localidades']] <- localidades[j,"nome_loc"]
        lista_dados[['categoria']] <- categorias1[w,"nome"]
        teste <- do.call("cbind",lista_dados)
        teste2 <- as.data.frame(teste)
        base_est_1089 <- bind_rows(base_est_1089, teste2)
    }
  }
}
# apagar o numero das linhas
row.names(base_est_1089) <- NULL
#_______________________________________________________________________________________________________________________
# pesquisa do couro - 1090 ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/1090/metadados"
# carregar todos os metadados
inpc <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- inpc$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- inpc$variaveis
# filtrar as categorias
categorias1 <- inpc$classificacoes$categorias[[1]]
categorias1[,"classif"] <- 12716
categorias2 <- inpc$classificacoes$categorias[[2]]
categorias2[,"classif"] <- 11532
# categorias <- categorias[categorias$id%in%c(7169,7170,7445,7486,7558,7625,7660,7712,7766,7786),]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/1090/localidades/N1|N3"
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
base_est_1090 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in c(1:nrow(categorias1))){
      for(k in c(1:nrow(categorias2))){
        codigo <- UUIDgenerate()
        lista_dados[['numero']] <- codigo
        lista_dados[['nome']] <- "Pesquisa Trimestral do Couro"
        lista_dados[['nomeCompleto']] <- "Pesquisa Trimestral do Couro"
        lista_dados[['descricao']] <-"Tem por objetivo obter informações estatísticas sobre a quantidade de couro cru de bovino adquirido e curtido. As informações produzidas fornecem aos órgãos do governo e entidades do setor privado subsídios para o acompanhamento e análise da evolução do setor coureiro. Permite ainda avaliar o abate bovino não captado pela Pesquisa Trimestral do Abate."
        lista_dados[['formato']] <- variaveis[i,"unidade"]
        lista_dados[['fonte']] <- "IBGE"
        lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/1090/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=",categorias1[w,"classif"],"[",categorias1[w,"id"],"]|",categorias2[k,"classif"],"[",categorias2[k,"id"],"]")
        lista_dados[['idAssunto']] <- 1
        lista_dados[['periodicidade']] <- inpc$periodicidade$frequencia
        lista_dados[['metrica']] <-variaveis[i,"nome"]
        lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
        lista_dados[['localidades']] <- localidades[j,"nome_loc"]
        lista_dados[['categoria']] <- paste0(categorias1[w,"nome"], " - ", categorias2[k,"nome"])
        teste <- do.call("cbind",lista_dados)
        teste2 <- as.data.frame(teste)
        base_est_1090 <- bind_rows(base_est_1090, teste2)
      }
    }
  }
}
# apagar o numero das linhas
row.names(base_est_1090) <- NULL
#_______________________________________________________________________________________________________________________
# pesquisa do leite ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/1086/metadados"
# carregar todos os metadados
inpc <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- inpc$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- inpc$variaveis
# filtrar as categorias
categorias1 <- inpc$classificacoes$categorias[[1]]
categorias1[,"classif"] <- 12716
categorias2 <- inpc$classificacoes$categorias[[2]]
categorias2[,"classif"] <- 12529
# categorias <- categorias[categorias$id%in%c(7169,7170,7445,7486,7558,7625,7660,7712,7766,7786),]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/1086/localidades/N1|N3"
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
base_est_1086 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in c(1:nrow(categorias1))){
      for(k in c(1:nrow(categorias2))){
        codigo <- UUIDgenerate()
        lista_dados[['numero']] <- codigo
        lista_dados[['nome']] <- "Pesquisa Trimestral do Leite"
        lista_dados[['nomeCompleto']] <- "Pesquisa Trimestral do Leite"
        lista_dados[['descricao']] <-"Tem por objetivo obter informações estatísticas relativas às quantidades de leite cru, resfriado ou não, adquiridas e industrializadas. A partir de 2019, a Pesquisa passou a investigar também o preço médio do leite cru pago aos produtores de leite pelas indústrias de laticínios, variável divulgada apenas como estatística experimental. As informações produzidas fornecem aos órgãos do governo e entidades do setor privado subsídios para o acompanhamento e análise da evolução do setor leiteiro, bem como constituem-se em elemento integrante no cálculo do Produto Interno Bruto da Agropecuária."
        lista_dados[['formato']] <- variaveis[i,"unidade"]
        lista_dados[['fonte']] <- "IBGE"
        lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/1086/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=",categorias1[w,"classif"],"[",categorias1[w,"id"],"]|",categorias2[k,"classif"],"[",categorias2[k,"id"],"]")
        lista_dados[['idAssunto']] <- 1
        lista_dados[['periodicidade']] <- inpc$periodicidade$frequencia
        lista_dados[['metrica']] <-variaveis[i,"nome"]
        lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
        lista_dados[['localidades']] <- localidades[j,"nome_loc"]
        lista_dados[['categoria']] <- paste0("Ref. temporal: ", categorias1[w,"nome"], " - Tipode inspeção: ", categorias2[k,"nome"])
        teste <- do.call("cbind",lista_dados)
        teste2 <- as.data.frame(teste)
        base_est_1086 <- bind_rows(base_est_1086, teste2)
      }
    }
  }
}
# apagar o numero das linhas
row.names(base_est_1086) <- NULL
#_______________________________________________________________________________________________________________________
# pesquisa de abates - 1092 ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/1092/metadados"
# carregar todos os metadados
inpc <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- inpc$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- inpc$variaveis
# filtrar as categorias
categorias1 <- inpc$classificacoes$categorias[[1]]
categorias1[,"classif"] <- 12716
categorias2 <- inpc$classificacoes$categorias[[2]]
categorias2[,"classif"] <- 18
categorias3 <- inpc$classificacoes$categorias[[3]]
categorias3[,"classif"] <- 12529
# categorias <- categorias[categorias$id%in%c(7169,7170,7445,7486,7558,7625,7660,7712,7766,7786),]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/1092/localidades/N1|N3"
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
base_est_1092 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in c(1:nrow(categorias1))){
      for(k in c(1:nrow(categorias2))){
        for(c in c(1:nrow(categorias3))){
          codigo <- UUIDgenerate()
          lista_dados[['numero']] <- codigo
          lista_dados[['nome']] <- "Pesquisa Trimestral do Abate de Animais"
          lista_dados[['nomeCompleto']] <- "Pesquisa Trimestral do Abate de Animais"
          lista_dados[['descricao']] <-"A pesquisa sobre abate de animais objetiva assegurar informações estatísticas de natureza conjuntural sobre a quantidade de animais abatidos e o peso total das carcaças, por espécie animal investigada. As informações produzidas são utilizadas por órgãos públicos e privados, para efeito de acompanhamento, planejamento, tomada de decisões, estudos e análises, bem como, constituem-se em elemento integrante das estimativas do Produto Interno Bruto realizado pelo IBGE."
          lista_dados[['formato']] <- variaveis[i,"unidade"]
          lista_dados[['fonte']] <- "IBGE"
          lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/1092/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=",categorias1[w,"classif"],"[",categorias1[w,"id"],"]|",categorias2[k,"classif"],"[",categorias2[k,"id"],"]|",categorias3[c,"classif"],"[",categorias3[c,"id"],"]")
          lista_dados[['idAssunto']] <- 1
          lista_dados[['periodicidade']] <- inpc$periodicidade$frequencia
          lista_dados[['metrica']] <- paste0("Abate de bovinos - ", variaveis[i,"nome"])
          lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
          lista_dados[['localidades']] <- localidades[j,"nome_loc"]
          lista_dados[['categoria']] <- paste0("Ref. temporal: ", categorias1[w,"nome"], " - Tipo de rebanho bovino: ", categorias2[k,"nome"], " - Tipo de inspeção: ",categorias3[c,"nome"])
          teste <- do.call("cbind",lista_dados)
          teste2 <- as.data.frame(teste)
          base_est_1092 <- bind_rows(base_est_1092, teste2)
        }
      }
    }
  }
}
# apagar o numero das linhas
row.names(base_est_1092) <- NULL
#_______________________________________________________________________________________________________________________
# pesquisa de abates - 1093 ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/1093/metadados"
# carregar todos os metadados
inpc <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- inpc$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- inpc$variaveis
# filtrar as categorias
categorias1 <- inpc$classificacoes$categorias[[1]]
categorias1[,"classif"] <- 12716
categorias2 <- inpc$classificacoes$categorias[[2]]
categorias2[,"classif"] <- 12529
# categorias <- categorias[categorias$id%in%c(7169,7170,7445,7486,7558,7625,7660,7712,7766,7786),]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/1093/localidades/N1|N3"
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
base_est_1093 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in c(1:nrow(categorias1))){
      for(k in c(1:nrow(categorias2))){
          codigo <- UUIDgenerate()
          lista_dados[['numero']] <- codigo
          lista_dados[['nome']] <- "Pesquisa Trimestral do Abate de Animais"
          lista_dados[['nomeCompleto']] <- "Pesquisa Trimestral do Abate de Animais"
          lista_dados[['descricao']] <-"A pesquisa sobre abate de animais objetiva assegurar informações estatísticas de natureza conjuntural sobre a quantidade de animais abatidos e o peso total das carcaças, por espécie animal investigada. As informações produzidas são utilizadas por órgãos públicos e privados, para efeito de acompanhamento, planejamento, tomada de decisões, estudos e análises, bem como, constituem-se em elemento integrante das estimativas do Produto Interno Bruto realizado pelo IBGE."
          lista_dados[['formato']] <- variaveis[i,"unidade"]
          lista_dados[['fonte']] <- "IBGE"
          lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/1093/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=",categorias1[w,"classif"],"[",categorias1[w,"id"],"]|",categorias2[k,"classif"],"[",categorias2[k,"id"],"]")
          lista_dados[['idAssunto']] <- 1
          lista_dados[['periodicidade']] <- inpc$periodicidade$frequencia
          lista_dados[['metrica']] <- paste0("Abate de suínos - ", variaveis[i,"nome"])
          lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
          lista_dados[['localidades']] <- localidades[j,"nome_loc"]
          lista_dados[['categoria']] <- paste0("Ref. temporal: ", categorias1[w,"nome"], " - Tipo de inspeção: ",categorias2[k,"nome"])
          teste <- do.call("cbind",lista_dados)
          teste2 <- as.data.frame(teste)
          base_est_1093 <- bind_rows(base_est_1093, teste2)
      }
    }
  }
}
# apagar o numero das linhas
row.names(base_est_1093) <- NULL
#_______________________________________________________________________________________________________________________
# pesquisa de abates - 1094 ----
#_______________________________________________________________________________________________________________________
# metadados do INPC
url <- "https://servicodados.ibge.gov.br/api/v3/agregados/1094/metadados"
# carregar todos os metadados
inpc <- fromJSON(url)
# filtrar o nivel territorial
nivel_territorial <- inpc$nivelTerritorial$Administrativo
# filtrar as variaveis
variaveis <- inpc$variaveis
# filtrar as categorias
categorias1 <- inpc$classificacoes$categorias[[1]]
categorias1[,"classif"] <- 12716
categorias2 <- inpc$classificacoes$categorias[[2]]
categorias2[,"classif"] <- 12529
# categorias <- categorias[categorias$id%in%c(7169,7170,7445,7486,7558,7625,7660,7712,7766,7786),]
# url das localidades
url_loc <- "https://servicodados.ibge.gov.br/api/v3/agregados/1094/localidades/N1|N3"
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
base_est_1094 <- data.frame()
# preencher a lista com as series
for(i in 1:nrow(variaveis)){
  for(j in 1:nrow(localidades)){
    for(w in c(1:nrow(categorias1))){
      for(k in c(1:nrow(categorias2))){
          codigo <- UUIDgenerate()
          lista_dados[['numero']] <- codigo
          lista_dados[['nome']] <- "Pesquisa Trimestral do Abate de Animais"
          lista_dados[['nomeCompleto']] <- "Pesquisa Trimestral do Abate de Animais"
          lista_dados[['descricao']] <-"A pesquisa sobre abate de animais objetiva assegurar informações estatísticas de natureza conjuntural sobre a quantidade de animais abatidos e o peso total das carcaças, por espécie animal investigada. As informações produzidas são utilizadas por órgãos públicos e privados, para efeito de acompanhamento, planejamento, tomada de decisões, estudos e análises, bem como, constituem-se em elemento integrante das estimativas do Produto Interno Bruto realizado pelo IBGE."
          lista_dados[['formato']] <- variaveis[i,"unidade"]
          lista_dados[['fonte']] <- "IBGE"
          lista_dados[['urlAPI']] <- paste0("https://servicodados.ibge.gov.br/api/v3/agregados/1094/periodos/all/variaveis/",variaveis[i,"id"],"?localidades=",localidades[j,"id_nivel"],"[",localidades[j,"id_loc"],"]&classificacao=",categorias1[w,"classif"],"[",categorias1[w,"id"],"]|",categorias2[k,"classif"],"[",categorias2[k,"id"],"]")
          lista_dados[['idAssunto']] <- 1
          lista_dados[['periodicidade']] <- inpc$periodicidade$frequencia
          lista_dados[['metrica']] <- paste0("Abate de frangos - ", variaveis[i,"nome"])
          lista_dados[['nivelGeografico']] <- localidades[j,"nome_nivel"]
          lista_dados[['localidades']] <- localidades[j,"nome_loc"]
          lista_dados[['categoria']] <- paste0("Ref. temporal: ", categorias1[w,"nome"], " - Tipo de inspeção: ",categorias2[k,"nome"])
          teste <- do.call("cbind",lista_dados)
          teste2 <- as.data.frame(teste)
          base_est_1094 <- bind_rows(base_est_1094, teste2)
      }
    }
  }
}
# apagar o numero das linhas
row.names(base_est_1094) <- NULL

base_df_2 <- bind_rows(base_est_254, base_est_259, base_est_188, base_est_1618, base_est_7524, base_est_1088, 
                       base_est_1089, base_est_1090, base_est_1086, base_est_1092, base_est_1093, base_est_1094)

# exportar como csv
write.csv(base_df_2, file="C:/Users/Kleber/Documents/agropecuaria.csv", row.names = F)
     

