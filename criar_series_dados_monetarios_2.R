library(uuid)
library(readxl)


dados_monetarios <- read_excel("dados_monetarios.xlsx")

lista_dados <- list()

for(i in c(1:nrow(dados_monetarios))){
  numero <- UUIDgenerate()
  nome <- dados_monetarios[i,"nome"]
  nome_completo <- dados_monetarios[i,"nome_completo"]
  descricao <- dados_monetarios[i,"descricao"]
  formato <- dados_monetarios[i,"formato"]
  fonte <- dados_monetarios[i,"fonte"]
  urlAPI <- dados_monetarios[i,"urlAPI"]
  idAssunto <- dados_monetarios[i,"idAssunto"]
  if(dados_monetarios[i,"per"]=="M"){
    periodicidade <- "mensal"
  } else if(dados_monetarios[i,"per"]=="D"){
    periodicidade <- "diária"
  } else if(dados_monetarios[i,"per"]=="A"){
    periodicidade <- "anual"
  } else if(dados_monetarios[i,"per"]=="T"){
    periodicidade <- "trimestral"
  } else {
    periodicidade <- ''
  }
  metrica <- dados_monetarios[i,"metrica"]
  nivel_geog <- dados_monetarios[i,"nivel_geog"]
  localidades <- dados_monetarios[i,"localidades"]
  categoria <- dados_monetarios[i,"categoria"]
  
  string_final <- paste0("<String, dynamic>{'numero': '",numero,"', ","'nome': '",nome,"', 'nomeCompleto': '",nome_completo, 
                         "', 'descricao': '",descricao, "', 'formato': '",formato,  "', 'fonte': '",fonte,
                         "', 'urlAPI': '", urlAPI, "', 'idAssunto': ",idAssunto, ", 'periodicidade': '",periodicidade,
                         "', 'metrica': '",metrica, "', 'nivelGeografico': '",nivel_geog, "', 'localidades': '",localidades,
                         "', 'categoria': '",categoria,"'},")
  lista_dados[[i]] <- string_final
  
}

# transformar em data.frame
teste2 <- do.call("rbind",lista_dados)
teste2 <- as.data.frame(teste2)
# apagar o numero das linhas
row.names(teste2) <- NULL
# exportar como csv
write.csv(teste2, file="C:/Users/Kleber/Documents/dados_monetarios_2.csv", row.names = F)


