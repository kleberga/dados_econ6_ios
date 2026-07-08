library(uuid)
library(readxl)


dados_monetarios <- read_excel("projecoes_2.xlsx")

lista_nova <- list()
base_df <- data.frame()
for(i in c(1:nrow(dados_monetarios))){
  
  lista_nova[['numero']] <- UUIDgenerate()
  lista_nova[['nome']] <- as.character(dados_monetarios[i,"nome"])
  lista_nova[['nomeCompleto']] <- as.character(dados_monetarios[i,"nomeCompleto"])
  lista_nova[['descricao']] <- as.character(dados_monetarios[i,"descricao"])
  lista_nova[['formato']] <- as.character(dados_monetarios[i,"formato"])
  lista_nova[['fonte']] <- as.character(dados_monetarios[i,"fonte"])
  
  
  lista_nova[['urlAPI']] <- as.character(dados_monetarios[i,"urlAPI"])
  lista_nova[['idAssunto']] <- as.character(dados_monetarios[i,"idAssunto"])
  lista_nova[['periodicidade']] <- as.character(dados_monetarios[i,"periodicidade"])
  lista_nova[['metrica']] <- as.character(dados_monetarios[i,"metrica"])
  lista_nova[['nivelGeografico']] <- as.character(dados_monetarios[i,"nivelGeografico"])
  lista_nova[['localidades']] <- as.character(dados_monetarios[i,"localidades"])
  lista_nova[['categoria']] <- as.character(dados_monetarios[i,"categoria"])
  
  
  teste <- do.call("cbind",lista_nova)
  teste2 <- as.data.frame(teste)
  base_df <- bind_rows(base_df, teste2)
  
}
# apagar o numero das linhas
row.names(base_df) <- NULL
# exportar como csv
write.csv(base_df, file="C:/Users/Kleber/Documents/projecoes_2.csv", row.names = F)


