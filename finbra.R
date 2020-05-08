#------------------------------------------------------------------------------#
# FINBRA / SICONFI - Receitas Orçamentarias dos Municípios 2013 - 2017
#                                                   Andre Valente - Fev/2018
#------------------------------------------------------------------------------#
#
# Origem dos dados: Contas Anuais / ANO / Municipios / Receitas Orçamentárias (AnexoI-C)
#https://siconfi.tesouro.gov.br/siconfi/pages/public/consulta_finbra/finbra_list.jsf
#https://www.tesouro.fazenda.gov.br/web/stn/finbra-financas-municipais - anterior a 2013
setwd("~/Dropbox/TESE_R")
#pacotes <- c("data.table","eeptools","grid","gridExtra","ggplot","xtable")
install.packages("data.table"); library(data.table)
install.packages("eeptools"); library(eeptools)
#install.packages("lubridate"); library(lubridate)
install.packages("grid"); library(grid)
install.packages("gridExtra"); library(gridExtra)
# 
#
#---- Tabela FINBRA ----
# ler arquivos
f13 <- setDT(read.table("./dados/finbra/finbra2013.csv", sep=";", header=FALSE,
                        fileEncoding="ISO-8859-14", na.strings = "NA",
                        quote='"', fill=TRUE, stringsAsFactors = FALSE))
names(f13) <- as.character(f13[4,]); f13 <- f13[!1:4,]
#
f14 <- setDT(read.table("./dados/finbra/finbra2014.csv", sep=";", header=FALSE,
                        fileEncoding="ISO-8859-14", na.strings = "NA",
                        quote='"', fill=TRUE, stringsAsFactors = FALSE))
names(f14) <- as.character(f14[4,]); f14 <- f14[!1:4,]
#             
f15 <- setDT(read.table("./dados/finbra/finbra2015.csv", sep=";", header=FALSE,
                        fileEncoding="ISO-8859-14", na.strings = "NA",
                        quote='"', fill=TRUE, stringsAsFactors = FALSE))
names(f15) <- as.character(f15[4,]); f15 <- f15[!1:4,]
#              
f16 <- setDT(read.table("./dados/finbra/finbra2016.csv", sep=";", header=FALSE,
                        fileEncoding="ISO-8859-14", na.strings = "NA",
                        quote='"', fill=TRUE, stringsAsFactors = FALSE))
names(f16) <- as.character(f16[4,]); f16 <- f16[!1:4,]
# asdf             
f17 <- setDT(read.table("./dados/finbra/finbra2017.csv", sep=";", header=FALSE,
                        fileEncoding="ISO-8859-14", na.strings = "NA",
                        quote='"', fill=TRUE, stringsAsFactors = FALSE))
names(f17) <- as.character(f17[4,]); f17 <- f17[!1:4,]
# Rubricas das TVF e TVE
f13 <- f13[Conta =="Total Receitas" |
             Conta =="1.7.6.1.00.00.00 - Transferências de Convênios da União e de Suas Entidades" |
             Conta =="1.7.6.2.00.00.00 - Transferências de Convênios dos Estados e do Distrito Federal e de Suas Entidades",]
f13 <- f13[Coluna == "Receitas Realizadas",]
#
f14 <- f14[Conta =="Total Receitas" |
             Conta =="1.7.6.1.00.00.00 - Transferências de Convênios da União e de Suas Entidades" |
             Conta =="1.7.6.2.00.00.00 - Transferências de Convênios dos Estados e do Distrito Federal e de Suas Entidades",]
f14 <- f14[Coluna == "Receitas Brutas Realizadas",]
#
f15 <- f15[Conta =="Total Receitas" |
             Conta =="1.7.6.1.00.00.00 - Transferências de Convênios da União e de Suas Entidades" |
             Conta =="1.7.6.2.00.00.00 - Transferências de Convênios dos Estados e do Distrito Federal e de Suas Entidades",]
f15 <- f15[Coluna == "Receitas Brutas Realizadas",]
#
f16 <- f16[Conta =="Total Receitas" |
             Conta =="1.7.6.1.00.00.00 - Transferências de Convênios da União e de Suas Entidades" |
             Conta =="1.7.6.2.00.00.00 - Transferências de Convênios dos Estados e do Distrito Federal e de Suas Entidades",]
f16 <- f16[Coluna == "Receitas Brutas Realizadas",]
#
f17 <- f17[Conta =="Total Receitas" |
             Conta =="1.7.6.1.00.00.00 - Transferências de Convênios da União e de Suas Entidades" |
             Conta =="1.7.6.2.00.00.00 - Transferências de Convênios dos Estados e do Distrito Federal e de Suas Entidades",]
f17 <- f17[Coluna == "Receitas Brutas Realizadas",]
#
# recodificando as variáveis
aux <- c("Total Receitas",
         "1.7.6.1.00.00.00 - Transferências de Convênios da União e de Suas Entidades",
         "1.7.6.2.00.00.00 - Transferências de Convênios dos Estados e do Distrito Federal e de Suas Entidades")
abrev <- c("REC", "TVF", "TVE")
for (i in aux) {
  f13[Conta==i,Conta:=abrev[which(aux==i)]]
  f14[Conta==i,Conta:=abrev[which(aux==i)]]
  f15[Conta==i,Conta:=abrev[which(aux==i)]]
  f16[Conta==i,Conta:=abrev[which(aux==i)]]
  f17[Conta==i,Conta:=abrev[which(aux==i)]]
};rm(aux,abrev,i)
#
# transformando as variáveis
# limpando o nome dos municípios
f13[,Instituição := substr(Instituição,25,nchar(Instituição)-5)]
f14[,Instituição := substr(Instituição,25,nchar(Instituição)-5)]
f15[,Instituição := substr(Instituição,25,nchar(Instituição)-5)]
f16[,Instituição := substr(Instituição,25,nchar(Instituição)-5)]
f17[,Instituição := substr(Instituição,25,nchar(Instituição)-5)]
# transformar valores de receitas para numérico
f13[,Valor:=as.numeric(gsub(",",".",Valor))]; f14[,Valor:=as.numeric(gsub(",",".",Valor))]
f15[,Valor:=as.numeric(gsub(",",".",Valor))]; f16[,Valor:=as.numeric(gsub(",",".",Valor))]
f17[,Valor:=as.numeric(gsub(",",".",Valor))]
# transformando as linhas em colunas
f13 <- reshape(f13, idvar = "Instituição", timevar = "Conta", direction = "wide")
f13 <- f13[,c(2,3,1,4,6,11,16)]
f14 <- reshape(f14, idvar = "Instituição", timevar = "Conta", direction = "wide")
f14 <- f14[,c(2,3,1,4,6,11,16)]
f15 <- reshape(f15, idvar = "Instituição", timevar = "Conta", direction = "wide")
f15 <- f15[,c(2,3,1,4,6,11,16)]
f16 <- reshape(f16, idvar = "Instituição", timevar = "Conta", direction = "wide")
f16 <- f16[,c(2,3,1,4,6,11,16)]
f17 <- reshape(f17, idvar = "Instituição", timevar = "Conta", direction = "wide")
f17 <- f17[,c(2,3,1,4,6,11,16)]
# Nomes das colunas
names(f13) <- names(f14) <-names(f15) <-names(f16) <-names(f17) <-
    c("ibge","uf","municipio","pop","rec","tve","tvf")
# remover municípios que não declararam receitas
f13 <- f13[!(rec < 1000 | is.na(rec)),]; f14 <- f14[!(rec < 1000 | is.na(rec)),] 
f15 <- f15[!(rec < 1000 | is.na(rec)),]; f16 <- f16[!(rec < 1000 | is.na(rec)),] 
f17 <- f17[!(rec < 1000 | is.na(rec)),] 
#
f13[!is.na(tve),ptve := round((tve/rec)*100,2)]; f13[!is.na(tvf),ptvf := round((tvf/rec)*100,2)]
f14[!is.na(tve),ptve := round((tve/rec)*100,2)]; f14[!is.na(tvf),ptvf := round((tvf/rec)*100,2)]
f15[!is.na(tve),ptve := round((tve/rec)*100,2)]; f15[!is.na(tvf),ptvf := round((tvf/rec)*100,2)]
f16[!is.na(tve),ptve := round((tve/rec)*100,2)]; f16[!is.na(tvf),ptvf := round((tvf/rec)*100,2)]
f17[!is.na(tve),ptve := round((tve/rec)*100,2)]; f17[!is.na(tvf),ptvf := round((tvf/rec)*100,2)]
# TVE - producao do bd para análise das Uf's
tve.3 <- f13[,round(mean(ptve,na.rm = TRUE),2),by=uf]; 
tve.4 <- f14[,round(mean(ptve,na.rm = TRUE),2),by=uf]
tve.5 <- f15[,round(mean(ptve,na.rm = TRUE),2),by=uf]
tve.6 <- f16[,round(mean(ptve,na.rm = TRUE),2),by=uf]
tve.7 <- f17[,round(mean(ptve,na.rm = TRUE),2),by=uf]
tve.uf <- merge(tve.3, tve.4, by="uf", all=TRUE); names(tve.uf) <- c("uf", "tve.13","tve.14")
tve.uf <- merge(tve.uf, tve.5, by="uf", all=TRUE); names(tve.uf)[4] <- "tve.15";
tve.uf <- merge(tve.uf, tve.6, by="uf", all=TRUE); names(tve.uf)[5] <- "tve.16";
tve.uf <- merge(tve.uf, tve.7, by="uf", all=TRUE); names(tve.uf)[6] <- "tve.17";
# TVF - producao do bd para análise das Uf's
tvf.3 <- f13[,round(mean(ptvf,na.rm = TRUE),2),by=uf]; 
tvf.4 <- f14[,round(mean(ptvf,na.rm = TRUE),2),by=uf]
tvf.5 <- f15[,round(mean(ptvf,na.rm = TRUE),2),by=uf]
tvf.6 <- f16[,round(mean(ptvf,na.rm = TRUE),2),by=uf]
tvf.7 <- f17[,round(mean(ptvf,na.rm = TRUE),2),by=uf]
tvf.uf <- merge(tvf.3, tvf.4, by="uf", all=TRUE); names(tvf.uf) <- c("uf", "tvf.13","tvf.14")
tvf.uf <- merge(tvf.uf, tvf.5, by="uf", all=TRUE); names(tvf.uf)[4] <- "tvf.15";
tvf.uf <- merge(tvf.uf, tvf.6, by="uf", all=TRUE); names(tvf.uf)[5] <- "tvf.16";
tvf.uf <- merge(tvf.uf, tvf.7, by="uf", all=TRUE); names(tvf.uf)[6] <- "tvf.17";
# Produção da tabela 1
summary(tvf.uf)
nrow(f13[!is.na(ptvf) & !is.na(rec),]); f13[!is.na(ptvf) & !is.na(rec),var(ptvf)]; f13[!is.na(ptvf) & !is.na(rec),sd(ptvf)] 
nrow(f14[!is.na(ptvf) & !is.na(rec),]); f14[!is.na(ptvf) & !is.na(rec),var(ptvf)]; f14[!is.na(ptvf) & !is.na(rec),sd(ptvf)] 
nrow(f15[!is.na(ptvf) & !is.na(rec),]); f15[!is.na(ptvf) & !is.na(rec),var(ptvf)]; f15[!is.na(ptvf) & !is.na(rec),sd(ptvf)] 
nrow(f16[!is.na(ptvf) & !is.na(rec),]); f16[!is.na(ptvf) & !is.na(rec),var(ptvf)]; f16[!is.na(ptvf) & !is.na(rec),sd(ptvf)] 





a <- merge(,
           , by="uf", all = TRUE)
names(a) <- c("uf", "2013", "2014")
a <- merge(a, , by="uf",all = TRUE)
a <- merge(a, , by="uf", all = TRUE)
a <- merge(a, , by="uf", all = TRUE)
names(a) <- c("uf", "2013", "2014","2015","2016","2017") 
a[is.nan(2016), 2016 := 0]





summary(f13[,.(round(mean(ptve,na.rm = TRUE),2),median(ptve,na.rm = TRUE),
       round(mean(ptvf,na.rm = TRUE),2),median(ptvf,na.rm = TRUE)),by=uf])
f13[is.na(ptve),ptve := 0]
f13[is.na(ptvf),ptvf := 0]









