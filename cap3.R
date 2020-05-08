#-----------------------------------------------------------------------------#
#        [ SICONV - Transferência Voluntárias Federais 2009 à 2017 ]          #
#       cap2.R - Script para Produção do Banco de Dados do Capítulo 3         #
#                                                  André Valente - Jan/2019   #
#-----------------------------------------------------------------------------#
#
setwd("C:/Users/Andre/Google Drive/TESE_R")
x <- c("data.table","xlsx","plyr")
#lapply(x, install.packages, character.only = TRUE) 
lapply(x, library, character.only = TRUE) 
rm(x)
#
cap2 <- readRDS("./dados/cap2.rds")
cap2.em <- readRDS("./dados/cap2_emendas.rds")
cap2.des <- readRDS("./dados/cap2_desembolso.rds")
cap2.des <- merge(cap2.des, cap2[,c("convenio","ibge","municipio")],
                  all.x=TRUE, by=c("convenio"))
names(cap2.des)[2] <- "ano"
#
#
tvf.TOTAL <- function() {##-- TVF TOTAL: tvftot.qt, tvttot, tvflib ----
  bd <- cap2[entidade %in% c("MUN", "OSC", "CPUB"), 
             c("convenio", "ibge","municipio","ano","vlrepasse")]
  # Variáveis: tvftot.qt e tvftot
    bd1 <- bd[,c(2,3,4,5)]
    bd1[, tvftot.qt := as.integer(.(count=.N)), by= c( "ibge", "ano")]
    bd1[, tvftot := sum( vlrepasse ), by= c( "ibge", "ano")]
    bd1 <- bd1[, c("ano","ibge","municipio","tvftot.qt","tvftot")]
    setkey(bd1); bd1 <- unique(bd1)
    # transformando dados em painel
      bd1 <- setDT(bd1)[ CJ(ibge = ibge, ano = ano, unique=TRUE),
                         on=.(ibge, ano)]  
      bd1 <- bd1[ano %in% seq(2009,2017,1),]
    # ajustando variáveis ausentes
      x <- unique(bd1[!is.na(municipio),c("ibge","municipio")])
      bd1 <- merge(bd1, x, all.x=TRUE, by=c("ibge"))
      bd1 <- bd1[,c(2,1,6,4,5)]; names(bd1)[3] <- "municipio"; rm(x)
      bd1[is.na(tvftot.qt), tvftot.qt := 0]
      bd1[is.na(tvftot), tvftot := 0]
  # Variável: tvflib
    bd2 <- cap2.des[convenio %in% unique(bd$convenio),]
    bd2[, tvflib := sum( vldes ), by= c( "ibge", "ano")]
    bd2 <- bd2[, c("ano","ibge","municipio","tvflib")]
    setkey(bd2); bd2 <- unique(bd2)
    bd1 <- merge(bd1,bd2[,c("ano","ibge","tvflib")],all.x=TRUE, by=c("ano","ibge"))
    bd1[is.na(tvflib), tvflib := 0]
  #
  saveRDS(bd1,"./dados/cap3.RDS")
}

tvf.MUN <- function() {##-- TVF MUNICIPIO: tvftot.qt, tvttot, tvflib ----
  bd <- cap2[entidade %in% c("MUN", "CPUB") & emenda == 0, 
             c("convenio", "ibge","municipio","ano","vlrepasse")]
  # Variáveis: tvfmun.qt e tvfmun
    bd1 <- bd[,c(2,3,4,5)]
    bd1[, tvfmun.qt := as.integer(.(count=.N)), by= c( "ibge", "ano")]
    bd1[, tvfmun := sum( vlrepasse ), by= c( "ibge", "ano")]
    bd1 <- bd1[, c("ano","ibge","municipio","tvfmun.qt","tvfmun")]
    setkey(bd1); bd1 <- unique(bd1)
  # Variável: tvfmunl
    bd2 <- cap2.des[convenio %in% unique(bd$convenio),]
    bd2[, tvfmunl := sum( vldes ), by= c( "ibge", "ano")]
    bd2 <- bd2[, c("ano","ibge","municipio","tvfmunl")]
    setkey(bd2); bd2 <- unique(bd2)
  #
    bd3 <- readRDS("./dados/cap3.RDS")
    bd3 <- merge(bd3, bd1[,!c("municipio")], all.x=TRUE, by=c("ano","ibge"))
    bd3 <- merge(bd3, bd2[,!c("municipio")], all.x=TRUE, by=c("ano","ibge"))
  #
    bd3[is.na(tvfmun.qt), tvfmun.qt := 0]
    bd3[is.na(tvfmun), tvfmun := 0]
    bd3[is.na(tvfmunl), tvfmunl := 0]
  #
    saveRDS(bd3,"./dados/cap3.RDS")
}

tvf.OSC <- function() {##-- TVF OSC: tvfosc.qt, tvtosc, tvfoscl ----
  bd <- cap2[entidade %in% c("OSC") & emenda == 0, 
             c("convenio", "ibge","municipio","ano","vlrepasse")]
  # Variáveis: tvfosc.qt e tvfosc
    bd1 <- bd[,c(2,3,4,5)]
    bd1[, tvfosc.qt := as.integer(.(count=.N)), by= c( "ibge", "ano")]
    bd1[, tvfosc := sum( vlrepasse ), by= c( "ibge", "ano")]
    bd1 <- bd1[, c("ano","ibge","municipio","tvfosc.qt","tvfosc")]
    setkey(bd1); bd1 <- unique(bd1)
  # Variável: tvfoscl
    bd2 <- cap2.des[convenio %in% unique(bd$convenio),]
    bd2[, tvfoscl := sum( vldes ), by= c( "ibge", "ano")]
    bd2 <- bd2[, c("ano","ibge","municipio","tvfoscl")]
    setkey(bd2); bd2 <- unique(bd2)
  #
    bd3 <- readRDS("./dados/cap3.RDS")
    bd3 <- merge(bd3, bd1[,!c("municipio")], all.x=TRUE, by=c("ano","ibge"))
    bd3 <- merge(bd3, bd2[,!c("municipio")], all.x=TRUE, by=c("ano","ibge"))
  #
    bd3[is.na(tvfosc.qt), tvfosc.qt := 0]
    bd3[is.na(tvfosc), tvfosc := 0]
    bd3[is.na(tvfoscl), tvfoscl := 0]
  #
    saveRDS(bd3,"./dados/cap3.RDS")
}

tvf.EMENDA.MUN <- function() {##-- TVF EMENDA MUN: tvfmunem.qt, tvtmunem, tvfmuneml ----
  emendas <- cap2.em[tipo =="I" & entidade %in% c("MUN", "CPUB"),]
  emendas <- unique(emendas$convenio)
  bd <- cap2[ entidade %in% c("MUN", "CPUB") & 
              emenda == 1 & convenio %in% emendas, 
             c("convenio", "ibge","municipio","ano","vlrepasse")]
  # Variáveis: tvfmunem.qt e tvfmunem
    bd1 <- bd[,c(2,3,4,5)]
    bd1[, tvfmunem.qt := as.integer(.(count=.N)), by= c( "ibge", "ano")]
    bd1[, tvfmunem := sum( vlrepasse ), by= c( "ibge", "ano")]
    bd1 <- bd1[, c("ano","ibge","municipio","tvfmunem.qt","tvfmunem")]
    setkey(bd1); bd1 <- unique(bd1)
  # Variável: tvfmuneml
    bd2 <- cap2.des[convenio %in% unique(bd$convenio),]
    bd2[, tvfmuneml := sum( vldes ), by= c( "ibge", "ano")]
    bd2 <- bd2[, c("ano","ibge","municipio","tvfmuneml")]
    setkey(bd2); bd2 <- unique(bd2)
  #
  bd3 <- readRDS("./dados/cap3.RDS")
  bd3 <- merge(bd3, bd1[,!c("municipio")], all.x=TRUE, by=c("ano","ibge"))
  bd3 <- merge(bd3, bd2[,!c("municipio")], all.x=TRUE, by=c("ano","ibge"))
  #
  bd3[is.na(tvfmunem.qt), tvfmunem.qt := 0]
  bd3[is.na(tvfmunem), tvfmunem := 0]
  bd3[is.na(tvfmuneml), tvfmuneml := 0]
  #
  saveRDS(bd3,"./dados/cap3.RDS")
}

tvf.EMENDA.OSC <- function() {##-- TVF EMENDA MUN: tvfoscem.qt, tvfoscem, tvfosceml ----
  emendas <- cap2.em[tipo =="I" & entidade %in% c("OSC"),]
  emendas <- unique(emendas$convenio)
  bd <- cap2[ entidade %in% c("OSC") & 
                emenda == 1 & convenio %in% emendas, 
              c("convenio", "ibge","municipio","ano","vlrepasse")]
  # Variáveis: tvfoscem.qt e tvfoscem
    bd1 <- bd[,c(2,3,4,5)]
    bd1[, tvfoscem.qt := as.integer(.(count=.N)), by= c( "ibge", "ano")]
    bd1[, tvfoscem := sum( vlrepasse ), by= c( "ibge", "ano")]
    bd1 <- bd1[, c("ano","ibge","municipio","tvfoscem.qt","tvfoscem")]
    setkey(bd1); bd1 <- unique(bd1)
  # Variável: tvfosceml
    bd2 <- cap2.des[convenio %in% unique(bd$convenio),]
    bd2[, tvfosceml := sum( vldes ), by= c( "ibge", "ano")]
    bd2 <- bd2[, c("ano","ibge","municipio","tvfosceml")]
    setkey(bd2); bd2 <- unique(bd2)
  #
    bd3 <- readRDS("./dados/cap3.RDS")
    bd3 <- merge(bd3, bd1[,!c("municipio")], all.x=TRUE, by=c("ano","ibge"))
    bd3 <- merge(bd3, bd2[,!c("municipio")], all.x=TRUE, by=c("ano","ibge"))
  #
    bd3[is.na(tvfoscem.qt), tvfoscem.qt := 0]
    bd3[is.na(tvfoscem), tvfoscem := 0]
    bd3[is.na(tvfosceml), tvfosceml := 0]
  #
    saveRDS(bd3,"./dados/cap3.RDS")
}

#
S2id <- function() { #-- Variáveis S2iD ----
  # Dados sobre Municípios em Situação de Emergência e Calamidade Pública 
    s2id <- setDT(read.table("./dados/S2iD/S2iD.csv",
                sep=";",header=TRUE, fileEncoding="UTF-8", na.strings = "NA",
                quote='"', fill=TRUE, stringsAsFactors = FALSE))
    s2id[, Cód..IBGE := as.character(Cód..IBGE)]; names(s2id)[1] <- "ibge"
    names(s2id)[2] <- "ano"; s2id[, ano := as.character(ano)]
    setkey(s2id, ibge, ano, SE.ECP); 
    s2id <- unique(s2id) # remover registros duplicados
  #
    s2id[, sit.emer := ifelse(SE.ECP =="SE",  1, 0)]
    bd1 <- s2id[sit.emer == 1,c(1,2,5)] # adic. var. situacao de emergencia
    setkey(bd1); bd1 <- unique(bd1)
    s2id[, cal.pub  := ifelse(SE.ECP =="ECP", 1, 0)]
    bd2 <- s2id[cal.pub  == 1,c(1,2,6)] # adic. var. estado de calamidade pub.
    setkey(bd2); bd2 <- unique(bd2)
  #
    bd3 <- readRDS("./dados/cap3.RDS")
    bd3 <- merge(bd3, bd1[], all.x=TRUE, by=c("ano","ibge"))
    bd3 <- merge(bd3, bd2[], all.x=TRUE, by=c("ano","ibge"))
  #
    bd3[is.na(sit.emer), sit.emer:= 0]
    bd3[is.na(cal.pub), cal.pub:= 0]
  #
    saveRDS(bd3,"./dados/cap3.RDS")
}
#
ciclos <- function(){# Variáveis Ciclos Eleitorais ----
  #
  bd <- readRDS("./dados/cap3.RDS")
  bd[, ps.anoel:= ifelse(ano %in% c("2010","2014"),1,0)]
  bd[, pf.anoel:= ifelse(ano %in% c("2012","2016"),1,0)]
  #
  saveRDS(bd,"./dados/cap3.RDS")
}

pbf <- function() { ## Variável Programa Bolsa Família ----
  # http://www.portaltransparencia.gov.br/
  #vdownload-de-dados/bolsa-familia-pagamentos
  ## Fonte: Ministério Desenvolvimento Social
  mds <- setDT(read.table("./dados/mds_pbf/201712_BolsaFamilia_Pagamentos.csv",
               sep=";",header=TRUE, fileEncoding="latin1", na.strings = "NA",
               quote='"', fill=TRUE, stringsAsFactors = FALSE,
               colClasses = c(rep("NULL", 2),rep("character", 3),
                                       rep("NULL", 2),"character")))
  names(mds) <- c("uf","siafi","municipio","valor")
  mds[, valor := as.numeric(gsub(",",".",valor))]
  mds[, siafi := as.numeric(siafi)]; mds[, siafi := as.character(siafi)]
  mds[,valor := sum(valor), by=c("uf","siafi")]
  setkey(mds); mds <- unique(mds)
  pbf17 <- mds; rm(mds)
  saveRDS(pbf17, "./dados/mds_pbf/pbf17.RDS")
#
  mds <- setDT(read.table("./dados/mds_pbf/201512_BolsaFamilia_Pagamentos.csv",
               sep=";",header=TRUE, fileEncoding="latin1", na.strings = "NA",
               quote='"', fill=TRUE, stringsAsFactors = FALSE,
               colClasses = c(rep("NULL", 2),rep("character", 3),
                                       rep("NULL", 2),"character")))
  names(mds) <- c("uf","siafi","municipio","valor")
  mds[, valor := as.numeric(gsub(",",".",valor))]
  mds[, siafi := as.numeric(siafi)]; mds[, siafi := as.character(siafi)]
  mds <- mds[,valor := sum(valor), by=c("uf","siafi")]
  setkey(mds); mds <- unique(mds)
  pbf15 <- mds; rm(mds)
  saveRDS(pbf15, "./dados/mds_pbf/pbf15.RDS")
#
  mds <- setDT(read.table("./dados/mds_pbf/201412_BolsaFamilia_Pagamentos.csv",
               sep=";",header=TRUE, fileEncoding="latin1", na.strings = "NA",
               quote='"', fill=TRUE, stringsAsFactors = FALSE,
               colClasses = c(rep("NULL", 2),rep("character", 3),
                                       rep("NULL", 2),"character")))
  names(mds) <- c("uf","siafi","municipio","valor")
  mds[, valor := as.numeric(gsub(",",".",valor))]
  mds[, siafi := as.numeric(siafi)]; mds[, siafi := as.character(siafi)]
  mds <- mds[,valor := sum(valor), by=c("uf","siafi")]
  setkey(mds); mds <- unique(mds)
  pbf14 <- mds; rm(mds)
  saveRDS(pbf14, "./dados/mds_pbf/pbf14.RDS")
#
  names(pbf14)[4] <- "2014"; names(pbf15)[4] <- "2015"; names(pbf17)[4] <- "2017"
  pbf15 <- pbf15[,c(2,4)]; pbf17 <- pbf17[,c(2,4)]
  mds <- merge(pbf14,pbf15, by=c("siafi"), all.x = TRUE)
  mds <- merge(mds,pbf17, by=c("siafi"), all.x = TRUE)
  mds <- mds[,c(1,4,5,6)]
  rm(pbf14, pbf15, pbf17)
#
# Mapeamento dos Códigos IBGE x TSE x SIAFI
# Origem dos Dados - https://github.com/meirelesff/codesBR
  load(file = "./dados/sysdata.rda")
  codigos <- setDT(codigos)
# arrumando codigos do TSE Incorretos
  codigos[cod_tse=="90017", cod_tse := "89931"]; 
  codigos[cod_tse=="37904", cod_tse := "37915"]
  codigos[cod_tse=="35104", cod_tse := "33090"]
# saveRDS(codigos, "./dados/codigos.rds");
  codigos <- codigos[,c(9,11)]; names(codigos) <- c("ibge","siafi")
  mds <- merge(mds,codigos, by=c("siafi"), all.x = TRUE)
  mds <- mds[, c(5,2,3,4)]
  rm(codigos)
#
# Programa Bolsa Família  (Fonte: IPEADATA)
# http://www.ipeadata.gov.br/Default.aspx
  pbf <- setDT(read.table("./dados/mds_pbf/ipeadata-pbf-mun-dez.csv",
               sep=";",header=TRUE, fileEncoding="UTF-8", na.strings = "NA",
               quote='"', fill=TRUE, stringsAsFactors = FALSE, skip=1))
  pbf[,X:= NULL]
  names(pbf) <- c("uf","ibge", "municipio",
                  "2009", "2010", "2011", "2012", "2013", "2016")
  pbf[, ibge := as.character(ibge)]
  pbf[, "2009" := as.numeric(pbf$`2009`)] 
  pbf[, "2010" := as.numeric(pbf$`2010`)]
  pbf[, "2011" := as.numeric(pbf$`2011`)] 
  pbf[, "2012" := as.numeric(pbf$`2012`)]
  pbf[, "2013" := as.numeric(pbf$`2013`)]
  pbf[, "2016" := as.numeric(pbf$`2016`)]
  pbf <- merge(pbf,mds, by="ibge", all.x = TRUE); #rm(mds)
  rm(mds)
  pbf <- pbf[,c(1,3,2,4,5,6,7,8,10,11,9,12)]
  pbf <- melt(pbf, id.vars = c("uf", "ibge", "municipio"),
              measure.vars = c("2009", "2010", "2011", "2012", "2013",
                               "2014", "2015", "2016", "2017"))
  names(pbf)[4] <- "ano"; names(pbf)[5] <- "pbf"
  setkey(pbf, ibge, ano)
  #saveRDS(pbf, "./dados/mds_pbf/pbf.rds")
# deflacionar os valores do PBF para Dezembro de 2017 pelo IPCA
  ipca    <- c(1.6353082, #2009
               1.5480680, #2010
               1.4516647, #2011
               1.3755417, #2012
               1.3004487, #2013
               1.2204433, #2014
               1.1047118, #2015
               1.0325619, #2016
               1.0044000) #2017
  i <- 1;
  for (a in rep(2009:2017,1)) {
    pbf[ano == a, pbf := pbf*ipca[i]]; i <- i+1;
  }
  rm(a,i,ipca)
  saveRDS(pbf,"./dados/mds_pbf/pbf_dez2017.rds")
  pbf <- pbf[,c(2,4,5)]
  #
  bd <- readRDS("./dados/cap3.RDS")
  bd <- merge(bd, pbf, by=c("ibge","ano"), all.x = TRUE)
  bd[is.na(pbf), pbf:= 0]
  # NA's: 1504752; 2206720; 4202131; 4212650; 4220000; 4314548; 5006275
  saveRDS(bd, "./dados/cap3.RDS")
#
}
#
pop_pib <- function(){## Populacao e PIB dos municípios -----
  pop <- readRDS("./dados/IBGE/pop_pib.RDS")
  bd <- readRDS("./dados/cap3.RDS")
  #
    bd <- merge(bd, pop[,c("ibge","ano","pop","pib")],
                all.x = TRUE, by= c("ibge","ano"))
    #"1504752" "4212650" "4220000" "4314548" "5006275"
    removeMUN <- bd[is.na(pop), unique(ibge)]
    bd <- bd[!(ibge %in% removeMUN),]
    #
      saveRDS(bd, "./dados/cap3.RDS")
}  

#
ifgf <- function() { # Índice Firjan de Gestão Municipal ----
  ifgf <- setDT(read.table("./dados/Firjan/ifgf.csv", sep=";", header=TRUE, 
                           quote='"', fileEncoding="UTF-8", na.strings = "NA",
                           fill=TRUE, stringsAsFactors = FALSE))
  names(ifgf) <- c("ibge","uf","municipio","2009","2010","2011","2012",
                   "2013","2014","2015","2016","2017")
  ifgf[,ibge := as.character(ibge)]
  ifgf[,"2009":= round(as.numeric(`2009`),3)]; 
  ifgf[,"2010":= round(as.numeric(`2010`),3)]
  ifgf[,"2011":= round(as.numeric(`2011`),3)]
  ifgf[,"2012":= round(as.numeric(`2012`),3)]
  ifgf[,"2013":= round(as.numeric(`2013`),3)]
  ifgf[,"2014":= round(as.numeric(`2014`),3)]
  ifgf[,"2015":= round(as.numeric(`2015`),3)]
  ifgf[,"2016":= round(as.numeric(`2016`),3)]
  ifgf[,"2017":= round(as.numeric(`2016`),3)]
  ifgf <- melt(ifgf, id.vars = c("ibge", "uf", "municipio"),
                measure.vars = c("2009", "2010", "2011", "2012",
                                 "2013", "2014", "2015", "2016", "2017"))
  ifgf <- ifgf[,c(1,4,5)]; names(ifgf) <- c("ibgeifgf","ano","ifgf")
  bd <- readRDS("./dados/cap3.RDS")
  x <- bd[,c("ibge")]; x <- unique(x); x[,ibgeifgf := substr(ibge, 1, 6)]
  ifgf <- merge(ifgf, x, by=c("ibgeifgf"), all.y = TRUE)
  ifgf <- ifgf[,c(4,2,3)]
  bd <- merge(bd, ifgf, by=c("ibge", "ano"), all.x = TRUE)
  rm(ifgf,x)
  #
    saveRDS(bd, "./dados/cap3.RDS")
}

#
## Variáveis Políticas TSE ----
# no arquivo cap3_tse.R
