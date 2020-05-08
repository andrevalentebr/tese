#------------------------------------------------------------------------------#
# CAP3 - EFEITO DO ALINHAMENTO POLÍTICO SOBRE A ALOCAÇÃO DAS TVF AOS MUNICÍPIOS
#        [ SICONV - Transferência voluntárias 2009 à 2017 ]
#                                                   Andre Valente - Nov/2018
#------------------------------------------------------------------------------#
#
## Variáveis Políticas TSE ----
# 
# ----------- Variáveis Globais -----------#
library(data.table)
library(xlsx)
setwd("C:/Users/Andre/Google Drive/TESE_R")
pastaDados <- file.path(getwd(),"dados")
anos    <- c("2006", "2008", "2010", "2012", "2014", "2016")
dir     <- c("consulta_cand_2006","consulta_cand_2008",
             "consulta_cand_2010","consulta_cand_2012",
             "consulta_cand_2014","consulta_cand_2016",
        "votacao_candidato_munzona_2006", "votacao_candidato_munzona_2008",
        "votacao_candidato_munzona_2010","votacao_candidato_munzona_2012",
        "votacao_candidato_munzona_2014","votacao_candidato_munzona_2016")
siglas <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO",
            "MA","MT","MS","MG","PA","PB","PR","PE","PI",
            "RJ","RN","RS","RO","RR","SC","SP","SE","TO")

## Alinhamento Partidário entre prefeitos, governadores e presidente ------
# ----------- Carga dos dados do tse-----------#
aliPart <- function () {
  anos   <- cc("2006", "2008", "2010", "2012", "2014", "2016")
  siglas <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO",
              "MA","MT","MS","MG","PA","PB","PR","PE","PI",
              "RJ","RN","RS","RO","RR","SC","SP","SE","TO")
  for (ano in anos) {
    for (sigla in siglas) {
      #----------- dados de votação -----------#
      if (ano %in% c("2008", "2012", "2016") & sigla=="DF") {next}
        #campos  - ano,turno,eleicao,uf,codmun,cargo,eleito,partido 
      if (ano %in% c("2006", "2008", "2010", "2012")) {
        arq <- paste("./dados/tse/candidatos/consulta_cand_",
                     ano, "_", sigla,".txt", sep="")
        al <- setDT(read.table(arq, sep = ";", header = FALSE, quote='"',
                               na.strings = "NA", stringsAsFactors = FALSE, 
                               fileEncoding = "latin1", fill=TRUE))
      }
      if (ano %in% c("2014", "2016")) {
        arq <- paste("./dados/tse/candidatos/consulta_cand_",
                     ano, "_", sigla,".csv", sep="")
        al <- setDT(read.table(arq, sep = ";", header = TRUE, quote='"',
                               na.strings = "NA", stringsAsFactors = FALSE,
                               fileEncoding = "latin1", fill=TRUE))
      }
      #----------- junçao dos dados dos estados em um único BD
      if (sigla =="AC"){
        ap <- al
      } else {
        ap <- rbind(ap, al)
      }
    }
    if (ano =="2006"){al2006 <- ap}
    if (ano =="2008"){al2008 <- ap}
    if (ano =="2010"){al2010 <- ap}
    if (ano =="2012"){al2012 <- ap}
    if (ano =="2014"){al2014 <- ap}
    if (ano =="2016"){al2016 <- ap}
  }
  # Governadores (2006, 2010,2014)
    al2006 <- al2006[V5=="ELEICOES 2006" & V10=="GOVERNADOR" & V43=="ELEITO",]
    al2006 <- unique(al2006[,c("V3","V6","V10","V19")])
    names(al2006) <- c("ano","uf","cargo","pgov")
  #
    al2010 <- al2010[V5=="ELEIÇÕES 2010" & V10=="GOVERNADOR" & V43=="ELEITO",]
    al2010 <- unique(al2010[,c("V3","V6","V10","V19")])
    names(al2010) <- c("ano","uf","cargo","pgov")
  #  
    al2014 <- al2014[NM_TIPO_ELEICAO=="ELEIÇÃO ORDINÁRIA" & 
                      DS_CARGO=="GOVERNADOR" & 
                      DS_SIT_TOT_TURNO=="ELEITO",]
    al2014 <- unique(al2014[,c("ANO_ELEICAO","SG_UF",
                               "DS_CARGO","SG_PARTIDO")])
    names(al2014) <- c("ano","uf","cargo","pgov")
  #
    al <- rbind(rbind(al2006, al2010),al2014)
    assign("algov", al, .GlobalEnv); rm(al)
  # Prefeitos eleitos (2008, 2012 e 2016)
    al2008 <- al2008[V5=="Eleições 2008" & V10=="PREFEITO" & V43=="ELEITO",]
    al2008 <- unique(al2008[,c("V3","V6","V7","V10","V19")]) 
    #identificar as cidades pelo codigo tse: 33090, 37915, 89931 
    names(al2008) <- c("ano", "uf", "cod_tse","cargo","ppref")
  #
    al2012 <- al2012[V5=="ELEIÇÃO MUNICIPAL 2012" & 
                     V10=="PREFEITO" & V43=="ELEITO",]
    al2012 <- unique(al2012[,c("V3","V6","V7","V10","V19")]) 
    #identificar as cidades pelo codigo tse: 33090, 37915, 89931 
    names(al2012) <- c("ano", "uf", "cod_tse","cargo","ppref")
  #
    al2016 <- al2016[NM_TIPO_ELEICAO=="ELEIÇÃO ORDINÁRIA" & 
                     DS_CARGO=="PREFEITO" & DS_SIT_TOT_TURNO=="ELEITO",]
    al2016 <- unique(al2016[,c("ANO_ELEICAO","SG_UF","SG_UE",
                               "DS_CARGO", "SG_PARTIDO")]) 
    #identificar as cidades pelo codigo tse: 33090, 37915, 89931 
    names(al2016) <- c("ano", "uf", "cod_tse","cargo","ppref")
  #    
    al <- rbind(rbind(al2008, al2012),al2016)
    assign("alpref", al, .GlobalEnv); rm(al)
#
#
# preparar a base de dados para ser mesclada
#
  alpref[, ano := as.character(ano)]
  alpref[ano=="2008", ano := "2009"]
  alpref[ano=="2012", ano := "2013"]
  alpref[ano=="2016", ano := "2017"]
  #
  temp <- alpref[ano=="2009",]; temp[,ano:= "2010"];
  alpref <- rbind(alpref,temp)
  temp <- alpref[ano=="2009",]; temp[,ano:= "2011"];
  alpref <- rbind(alpref,temp)
  temp <- alpref[ano=="2009",]; temp[,ano:= "2012"];
  alpref <- rbind(alpref,temp)
  temp <- alpref[ano=="2013",]; temp[,ano:= "2014"];
  alpref <- rbind(alpref,temp)
  temp <- alpref[ano=="2013",]; temp[,ano:= "2015"];
  alpref <- rbind(alpref,temp)
  temp <- alpref[ano=="2013",]; temp[,ano:= "2016"];
  alpref <- rbind(alpref,temp)
  #
  alpref[,cod_tse := as.character(cod_tse)]; setkey(alpref)
#
  algov[, ano := as.character(ano)]
  algov[ano=="2006", ano := "2009"]
  algov[ano=="2010", ano := "2011"]
  algov[ano=="2014", ano := "2015"]
  temp <- algov[ano=="2009",]; temp[,ano:= "2010"];
  algov <- rbind(algov,temp)
  temp <- algov[ano=="2011",]; temp[,ano:= "2012"];
  algov <- rbind(algov,temp)
  temp <- algov[ano=="2011",]; temp[,ano:= "2013"];
  algov <- rbind(algov,temp)
  temp <- algov[ano=="2011",]; temp[,ano:= "2014"];
  algov <- rbind(algov,temp)
  temp <- algov[ano=="2015",]; temp[,ano:= "2016"];
  algov <- rbind(algov,temp)
  temp <- algov[ano=="2015",]; temp[,ano:= "2017"];
  algov <- rbind(algov,temp)
  setkey(algov)
#
# Adicionar código do IBGE
  codigos <- readRDS("./dados/codigos.RDS")
  al <- merge(alpref,codigos[,c("cod_tse","cod_ibge","nome_municipio")],
              all.x = TRUE, by="cod_tse")
  al <- al[,c(1,6,7,3,4,2,5)]
  # tabela
    write.xlsx(al, "./dados/tse/Prefeitos.xlsx",
             sheet="tab1",append=TRUE)  
  al <- al[,c(2,6,7)]; names(al)[1] <- "ibge"
  # partido do prefeito e do governador
    bd <- readRDS("./dados/cap3.RDS")
    bd <- merge(bd,al, all.x = TRUE, by=c("ibge", "ano"))
    al <- algov[,c(1,2,4)]
    names(codigos)[9] <- "ibge"; names(codigos)[1] <- "desc_uf";
    names(codigos)[2] <- "uf";
    bd <- merge(bd,codigos[,c("ibge","uf")], all.x = TRUE, by=c("ibge"))
    bd <- merge(bd,al, all.x = TRUE, by=c("uf", "ano"))
  # Alinhado ao Partido do Governador
    bd[, p.gov := ifelse(ppref == pgov, "1", "0")]
    bd[,p.gov := as.integer(p.gov)]
  # Alihado ao Partido do Presidente
    bd[ano %in% seq("2009", "2015", 1),ppres:="PT"]
    bd[ano %in% c("2016", "2017"),ppres:="PMDB"]
    bd[, p.pres := ifelse(ppref == ppres, "1", "0")]
    bd[,p.pres := as.integer(p.pres)]
  # Alinhamento com a coalisão do presidente
    bd[ano=="2009", p.coal := ifelse(ppref %in% 
        c("PT","PR","PC do B","PSB","PTB","PMDB","PP","PDT","PRB"), 1, 0)]
    bd[ano=="2010", p.coal := ifelse(ppref %in% 
        c("PT","PR","PC do B","PSB","PTB","PMDB","PP","PRB"), 1, 0)]
    bd[ano=="2011", p.coal := ifelse(ppref %in% 
        c("PT","PR","PC do B","PSB","PMDB","PDT","PP"), 1, 0)]
    bd[ano=="2012", p.coal := ifelse(ppref %in% 
        c("PT","PR","PC do B","PSB","PMDB","PDT","PP","PRB"), 1, 0)]
    bd[ano=="2013", p.coal := ifelse(ppref %in% 
        c("PT","PR","PC do B","PSB","PMDB","PDT","PP","PRB"), 1, 0)]
    bd[ano=="2014", p.coal := ifelse(ppref %in% 
        c("PT","PR","PC do B","PMDB","PDT","PP","PRB"), 1, 0)]
    bd[ano=="2015", p.coal := ifelse(ppref %in% 
        c("PT","PR","PC do B","PMDB","PDT","PP","PRB","PSB","PTB"), 1, 0)]
    bd[ano=="2016", p.coal := ifelse(ppref %in% 
        c("PMDB","PSDB","PR","PRB","PSD","PTB","DEM","PPS","PP"), 1, 0)]
    bd[ano=="2017", p.coal := ifelse(ppref %in% 
        c("PMDB","PSDB","PR","PRB","PSD","PTB","DEM","PP"), 1, 0)]
    bd[,p.coal := as.integer(p.coal)]
    setkey(bd,ibge,ano); 
    saveRDS(bd, "./dados/cap3.RDS") 
    rm(al, algov, alpref, bd, temp)
}
#
#
## Margem de Votos e cálculo de Dominância Eleitoral ---------
#
cargaVotos <- function () {
  anos    <- c("2006", "2008", "2010", "2012", "2014", "2016")
  siglas <- c("AC","AL","AP","AM","BA","BR","CE","DF","ES","GO",
              "MA","MT","MS","MG","PA","PB","PR","PE","PI",
              "RJ","RN","RS","RO","RR","SC","SP","SE","TO")
  for (ano in anos) {
    for (sigla in siglas) {
      if (ano %in% c("2008", "2012", "2016") & 
          sigla %in% c("DF","BR")) {next}
      #----------- dados de votação -----------#
      arq <- paste("./dados/tse/votos/votacao_candidato_munzona_",
                   ano, "_", sigla,".txt", sep="")
      vt <- setDT(read.table(arq, sep = ";", header = FALSE, quote='"',
                             na.strings = "NA", stringsAsFactors = FALSE,
                             fileEncoding = "latin1", fill=TRUE))
      
      print(paste0(ano," - ", sigla))
      #----------- junçao dos dados dos estados em um único BD
      if (sigla =="AC"){
        votos <- vt
      } else {
        votos <- rbind(votos, vt)
      }
    }
    if (ano =="2006"){saveRDS(votos, "./dados/tse/vt06.rds")}
    if (ano =="2008"){saveRDS(votos, "./dados/tse/vt08.rds")}
    if (ano =="2010"){saveRDS(votos, "./dados/tse/vt10.rds")}
    if (ano =="2012"){saveRDS(votos, "./dados/tse/vt12.rds")}
    if (ano =="2014"){saveRDS(votos, "./dados/tse/vt14.rds")}
    if (ano =="2016"){saveRDS(votos, "./dados/tse/vt16.rds")}
  }
}
## Calcula as variáveis de Dominância e Margem de Votos
DomMargVotos <- function(){
 # limpando e transformando os dados  
  vt06 <- readRDS("./dados/tse/vt06.RDS")
  vt06 <- vt06[ V5=="ELEIÇÕES 2006" & 
                V16 %in% c("DEPUTADO FEDERAL","GOVERNADOR","PRESIDENTE"),]
  vt06 <- vt06[,c("V3","V4","V6","V8","V13","V16","V22","V24","V29")]
  vt06[, V13 := as.character(V13)]
  names(vt06) <- c("ano","turno","uf","cod_tse","candidato",
                   "cargo","resultado","partido","votos")
#
  vt10 <- readRDS("./dados/tse/vt10.RDS")
  vt10 <- vt10[ V5=="ELEIÇÕES 2010" & 
                V16 %in% c("DEPUTADO FEDERAL","GOVERNADOR","PRESIDENTE"),]
  vt10 <- vt10[,c("V3","V4","V6","V8","V13","V16","V22","V24","V29")]
  vt10[, V13 := as.character(V13)]
  names(vt10) <- c("ano","turno","uf","cod_tse","candidato",
                   "cargo","resultado","partido","votos")
#
  vt14 <- readRDS("./dados/tse/vt14.RDS")
  vt14 <- vt14[ V5=="ELEIÇÕES GERAIS 2014" & 
                V16 %in% c("DEPUTADO FEDERAL","GOVERNADOR","PRESIDENTE"),]
  vt14 <- vt14[,c("V3","V4","V6","V8","V13","V16","V22","V24","V29")]
  vt14[, V13 := as.character(V13)]
  names(vt14) <- c("ano","turno","uf","cod_tse","candidato",
                   "cargo","resultado","partido","votos")
# Prefeitos - 2008, 2012 e 2016
  vt08 <- readRDS("./dados/tse/vt08.RDS")
  vt08 <- vt08[V5=="ELEIÇÕES 2008" & V16 %in% c("PREFEITO"),]
  vt08 <- vt08[,c("V3","V4","V6","V8","V13","V16","V22","V29")]
  vt08[, V13 := as.character(V13)]
  names(vt08) <- c("ano","turno","uf","cod_tse","candidato",
                   "cargo","resultado","votos")
#  
  vt12 <- readRDS("./dados/tse/vt12.RDS")
  vt12 <- vt12[V5=="ELEIÇÃO MUNICIPAL 2012" & V16 %in% c("PREFEITO"),]
  vt12 <- vt12[,c("V3","V4","V6","V8","V13","V16","V22","V29")]
  vt12[, V13 := as.character(V13)]
  names(vt12) <- c("ano","turno","uf","cod_tse","candidato",
                   "cargo","resultado","votos")
#  
  vt16 <- readRDS("./dados/tse/vt16.RDS")
  vt16 <- vt16[V5=="ELEIÇÕES MUNICIPAIS 2016" & V16 %in% c("PREFEITO"),]
  vt16 <- vt16[,c("V3","V4","V6","V8","V13","V16","V22","V29")]
  vt16[, V13 := as.character(V13)]
  names(vt16) <- c("ano","turno","uf","cod_tse","candidato",
                   "cargo","resultado","votos")
#
  temp <- rbind(rbind(vt06,vt10),vt14)
  dfed <- temp[cargo %in% c("DEPUTADO FEDERAL"),]
  gov <- temp[cargo %in% c("GOVERNADOR"),]
  pres <- temp[cargo %in% c("PRESIDENTE"),]
  pref <- rbind(rbind(vt08,vt12),vt16)
  rm(vt06,vt08,vt10,vt12,vt14,vt16,temp)

# PREFEITOS - Variáveis Dominância e Margem de Vitória 
  setkey(pref, ano, turno, cod_tse, candidato); 
  pref[,votos:= sum(votos),by=c("ano","turno","cod_tse","candidato")]
  pref <- unique(pref)
  pref[,tvot:= sum(votos),by=c("ano","turno","cod_tse")]
# dominância e margem de vitoria prefeito
  pref[,pf.dm:= round(votos/tvot,4)]; 
  setorder(pref, ano, turno, cod_tse, resultado, -pf.dm)
  pref[,pf.mv := .SD[,pf.dm[1]-pf.dm[2]],by=c("ano","turno","cod_tse")];
  pref[,pf.mv := ifelse(pf.dm==1,1,pf.mv)] #municipios com só um candidato
  pref <- pref[!is.na(pf.mv),]
  # remover 1 turno quando houver 2 turno
    x <- pref[resultado=="2º TURNO",c("ano","turno","cod_tse")] 
    setkey(x); x <- unique(x); x[,del:= 1]
    pref <- merge(pref,x, all.x = TRUE, by=c("ano","turno","cod_tse"))
    pref <- pref[is.na(del),]; pref[, del := NULL]; rm(x)
  pref <- pref[resultado=="ELEITO",]
  pref <- pref[,c("ano","cod_tse","pf.dm","pf.mv")]
  # ajustando os prefeitos para Dados em Painel 
  #(2008 -> 2009, 2010, 2011, 2012; 
  #2012 -> 2013, 2014, 2015, 2016; 2016 -> 2017)
    pref[,ano:=as.character(ano)]
    pref[ano=="2016", ano:="2017"]
    pref[ano=="2012", ano:="2016"]
    p <- pref[ano=="2016",]; p <- p[ano=="2016", ano:="2015"] 
    pref <- rbind(pref,p)
    #
    p <- pref[ano=="2016",]; p <- p[ano=="2016", ano:="2014"]
    pref <- rbind(pref,p)
    #
    p <- pref[ano=="2016",]; p <- p[ano=="2016", ano:="2013"]
    pref <- rbind(pref,p)
    #
    pref[ano=="2008", ano:="2012"]
    p <- pref[ano=="2012",]; p <- p[ano=="2012", ano:="2011"]
    pref <- rbind(pref,p)
    p <- pref[ano=="2012",]; p <- p[ano=="2012", ano:="2010"]
    pref <- rbind(pref,p)
    p <- pref[ano=="2012",]; p <- p[ano=="2012", ano:="2009"]
    pref <- rbind(pref,p)
    rm(p)
  # mesclar cod_tse para obter codigo ibge
    pref[,cod_tse := as.character(cod_tse)]
    codigos <- readRDS("./dados/codigos.RDS")
    pref <- merge(pref,codigos[,c(10,9,8,2)], all.x = TRUE, by="cod_tse")
    pref <- pref[,c(2,7,5,6,1,3,4)]
    saveRDS(pref, "./dados/tse/dominancia_pref.rds")
  #
  # GOVERNADORES - Variáveis Dominância e Margem de Vitória
  #gov[,partido:= NULL]
    setkey(gov, ano, turno, cod_tse, candidato); 
    gov[,votos:= sum(votos),by=c("ano","turno","cod_tse","candidato")]
    gov <- unique(gov)
    # remover 1 turno quando houver 2 turno
      x <- gov[resultado=="2º TURNO",c("ano","turno","cod_tse")]; 
      setkey(x); x <- unique(x); x[,del:= 1]
      gov <- merge(gov,x, all.x = TRUE, by=c("ano","turno","cod_tse"))
      gov <- gov[is.na(del),]; gov[, del := NULL]; rm(x)
    # removendo o primeiro turno de 2014 (não tem resultado 2o. Turno)
      x <- as.data.table(table(gov[resultado=="ELEITO",c(cod_tse)]));
      x <- x[N>3,] # > 3 eleitos houve 2o turno para um dos eleitos em 2014
      gov <- gov[!(turno==1 & cod_tse %in% x$V1),]; rm(x) # remove o 1o. Turno 
  # Total de votos no município e na uf
    gov[,tvot:= sum(votos),by=c("ano","turno","cod_tse")]
    gov[,tot.cand.uf := sum(votos),by=c("ano","turno","uf","candidato")]
  # Dominância do governador do município e do município no estado
    gov[,gv.dm:= round(votos/tvot,4)];  
    gov[,gv.dmuf:= round(votos/tot.cand.uf,4)] 
  # Margem de vitória do Gov no município
    setorder(gov, ano, turno, cod_tse, resultado, -gv.dm)
    gov[,gv.mv := .SD[,gv.dm[1]-gv.dm[2]],by=c("ano","turno","cod_tse")]
  # removendo registros duplicados e campos desnecessários
    gov <- gov[resultado=="ELEITO",]
    gov <- gov[,c("ano","cod_tse","partido","gv.dm","gv.dmuf","gv.mv")]
    setkey(gov); gov <- unique(gov)
  # ajustando os Governadores para Dados em Painel 
  #(2006 -> 2009, 2010; 
    #2010 -> 2011, 2012, 2013, 2014; 2014 -> 2015, 2016, 2017)
    gov[,ano:=as.character(ano)]
    gov[ano=="2014", ano:="2017"]
    p <- gov[ano=="2017",]; p <- p[ano=="2017", ano:="2016"] 
    gov <- rbind(gov,p)
    p <- gov[ano=="2017",]; p <- p[ano=="2017", ano:="2015"]
    gov <- rbind(gov,p)
    #
    gov[ano=="2010", ano:="2014"]
    p <- gov[ano=="2014",]; p <- p[ano=="2014", ano:="2013"]
    gov <- rbind(gov,p)
    p <- gov[ano=="2014",]; p <- p[ano=="2014", ano:="2012"]
    gov <- rbind(gov,p)
    p <- gov[ano=="2014",]; p <- p[ano=="2014", ano:="2011"]
    gov <- rbind(gov,p)
    #
    gov[ano=="2006", ano:="2010"]
    p <- gov[ano=="2010",]; p <- p[ano=="2010", ano:="2009"]
    gov <- rbind(gov,p)
    rm(p)
  # mesclar cod_tse para obter codigo ibge
    gov[,cod_tse := as.character(cod_tse)]
    gov <- merge(gov,codigos[,c(10,9,8,2)], all.x = TRUE, by="cod_tse")
    gov <- gov[,c(2,9,7,8,1,3,4,5,6)]
  #View(gov[is.na(sigla_uf),]) <-- Municipios sem correspondência 
    saveRDS(gov, "./dados/tse/dominancia_gov.rds")
  #
  # PRESIDENTE - Variáveis Dominância e Margem de Vitória
    pres[,partido:= NULL]
    setkey(pres, ano, turno, cod_tse, candidato); 
    pres[,votos:= sum(votos),by=c("ano","turno","cod_tse","candidato")]
    pres <- unique(pres)
    pres[,tvot:= sum(votos),by=c("ano","turno","cod_tse")]
  #
    pres[,ps.dm:= round(votos/tvot,4)]; 
    setorder(pres, ano, cod_tse, turno, resultado, -ps.dm)
    pres[,ps.mv := .SD[,ps.dm[1]-ps.dm[2]],by=c("ano","turno","cod_tse")]
    # remover 1 turno quando houver 2 turno
      pres <- pres[!(uf %in% c("ZZ", "VT")),]
      x <- pres[resultado=="2º TURNO",c("ano","turno","cod_tse")]; 
      setkey(x); x <- unique(x); x[,del:= 1]
      pres <- merge(pres,x, all.x = TRUE, by=c("ano","turno","cod_tse"))
      pres <- pres[is.na(del),]; pres[, del := NULL]; rm(x)
    pres <- pres[resultado=="ELEITO",]
  #
    pres <- pres[,c("ano","cod_tse","ps.dm","ps.mv")]
    setkey(pres); pres <- unique(pres)
  # ajustando Presidentes para Dados em Painel 
  #(2006 -> 2009, 2010; 
    #2010 -> 2011, 2012, 2013, 2014; 2014 -> 2015, 2016, 2017)
    pres[,ano:=as.character(ano)]
    pres[ano=="2014", ano:="2017"]
    p <- pres[ano=="2017",]; p <- p[ano=="2017", ano:="2016"]
    pres <- rbind(pres,p)
    p <- pres[ano=="2017",]; p <- p[ano=="2017", ano:="2015"]
    pres <- rbind(pres,p)
    #
    pres[ano=="2010", ano:="2014"]
    p <- pres[ano=="2014",]; p <- p[ano=="2014", ano:="2013"]
    pres <- rbind(pres,p)
    p <- pres[ano=="2014",]; p <- p[ano=="2014", ano:="2012"]
    pres <- rbind(pres,p)
    p <- pres[ano=="2014",]; p <- p[ano=="2014", ano:="2011"]
    pres <- rbind(pres,p)
    #
    pres[ano=="2006", ano:="2010"]
    p <- pres[ano=="2010",]; p <- p[ano=="2010", ano:="2009"]
    pres <- rbind(pres,p)
    rm(p)
  # mesclar cod_tse para obter codigo ibge
    pres[,cod_tse := as.character(cod_tse)]
    pres <- merge(pres,codigos[,c(10,9,8,2)], all.x = TRUE, by="cod_tse")
    pres <- pres[,c(2,7,5,6,1,3,4)]
  #View(pres[is.na(sigla_uf),]) <-- Municipios sem correspondência 
    saveRDS(pres, "./dados/tse/dominancia_pres.rds")
  #
  # DEPUTADO FEDERAL - Variáveis Dominância e Margem de Vitória
  #- Normalizandos os resultados eleitorais dos deputados federais
    dfed[resultado=="MÉDIA", resultado := "ELEITO"]
    dfed[resultado=="ELEITO POR QP", resultado := "ELEITO"]
    dfed[resultado=="ELEITO POR MÉDIA", resultado := "ELEITO"]
    dfed[,turno:=NULL]
  # agregando votos por candidato no município
    dfed[,votos:= sum(votos),by=c("ano","cod_tse","candidato")]
    setkey(dfed); dfed <- unique(dfed)  #- removendo registros redundantes
    dfed[,tvot:= sum(votos),by=c("ano","cod_tse")] #- tot vot válidos no Mun   
    dfed[,tvot.uf:= sum(votos),by=c("ano","uf")] #- tot vot valido na UF
  #dominancia no município dos deputados eleitos
    dfed[resultado=="ELEITO", df.dmnde := as.numeric(sum(votos)), 
         by=c("ano","cod_tse")]
    dfed[,df.dmnde := round(df.dmnde/tvot,4)]
    dfed[,df.dmnde := round(mean(df.dmnde,na.rm=TRUE),4),
         by=c("ano","cod_tse")]
  #dominancia no mun dos dep. eleitos do partido do presidente PT e PMDB
    dfed[partido=="PT",df.dmpt := as.numeric(sum(votos)), 
         by=c("ano","cod_tse")]
    dfed[,df.dmpt := round(df.dmpt/tvot,4), by=c("ano","cod_tse")]
    dfed[,df.dmpt := round(mean(df.dmpt,na.rm=TRUE),4), 
         by=c("ano","cod_tse")]
    dfed[is.na(df.dmpt),df.dmpt:=round(0,4)]
    dfed[partido=="PSDB",df.dmpsdb := as.numeric(sum(votos)), 
         by=c("ano","cod_tse")]
    dfed[,df.dmpsdb := round(df.dmpsdb/tvot,4), by=c("ano","cod_tse")]
    dfed[,df.dmpsdb := round(mean(df.dmpsdb,na.rm=TRUE),4), 
         by=c("ano","cod_tse")]
    dfed[is.na(df.dmpsdb),df.dmpsdb:=round(0,4)]
    dfed[partido=="PMDB",df.dmmdb := as.numeric(sum(votos)), 
         by=c("ano","cod_tse")]
    dfed[,df.dmmdb := round(df.dmmdb/tvot,4), by=c("ano","cod_tse")]
    dfed[,df.dmmdb := round(mean(df.dmmdb,na.rm=TRUE),4), 
         by=c("ano","cod_tse")]
    dfed[is.na(df.dmmdb),df.dmmdb:=round(0,4)]
  # Qtde de deputados eleitos com voto no município
    dfed[(resultado=="ELEITO" & votos > 0) , 
         df.nde := as.numeric(nrow(as.data.table(unique(candidato)))), 
         by=c("ano","cod_tse")]
    dfed[, df.nde := mean(df.nde,na.rm = TRUE), by=c("ano","cod_tse")]
    #Qtde de candidatos eleitos na UF
    dfed[resultado=="ELEITO" , 
         df.nde.uf := as.numeric(nrow(as.data.table(unique(candidato)))), 
         by=c("ano", "uf")]
    dfed[,df.nde.uf := round(mean(df.nde.uf,na.rm=TRUE),4), by=c("ano","uf")]
  # Qtde de deputados eleitos do PT e MDB com voto no município
    dfed[(resultado=="ELEITO" & partido=="PT" & votos > 0), 
         df.ndept := as.numeric(nrow(as.data.table(unique(candidato)))),
         by=c("ano","cod_tse")]
    dfed[, df.ndept := mean(df.ndept,na.rm = TRUE), by=c("ano","cod_tse")]
    dfed[is.na(df.ndept),df.ndept:=round(0,4)]
    dfed[(resultado=="ELEITO" & partido=="PSDB" & votos > 0), 
         df.ndepsdb := as.numeric(nrow(as.data.table(unique(candidato)))),
         by=c("ano","cod_tse")]
    dfed[, df.ndepsdb := mean(df.ndepsdb,na.rm = TRUE), 
         by=c("ano","cod_tse")]
    dfed[is.na(df.ndepsdb),df.ndepsdb:=round(0,4)]
    dfed[(resultado=="ELEITO" & partido=="PMDB" & votos > 0), 
         df.ndemdb := as.numeric(nrow(as.data.table(unique(candidato)))),
         by=c("ano","cod_tse")]
    dfed[, df.ndemdb := mean(df.ndemdb,na.rm = TRUE), by=c("ano","cod_tse")]
    dfed[is.na(df.ndemdb),df.ndemdb:=round(0,4)]
  # 
  #--- Calculo do gini da competição de votos efetivos
  #install.packages("ineq")
    library(ineq)
    dfed[(resultado=="ELEITO" & votos > 0),
         df.ginicve:=round(ineq(votos),4),by=c("ano","cod_tse")]
    dfed[, df.ginicve := round(mean(df.ginicve,na.rm=TRUE),4),
         by=c("ano","cod_tse")]
    dfed[,df.ginicvep := round(df.ginicve*(df.nde/df.nde.uf),4),
         by=c("ano","cod_tse")]
  # selecionando as variáveis e removendo dados duplicados
    dfed <- dfed[resultado=="ELEITO",
                 c("ano","cod_tse","df.dmnde","df.dmpt","df.dmpsdb",
                   "df.dmmdb","df.nde","df.nde.uf","df.ndept","df.ndepsdb",
                   "df.ndemdb","df.ginicve","df.ginicvep")]
    setkey(dfed); dfed <- unique(dfed)
  # ajustando Deputados Federais para Dados em Painel 
  #(2006 -> 2009, 2010; 
  #2010 -> 2011, 2012, 2013, 2014; 2014 -> 2015, 2016, 2017)
    dfed[,ano:=as.character(ano)]
  #
    dfed[ano=="2014", ano:="2017"]
    p <- dfed[ano=="2017",]; p <- p[ano=="2017", ano:="2016"]
    dfed <- rbind(dfed,p)
    p <- dfed[ano=="2017",]; p <- p[ano=="2017", ano:="2015"]
    dfed <- rbind(dfed,p)
  #
    dfed[ano=="2010", ano:="2014"]
    p <- dfed[ano=="2014",]; p <- p[ano=="2014", ano:="2013"]
    dfed <- rbind(dfed,p)
    p <- dfed[ano=="2014",]; p <- p[ano=="2014", ano:="2012"]
    dfed <- rbind(dfed,p)
    p <- dfed[ano=="2014",]; p <- p[ano=="2014", ano:="2011"]
    dfed <- rbind(dfed,p)
  #
    dfed[ano=="2006", ano:="2010"]
    p <- dfed[ano=="2010",]; p <- p[ano=="2010", ano:="2009"]
    dfed <- rbind(dfed,p)
    rm(p)
  # mesclar cod_tse para obter codigo ibge
    dfed[,cod_tse := as.character(cod_tse)]
    dfed <- merge(dfed,codigos[,c(10,9,8,2)], all.x = TRUE, by="cod_tse")
    dfed <- dfed[,c(2,16,14,15,1,3,4,5,6,7,9,10,11,13)]
    saveRDS(dfed, "./dados/tse/dominancia_dfed.rds")
 #    
 # Incorporar as variáveis no Banco de Dados do Capítulo 3
  bd <- readRDS("./dados/cap3.RDS")
  pres <- pres[,c("ano", "cod_ibge","ps.dm","ps.mv")]
  names(pres)[2] <- "ibge"
  bd <- merge(bd, pres, all.x= TRUE, by=c("ano","ibge"))
  #
  gov <- gov[,c("ano", "cod_ibge","gv.dm","gv.dmuf","gv.mv")]
  names(gov)[2] <- "ibge"
  bd <- merge(bd, gov, all.x= TRUE, by=c("ano","ibge"))
  #
  pref <- pref[,c("ano", "cod_ibge","pf.dm","pf.mv")]
  names(pref)[2] <- "ibge"
  bd <- merge(bd, pref, all.x= TRUE, by=c("ano","ibge"))
  #
  dfed <- dfed[,c("ano", "cod_ibge","df.dmnde","df.dmpt","df.dmpsdb",
                  "df.dmmdb","df.nde","df.ndept","df.ndepsdb",
                  "df.ndemdb","df.ginicvep")]
  names(dfed)[2] <- "ibge"
  bd <- merge(bd, dfed, all.x= TRUE, by=c("ano","ibge"))
  # 
    saveRDS(bd, "./dados/cap3.rds")
}
  