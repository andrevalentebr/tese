#-----------------------------------------------------------------------------#
#        [ SICONV - Transferência Voluntárias Federais 2009 à 2017 ]          #
#       cap2.R - Script para Produção do Banco de Dados do Capítulo 2         #
#                                                  André Valente - Jan/2018   #
#-----------------------------------------------------------------------------#
# 
setwd("C:/Users/Andre/Google Drive/TESE_R")
#install.packages("data.table")
library("data.table")
#
#
#
Convenios()  # grava conv.RDS
Desembolso() #grava conv.RDS e des.RDS deflacionados e atualiza bd.RDS
Propostas()  # grava prop.RDS e atualiza bd.RDS
Emendas()    # grava em.RDS e atualiza bd.RDS
Populacao()  # grava pop.RDS
Pib()        # grava pib.RDS e associa pop.RDS e pib.RDS em pip_pop.RDS
BD_cap2()    # Finaliza os BD's cap2.RDS e cap2_emendas.RDS para as análises
#
#---- Tabela CONVENIOS ----
#
Convenios <- function() {
  conv <- setDT(read.csv2(
        "C:/Users/Andre/Google Drive/TESE_R/dados/siconv/siconv_convenio.csv",
        sep=";",header=TRUE, encoding="UTF-8", na.strings = "NA",
        quote='"', fill=TRUE, stringsAsFactors = FALSE))
  conv <- conv[,c("NR_CONVENIO","ID_PROPOSTA","MES","ANO",
        "VL_GLOBAL_CONV","VL_REPASSE_CONV","VL_CONTRAPARTIDA_CONV")]
  names(conv) <- c("convenio","proposta","mes","ano",
        "vlglobal","vlrepasse","vlcontrapart")
# Tipar e recodificar as variáveis 
  conv[,convenio:= as.character(convenio)]
  conv[,proposta:= as.character(proposta)]
  conv[,mes:= as.character(mes)]
  conv[,ano:= as.character(ano)]
  conv[,vlglobal:=as.numeric(gsub(",",".",vlglobal))]
  conv[,vlrepasse:=as.numeric(gsub(",",".",vlrepasse))]
  conv[,vlcontrapart:=as.numeric(gsub(",",".",vlcontrapart))]
# Limpar Base de Dados
  conv <- conv[!is.na(ano),] # remover convenios cancelados
  saveRDS(conv, "./dados/siconv/conv.rds")
}
#
#-- Tabela DESEMBOLSOS ----
#
# Verificar tabela desembolso para realizar a deflação no campo DESEMBOLSO
Desembolso <- function() {
  des <- setDT(read.csv2(
    "C:/Users/Andre/Google Drive/TESE_R/dados/siconv/siconv_desembolso.csv", 
    sep=";",header=TRUE, encoding="UTF-8", na.strings = "NA",
    quote='"', fill=TRUE, stringsAsFactors = FALSE))
  # removendo registros de desembolsos duplicados
    #SAO JOSE DE MIPIBU - RN 2412203
    des <- des[!(NR_CONVENIO=="740215" & MES_DESEMBOLSO=="9"),] 
    #CAVALCANTE - RN 5205307
    des <- des[!(NR_CONVENIO=="721217" & NR_SIAFI=="2010OB800120"),] 
    #ASSOCIACAO RURAL DE LENCOIS PAULISTA - SP 3526803
    des <- des[!(NR_CONVENIO=="731927" & MES_DESEMBOLSO=="3"),]
    #ASSOCIACAO DOS ARTISTAS DE PEDRO LEOPOLDO E REGIAO COOPERART - MG 3149309
    des <- des[!(NR_CONVENIO=="704758" & NR_SIAFI=="2010OB800010"),] 
  #
    des <- des[,c("NR_CONVENIO","ANO_DESEMBOLSO",
                  "MES_DESEMBOLSO","VL_DESEMBOLSADO")]
    names(des) <- c("convenio","anodes","mesdes","vldes")
  # Tipar e recodificar as variáveis 
    des[,convenio := as.character(convenio)]
    des[,anodes := as.character(anodes)]
    des[,mesdes := as.character(mesdes)]
    des[,vldes:=as.numeric(gsub(",",".",vldes))]
# Corrigir anomalia nos dados
  des[anodes=="1900", anodes := "2014"]
#
## DEFLACIONAR e JUNTAR Convenios e Desembolsos 
conv <- readRDS("./dados/siconv/conv.rds")
ipca    <- c(1.7998468,1.7901798,1.7814507,1.7729406,1.7632428,1.7494223,
             1.7365717,1.7274164,1.7225931,1.7181260,1.7104291,1.7042936, #2008
             1.6995349,1.6914161,1.6821642,1.6788066,1.6707868,1.6629709,
             1.6570056,1.6530383,1.6505625,1.6466106,1.6420130,1.6353082, #2009
             1.6292799,1.6171513,1.6046351,1.5963342,1.5872866,1.5804905,
             1.5804905,1.5803325,1.5797006,1.5726238,1.5609169,1.5480680, #2010
             1.5383762,1.5257128,1.5136039,1.5017402,1.4902652,1.4832937,
             1.4810721,1.4787061,1.4732551,1.4654880,1.4592134,1.4516647, #2011
             1.4444425,1.4363987,1.4299639,1.4269672,1.4178927,1.4128066,
             1.4116773,1.4056330,1.3998935,1.3919593,1.3837949,1.3755417, #2012
             1.3647601,1.3531232,1.3450529,1.3387607,1.3314378,1.3265296,
             1.3230896,1.3226928,1.3195259,1.3149237,1.3074711,1.3004487, #2013
             1.2885936,1.2815451,1.2727631,1.2611604,1.2527669,1.2470305,
             1.2420623,1.2419381,1.2388410,1.2318196,1.2266676,1.2204433, #2014
             1.2109976,1.1961651,1.1817478,1.1663519,1.1581292,1.1496220,
             1.1406112,1.1335830,1.1310946,1.1250195,1.1158693,1.1047118, #2015
             1.0942074,1.0804852,1.0708476,1.0662626,1.0597979,1.0515954,
             1.0479277,1.0425066,1.0379397,1.0371100,1.0344205,1.0325619, #2016
             1.0294735,1.0255763,1.0222030,1.0196539,1.0182284,1.0150816,
             1.0174217,1.0149857,1.0130609,1.0114426,1.0072123,1.0044000) #2017

  anos <- rep(2008:2017); mes  <- seq(1:12); idx <- 1
  for(a in anos) {
    for (m in mes){
      # atualiza as TVF contratadas com o gov. federal 
        conv[ano==a & mes==m, vlglobal     := vlglobal*ipca[idx]]
        conv[ano==a & mes==m, vlrepasse    := vlrepasse*ipca[idx]]
        conv[ano==a & mes==m, vlcontrapart := vlcontrapart*ipca[idx]]
      # atualiza as TVF efetivamente liberadas aos municípios
        des[anodes==a & mesdes==m, vldes := vldes*ipca[idx]]
      idx <- idx +1
    }
  }
  #rm(ipca,anos,idx,a,m,mes)
  # Somar os desembolsos deflacionados por Convenio e ano
    des <- des[,!c("mesdes")]
    des <- des[,vldes := sum(vldes),by=.(convenio,anodes)]
    setkey(des); des <- unique(des)
    des <- des[complete.cases(des),] # somente convenio com desembolso
    saveRDS(des, "./dados/siconv/des.rds")
  # Associar os valores desembolsados com os convenios no Banco de Dados
    des <- des[,!c("anodes")]
    des[,vldes := sum(vldes),by=.(convenio)]
    setkey(des); des <- unique(des)
    setkey(conv,convenio); setkey(des,convenio)
    bd <- merge(conv,des, all.x = TRUE, by=c("convenio"))
    bd <- unique(bd)
    bd[is.na(vldes),vldes :=0]
    saveRDS(bd, "./dados/siconv/bd.rds")
}
# 
#
#-- Tabela PROPOSTA ----
#
Propostas <- function() {
  prop <- setDT(read.csv2(
    "C:/Users/Andre/Google Drive/TESE_R/dados/siconv/siconv_proposta.csv", 
    sep=";",header=TRUE, encoding="UTF-8", na.strings = "NA",
    quote='"', fill=TRUE, stringsAsFactors = FALSE))
  # removendo as propostas enviadas em 2018 que estão fora da análise
    prop <- prop[,c("ID_PROPOSTA","UF_PROPONENTE","MUNIC_PROPONENTE",
                    "COD_MUNIC_IBGE","DESC_ORGAO_SUP","NATUREZA_JURIDICA",
                    "MODALIDADE")]
    names(prop) <- c("proposta","uf","municipio","ibge",
                     "orgao","entidade","modalidade")
    prop <- prop[,c(1,4,2,3,5,6,7)] #ordenar campos
  # tipagem das variáveis
    prop[,proposta:= as.character(proposta)]
    prop[,ibge:= as.character(ibge)]
    prop[,uf:= as.character(uf)]
    prop[,orgao:= as.character(orgao)]
    saveRDS(prop, "./dados/siconv/prop.rds")
  # Associar os dados adicionais da proposta no Banco de Dados
    bd <- readRDS("./dados/siconv/bd.RDS")
    setkey(bd,proposta); setkey(prop,proposta)
    bd <- merge(bd,prop, all.x = TRUE, by=c("proposta"))
  # Arrumar Codigo do IBGE AUSENTES para os Municípios do Banco de Dados
    setorder(bd,municipio)
    aux <- bd[is.na(ibge),unique(sort(municipio))]
    cdibge <- c("5201108", "2901957", "4220000", "4203303", "2516409", 
                "4304507", "4204301", "5205901", "4306908", "4205407", 
                "5210406", "4113205", "4113908", "4312401", "2206720",
                "5215702", "4314548", "5217104", "4314605", "4316402",
                "2513653", "4216206", "4319802", "4126306", "4320701",
                "4320800", "4218707", "4128203")
    for (i in aux) {
      bd[municipio==i,ibge:=cdibge[which(aux==i)]]
    };rm(aux,cdibge,i)
  # Abreviar a descrição dos Órgaos Superiores
    aux   <- sort(unique(prop$orgao))
    abrev <- c("CNJ", "IFMG", "JE", "MMIRDH", "MDIC", "MTPAC", "MAPA",
               "MCTIC", "MCULT", "MDEF", "MEC", "MFAZ", "MINAC", "MJ",
               "MPA", "MS", "CGU", "MCID", "MCOM", "MRE", "MME", "MDA",
               "MDS", "MESP", "MMA", "MPOG", "MTE", "MTPS", "MTUR",
               "MDH", "PRES", "SEAFDA")
    #View(cbind(abrev,aux))
    for (i in aux) {
      bd[orgao==i,orgao:=abrev[which(aux==i)]]
    }; rm(aux,abrev,i)
  # Abreviar a Natureza Jurídica do Convenente 
    # adm. pub mun -> MUN; adm. pub Est/DF -> EST; cons.publico -> CPUB;
    # emp pub/soc. eco mista -> EPSEM; org soc civil -> OSC
    aux <- sort(unique(bd$entidade))
    abrev <- c("EST","MUN","CPUB","EPSEM","OSC")
    for (i in aux) {
      bd[entidade==i,entidade:=abrev[which(aux==i)]]
    }; rm(aux,abrev,i)
  # Abreviar a Modalidade do Convênio
    # contr. repasse -> CR; convenio -> CO; term colaboraçao -> TC; 
    # term. fomento -> TF; term parceria -> TP 
    aux <- sort(unique(bd$modalidade))
    abrev <- c("CR","CO","TC","TF","TP")
    for (i in aux) {
      bd[modalidade==i,modalidade:=abrev[which(aux==i)]]
    };rm(aux,abrev,i)
  # GRAVAR a base de dados 
    saveRDS(bd, "./dados/siconv/bd.rds")
}
# 
#
#-- Tabela EMENDAS ----
#
Emendas <- function() {
  em <- setDT(read.csv2(
    "C:/Users/Andre/Google Drive/TESE_R/dados/siconv/siconv_emenda.csv", 
    sep=";",header=TRUE, encoding="UTF-8", na.strings = "NA",
    quote='"', fill=TRUE, stringsAsFactors = FALSE))
  # selecionando os campos  
    em <- em[,c("ID_PROPOSTA","NR_EMENDA",
                "IND_IMPOSITIVO","TIPO_PARLAMENTAR","VALOR_REPASSE_EMENDA")]
    names(em) <- c("proposta","emenda","impos","tipo","valor")
    em[,proposta := as.character(proposta)]
  # Recodificando as variáveis
    em[, impos := ifelse(impos == "SIM", 1, 0)]
    em[, tipo := ifelse(tipo == "INDIVIDUAL", "I", 
                 ifelse(tipo == "COMISSAO"  , "C",
                 ifelse(tipo == "BANCADA"   , "B", "R")))]
  # calculo do valor atualizado das emendas
    em[,v1 := sum(valor), by=.(proposta,impos,tipo)]
    em[,tot := sum(valor), by=.(proposta)]
    em[,v2 := v1/tot]
    # recuperar o banco para associar dados
      bd <- readRDS("./dados/siconv/bd.RDS")
      setkey(em,proposta); setkey(bd, proposta)
      em <- merge(em, bd[,c("proposta", "convenio", "ano","vlrepasse",
                            "ibge", "municipio", "uf",
                            "orgao", "entidade", "modalidade")],
                  by=c("proposta"), all.x = TRUE)
      # head(em[!(v1==tot),],10)
  # manipular as Emendas por convenios
    em <- em[!is.na(convenio),]
    em[,vlemenda := round(vlrepasse*v2,2)]
    em <- em[,c("convenio", "ano", "impos","tipo", "vlemenda","vlrepasse",
                "ibge", "municipio", "uf", "orgao", "entidade", "modalidade")]
    setkey(em); em <- unique(em)
    saveRDS(em, "./dados/siconv/em.rds")
  # Identificar convenios por emendas parlament. no Banco de Dados 
    bd[, emenda:= ifelse(convenio %in% unique(em$convenio),1,0)]
    saveRDS(bd,"./dados/siconv/bd.rds")
  #  
}
#
#
#-- Finaliza o Banco de Dados do capítulo 2 [cap2.RDS] -----
#
BD_cap2 <- function() {
  #
  cap2 <- readRDS("./dados/siconv/bd.RDS")
  # reordena os campos
  cap2 <- cap2[,c("convenio", "ano", "mes", "ibge", "municipio", "uf",
                  "orgao", "entidade", "modalidade", "vlglobal",
                  "vlrepasse", "vlcontrapart", "vldes", "emenda")]
  cap2[municipio=="SANTAREM" & uf=="PA", ibge:= "1506807"]
  cap2[municipio=="CAMPO ALEGRE" & uf=="AL", ibge:= "2701407"]
  cap2[municipio=="SOBRADINHO" & uf=="BA", ibge:= "2930774"]
  cap2[municipio=="SOLEDADE" & uf=="PB", ibge:= "2516102"]
  setkey(cap2,ano,convenio)
  saveRDS(cap2, "./dados/cap2.RDS")
  #
  em <- readRDS("./dados/siconv/em.rds")
  em <- em[,c("ano",  "uf", "ibge", "municipio", "convenio", "tipo", "impos",
              "vlemenda","vlrepasse", "orgao", "entidade", "modalidade")]
  setkey(em,convenio)
  setorder(em, uf,ibge,ano,-vlrepasse)
  saveRDS(em, "./dados/cap2_emendas.RDS")
  #
  des <- readRDS("./dados/siconv/des.rds")
  setkey(des)
  setorder(des, anodes, -convenio)
  saveRDS(des, "./dados/cap2_desembolso.RDS")
}

#
#
  #-- Tabela IBGE POPULAÇÃO -----
Populacao <- function(){
  # população do senso 2010
    p10 <- setDT(read.table("./dados/IBGE/pop/AtlasBrasil_Indicadores.csv",
                 sep=";",header=TRUE, fileEncoding="UTF-8", na.strings = "NA",
                 quote='"', fill=TRUE, stringsAsFactors = FALSE))
    p10[,ano:="2010"]
    p10 <- p10[,c(1,13,2,4)]
    names(p10) <- c("ibge", "ano", "municipio", "pop")
    p10[, ibge := as.character(ibge)]
  # população estimada de 2009
    p09 <- setDT(read.table("./dados/IBGE/pop/pop2009.csv",
                 sep=";",header=TRUE, fileEncoding="UTF-8", na.strings = "NA",
                 quote='"', fill=TRUE, stringsAsFactors = FALSE))
    # Tipar e recodificar a população de 2009
      p09[,ano:="2009"]
      p09[,coduf := as.character(coduf)]
      p09[,ibge := as.character(ibge)]
      p09[,pop := as.integer(pop)] 
      p09[,i := ifelse(nchar(ibge)==2,"000",ifelse(nchar(ibge)==3,"00",       
                ifelse(nchar(ibge)==4,"0","")))] 
      p09[,ibge := paste0(coduf,i,ibge)]
      p09 <- p09[,c("ibge","ano","municipio","pop")]
  # população estimada de 2011 a 2017
    p11 <- setDT(read.table("./dados/IBGE/pop/pop2011-2017.csv",
                 sep=";",header=TRUE, fileEncoding="UTF-8", na.strings = "NA",
                 quote='"', fill=TRUE, stringsAsFactors = FALSE))
    # Tipar e recodificar a população de 2009
      p11[,coduf := as.character(coduf)]; 
      p11[,ibge := as.character(ibge)]
      p11[,i := ifelse(nchar(ibge)==2,"000",ifelse(nchar(ibge)==3,"00",       
                ifelse(nchar(ibge)==4,"0","")))] 
      p11[,ibge := paste0(coduf,i,ibge)]
      p11 <- p11[,c("ibge","ano","municipio","pop")]
  # Associar a população nos municípios entre 2009 e 2017
    pop <- rbind(p09,p11)
    pop <- rbind(pop,p10)
    setkey(pop, ibge, ano)
    saveRDS(pop, "./dados/IBGE/pop/pop.RDS")
#rm(p09,p10,p11,pop)
}
#
#
#-- Tabela IBGE - PIB (Produto Interno Bruto) -----
#
Pib <- function() {
  pib <- setDT(read.table("./dados/IBGE/pib/pibm2002-2015.csv",
               sep=";",header=TRUE, fileEncoding="UTF-8", na.strings = "NA",
               quote='"', fill=TRUE, stringsAsFactors = FALSE))
  # Tipar, recodificar e selecionar os anos
    pib[, ano := as.character(ano)]
    pib[, ibge := as.character(ibge)] 
    pib[, pib := as.numeric(gsub(",",".",pib))]
    pib[, pib := pib*1000]; 
    pib <- pib[ano %in% rep(2009:2017),]
    setkey(pib, ibge, ano)
  # inserindo pib para os anos de 2016 e 2017
    # reshape nos dados (anos como variáveis)  
      pib <- dcast(pib, ibge ~ ano, value.var = "pib") 
    # variação do pib brasileiro em 2016 de -3.60%
      pib[,`2016`:= `2015`*(1-0.036)] 
    # variação do pib brasileiro em 2017 de 1%
      pib[,`2017`:= `2016`*(1+0.01)]
    # reshape nos dados para o formato em painel  
      pib <- melt(pib, measure.vars = c(as.character(rep(2009:2017))),
                  variable.name = "ano", value.name = "pib")
  # os valores do PIB vieram deflacionados pelo IBGE a partir de 2015
  # a deflação terá como referência 2015 ajustanto pelo IPCA para 2017
    pib[,pib := pib*1.1047118] # fonte do índice: Calculadora do BACEN
    setkey(pib, ibge, ano)
    saveRDS(pib, "./dados/IBGE/pib/pib.RDS")
  # Integrando as Bases do IBGE de população e Pibm  
    pop <- readRDS("./dados/IBGE/pop/pop.RDS")
    pop <- pop[,!c("municipio")]
    poppib <- merge(pib, pop, all.x = TRUE ,by=c("ibge","ano"))
    poppib[,poppib.pc := pib/pop]
    setkey(poppib, ibge, ano)
    saveRDS(poppib, "./dados/IBGE/pop_pib.RDS")
#rm(pib)
}

if (FALSE){
  #verifica se as variáveis do modelo estão repetidas no BD
  # verifica duplicidade nos dados
  BD[, if(.N>1) .SD, .(var1, var2)]
}


