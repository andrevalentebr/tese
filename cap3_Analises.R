#------------------------------------------------------------------------------#
# CAP3 - EFEITO DO ALINHAMENTO POLÍTICO SOBRE A ALOCAÇÃO DAS TVF AOS MUNICÍPIOS
#        [ SICONV - Transferência voluntárias 2009 à 2017 ]
#                                                   Andre Valente - Dez/2018
#            ANÁLISE DOS DADOS DO CAPÍTULO 3 - MODELOS EM PAINEL
#------------------------------------------------------------------------------#
# 
setwd("C:/Users/Andre/Google Drive/TESE_R")
x <- c("data.table", "plm", "xlsx","plyr","lmtest")
#lapply(x, install.packages, character.only = TRUE) 
lapply(x, library, character.only = TRUE) 
rm(x)
#
cap3 <- readRDS("./dados/cap3.RDS")
#
#round(2.718281828^0.0555,2)
# Todas as variáveis do BD3
#
totC <- function(){## --- CONVENIADO ----
  bd <- cap3
  bd[,tvf := log((tvftot)/pop),]; bd[tvf == -Inf, tvf := NA]
  bd[,pbf := log(pbf*12/pop)]; bd[pbf == -Inf, pbf := NA]
  bd[,pib := log(pib/pop)]; bd[pib == -Inf, pib := NA]
  bd[,p.presgov := (p.pres*p.gov)]; bd[pib == -Inf, pib := NA]
  bd <- bd[,
           c("ibge", "ano","tvf","ifgf","pib","pbf","pop",
             "sit.emer", "cal.pub","ps.anoel","pf.anoel",
             "p.pres", "p.gov", "p.presgov", "p.coal",
             "ps.dm","pf.dm","pf.mv","gv.dm","gv.dmuf",
             "df.nde","df.ndept","df.ndepsdb","df.ndemdb",
             "df.dmnde","df.dmpt","df.dmpsdb","df.dmmdb", 
             "df.ginicvep"
           )]
  bd <- bd[complete.cases(bd) & ano %in% seq(2009,2017,1),]
  # Variáveis Dependentes e Independentes
  vd <- bd[,c(tvf)]
  vi <- as.matrix(bd[,!c("ibge", "ano","tvf")]) 
  #
    bd.p <- pdata.frame(bd, index=c("ibge","ano"))
    totc <<- plm(vd ~ vi, data=bd.p, model = "fd")
    coeftest(totc, vcovHC)
}
totD <- function(){## --- DESEMBOLSADO ----
  bd <- cap3
  bd[,tvf := log((tvflib)/pop),]; bd[tvf == -Inf, tvf := NA]
  bd[,pbf := log(pbf*12/pop)]; bd[pbf == -Inf, pbf := NA]
  bd[,pib := log(pib/pop)]; bd[pib == -Inf, pib := NA]
  bd[,p.presgov := (p.pres*p.gov)]; bd[pib == -Inf, pib := NA]
  bd <- bd[,
           c("ibge", "ano","tvf","ifgf","pib","pbf","pop",
             "sit.emer", "cal.pub","ps.anoel","pf.anoel",
             "p.pres", "p.gov", "p.presgov", "p.coal",
             "ps.dm","pf.dm","pf.mv","gv.dm","gv.dmuf",
             "df.nde","df.ndept","df.ndepsdb","df.ndemdb",
             "df.dmnde","df.dmpt","df.dmpsdb","df.dmmdb", 
             "df.ginicvep" )]
  bd <- bd[complete.cases(bd) & ano %in% seq(2009,2017,1),]
  # Variáveis Dependentes e Independentes
  vd <- bd[,c(tvf)]
  vi <- as.matrix(bd[,!c("ibge", "ano","tvf")]) 
  #
  bd.p <- pdata.frame(bd, index=c("ibge","ano"))
  totd <<- plm(vd ~ vi, data=bd.p, model = "fd")
  coeftest(totd, vcovHC)
}
munC <- function(){## --- MUNICIPIO CONVENIO ----
  bd <- cap3
  bd[,tvf := log((tvfmun)/pop),]; bd[tvf == -Inf, tvf := NA]
  bd[,pbf := log(pbf*12/pop)]; bd[pbf == -Inf, pbf := NA]
  bd[,pib := log(pib/pop)]; bd[pib == -Inf, pib := NA]
  bd[,p.presgov := (p.pres*p.gov)]; bd[pib == -Inf, pib := NA]
  bd <- bd[,
           c("ibge", "ano","tvf","ifgf","pib","pbf","pop",
             "sit.emer", "cal.pub","ps.anoel","pf.anoel",
             "p.pres", "p.gov", "p.presgov", "p.coal",
             "ps.dm","pf.dm","pf.mv","gv.dm","gv.dmuf",
             "df.nde","df.ndept","df.ndepsdb","df.ndemdb",
             "df.dmnde","df.dmpt","df.dmpsdb","df.dmmdb", 
             "df.ginicvep" )]
  bd <- bd[complete.cases(bd) & ano %in% seq(2009,2017,1),]
  # Variáveis Dependentes e Independentes
  vd <- bd[,c(tvf)]
  vi <- as.matrix(bd[,!c("ibge", "ano","tvf")]) 
  #
  bd.p <- pdata.frame(bd, index=c("ibge","ano"))
  munc <<- plm(vd ~ vi, data=bd.p, model = "fd")
  coeftest(munc, vcovHC)
}
munD <- function(){## --- MUNICIPIO DESEMBOLSADO ----
  bd <- cap3
  bd[,tvf := log((tvfmunl)/pop),]; bd[tvf == -Inf, tvf := NA]
  bd[,pbf := log(pbf*12/pop)]; bd[pbf == -Inf, pbf := NA]
  bd[,pib := log(pib/pop)]; bd[pib == -Inf, pib := NA]
  bd[,p.presgov := (p.pres*p.gov)]; bd[pib == -Inf, pib := NA]
  bd <- bd[,
           c("ibge", "ano","tvf","ifgf","pib","pbf","pop",
             "sit.emer", "cal.pub","ps.anoel","pf.anoel",
             "p.pres", "p.gov", "p.presgov", "p.coal",
             "ps.dm","pf.dm","pf.mv","gv.dm","gv.dmuf",
             "df.nde","df.ndept","df.ndepsdb","df.ndemdb",
             "df.dmnde","df.dmpt","df.dmpsdb","df.dmmdb", 
             "df.ginicvep" )]
  bd <- bd[complete.cases(bd) & ano %in% seq(2009,2017,1),]
  # Variáveis Dependentes e Independentes
  vd <- bd[,c(tvf)]
  vi <- as.matrix(bd[,!c("ibge", "ano","tvf")]) 
  #
  bd.p <- pdata.frame(bd, index=c("ibge","ano"))
  mund <<- plm(vd ~ vi, data=bd.p, model = "fd")
  coeftest(mund, vcovHC)
}
oscC <- function(){## --- OSC CONVENIO ----
  bd <- cap3
  bd[,tvf := log((tvfosc)/pop),]; bd[tvf == -Inf, tvf := NA]
  bd[,pbf := log(pbf*12/pop)]; bd[pbf == -Inf, pbf := NA]
  bd[,pib := log(pib/pop)]; bd[pib == -Inf, pib := NA]
  bd[,p.presgov := (p.pres*p.gov)]; bd[pib == -Inf, pib := NA]
  bd <- bd[,
           c("ibge", "ano","tvf","ifgf","pib","pbf","pop",
             "sit.emer", "cal.pub","ps.anoel","pf.anoel",
             "p.pres", "p.gov", "p.presgov", "p.coal",
             "ps.dm","pf.dm","pf.mv","gv.dm","gv.dmuf",
             "df.nde","df.ndept","df.ndepsdb","df.ndemdb",
             "df.dmnde","df.dmpt","df.dmpsdb","df.dmmdb", 
             "df.ginicvep" )]
  bd <- bd[complete.cases(bd) & ano %in% seq(2009,2017,1),]
  # Variáveis Dependentes e Independentes
  vd <- bd[,c(tvf)]
  vi <- as.matrix(bd[,!c("ibge", "ano","tvf")]) 
  #
  bd.p <- pdata.frame(bd, index=c("ibge","ano"))
  oscc <<- plm(vd ~ vi, data=bd.p, model = "fd")
  coeftest(oscc, vcovHC)
}
oscD <- function(){## --- OSC DESEMBOLSO ----
  bd <- cap3
  bd[,tvf := log((tvfoscl)/pop),]; bd[tvf == -Inf, tvf := NA]
  bd[,pbf := log(pbf*12/pop)]; bd[pbf == -Inf, pbf := NA]
  bd[,pib := log(pib/pop)]; bd[pib == -Inf, pib := NA]
  bd[,p.presgov := (p.pres*p.gov)]; bd[pib == -Inf, pib := NA]
  bd <- bd[,
           c("ibge", "ano","tvf","ifgf","pib","pbf","pop",
             "sit.emer", "cal.pub","ps.anoel","pf.anoel",
             "p.pres", "p.gov", "p.presgov", "p.coal",
             "ps.dm","pf.dm","pf.mv","gv.dm","gv.dmuf",
             "df.nde","df.ndept","df.ndepsdb","df.ndemdb",
             "df.dmnde","df.dmpt","df.dmpsdb","df.dmmdb", 
             "df.ginicvep" )]
  bd <- bd[complete.cases(bd) & ano %in% seq(2009,2017,1),]
  # Variáveis Dependentes e Independentes
  vd <- bd[,c(tvf)]
  vi <- as.matrix(bd[,!c("ibge", "ano","tvf")]) 
  #
  bd.p <- pdata.frame(bd, index=c("ibge","ano"))
  oscd <<- plm(vd ~ vi, data=bd.p, model = "fd")
  coeftest(oscd, vcovHC)
}
emC <- function(){## --- EMENDAS CONVENIADAS ----
  bd <- cap3
  bd[,tvf := log((tvfmunem+tvfoscem)/pop),]; bd[tvf == -Inf, tvf := NA]
  bd[,pbf := log(pbf*12/pop)]; bd[pbf == -Inf, pbf := NA]
  bd[,pib := log(pib/pop)]; bd[pib == -Inf, pib := NA]
  bd[,p.presgov := (p.pres*p.gov)]; bd[pib == -Inf, pib := NA]
  bd <- bd[,
           c("ibge", "ano","tvf","ifgf","pib","pbf","pop",
             "sit.emer", "cal.pub","ps.anoel","pf.anoel",
             "p.pres", "p.gov", "p.presgov", "p.coal",
             "ps.dm","pf.dm","pf.mv","gv.dm","gv.dmuf",
             "df.nde","df.ndept","df.ndepsdb","df.ndemdb",
             "df.dmnde","df.dmpt","df.dmpsdb","df.dmmdb", 
             "df.ginicvep" )]
  bd <- bd[complete.cases(bd) & ano %in% seq(2009,2017,1),]
  # Variáveis Dependentes e Independentes
  vd <- bd[,c(tvf)]
  vi <- as.matrix(bd[,!c("ibge", "ano","tvf")]) 
  #
  bd.p <- pdata.frame(bd, index=c("ibge","ano"))
  emc <<- plm(vd ~ vi, data=bd.p, model = "fd")
  coeftest(emc, vcovHC)
}
emD <- function(){## --- EMENDAS CONVENIADAS ----
  bd <- cap3
  bd[,tvf := log((tvfmuneml+tvfosceml)/pop),]; bd[tvf == -Inf, tvf := NA]
  bd[,pbf := log(pbf*12/pop)]; bd[pbf == -Inf, pbf := NA]
  bd[,pib := log(pib/pop)]; bd[pib == -Inf, pib := NA]
  bd[,p.presgov := (p.pres*p.gov)]; bd[pib == -Inf, pib := NA]
  bd <- bd[,
           c("ibge", "ano","tvf","ifgf","pib","pbf","pop",
             "sit.emer", "cal.pub","ps.anoel","pf.anoel",
             "p.pres", "p.gov", "p.presgov", "p.coal",
             "ps.dm","pf.dm","pf.mv","gv.dm","gv.dmuf",
             "df.nde","df.ndept","df.ndepsdb","df.ndemdb",
             "df.dmnde","df.dmpt","df.dmpsdb","df.dmmdb", 
             "df.ginicvep" )]
  bd <- bd[complete.cases(bd) & ano %in% seq(2009,2017,1),]
  # Variáveis Dependentes e Independentes
  vd <- bd[,c(tvf)]
  vi <- as.matrix(bd[,!c("ibge", "ano","tvf")]) 
  #
  bd.p <- pdata.frame(bd, index=c("ibge","ano"))
  emd <<- plm(vd ~ vi, data=bd.p, model = "fd")
  coeftest(emd, vcovHC)
}
