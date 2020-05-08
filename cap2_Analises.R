#------------------------------------------------------------------------------#
# CAP2 - ESTATÍSTICA DESCRITIVA DOS CONVÊNIOS
#        [ SICONV - Transferência voluntárias 2009 à 2017 ]
#                                                   Andre Valente - Jan/2019
#------------------------------------------------------------------------------#
#
setwd("C:/Users/Andre/Google Drive/TESE_R")
x <- c("data.table", "ggplot2", "gridExtra", "rgdal", "rgeos",
       "dplyr", "tidyr", "tmap", "xlsx","plyr")
#lapply(x, install.packages, character.only = TRUE) 
lapply(x, library, character.only = TRUE) 
rm(x)
#
cap2 <- readRDS("./dados/cap2.rds")
cap2.em <- readRDS("./dados/cap2_emendas.rds")
cap2.des <- readRDS("./dados/cap2_desembolso.rds")

#----------- Índice das figuras

#
fig1 <- function(){ # Total de convenios por ano -------
  bd <- cap2[,c("convenio","ano","mes")]
  bd[, convenios:=.(count=.N), by =.(ano)]
  bd <- bd[!ano =="2008",c("ano", "convenios")]
  setkey(bd); bd <- unique(bd)
  # gráfico da quantidade de convenios por ano
  g1 <- ggplot(bd, aes( ano , convenios/1000, group=1)) +
        geom_line(size=1.6, colour="blue4") +
        geom_point(size=2.5, shape= 21, fill="white") + theme_bw() +
          theme(axis.text.x = element_text(face="bold"),
          axis.text.y = element_text(angle = 45, hjust=1, face="bold"),
          legend.title = element_blank() ,legend.position = "bottom") +
          labs(x = "Ano", y = "Quantidade de Convênios (mil)") +
          scale_y_continuous( limits=c(8, 22), breaks = seq(8,22,2))
  g1; ggsave("Cap2_fig1.jpeg",plot = g1, 
             device = "jpeg", path = "./dados/graficos",
             width = 16.77, height = 8.20, limitsize = TRUE, 
             units = c("cm"), dpi = 300)
}
#
#
tab1 <- function(){ #Total de convenios por ano e mes ----
  bd <- cap2[,c("convenio","ano","mes")]
  bd[, convenios:=.(count=.N), by =.(ano,mes)]
  bd <- bd[!ano %in% c("2008","2018"),c("ano", "mes", "convenios")]
  setkey(bd); bd <- unique(bd)
  bd <- dcast(bd, mes ~ ano, value.var = "convenios")
  bd$mes <- as.integer(bd$mes); setorder(bd,mes)
  # tabela
  write.xlsx(bd, "./dados/graficos/Cap2_tabelas.xlsx",
              sheet="tab1",append=TRUE)
}
#
# 
fig2 <- function(){  # Valor Global por ano ----
  bd <- cap2[,c("convenio","ano","vlglobal")]
  bd[, global:=sum(vlglobal/1000000000), by =.(ano)]
  bd <- bd[!ano %in% c("2008"),c("ano", "global")]
  setkey(bd); bd <- unique(bd)
  # grafico dos valores Globais conveniados por ano (bilhoes)
  g2 <- ggplot(bd, aes( ano , global, group=1)) +
        geom_line(size=1.6, colour="blue4") +
        geom_point(size=2.5, shape= 21, fill="white") + theme_bw() +
        theme(axis.text.x = element_text(face="bold"),
              axis.text.y = element_text(angle = 45, hjust=1, face="bold"),
              legend.title = element_blank() ,legend.position = "bottom") +
              labs(x = "Ano", y = "Valor Global Conveniado (bi)*") +
              scale_y_continuous( limits=c(6, 26), breaks = seq(6,26,2))  
  g2; ggsave("Cap2_fig2.jpeg",plot = g2, 
             device = "jpeg", path = "./dados/graficos",
             width = 16.77, height = 8.20, limitsize = TRUE, 
             units = c("cm"), dpi = 300)
}
#
# 
fig3 <- function(){  # Médio dos Vl Globais conveniados por ano (bi) ----
  bd <- cap2[,c("convenio","ano","vlglobal")]
  bd[, global:=sum(vlglobal), by =.(ano)]
  bd <- bd[!ano %in% c("2008"),c("ano", "global")]
  setkey(bd); bd <- unique(bd)
  #
  bd1 <- cap2[,c("convenio","ano","mes")]
  bd1[, convenios:=.(count=.N), by =.(ano)]
  bd1 <- bd1[!ano =="2008",c("ano", "convenios")]
  setkey(bd1); bd1 <- unique(bd1)
  bd <- cbind(bd,bd1[,c(2)])
  bd[,media := round(global/convenios/1000,2)]
  # grafico 
  g3 <- ggplot(bd, aes( ano , media, group=1)) +
        geom_line(size=1.6, colour="blue4") +
        geom_point(size=2.5, shape= 21, fill="white") + theme_bw() +
        theme(axis.text.x = element_text(face="bold"),
          axis.text.y = element_text(angle = 45, hjust=1, face="bold"),
          legend.title = element_blank() ,legend.position = "bottom") +
          labs(x = "Ano", y = "Valor médio dos convênios (mil)*") +
          scale_y_continuous( limits=c(600, 1810), breaks = seq(600,1801,150))
  g3; ggsave("Cap2_fig3.jpeg",plot = g3, 
             device = "jpeg", path = "./dados/graficos",
             width = 16.77, height = 8.20, limitsize = TRUE, 
             units = c("cm"), dpi = 300)
}  
#
#
fig4 <- function(){   # Vl (Repasse) e Liberados por ano (bi) ----
  bd <- cap2[,c("convenio","ano","vlrepasse")]
  bd[, Repasse:=sum(vlrepasse), by =.(ano)]
  bd <- bd[!ano %in% c("2008","2018"),c("ano", "Repasse")]
  setkey(bd); bd <- unique(bd)
  setorder(bd, ano)
  #
  bd1 <- cap2.des
  bd1[, Liberado:= sum(vldes), by =.(anodes)]
  bd1 <- bd1[!anodes %in% c("2008","2018", "2019"),c("anodes", "Liberado")]
  names(bd1)[1] <- "ano";
  setkey(bd1); bd1 <- unique(bd1)
  setorder(bd1, ano)
  #
  bd <- cbind(bd,bd1[,c(2)])
  bd[,Repasse := round(Repasse/1000000000,1)]
  bd[,Liberado := round(Liberado/1000000000,1)]
  bd <- melt(bd, measure.vars = c("Repasse","Liberado"),
             variable.name = "Valores", value.name = "tvf.bi")
  bd$Valores <-factor(bd$Valores, levels = c("Repasse", "Liberado"))
  setorder(bd,ano,-Valores)
  # grafico 
  g4 <- ggplot(bd, aes(ano, tvf.bi, fill=Valores)) +
    geom_bar(stat="identity", position=position_dodge())+
    geom_text(aes(label= tvf.bi), vjust=1.6, color=c("gray99"),
              position = position_dodge(0.9), size=2.7) +
    theme_bw() + scale_fill_brewer(palette="Paired") +
    theme(legend.position = c(0.9, 0.8),
      axis.text.x = element_text(face="bold", size=9.5),
      axis.text.y = element_text(angle = 45, hjust=1, face="bold",size=9.5)) +
      labs(x = "Ano", y = "Valor dos Convenios (R$ bi)*") +
      scale_y_continuous( limits=c(0, 23), breaks = seq(0,23,2),
                          expand = c(0.01,0))
  g4; ggsave("Cap2_fig4.jpeg",plot = g4, 
             device = "jpeg", path = "./dados/graficos",
             width = 16.77, height = 8.20, limitsize = TRUE, 
             units = c("cm"), dpi = 300)

}
#
# 
fig5 <- function(){ # NatJuridica Repasse e Liberados por   ----
  bd <- cap2[,c("convenio","ano", "vlrepasse", "entidade")]
  bd[!ano %in% c("2008","2018"), Repasse:=sum(vlrepasse), by =.(entidade)]
  bd <- bd[complete.cases(bd),c("entidade", "Repasse")]
  setkey(bd); bd <- unique(bd)
  setorder(bd, entidade)
  #
  bd1 <- cap2.des
  bd1 <- merge(bd1,cap2[,c("convenio","entidade")],by=c("convenio"))
  bd1[!anodes %in% c("2008","2018", "2019"), 
      Liberado:= sum(vldes), by =.(entidade)]
  bd1 <- bd1[complete.cases(bd1),c("entidade", "Liberado")]
  setkey(bd1); bd1 <- unique(bd1)
  setorder(bd1, entidade)
  #
  bd <- cbind(bd,bd1[,c(2)])
  bd <- melt(bd, measure.vars = c("Repasse","Liberado"),
             variable.name = "Valores", value.name = "tvf.bi")
  bd[,tvf.bi := round(tvf.bi/1000000000,1)]
  bd$Valores <-factor(bd$Valores, levels = c("Liberado","Repasse"))
  bd$entidade <-factor(bd$entidade, 
                        levels = c("CPUB","EPSEM", "OSC", "EST","MUN"))
  setorder(bd,entidade)
  # gráfico
  g5 <- ggplot(bd, aes( entidade, tvf.bi, fill=Valores)) +
          geom_bar(stat="identity", position=position_dodge())+ coord_flip() +
          geom_text(aes(label=tvf.bi), hjust=-0.2, color="blue4",
              position = position_dodge(0.9), size=3) +
          theme_bw() + scale_fill_brewer(palette="Paired") + 
          theme(legend.position = c(0.9, 0.2),
          axis.text.x = element_text( size=9.5),
          axis.text.y = element_text(angle = 45, hjust=1,
                                     face="bold",size=9.5)) +
          labs(x = "Entidades Convenentes",
               y = "Valor dos Convenios (R$ bi)*") +
          scale_y_continuous( limits=c(0, 60.2), 
                              breaks = seq(0,60,4),expand = c(0.01,0))
  g5; ggsave("Cap2_fig5.jpeg",plot = g5, 
              device = "jpeg", path = "./dados/graficos",
              width = 16.77, height = 8.20, limitsize = TRUE, 
              units = c("cm"), dpi = 300)
}
#
# 
tab2 <- function(){  # Entidades: Qtde, Repasses e Liberações  ----
  bd <- cap2[,c("convenio","ano","vlrepasse","entidade")]
  bd[, Convenios:=as.numeric(.(count=.N)), by =.(entidade,ano)]
  bd <- bd[ano %in% seq(2009,2017,1),c("entidade", "ano", "Convenios")]
  setkey(bd); bd <- unique(bd)
  #
  bd1 <- cap2[,c("convenio","ano", "vlrepasse", "entidade")]
  bd1[!ano %in% c("2008","2018"), 
      Repasse:=sum(vlrepasse), by =.(entidade,ano)]
  bd1 <- bd1[complete.cases(bd1),c("entidade", "ano", "Repasse")]
  setkey(bd1); bd1 <- unique(bd1)
  #
  bd2 <- cap2.des
  bd2 <- merge(bd2,cap2[,c("convenio","entidade")],by=c("convenio"))
  bd2[!anodes %in% c("2008","2018", "2019"), 
      Liberado:= sum(vldes), by =.(entidade,anodes)]
  bd2 <- bd2[complete.cases(bd2),c("entidade", "anodes", "Liberado")]
  names(bd2)[2] <- "ano"
  setkey(bd1); bd2 <- unique(bd2)
  #
  bd <- merge(bd, bd1, all.x=TRUE, by=c("entidade", "ano"))
  bd <- merge(bd, bd2, all.x=TRUE, by=c("entidade", "ano"))
  bd[,Repasse := round(Repasse/1000000,2)]
  bd[,Liberado := round(Liberado/1000000,2)]
  bd <- melt(bd, measure.vars = c("Convenios","Repasse","Liberado"),
             variable.name = "Variaveis", value.name = "tvf.mi")
  bd <- dcast(bd, entidade + Variaveis ~ ano, value.var = "tvf.mi")
  bd$entidade <-factor(bd$entidade, 
                       levels = c("MUN","EST","OSC","EPSEM","CPUB"))
  bd <- ddply(bd, c('entidade', 'Variaveis'))
  # tabela
    write.xlsx(bd, "./dados/graficos/Cap2_tabelas.xlsx",
               sheet="tab2",append=TRUE)
}
#
# 
tab3 <- function(){ # Modalidade: Convênios por Natureza Jurídica ----
  bd <- cap2[,c("convenio","ano","entidade","modalidade")]
  bd <- bd[ano %in% seq(2009,2017,1),c("entidade", "modalidade")]  
  bd[, convenios:=.(count=.N), by =.(entidade,modalidade)]
  setkey(bd); bd <- unique(bd)
  bd <- dcast(bd, entidade  ~ modalidade, value.var = "convenios")
  bd$entidade <-factor(bd$entidade, 
                       levels = c("MUN","OSC","EST","EPSEM","CPUB"))
  bd <- ddply(bd, c('entidade'))
  # tabela
    write.xlsx(bd, "./dados/graficos/Cap2_tabelas.xlsx",
               sheet="tab3",append=TRUE)
}
#
# 
tab4 <- function(){  # Valores de Contrapartida: Nat. Juridica ----
  bd <- cap2[,c("convenio","ano","entidade",
                "vlglobal","vlrepasse","vlcontrapart")]
  bd <- bd[ano %in% seq(2009,2017,1),c(3,4,5,6)]  
  bd[, global:=round(sum(vlglobal)/1000000000,2), by =.(entidade)]
  bd[, repasse:=round(sum(vlrepasse)/1000000000,2), by =.(entidade)]
  bd[, contrap:=round(sum(vlcontrapart)/1000000000,2), by =.(entidade)]
  bd <- unique(bd[,c("entidade", "global", "repasse", "contrap")])
  bd[,contrap.pc := round(contrap/global,4)]
  setorder(bd, -contrap.pc)
  # tabela
    write.xlsx(bd, "./dados/graficos/Cap2_tabelas.xlsx",
               sheet="tab4",append=TRUE)
}
#
# 
tab5 <- function(){  # Valores de Contrapartida: Modalidade ----
  bd <- cap2[,c("convenio","ano","modalidade",
                "vlglobal","vlrepasse","vlcontrapart")]
  bd <- bd[ano %in% seq(2009,2017,1),c(3,4,5,6)]  
  bd[, global:=round(sum(vlglobal)/1000000000,2), by =.(modalidade)]
  bd[, repasse:=round(sum(vlrepasse)/1000000000,2), by =.(modalidade)]
  bd[, contrap:=round(sum(vlcontrapart)/1000000000,2), by =.(modalidade)]
  bd <- unique(bd[,c("modalidade", "global", "repasse", "contrap")])
  bd[,contrap.pc := round(contrap/global,4)]
  setorder(bd, -contrap.pc)
  # tabela
    write.xlsx(bd, "./dados/graficos/Cap2_tabelas.xlsx", 
               sheet="tab5",append=TRUE)
}
#
# 
fig6 <- function(){  # Ministérios: Qtde Convenios ------
  bd <- cap2[,c("convenio","ano","orgao")]
  bd <- bd[ano %in% seq(2009,2017,1),c("orgao", "convenio")]
  bd[, convenios:=.(count=.N), by =.(orgao)]
  bd <- bd[,c("orgao","convenios")]
  setkey(bd); bd <- unique(bd)
  setorder(bd, convenios)
  bd <- rbind(bd, list("OUTROS",as.integer(bd[convenios<426,sum(convenios)])))
  bd <- bd[!convenios<426,]
  setorder(bd,convenios)
  bd$orgao <-factor(bd$orgao, 
        levels = c("OUTROS","PRES", "MCTIC", "MDH", "MEC", "MCULT", "MJ", 
                   "MDEF", "SEAFDA", "MDA", "MDS", "MINAC", "MESP",
                   "MTUR", "MAPA", "MS", "MCID"))
  bd <- ddply(bd, c("orgao"))
  write.xlsx(bd, "./dados/graficos/Cap2_tabelas.xlsx", 
             sheet="fig6_tab",append=TRUE)
  # gráfico
    (g6 <- ggplot(bd, aes( orgao, convenios, fill=orgao)) +
            geom_bar(stat="identity", position=position_dodge(), 
                   fill = "steelblue") + coord_flip() +
            geom_text(aes(label= as.character(round(convenios/1000,1))),
                      hjust=1.2, color="white", fontface='bold', size=3,
                      position = position_dodge(0.9)) +  
            theme_bw() +
            theme(legend.position = c(0.9, 0.2),
              axis.text.x = element_text(size=9),
              axis.text.y = element_text(angle = 45, hjust=1, size=9.5)) +
            labs(x = "Órgãos Concedentes *", y = "Quantidade de convênios") +
            scale_y_continuous( limits=c(0, 25400), expand = c(0.005,0), 
                                breaks = seq(0,26000,2500)))
     ggsave("Cap2_fig6.jpeg",plot = g6, 
             device = "jpeg", path = "./dados/graficos",
             width = 16, height = 13, limitsize = TRUE, 
             units = c("cm"), dpi = 300)
}
#
# 
tab5.1 <- function(){  # Ministério: tabelas de analises ----
  # quantidade de convenios
    bd <- cap2[,c("convenio","ano","orgao","entidade")]
    bd <- bd[ano %in% seq(2009,2017,1),c(3,4)]
    bd[, convenios:=as.numeric(.(count=.N)), by =.(orgao,entidade)]
    bd <- unique(bd[,c("orgao", "entidade", "convenios")])
    bd <- melt(bd, measure.vars = c("convenios"),
               variable.name = "valor", value.name = "tvf.mi")
    bd <- dcast(bd, orgao + valor ~ entidade, value.var = "tvf.mi")  
    # tabela
      write.xlsx(bd, "./dados/graficos/Cap2_tabelas.xlsx", 
                 sheet="tab5.1_qteconv")
  # Ministérios: convenios por modalidade
    bd <- cap2[,c("convenio","ano","orgao","modalidade")]
    bd <- bd[ano %in% seq(2009,2017,1),c(3,4)]
    bd[, convenios:=as.numeric(.(count=.N)), by =.(orgao,modalidade)]
    bd <- unique(bd[,c("orgao", "modalidade", "convenios")])
    bd <- melt(bd, measure.vars = c("convenios"),
               variable.name = "valor", value.name = "tvf.mi")
    bd <- dcast(bd, orgao + valor ~ modalidade, value.var = "tvf.mi")  
    # tabela
      write.xlsx(bd, "./dados/graficos/Cap2_tabelas.xlsx", 
                 sheet="tab5.1_mod",append=TRUE)
  # Ministérios: valores repasse por entidade
    bd <- cap2[,c("convenio","ano","orgao","entidade", "vlrepasse")]
    bd <- bd[ano %in% seq(2009,2017,1),c(3,4,5)]
    bd[, Repasse:=sum(vlrepasse), by =.(orgao, entidade)]
    bd <- unique(bd[,c("orgao", "entidade", "Repasse")])
    bd <- dcast(bd, orgao ~ entidade, value.var = "Repasse") 
    # tabela
    write.xlsx(bd, "./dados/graficos/Cap2_tabelas.xlsx", 
               sheet="tab5.1_vlrepent",append=TRUE)
  # Ministérios: valores liberado por entidade
    bd <- cap2.des
    bd <- merge(bd,cap2[,c("convenio","orgao","entidade")],by=c("convenio"))
    bd[anodes %in% seq(2009, 2017,1), 
        Liberado:= sum(vldes), by =.(orgao, entidade)]
    bd <- bd[complete.cases(bd),c("orgao", "entidade", "Liberado")]
    setkey(bd); bd <- unique(bd)
    bd <- dcast(bd, orgao ~ entidade, value.var = "Liberado") 
    # tabela
      write.xlsx(bd, "./dados/graficos/Cap2_tabelas.xlsx", 
               sheet="tab5.1_vllibent",append=TRUE)
  # Ministérios: valores repasse e liberado
    bd <- cap2[,c("convenio","ano","orgao","vlrepasse")]
    bd <- bd[ano %in% seq(2009,2017,1),c(3,4)]
    bd[, Repasse:=sum(vlrepasse), by =.(orgao)]
    bd <- unique(bd[,c("orgao", "Repasse")])
    #
    bd1 <- cap2.des
    bd1 <- merge(bd1,cap2[,c("convenio","orgao")],by=c("convenio"))
    bd1[anodes %in% seq(2009, 2017,1), 
        Liberado:= sum(vldes), by =.(orgao)]
    bd1 <- bd1[complete.cases(bd1),c("orgao", "Liberado")]
    setkey(bd1); bd1 <- unique(bd1)
    #
    bd <- merge(bd, bd1, all.x=TRUE, by=c("orgao"))
    setorder(bd, -Repasse)
    # tabela
      write.xlsx(bd, "./dados/graficos/Cap2_tabelas.xlsx", 
                 sheet="tab5.1_vlreplib",append=TRUE)
  # Ministérios: valores de contrapartida
    bd <- cap2[,c("convenio","ano","orgao",
                  "vlglobal","vlrepasse","vlcontrapart")]
    bd <- bd[ano %in% seq(2009,2017,1),c(3,4,5,6)]  
    bd[, global:=sum(vlglobal), by =.(orgao)]
    bd[, repasse:=sum(vlrepasse), by =.(orgao)]
    bd[, contrap:=sum(vlcontrapart), by =.(orgao)]
    bd <- unique(bd[,c("orgao", "global", "repasse", "contrap")])      
    bd[,contrap.pc := round(contrap/global,4)]
    setorder(bd, -contrap.pc)
    # tabela
      write.xlsx(bd, "./dados/graficos/Cap2_tabelas.xlsx", 
                 sheet="tab5.1_vlcpart",append=TRUE)
}
#
#
# 
fig7 <- function(){ # Ministérios: Vl Conveniados e Liberados ----
  bd <- cap2[,c("convenio","ano", "vlrepasse", "orgao")]
  bd[ano %in% seq(2009, 2017,1), 
      Repasse:=sum(vlrepasse), by =.(orgao)]
  bd <- bd[complete.cases(bd),c("orgao", "Repasse")]
  setkey(bd); bd <- unique(bd)
  #
  bd1 <- cap2.des
  bd1 <- merge(bd1,cap2[,c("convenio","orgao")],by=c("convenio"))
  bd1[anodes %in% seq(2009, 2017,1), 
      Liberado:= sum(vldes), by =.(orgao)]
  bd1 <- bd1[complete.cases(bd1),c("orgao", "Liberado")]
  setkey(bd1); bd1 <- unique(bd1)
  #
  bd <- merge(bd, bd1, all.x=TRUE, by=c("orgao"))
  setorder(bd, -Repasse)
  # write.xlsx(bd,"./dados/graficos/Cap2_tabelas.xlsx",sheet="fig7_tab") 
  bd <- bd[,.SD[1:10],]
  bd$orgao <-factor(bd$orgao, levels = bd$orgao)
  bd <- melt(bd, measure.vars = c("Repasse","Liberado"),
             variable.name = "Valores", value.name = "tvf.bi")
  bd[,tvf.bi := round(tvf.bi/1000000000,1)]
  bd <- ddply(bd, c('orgao'))
  # grafico
    (g7 <- ggplot(bd, aes(orgao, tvf.bi, fill=Valores)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label= tvf.bi), vjust=1.6, color=c("gray99"),
                  position = position_dodge(0.9), size=2.7) +
        theme_bw() + scale_fill_brewer(palette="Paired") +
        theme(legend.position = c(0.9, 0.8),
              axis.text.x = element_text(face="bold", size=9.5),
              axis.text.y = element_text(angle = 45, 
                                         hjust=1, face="bold",size=9.5)) +
        labs(x = "Ministérios*", y = "Valor dos Convenios (R$ bi)**") +
        scale_y_continuous( limits=c(0, 24), breaks = seq(0,24,2),
                            expand = c(0.01,0)))
    ggsave("Cap2_fig7.jpeg",plot = g7, 
           device = "jpeg", path = "./dados/graficos",
           width = 16.77, height = 8.20, limitsize = TRUE, 
         units = c("cm"), dpi = 300)
}
#  
# 
fig8 <- function(){  #  Ministérios: Valor de contrapartida ----
  bd <- cap2[,c("convenio","ano","orgao",
                "vlglobal","vlrepasse","vlcontrapart")]
  bd <- bd[ano %in% seq(2009,2017,1),c(3,4,5,6)]  
  bd[, global:=sum(vlglobal), by =.(orgao)]
  bd[, repasse:=sum(vlrepasse), by =.(orgao)]
  bd[, contrap:=sum(vlcontrapart), by =.(orgao)]
  bd <- unique(bd[,c("orgao", "global", "repasse", "contrap")])      
  bd[,contrap.pc := round(contrap/global*100,1)]
  setorder(bd, -contrap.pc)
  #
  bd1 <- cap2.des
  bd1 <- merge(bd1,cap2[,c("convenio","orgao")],by=c("convenio"))
  bd1[anodes %in% seq(2009, 2017,1), 
      Liberado:= sum(vldes), by =.(orgao)]
  bd1 <- bd1[complete.cases(bd1),c("orgao", "Liberado")]
  setkey(bd1); bd1 <- unique(bd1)
  #
  bd <- merge(bd, bd1, all.x=TRUE, by=c("orgao"))
  setorder(bd, -repasse)
  # write.xlsx(bd,"./dados/graficos/Cap2_tabelas.xlsx",sheet="fig9_tab") 
  bd <- bd[,.SD[1:10],]
  bd$orgao <-factor(bd$orgao, levels = bd$orgao)
  bd <- ddply(bd, c('orgao'))
  # grafico
    (g8.1 <- ggplot(bd, aes(orgao, contrap.pc, fill="orgao")) +
              geom_bar(stat="identity", position=position_dodge())+
              geom_text(aes(label= contrap.pc), 
                        vjust=1.6, color=c("gray99"),
                        position = position_dodge(0.9), size=2.9) +
              theme_bw() + scale_fill_manual(values="steelblue") +
              theme(legend.position = "none",
                    axis.text.x = element_text(size=9),
                    axis.text.y = element_text(angle = 45, 
                                      hjust=1, size=9)) +
              labs(x = "Ministérios", y = "% de contrapartida") +
              scale_y_continuous( limits=c(0, 17), breaks = seq(0,17,4),
                                  expand = c(0.01,0)))
    (g8.2 <- ggplot(bd, aes( y =contrap.pc)) + 
              geom_boxplot(fill="steelblue") + 
              stat_boxplot(geom ='errorbar', width = 0.3) +
              theme_bw() + xlab("")+ylab(NULL) + 
              theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
                    axis.text.y=element_blank(),axis.ticks.y=element_blank(),
                    plot.margin = unit(c(0.2,0.2,0.61,0), "cm")))
    (g8 <- grid.arrange(g8.1, g8.2, ncol = 2,widths = c(6, 1), clip = FALSE))
    ggsave("Cap2_fig8.jpeg",plot = g8, 
            device = "jpeg", path = "./dados/graficos",
            width = 16, height = 7.94, units = c("cm"),
            dpi = 300, limitsize = TRUE)
}
#  
# 
tab6 <- function(){ # Emendas: vl de repasse e Qtde por Orgao x Ano ----
  # % de emendas do total de convenios
    bd <- readRDS("./dados/Cap2.rds") 
    bd <- bd[ano %in% seq(2009,2017,1),]
    table(bd$emenda)
  # qtde de emendas por tipo e impositiva
    bd <- readRDS("./dados/Cap2_emendas.rds") 
    bd <- bd[ano %in% seq(2009,2017,1),]
    bd <- bd[,qtde := .(count=.N), by=.(tipo,impos)]
    bd <- bd[,c("tipo","impos","qtde")]
    bd <- unique(bd)
    # tabela - tab7_01
    write.xlsx(bd, "./dados/graficos/Cap2_tabelas.xlsx", 
               sheet="tab6.1",append=TRUE)
  # qtde de emendas: orgao x ano
    bd <- cap2.em[,c("convenio","ano","vlrepasse","orgao")]
    bd[, Convenios:=as.numeric(.(count=.N)), by =.(orgao,ano)]
    bd[,Repasse:=round(sum(vlrepasse)/1000000,2), by =.(orgao,ano)]
    bd <- bd[ano %in% seq(2009,2017,1),c("orgao", "ano", "Convenios", "Repasse")]
    setkey(bd); bd <- unique(bd)
    bd <- setDT(bd)[ CJ(orgao = orgao, ano = ano, unique=TRUE),
                   on=.(orgao, ano)]
    #
    bd1 <- cap2.des
    bd1 <- merge(bd1,cap2.em[,c("convenio","orgao")],by=c("convenio"))
    bd1[anodes %in% seq(2009,2017,1), 
        Liberado:= round(sum(vldes)/1000000,2), by =.(orgao,anodes)]
    bd1 <- bd1[complete.cases(bd1),c("orgao", "anodes", "Liberado")]
    names(bd1)[2] <- "ano"
    setkey(bd1); bd1 <- unique(bd1)
    bd1 <- setDT(bd1)[ CJ(orgao = orgao, ano = ano, unique=TRUE),
                     on=.(orgao, ano)]
    #
    bd <- merge(bd, bd1, all.x=TRUE, by=c("orgao", "ano"))
    orgaos <- c("MCID","MS","MAPA","MTUR","MINAC","MESP","MDEF")
    bd <- bd[orgao %in% orgaos,]
    bd <- melt(bd, measure.vars = c("Convenios","Repasse","Liberado"),
               variable.name = "Variaveis", value.name = "tvf.mi")
    bd <- dcast(bd, orgao + Variaveis ~ ano, value.var = "tvf.mi")
    bd$orgao <-factor(bd$orgao, levels = orgaos)
    bd <- ddply(bd, c('orgao', 'Variaveis'))
    # tabela
      write.xlsx(bd, "./dados/graficos/Cap2_tabelas.xlsx", 
               sheet="tab6",append=TRUE)
}
# 
# ESTADOS ----- 
#
fig9.10_tab7 <- function(){ # Os Estados: Valores e Mapas 
  bd <- cap2[entidade=="EST" & ano %in% seq(2009,2017,1),
             c("uf","orgao","vlrepasse","vlcontrapart")]
  bd[, Convenios:=as.numeric(.(count=.N)), by =.(uf)]
  bd[,Repasse:=sum(vlrepasse), by =.(uf)]
  bd[,Contrap:=sum(vlcontrapart), by =.(uf)]
  bd[, conv.orgao:=as.numeric(.(count=.N)), by =.(uf,orgao)]
  bd[,rep.orgao:=sum(vlrepasse), by =.(uf,orgao)]
  bd[,ctp.orgao:=sum(vlcontrapart), by =.(uf,orgao)]
  bd <- bd[,c("uf","orgao","Convenios","Repasse","Contrap",
              "conv.orgao","rep.orgao","ctp.orgao")]
  setkey(bd); bd <- unique(bd)
  #
  bd1 <- cap2.des
  bd1 <- merge(bd1,cap2[,c("convenio","orgao", "uf", "entidade")],
                 by=c("convenio"))
  bd1 <- bd1[entidade=="EST" & anodes %in% seq(2009,2017,1), c(-2, -6)]
  bd1[,Liberado:=sum(vldes), by =.(uf)]
  bd1[,lib.orgao:=sum(vldes), by =.(uf,orgao)]
  bd1 <- bd1[complete.cases(bd1),c("uf","orgao","Liberado","lib.orgao")]
  setkey(bd1); bd1 <- unique(bd1)
  bd <- merge(bd, bd1, all.x=TRUE, by=c("uf","orgao"))
  # tabela das UF's: convenios repasse, contrap e liberado
    bd1 <- bd[complete.cases(bd),c(1,3,4,5,9)]
    setkey(bd1); bd1 <- unique(bd1)
    #tab7  
      write.xlsx(bd1, "./dados/graficos/Cap2_tabelas.xlsx", 
                 sheet="tab7",append=TRUE)
  # MAPA ESTADOS: Valor repassado e liberado per capita
    # populacao da UF
      pop <- readRDS("./dados/IBGE/pop_pib.RDS")
      pop <- pop[ano=="2017",]
      pop[,CD_GEOCUF := substr(ibge,1,2)]
      pop[, pop.uf := sum(pop), by=.(CD_GEOCUF)]
      pop <- pop[,c(6,7)]; setkey(pop); pop <- unique(pop)   
      # dados ibge dos municipios
        cduf <- readRDS("./dados/codigos.rds")
        cduf<- cduf[,c(2,3)]; setkey(cduf); cduf <- unique(cduf)
        names(cduf) <- c("uf", "CD_GEOCUF")
    # juntar as bases
      bd1 <- merge(bd1, cduf, all.x=TRUE, by=c("uf"))
      bd1 <- merge(bd1, pop, all.x=TRUE, by=c("CD_GEOCUF"))
      bd1[, Repasse.pc := round(Repasse/pop.uf,2)]
      bd1[, Liberado.pc := round(Liberado/pop.uf,2)]
  # MAPA
    library(RColorBrewer)
    library(tmap) # load tmap package
    # quantidade de contratos
      uf <- readOGR("./dados/IBGE/mapas/uf/BRUFE250GC_SIR.shp") 
      dados <- setDT(attr(uf, "data"))
      dados[,index := as.numeric(row.names(dados))]
      dados <- merge(dados, bd1, by=c("CD_GEOCUF"))
      dados[,lib.reg:= sum(Liberado)/sum(pop.uf), by=c("NM_REGIAO")]
      dados[,conv.reg := sum(Convenios), by=c("NM_REGIAO")]
      setorder(dados,index)
      uf@data <- dados
    # Valor liberado por regiao e por estado
      tmap_mode<-"plot"
      tm1 <- qtm(uf, fill ="lib.reg",fill.palette = "Blues",
                 fill.title="Valor Liberado (pcap)", fill.style="fixed", 
                 fill.breaks=c(30, 50, 100,130,140,150,160))
        tmap_save(tm1, "./dados/graficos/Cap2_fig9_EST01.png", 
                   width=1800, height=1720, asp=0)
      tm2 <- qtm(uf, fill ="Liberado.pc",fill.palette = "Blues", 
                 fill.title="Valor Liberado (pcap)", fill.style="fixed", 
                 fill.breaks=c(0,50,100,200,300,400,650))
        tmap_save(tm2, "./dados/graficos/Cap2_fig9_EST02.png", 
                  width=1800, height=1720, asp=0)
}      
# CONSÓRCIOS PÚBLICOS ------
# 
fig11 <- function() { # As CPUB's : total e percentual de convenios 
  bd <- cap2[ano %in% seq(2009,2017,1),]
  x <- bd[,ent := .(count=.N), by=c("entidade")]
  bd <- bd[, c("entidade","ent")]; setkey(bd); bd <- unique(bd)
  bd[, tot:= sum(ent)]; bd[,perc:= round(ent/tot*100,2)]; print(bd)
}    
#
fig11.1 <- function() { # As CPUB's : Qtde, Valores e Mapas 
  bd <- cap2[entidade=="CPUB" & ano %in% seq(2009,2017,1),
             c("ibge", "municipio", "uf","orgao",
               "vlrepasse","vlcontrapart","vldes")]
  # Análise por UF
  bd1 <- bd[,c("uf", "vlrepasse", "vlcontrapart","vldes")]
  bd1[, conv   := as.numeric( .(count=.N)), by= c("uf")]
  bd1[, rep    := sum( vlrepasse ), by= c("uf")]  
  bd1[, lib    := sum( vldes ),  by= c("uf")]  
  bd1[, ctp    := sum( vlcontrapart ),  by= c("uf")]  
  bd1 <- bd1[,c(1,5,6,7,8)]; setkey(bd1); bd1 <- unique(bd1)
  #tabela  
  write.xlsx(bd1, "./dados/graficos/Cap2_tabelas.xlsx", 
             sheet="fig11.uf",append=TRUE)
  # gráfico
  bd1[, Repasse  := round(rep/sum(rep)*100,1)]
  bd1[, Liberado  := round(lib/sum(lib)*100,1)]
  bd1 <- bd1[,c("uf","Repasse","Liberado")]
  setorder(bd1, -Repasse)
  bd1 <- melt(bd1, measure.vars = c("Repasse","Liberado"),
              variable.name = "Valores", value.name = "perc") 
  bd1$uf <-factor(bd1$uf, levels = unique(bd1$uf))
  bd1 <- ddply(bd1, c("uf", "Valores"))
  bd1 <- as.data.table(bd1)
  bd1 <- bd1[,.SD[1:22]]
  g1 <- ggplot(bd1, aes(uf, perc, fill=Valores)) +
    geom_bar(stat="identity", position=position_dodge())+
    geom_text(aes(label= perc), vjust=-0.6, color=c("steelblue4"),
              position = position_dodge(0.9), size=2.0) +
    theme_bw() + scale_fill_brewer(palette="Paired") +
    theme(legend.position = c(0.9, 0.8),
          axis.text.x = element_text(face="bold", size=9.5),
          axis.text.y = element_text(angle = 45, 
                                     hjust=1, face="bold",size=9.5)) +
    labs(x = "Estados", y = "% do total de recursos") +
    scale_y_continuous( limits=c(0, 41.5), breaks = seq(0,40,4),
                        expand = c(0.01,0))
  ggsave("Cap2_fig11.jpeg",plot = g1, 
         device = "jpeg", path = "./dados/graficos",
         width = 16.77, height = 8.20, limitsize = TRUE, 
         units = c("cm"), dpi = 300) 
  # Consórcio Público - por ORGAO  
  bd1 <- bd[,c("orgao", "vlrepasse", "vlcontrapart","vldes")]
  bd1[, conv   := as.numeric( .(count=.N)), by= c("orgao")]
  bd1[, rep    := sum( vlrepasse ), by= c("orgao")]  
  bd1[, lib    := sum( vldes ),  by= c("orgao")]  
  bd1[, ctp    := sum( vlcontrapart ),  by= c("orgao")]  
  bd1 <- bd1[,c(1,5,6,7,8)]; setkey(bd1); bd1 <- unique(bd1)
  #tabela  
  write.xlsx(bd1, "./dados/graficos/Cap2_tabelas.xlsx", 
             sheet="fig11.orgao",append=TRUE)
  # Consórcio Público - UF x ORGAO 
  bd1 <- bd[,c("uf", "orgao", "vlrepasse", "vlcontrapart","vldes")]
  bd1[, conv   := as.numeric( .(count=.N)), by= c("uf", "orgao")]
  bd1[, rep    := sum( vlrepasse ), by= c("uf", "orgao")]  
  bd1[, lib    := sum( vldes ),  by= c("uf", "orgao")]  
  bd1[, ctp    := sum( vlcontrapart ),  by= c("uf", "orgao")]  
  bd1 <- bd1[,c(1,2,6,7,8,9)]; setkey(bd1); bd1 <- unique(bd1)
  #tabela  
  write.xlsx(bd1, "./dados/graficos/Cap2_tabelas.xlsx", 
             sheet="fig11.uforgao",append=TRUE)
  # Consórcio Público ORGAO x Municipios
  bd1 <- bd[,c("ibge", "municipio", "uf", "orgao", "vlrepasse", 
               "vlcontrapart","vldes")]
  bd1[, conv   := as.numeric( .(count=.N)), by= c("ibge", "orgao")]
  bd1[, rep    := sum( vlrepasse ), by= c("ibge", "orgao")]  
  bd1[, lib    := sum( vldes ),  by= c("ibge", "orgao")]  
  bd1[, ctp    := sum( vlcontrapart ),  by= c("ibge", "orgao")]  
  bd1 <- bd1[,!c(5,6,7)]; setkey(bd1); bd1 <- unique(bd1)
  bd1[, convP   := conv/sum(conv, na.rm=TRUE)]
  bd1[, repP    := rep/sum(rep,  na.rm=TRUE)]  
  bd1[, libP    := lib/sum(lib,  na.rm=TRUE)]
  #tabela  
  write.xlsx(bd1, "./dados/graficos/Cap2_tabelas.xlsx", 
             sheet="fig11.munorgao",append=TRUE)
  # Consórcio Público - Municipio per capita  
    bd1 <- bd[,c("ibge","municipio","uf","vlrepasse","vldes")]
    # populacao da UF
      pop <- readRDS("./dados/IBGE/pop_pib.RDS")
      pop <- pop[ano=="2017",]
    bd1 <- merge(bd1, pop[,c("ibge","pop")], all.x=TRUE, by= c("ibge"))
    bd1[, rep    := sum( vlrepasse ), by= c("ibge")]  
    bd1[, lib    := sum( vldes ),  by= c("ibge")]    
    bd1 <- bd1[,c(1,2,3,6,7,8)]; setkey(bd1); bd1 <- unique(bd1)
    bd1[, rep    := round(rep/pop,2)]; bd1[, lib    := round(lib/pop,2)]
    bd1 <- bd1[!84,]
    setorder(bd1,lib); 
    #tabela  
      write.xlsx(bd1, "./dados/graficos/Cap2_tabelas.xlsx", 
                 sheet="fig11.munpcap",append=TRUE)
    bd1$municipio <-factor(bd1$municipio, levels = bd1$municipio)
    setorder(bd1,-lib); 
    bd1 <- bd1[1:15,c("municipio","lib")]
  #
  (g1 <- ggplot(bd1, aes( municipio, lib, fill=municipio)) +
      geom_bar(stat="identity", position=position_dodge(), 
               fill = "steelblue") + coord_flip() +
      geom_text(aes(label= round(lib,0)),
                hjust=1.2, color="white", fontface='bold', size=3,
                position = position_dodge(0.9)) +  
      theme_bw() +
      theme(legend.position = c(0.9, 0.2),
            axis.text.x = element_text(size=9),
            axis.text.y = element_text(angle = 30, hjust=1, size=8.5)) +
      labs(x = "Município sede dos Consórcios Públicos",
           y = "R$ per capita") +
      scale_y_continuous( limits=c(0, 4100), expand = c(0.005,0), 
                          breaks = seq(0,4000,500)))
  ggsave("Cap2_fig12.jpeg",plot = g1, 
         device = "jpeg", path = "./dados/graficos",
         width = 17, height = 13, limitsize = TRUE, 
         units = c("cm"), dpi = 300)
} 
# ORGANIZAÕES DA SOCIEDADE CIVIL -----
# 
item2.6 <- function() { 
    bd <- cap2[ano %in% seq(2009,2017,1),]
    x <- bd[,ent := .(count=.N), by=c("entidade")]
    bd <- bd[, c("entidade","ent")]; setkey(bd); bd <- unique(bd)
    bd[, tot:= sum(ent)]; bd[,perc:= round(ent/tot*100,2)]; print(bd)
}    
#
fig13 <- function() { # OSC: Qtde, Valores e Mapas 
  bd <- cap2[entidade=="OSC" & ano %in% seq(2009,2017,1),
             c("ibge", "municipio", "uf","orgao",
               "vlrepasse","vlcontrapart","vldes")]
  # OSC por UF
    bd1 <- bd[,c("uf", "vlrepasse", "vlcontrapart","vldes")]
    bd1[, conv   := as.numeric( .(count=.N)), by= c("uf")]
    bd1[, rep    := sum( vlrepasse ), by= c("uf")]  
    bd1[, lib    := sum( vldes ),  by= c("uf")]  
    bd1[, ctp    := sum( vlcontrapart ),  by= c("uf")]  
    bd1 <- bd1[,c(1,5,6,7,8)]; setkey(bd1); bd1 <- unique(bd1)
    bd1[, convP   := conv/sum(conv, na.rm=TRUE)]
    bd1[, repP    := rep/sum(rep,  na.rm=TRUE)]  
    bd1[, libP    := lib/sum(lib,  na.rm=TRUE)]
    bd1[, ctpP    := ctp/(ctp+rep)]
    tabela  
      write.xlsx(bd1, "./dados/graficos/Cap2_tabelas.xlsx", 
               sheet="fig13a.uf",append=TRUE)
    # gráfico
      bd1[, Repasse  := round(rep/sum(rep)*100,1)]
      bd1[, Liberado  := round(lib/sum(lib)*100,1)]
      bd1 <- bd1[,c("uf","Repasse","Liberado")]
      setorder(bd1, -Repasse)
      bd1 <- rbind(bd1, list("UF's",
                   as.numeric(bd1[Repasse<1,sum(Repasse)]),
                   as.numeric(bd1[Repasse<1,sum(Liberado)])))
      bd1 <- bd1[!Repasse < 1,]
      bd1 <- melt(bd1, measure.vars = c("Repasse","Liberado"),
                  variable.name = "Valores", value.name = "perc") 
      bd1$uf <-factor(bd1$uf, levels = unique(bd1$uf))
      g1 <- ggplot(bd1, aes(uf, perc, fill=Valores)) +
          geom_bar(stat="identity", position=position_dodge())+
          geom_text(aes(label= perc), vjust=-0.6, color=c("steelblue4"),
                    position = position_dodge(0.9), size=2.4) +
          theme_bw() + scale_fill_brewer(palette="Paired") +
          theme(legend.position = c(0.9, 0.8),
                axis.text.x = element_text(face="bold", size=9.5),
                axis.text.y = element_text(angle = 45, 
                                         hjust=1, face="bold",size=9.5)) +
          labs(x = "Estados", y = "% do total de recursos") +
          scale_y_continuous( limits=c(0, 18.5), breaks = seq(0,18,2),
                            expand = c(0.01,0))
      ggsave("Cap2_fig13.jpeg",plot = g1, 
             device = "jpeg", path = "./dados/graficos",
             width = 16.77, height = 8.20, limitsize = TRUE, 
             units = c("cm"), dpi = 300) 
  # OSC - por ORGAO  
    bd1 <- bd[,c("orgao", "vlrepasse", "vlcontrapart","vldes")]
    bd1[, conv   := as.numeric( .(count=.N)), by= c("orgao")]
    bd1[, rep    := sum( vlrepasse ), by= c("orgao")]  
    bd1[, lib    := sum( vldes ),  by= c("orgao")]  
    bd1[, ctp    := sum( vlcontrapart ),  by= c("orgao")]  
    bd1 <- bd1[,!c(2,3,4)]; setkey(bd1); bd1 <- unique(bd1)
    bd1[, convP   := conv/sum(conv, na.rm=TRUE)]
    bd1[, repP    := rep/sum(rep,  na.rm=TRUE)]  
    bd1[, libP    := lib/sum(lib,  na.rm=TRUE)]
    bd1[, ctpP    := ctp/(ctp+rep)]
    #tabela  
      write.xlsx(bd1, "./dados/graficos/Cap2_tabelas.xlsx", 
               sheet="fig13.orgao",append=TRUE)
  # OSC - UF x ORGAO 
    bd1 <- bd[,c("uf", "orgao", "vlrepasse", "vlcontrapart","vldes")]
    bd1[, conv   := as.numeric( .(count=.N)), by= c("uf", "orgao")]
    bd1[, rep    := sum( vlrepasse ), by= c("uf", "orgao")]  
    bd1[, lib    := sum( vldes ),  by= c("uf", "orgao")]  
    bd1[, ctp    := sum( vlcontrapart ),  by= c("uf", "orgao")]  
    bd1 <- bd1[,!c(3,4,5)]; setkey(bd1); bd1 <- unique(bd1)
    bd1[, convP   := conv/sum(conv, na.rm=TRUE)]
    bd1[, repP    := rep/sum(rep,  na.rm=TRUE)]  
    bd1[, libP    := lib/sum(lib,  na.rm=TRUE)]
    bd1[, ctpP    := ctp/(ctp+rep)]
    #tabela  
      write.xlsx(bd1, "./dados/graficos/Cap2_tabelas.xlsx", 
               sheet="fig13.uforgao",append=TRUE)
  # OSC - ORGAO x Municipios
    bd1 <- bd[,c("ibge", "municipio", "uf", "orgao", "vlrepasse", 
                 "vlcontrapart","vldes")]
    bd1[, conv   := as.numeric( .(count=.N)), by= c("ibge", "orgao")]
    bd1[, rep    := sum( vlrepasse ), by= c("ibge", "orgao")]  
    bd1[, lib    := sum( vldes ),  by= c("ibge", "orgao")]  
    bd1[, ctp    := sum( vlcontrapart ),  by= c("ibge", "orgao")]  
    bd1 <- bd1[,!c(5,6,7)]; setkey(bd1); bd1 <- unique(bd1)
    bd1[, convP   := conv/sum(conv, na.rm=TRUE)]
    bd1[, repP    := rep/sum(rep,  na.rm=TRUE)]  
    bd1[, libP    := lib/sum(lib,  na.rm=TRUE)]
    #tabela  
      write.xlsx(bd1, "./dados/graficos/Cap2_tabelas.xlsx", 
               sheet="fig13.munorgao",append=TRUE)
  # OSC - Municipio per capita  
    bd1 <- bd[,c("ibge","municipio","uf","vlrepasse","vldes")]
      # populacao da UF
      pop <- readRDS("./dados/IBGE/pop_pib.RDS")
      pop <- pop[ano=="2017",]
    bd1 <- merge(bd1, pop[,c("ibge","pop")], all.x=TRUE, by= c("ibge"))
    bd1[, rep    := sum( vlrepasse ), by= c("ibge")]  
    bd1[, lib    := sum( vldes ),  by= c("ibge")]    
    bd1 <- bd1[,!c(4,5)]; setkey(bd1); bd1 <- unique(bd1)
    bd1[, rep    := round(rep/pop,2)]; bd1[, lib    := round(lib/pop,2)]
    ibge.cap <- readRDS("./dados/IBGE/ibge_capitais.rds")
    bd1[,cap := ifelse(ibge %in% ibge.cap$ibge,1,0)]
    setorder(bd1,-cap, -lib)
    #tabela  
      write.xlsx(bd1, "./dados/graficos/Cap2_tabelas.xlsx", 
                 sheet="fig13.munpcap",append=TRUE)
    setorder(bd1, -cap, lib)
    bd1.1 <- bd1[cap==1,]
    bd1.1$municipio <-factor(bd1.1$municipio, levels = bd1.1$municipio)
    #
    (g1 <- ggplot(bd1.1, aes( municipio, lib, fill=municipio)) +
        geom_bar(stat="identity", position=position_dodge(), 
                 fill = "steelblue") + coord_flip() +
        geom_text(aes(label= round(lib,0)),
                  hjust=-0.1, color="steelblue", fontface='bold', size=3,
                  position = position_dodge(0.9)) +  
        theme_bw() +
        theme(legend.position = c(0.9, 0.2),
              axis.text.x = element_text(size=9),
              axis.text.y = element_text(angle = 30, hjust=1, size=8)) +
        labs(x = "Município das Organizações da Sociedade Civil",
             y = "R$ per capita") +
        scale_y_continuous( limits=c(0, 1680), expand = c(0.005,0), 
                            breaks = seq(0,1600,200)))
    ggsave("Cap2_fig14.jpeg",plot = g1, 
           device = "jpeg", path = "./dados/graficos",
           width = 16, height = 14.3, limitsize = TRUE, 
           units = c("cm"), dpi = 300)
    # MAPA -  tvf per capita das OSC aos municipios
      library(RColorBrewer)
      library(tmap) # load tmap package
      # OSC per capita municipios
        names(bd1)[1] <- "CD_GEOCMU"
        mun <- readOGR("./dados/IBGE/mapas/municipios/BRMUE250GC_SIR.shp") 
        dados <- setDT(attr(mun, "data"))
        dados[,index := as.numeric(row.names(dados))]
        dados <- merge(dados, bd1[, c("CD_GEOCMU","lib")], 
                       all.x= TRUE, by=c("CD_GEOCMU"))
        dados[is.na(lib), lib := 0]
        setorder(dados,index)
        mun@data <- dados
      # Valor liberado por regiao e por estado
        tmap_mode<-"plot"
      tm1 <- qtm(mun, fill ="lib", 
                 #fill.palette = c("#deebf7","#9ecae1","#6baed6","#4292c6","#2171b5","#08519c","#08306b")
                 fill.palette = c("#b2182b","#ef8a62","#fddbc7","#f7f7f7","#d1e5f0","#67a9cf","#2166ac"),
                 borders=NULL,
                 fill.title="Desembolso (pcap)", fill.style="fixed", 
                 fill.breaks=c(0,7,14,28,56,128,256,15050))
      tmap_save(tm1, "./dados/graficos/Cap2_fig14_OSC.png", 
                 width=1800, height=1720, asp=0)
} 
#
# MUNICÍPIOS -----
#
fig14 <- function() { # MUN: Qtde, Valores e Mapas 
  bd <- cap2[entidade=="MUN" & ano %in% seq(2009,2017,1),
             c("ibge", "municipio", "uf","orgao",
               "vlrepasse","vlcontrapart","vldes")]
  # MUN por UF
  bd1 <- bd[,c("uf", "vlrepasse", "vlcontrapart","vldes")]
  bd1[, conv   := as.numeric( .(count=.N)), by= c("uf")]
  bd1[, rep    := sum( vlrepasse ), by= c("uf")]  
  bd1[, lib    := sum( vldes ),  by= c("uf")]  
  bd1[, ctp    := sum( vlcontrapart ),  by= c("uf")]  
  bd1 <- bd1[,c(1,5,6,7,8)]; setkey(bd1); bd1 <- unique(bd1)
  bd1[, convP   := conv/sum(conv, na.rm=TRUE)]
  bd1[, repP    := rep/sum(rep,  na.rm=TRUE)]  
  bd1[, libP    := lib/sum(lib,  na.rm=TRUE)]
  bd1[, ctpP    := ctp/(ctp+rep)]
  # tabela  
    write.xlsx(bd1, "./dados/graficos/Cap2_tabelas.xlsx", 
             sheet="fig14.uf",append=TRUE)
  # gráfico
  bd1[, Repasse  := round(rep/sum(rep)*100,1)]
  bd1[, Liberado  := round(lib/sum(lib)*100,1)]
  bd1 <- bd1[,c("uf","Repasse","Liberado")]
  setorder(bd1, -Repasse)
  bd1 <- rbind(bd1, list("UF's",
                         as.numeric(bd1[Repasse<1,sum(Repasse)]),
                         as.numeric(bd1[Repasse<1,sum(Liberado)])))
  bd1 <- bd1[!Repasse < 1,]
  bd1 <- melt(bd1, measure.vars = c("Repasse","Liberado"),
              variable.name = "Valores", value.name = "perc") 
  bd1$uf <-factor(bd1$uf, levels = unique(bd1$uf))
  #
  g1 <- ggplot(bd1, aes(uf, perc, fill=Valores)) +
    geom_bar(stat="identity", position=position_dodge())+
    geom_text(aes(label= perc), vjust=-0.6, color=c("steelblue4"),
              position = position_dodge(0.9), size=2.4) +
    theme_bw() + scale_fill_brewer(palette="Paired") +
    theme(legend.position = c(0.9, 0.8),
          axis.text.x = element_text(face="bold", size=9.5),
          axis.text.y = element_text(angle = 45, 
                                     hjust=1, face="bold",size=9.5)) +
    labs(x = "Municipio", y = "% do total de recursos") +
    scale_y_continuous( limits=c(0, 13.5), breaks = seq(0,12,2),
                        expand = c(0.01,0))
  ggsave("Cap2_fig14.jpeg",plot = g1, 
         device = "jpeg", path = "./dados/graficos",
         width = 16.77, height = 8.20, limitsize = TRUE, 
         units = c("cm"), dpi = 300) 
  # MUN - por ORGAO  
    bd1 <- bd[,c("orgao", "vlrepasse", "vlcontrapart","vldes")]
    bd1[, conv   := as.numeric( .(count=.N)), by= c("orgao")]
    bd1[, rep    := sum( vlrepasse ), by= c("orgao")]  
    bd1[, lib    := sum( vldes ),  by= c("orgao")]  
    bd1[, ctp    := sum( vlcontrapart ),  by= c("orgao")]  
    bd1 <- bd1[,!c(2,3,4)]; setkey(bd1); bd1 <- unique(bd1)
    bd1[, convP   := conv/sum(conv, na.rm=TRUE)]
    bd1[, repP    := rep/sum(rep,  na.rm=TRUE)]  
    bd1[, libP    := lib/sum(lib,  na.rm=TRUE)]
    bd1[, ctpP    := ctp/(ctp+rep)]
    #tabela  
      write.xlsx(bd1, "./dados/graficos/Cap2_tabelas.xlsx", 
                 sheet="fig14.orgao",append=TRUE)
  # MUN - UF x ORGAO 
    bd1 <- bd[,c("uf", "orgao", "vlrepasse", "vlcontrapart","vldes")]
    bd1[, conv   := as.numeric( .(count=.N)), by= c("uf", "orgao")]
    bd1[, rep    := sum( vlrepasse ), by= c("uf", "orgao")]  
    bd1[, lib    := sum( vldes ),  by= c("uf", "orgao")]  
    bd1[, ctp    := sum( vlcontrapart ),  by= c("uf", "orgao")]  
    bd1 <- bd1[,!c(3,4,5)]; setkey(bd1); bd1 <- unique(bd1)
    bd1[, convP   := conv/sum(conv, na.rm=TRUE)]
    bd1[, repP    := rep/sum(rep,  na.rm=TRUE)]  
    bd1[, libP    := lib/sum(lib,  na.rm=TRUE)]
    bd1[, ctpP    := ctp/(ctp+rep)]
    #tabela  
      write.xlsx(bd1, "./dados/graficos/Cap2_tabelas.xlsx", 
                 sheet="fig14.uforgao",append=TRUE)
  # MUN - ORGAO x Municipios
    bd1 <- bd[,c("ibge", "municipio", "uf", "orgao", "vlrepasse", 
                 "vlcontrapart","vldes")]
    bd1[, conv   := as.numeric( .(count=.N)), by= c("ibge", "orgao")]
    bd1[, rep    := sum( vlrepasse ), by= c("ibge", "orgao")]  
    bd1[, lib    := sum( vldes ),  by= c("ibge", "orgao")]  
    bd1[, ctp    := sum( vlcontrapart ),  by= c("ibge", "orgao")]  
    bd1 <- bd1[,!c(5,6,7)]; setkey(bd1); bd1 <- unique(bd1)
    bd1[, convP   := conv/sum(conv, na.rm=TRUE)]
    bd1[, repP    := rep/sum(rep,  na.rm=TRUE)]  
    bd1[, libP    := lib/sum(lib,  na.rm=TRUE)]
    #tabela  
      write.xlsx(bd1, "./dados/graficos/Cap2_tabelas.xlsx", 
                 sheet="fig14.munorgao",append=TRUE)
  # MUN - valor absoluto e valor per capita  
    bd1 <- bd[,c("ibge","municipio","uf","vlrepasse","vldes")]
    # populacao da UF
      pop <- readRDS("./dados/IBGE/pop_pib.RDS")
      pop <- pop[ano=="2017",]
      bd1 <- merge(bd1, pop[,c("ibge","pop")], all.x=TRUE, by= c("ibge"))
    bd1[, rep    := sum( vlrepasse ), by= c("ibge")]  
    bd1[, lib    := sum( vldes ),  by= c("ibge")]    
    bd1 <- bd1[,!c(4,5)]; setkey(bd1); bd1 <- unique(bd1)
    bd1[, rep.pc    := round(rep/pop,2)]; bd1[, lib.pc    := round(lib/pop,2)]
    ibge.cap <- readRDS("./dados/IBGE/ibge_capitais.rds")
    bd1[,cap := ifelse(ibge %in% ibge.cap$ibge,1,0)]
    setorder(bd1,-cap, -lib)
    #tabela  
      #write.xlsx(bd1, "./dados/graficos/Cap2_tabelas.xlsx", 
      #           sheet="fig14.munpcap",append=TRUE)
    setorder(bd1, -cap, lib)
    bd1.1 <- bd1[cap==1,]
    bd1.1$municipio <-factor(bd1.1$municipio, levels = bd1.1$municipio)
    # grafico desembolso e desembolso
      (g1.1 <- ggplot(bd1.1, aes( municipio, round(lib/1000000,0), fill=municipio)) +
              geom_bar(stat="identity", position=position_dodge(), 
                       fill = "steelblue") + coord_flip() +
              geom_text(aes(label= round(lib/1000000,0)),
                        hjust=-0.2, color="steelblue", fontface='bold', size=3.1,
                        position = position_dodge(0.9)) +  
              theme_bw() +
              theme(legend.position = c(0.9, 0.2),
                    axis.text.x = element_text(size=9),
                    axis.text.y = element_text(angle = 30, hjust=1, size=8.5)) +
              labs(x = "Capitais Brasileiras",
                   y = "Valor Desembolsado (R$ milhões)") +
              scale_y_continuous( limits=c(0, 485), expand = c(0.005,0), 
                                  breaks = seq(0,470,50)))
        (g1.2 <- ggplot(bd1.1, aes( y =round(lib/1000000,0))) + 
              geom_boxplot(fill="steelblue") + 
              stat_boxplot(geom ='errorbar', width = 0.3) +
              theme_bw() + xlab("")+ylab(NULL) + 
              theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
                    axis.text.y=element_blank(),axis.ticks.y=element_blank(),
              plot.margin = unit(c(0.2,0.2,0.61,0), "cm")))

        (g1 <- grid.arrange(g1.1, g1.2, ncol = 2,widths = c(9, 1), clip = FALSE))
    ggsave("Cap2_fig15.jpeg",plot = g1, 
           device = "jpeg", path = "./dados/graficos",
           width = 18, height = 13.7, units = c("cm"),
           dpi = 300, limitsize = TRUE)
      ggsave("Cap2_fig15.jpeg",plot = g1, 
             device = "jpeg", path = "./dados/graficos",
             width = 17, height = 13, limitsize = TRUE, 
             units = c("cm"), dpi = 300)
      # grafico desembolso e desembolso percapita
      (g1.1 <- ggplot(bd1.1, aes( municipio, round(lib.pc,0), fill=municipio)) +
          geom_bar(stat="identity", position=position_dodge(), 
                   fill = "steelblue") + coord_flip() +
          geom_text(aes(label= round(lib.pc,0)),
                    hjust=-0.2, color="steelblue", fontface='bold', size=3.1,
                    position = position_dodge(0.9)) +  
          theme_bw() +
          theme(legend.position = c(0.9, 0.2),
                axis.text.x = element_text(size=9),
                axis.text.y = element_text(angle = 30, hjust=1, size=8.5)) +
          labs(x = "Capitais Brasileiras",
               y = "Valor Desembolsado per capita (R$)") +
          scale_y_continuous( limits=c(0, 1500), expand = c(0.005,0), 
                              breaks = seq(0,1400,100)))
      (g1.2 <- ggplot(bd1.1, aes( y =round(lib.pc,0))) + 
          geom_boxplot(fill="steelblue") + 
          stat_boxplot(geom ='errorbar', width = 0.3) +
          theme_bw() + xlab("")+ylab(NULL) + 
          theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks.y=element_blank(),
                plot.margin = unit(c(0.2,0.2,0.61,0), "cm")))
      (g1 <- grid.arrange(g1.1, g1.2, ncol = 2,widths = c(9, 1), clip = FALSE))
      ggsave("Cap2_fig16.jpeg",plot = g1, 
             device = "jpeg", path = "./dados/graficos",
             width = 18, height = 13.7, units = c("cm"),
             dpi = 300, limitsize = TRUE)
    
  # MAPA -  tvf per capita das OSC aos municipios
    library(RColorBrewer)
    library(tmap) # load tmap package
  # OSC per capita municipios
    names(bd1)[1] <- "CD_GEOCMU"
    mun <- readOGR("./dados/IBGE/mapas/municipios/BRMUE250GC_SIR.shp") 
    dados <- setDT(attr(mun, "data"))
    dados[,index := as.numeric(row.names(dados))]
    dados <- merge(dados, bd1[, c("CD_GEOCMU","lib.pc")], 
                   all.x= TRUE, by=c("CD_GEOCMU"))
    dados[is.na(lib.pc), lib.pc := 0]
    setorder(dados,index)
    mun@data <- dados
  # Valor liberado por regiao e por estado
    tmap_mode<-"plot"
    tm1 <- qtm(mun, fill ="lib.pc", 
             #fill.palette = c("#deebf7","#9ecae1","#6baed6",
             #"#4292c6","#2171b5","#08519c","#08306b")
             fill.palette = c("#b2182b","#ef8a62","#fddbc7",
                              "#f7f7f7","#d1e5f0","#67a9cf","#2166ac"),
             borders=NULL,
             fill.title="Desembolso (pcap)", fill.style="fixed", 
             fill.breaks=c(0,1,100,200,400,1000,4000,6200))
    tmap_save(tm1, "./dados/graficos/Cap2_fig17_MUN.png", 
            width=1800, height=1720, asp=0)

  # classificação ibge
    bd1[pop <5000, cpop := 1]
    bd1[pop >=5000 & pop < 10000, cpop := 2]
    bd1[pop >=10000 & pop < 20000, cpop := 3]
    bd1[pop >=20000 & pop < 50000, cpop := 4]
    bd1[pop >=50000 & pop < 100000, cpop := 5]
    bd1[pop >=100000 & pop < 200000, cpop := 6]
    bd1[pop >=200000 & pop < 500000, cpop := 7]
    bd1[pop >=500000 , cpop := 8]
    table(bd1$cpop)
    bd1[,pop.cpop := sum(pop), by= c("cpop")]
    bd1[,lib.cpop := sum(lib), by= c("cpop")]
    bd1[,libpc.cpop := lib.cpop/pop.cpop]
    x <- bd1[,c("cpop","lib.cpop", "libpc.cpop")]; setkey(x); x <- unique(x)
    #tabela  
      write.xlsx(x, "./dados/graficos/Cap2_tabelas.xlsx", 
               sheet="fig14.popibge",append=TRUE)
  
  } 

#0-1
#1-50;
#50-100
#100-200


#0  1 50  100 200 400
4000 6000
# FIM ------       
    
IF (FALSE ){    
    
  #      tmap_arrange(tm1, tm2, ncol = 1, nrow = 2)
  #      tmap_save(tm2, "./dados/graficos/Cap2_fig9_mapa01.png", 
  #                width=1800, height=1720, asp=0)
  
  #  tmap_save(tmap_arrange(tm1, tm2), "./dados/graficos/mapaUF.png", 
  #           width=1920, height=1080, asp=0)
  
  bd[, conv.uf     := as.numeric( .(count=.N)), by= c("uf")]
  bd[, conv.mun    := as.numeric( .(count=.N)), by= c("ibge")]
  bd[, conv.mun.uf := as.numeric( .(count=.N)), by= c("ibge","uf")]
  bd[, conv.org    := as.numeric( .(count=.N) ),by= c("orgao")]
  bd[, conv.org.uf := as.numeric( .(count=.N) ),by= c("uf","orgao")]
  #
  bd[, rep.uf  := sum( vlrepasse ),by= c("uf")]
  bd[, rep.mun := sum( vlrepasse ),by= c("ibge","uf")]
  bd[, rep.org := sum( vlrepasse ),by= c("uf","orgao")]
  #
  bd[, ctp.uf  := sum( vlcontrapart ),by= c("uf")]
  bd[, ctp.mun := sum( vlcontrapart ),by= c("ibge","uf")]
  bd[, ctp.org := sum( vlcontrapart ),by= c("uf","orgao")]
  #
  bd[, lib.uf  := sum( vldes ),by= c("uf")]
  bd[, lib.mun := sum( vldes ),by= c("ibge","uf")]
  bd[, lib.org := sum( vldes ),by= c("uf","orgao")]
  
  bd <- bd[,c("ibge","municipio","uf","orgao",
              "conv.uf","conv.mun","conv.mun.uf","conv.org","conv.org.uf",
              "rep.uf","rep.mun","rep.org","ctp.uf","ctp.mun","ctp.org",
              "lib.uf","lib.mun","lib.org")]
  setkey(bd); bd <- unique(bd)
  # Planilhas para análises
  #tab7  
  write.xlsx(unique(bd[,c(3,10,13,16)]), 
             "./dados/graficos/Cap2_tabelas.xlsx", 
             sheet="fig11.1",append=TRUE)
  
  
  
}
        

