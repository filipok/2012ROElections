################################################################################
ref.2012.sv <- read.csv("2012referendum/2. rezultate/sv_date.csv", sep = ";", 
                           stringsAsFactors = FALSE)
################################################################################
colnames(ref.2012.sv)[1] <- "JUD"
colnames(ref.2012.sv)[2] <- "DEN_JUD"
colnames(ref.2012.sv)[6] <- "SV"
colnames(ref.2012.sv)[5] <- "DEN_CIRC_R"
ref.2012.sv$DEN_JUD <- gsub("BACĂU", "BACAU", ref.2012.sv$DEN_JUD) #spelling
ref.2012.sv$DEN_JUD <- gsub("BOTOSANI", "BOTOŞANI", ref.2012.sv$DEN_JUD) #spelling
ref.2012.sv$DEN_JUD <- gsub("BRĂILA", "BRAILA", ref.2012.sv$DEN_JUD) #spelling
ref.2012.sv$DEN_JUD <- gsub("CĂLĂRAŞI", "CALARAŞI", ref.2012.sv$DEN_JUD) #spelling
ref.2012.sv$DEN_JUD <- gsub("CONSTANTA", "CONSTANŢA", ref.2012.sv$DEN_JUD) #spelling
ref.2012.sv$DEN_JUD <- gsub("SĂLAJ", "SALAJ", ref.2012.sv$DEN_JUD) #spelling
ref.2012.sv$DEN_JUD <- gsub("Municipiul Bucuresti", "MUNICIPIUL BUCUREŞTI", ref.2012.sv$DEN_JUD) #spelling
#am siruta la referendum şi am şi la sv.xlsx în locale 2012
#
# cumulare la nivel de CIRC/SIRUTA (similar funcţiei CumulatCirc din locale2012)
work <- ref.2012.sv[ref.2012.sv$tara == "",]
ref.2012.circ <- aggregate(subset(work, 
                                  select = -c(JUD, DEN_JUD, tara, siruta, SV, 
                                              DEN_CIRC_R, adresa, sector, mediu, 
                                              sortare, moc)), 
                           by = list(work$JUD, work$siruta), sum)
colnames(ref.2012.circ)[1] <- "JUD"
colnames(ref.2012.circ)[2] <- "siruta"
coloane <- work[, c("JUD", "DEN_JUD", "siruta", "DEN_CIRC_R", "mediu",
                    "sortare",   "moc")]
coloane <- unique(coloane)
ref.2012.circ <- merge(ref.2012.circ, coloane)
ref.2012.circ1 <- ref.2012.circ[,c("JUD", "DEN_JUD", "siruta", "DEN_CIRC_R")]
ref.2012.circ2 <- subset(ref.2012.circ, select = -c(JUD, siruta, DEN_JUD, DEN_CIRC_R))
ref.2012.circ <- cbind(ref.2012.circ1, ref.2012.circ2)
rm(ref.2012.circ1, ref.2012.circ2)
#cumulare la nivel de judeţ
work <- ref.2012.sv[ref.2012.sv$tara == "",]
ref.2012.jud <- aggregate(subset(work, 
                                 select = -c(JUD, DEN_JUD, tara, siruta, SV, 
                                             DEN_CIRC_R, adresa, sector, mediu, 
                                             sortare, moc)), 
                          by = list(work$JUD), sum)
colnames(ref.2012.jud)[1] <- "JUD"
coloane <- work[, c("JUD", "DEN_JUD")]
coloane <- unique(coloane)
ref.2012.jud <- merge(ref.2012.jud, coloane)
ref.2012.jud1 <- ref.2012.jud[,c("JUD", "DEN_JUD")]
ref.2012.jud2 <- subset(ref.2012.jud, select = -c(JUD, DEN_JUD))
ref.2012.jud <- cbind(ref.2012.jud1, ref.2012.jud2)
rm(ref.2012.jud1, ref.2012.jud2, coloane)
#cumulare la nivel naţional
ref.2012.nat <- aggregate(subset(ref.2012.sv, select = -c(mediu, sortare, moc, 
                                                             sector, adresa, SV, 
                                                             DEN_CIRC_R, siruta, tara, 
                                                             DEN_JUD, JUD)), 
                          by = list(ref.2012.sv$tara), FUN = sum)
colnames(ref.2012.nat)[1] <- "Tara"
ref.2012.nat[1,1] <- "ROMANIA"
rm(work)
#funcţie procente
Procente.Ref = function (x){
  work <- subset(x, select = -c(tapu, vveda, vvenu, tvn, nvc))
  work$TAPU_P_Ref <- x$tapu/x$ta
  work$TVVE_P_Ref <- (x$tapu - x$tvn)/x$tapu
  work$TVN_P_Ref <- x$tvn/x$tapu
  work$VVEDA <- x$vveda/(x$tapu - x$tvn)
  work$VVENU <- x$vvenu/(x$tapu - x$tvn)
  work
}

ref.2012.sv.pro <- Procente.Ref(ref.2012.sv)
ref.2012.circ.pro <- Procente.Ref(ref.2012.circ)
ref.2012.jud.pro <- Procente.Ref(ref.2012.jud)
ref.2012.sv.nat <- Procente.Ref(ref.2012.nat)

#vizualizare participare maximă
#q<- ref.2012.sv.pro[ref.2012.sv.pro$TAPU_P > 1 & ref.2012.sv.pro$tara == "",]
#View(q[order(q$TAPU_P),])