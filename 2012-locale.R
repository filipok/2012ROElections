#CARE E SURSA DIFERENŢELOR DINTRE TAP ŞI SITUAŢIA PRE-ALEGERI?
#
#
#?funcţii pentru selectarea unui anumit judeţ sau comune din judeţ
#
#Acest script prelucrează rezultatele alegerilor locale din 2012 din România
#
#Pregătire: del cols C, E, G, I, AQ, AS, AU in x_baza.xlsx & save as x_baza.csv
#
###################################
#Exemple de utilizare a funcţiilor#
###################################
#pe judeţ pentru cele unde avem CIRC = 0
#cju.abs.jud <- AbsolutJudet(CJU)
#pcj.abs.jud <- AbsolutJudet(PCJ)
#pe judeţ pentru cele unde nu avem CIRC = 0, cu agregare independenţi
#clo.cum.jud <- CumulatJudet(CLO) #cumulare pe bază de circumscripţie
#pri.cum.jud <- CumulatJudet(PRI) #cumulare pe bază de circumscripţie
#
#pe circumscripţie -> CIRC != 0 & SV == 0
#pentru cele cu date la nivel de circumscripţie
#clo.abs.circ <- AbsolutCirc(CLO, TRUE)
#pri.abs.circ <- AbsolutCirc(PRI, TRUE)
#pentru cele fără date la nivel de circumscripţie
#cju.cum.circ <- CumulatCirc(CJU) #cumulare pe bază de secţie de votare
#pcj.cum.circ <- CumulatCirc(PCJ) #cumulare pe bază de secţie de votare
#
#pe secţie de votare - pentru PRI şi CLO avem date doar pentru Bucureşti
#pcj.abs.sv <- AbsolutSv(PCJ)
#cju.abs.sv <- AbsolutSv(CJU)
#clo.abs.sv.buc <- AbsolutSv(CLO, TRUE)
#pri.abs.sv.buc <- AbsolutSv(PRI, TRUE)
#
#Procente(AbsolutJudet(CJU, TRUE)) #exemplu utilizare funcţie Procente
#SimpLocale2012(AbsolutSv(CJU)) #exemplu utilizare funcţie SimpLocale2012
#Procente(SimpLocale2012(AbsolutSv(CJU, TRUE))) #combinare funcţii
#
#Rezultate primării la nivel de circumscripţie TUR 1 + TUR 2
#pri.circ <- Procente(SimpLocale2012(AbsolutCirc(PRI, TRUE)))
#pri.circ.2 <- TUR2[TUR2$TIPPV == 1,]
#pri.circ.2 <- Procente(SimpLocale2012(AbsolutCirc(pri.circ.2, 
#                                                  TRUE)))
#pri.circ.all <- rbind(pri.circ, pri.circ.2)

#
#clear everything
rm(list = ls(all = TRUE))
#
################################################################################
locale2012 <- read.csv("2012locale/x_baza.csv", sep = ";", 
                       stringsAsFactors = FALSE)
################################################################################
locale2012$DEN_CIRC <- gsub("23.aug", "23 AUGUST", locale2012$DEN_CIRC) #eroare
locale2012$CODU <- as.numeric(locale2012$CODU)
tururi <- split(locale2012, locale2012$TUR)
rm(locale2012)
#separate first and second round
TUR1 <- tururi[[1]]
TUR2 <- tururi[[2]]
TUR2$DEN_JUD <- gsub("CĂLĂRAŞI", "CALARAŞI", TUR2$DEN_JUD) #unificare spelling
rm(tururi)
#separate first round: local council, county council, mayor and county president
tipuri <- split(TUR1, TUR1$TIPPV)
rm(TUR1)
PCJ <- tipuri[[1]]
PRI <- tipuri[[2]]
CLO <- tipuri[[3]]
CJU <- tipuri[[4]]
rm(tipuri)
################################################################################
#încărcare statistica sectii pre-alegeri
aleg.loc.2012 <- read.csv("2012locale/1. statistici/_statistica_alegatori_pe_sectii_de_vot_locale_2012.csv"
                       , sep = ";", stringsAsFactors = FALSE)
################################################################################
colnames(aleg.loc.2012)[1] <- "CIRC"
colnames(aleg.loc.2012)[4] <- "SV"
colnames(aleg.loc.2012)[3] <- "DEN_JUD"
colnames(aleg.loc.2012)[5] <- "Numar.alegatori"
aleg.loc.2012[,3] <- toupper(aleg.loc.2012[,3])
aleg.loc.2012$DEN_JUD <- gsub("BACĂU", "BACAU", aleg.loc.2012$DEN_JUD)
aleg.loc.2012$DEN_JUD <- gsub("BRĂILA", "BRAILA", aleg.loc.2012$DEN_JUD)
aleg.loc.2012$DEN_JUD <- gsub("BUZĂU", "BUZAU", aleg.loc.2012$DEN_JUD)
aleg.loc.2012$DEN_JUD <- gsub("CĂLĂRAŞI", "CALARAŞI", aleg.loc.2012$DEN_JUD)
aleg.loc.2012$DEN_JUD <- gsub("SĂLAJ", "SALAJ", aleg.loc.2012$DEN_JUD)
aleg.loc.2012$DEN_JUD <- gsub("DÎMBOVIŢA", "DÂMBOVIŢA", aleg.loc.2012$DEN_JUD)
aleg.loc.2012$DEN_JUD <- gsub("BUCUREŞTI", "MUNICIPIUL BUCUREŞTI", 
                              aleg.loc.2012$DEN_JUD)
aleg.loc.2012$Numar.alegatori <- gsub("\\.","",aleg.loc.2012$Numar.alegatori)
aleg.loc.2012$Numar.alegatori <- as.numeric(aleg.loc.2012$Numar.alegatori)
#test <- merge(CJU.test[, c(1:6)], aleg.loc.2012, all.x = TRUE)
#
################################################################################
#încărcare denumiri secţii de votare locale 2012 şi schimbare ca la referend2012
nume.sec.loc.2012 <- read.csv("2012locale/sv.csv", sep = ";", 
                              stringsAsFactors = FALSE)
################################################################################
# nume.sec.loc.2012$adresa <- toupper(nume.sec.loc.2012$adresa)
# nume.sec.loc.2012$adresa <- gsub("Â", "A", nume.sec.loc.2012$adresa)
# nume.sec.loc.2012$adresa <- gsub("Ă", "A", nume.sec.loc.2012$adresa)
# nume.sec.loc.2012$adresa <- gsub("Ţ", "T", nume.sec.loc.2012$adresa)
# nume.sec.loc.2012$adresa <- gsub("Ş", "S", nume.sec.loc.2012$adresa)
# nume.sec.loc.2012$adresa <- gsub (", STR\\. ", " STRADA: ", 
#                                   nume.sec.loc.2012$adresa)
# nume.sec.loc.2012$adresa <- gsub (" TELEFON(.*)$", "", nume.sec.loc.2012$adresa)
colnames(nume.sec.loc.2012)[1] <- "JUD"
colnames(nume.sec.loc.2012)[2] <- "DEN_JUD"
colnames(nume.sec.loc.2012)[4] <- "DEN_CIRC"
colnames(nume.sec.loc.2012)[5] <- "CIRC"
colnames(nume.sec.loc.2012)[6] <- "SV"
nume.sec.loc.2012$DEN_JUD <- gsub("BACĂU", "BACAU", nume.sec.loc.2012$DEN_JUD)
nume.sec.loc.2012$DEN_JUD <- gsub("BRĂILA", "BRAILA", nume.sec.loc.2012$DEN_JUD)
nume.sec.loc.2012$DEN_JUD <- gsub("BUZĂU", "BUZAU", nume.sec.loc.2012$DEN_JUD)
nume.sec.loc.2012$DEN_JUD <- gsub("CĂLĂRAŞI", "CALARAŞI", nume.sec.loc.2012$DEN_JUD)
nume.sec.loc.2012$DEN_JUD <- gsub("SĂLAJ", "SALAJ", nume.sec.loc.2012$DEN_JUD)
#
################################################################################
#determinare partidul noilor primari
################################################################################
new.prim <- PRI[PRI$MAN_1 == 1,c("JUD","DEN_JUD", "CIRC", "DEN_CIRC", "SV", 
                                 "CODU")]
TUR2.prim <- TUR2[TUR2$TIPPV ==1 & TUR2$MAN_1 == 1, 
                  c("JUD", "DEN_JUD", "CIRC", "DEN_CIRC", "SV", "CODU")]
new.prim <- rbind(new.prim, TUR2.prim)
echivalare.coduri <- read.csv("2012locale/coduri-partide.csv", sep = ";", 
                              stringsAsFactors = FALSE)
new.prim <- merge (new.prim, echivalare.coduri, all.x = TRUE)
new.prim <- merge (new.prim, unique(nume.sec.loc.2012[,c(1:5)]), 
                       all.x = TRUE)
rm(TUR2.prim)
################################################################################
#FUNCŢII
################################################################################
TransformTabel = function (x, indep.agreg = FALSE){
  #funcţie de transformare în tabel
  work.transf <- x
  #util dacă reactivez verificarea corectitudinii tabelului
  nume_tabel.transf<- deparse(substitute(x))
  ###
  #1. creez baza tabelului cu datele de participare din work.transf$CODU[i] == 0
  ###
  tabel.transf <- work.transf[work.transf$CODU == 0, c("JUD", "DEN_JUD", "CIRC", 
                                                       "DEN_CIRC", "SV", "TA", 
                                                       "TAP", "TACO", "TASU", 
                                                       "TASP", "TAPU", "TAPUP", 
                                                       "TAPUCO", "TAPUSU", 
                                                       "TAPUSP", "TVVE", "TVN", 
                                                       "BVP", "BVA", "NR_CONS")]
  indep <- work.transf[work.transf$CODU == 999,] #tabel separat cu independenţii
  ###
  #2. prelucrăm datele privind voturile date partidelor (VVE)
  ###
  #acum lucrăm doar cu rândurile cu CODU != 0 &  CODU != 999
  #în plus, păstrăm doar coloanele necesare pentru reshape
  work.transf <- work.transf[work.transf$CODU != 0 & work.transf$CODU != 999, 
                             c("JUD", "CIRC", "SV", "CODU", "VVE")]
  work.transf <- reshape(work.transf, v.names = "VVE", 
                         idvar = c("JUD", "CIRC", "SV"), timevar = "CODU", 
                         direction = "wide")
  #acum aranjăm numele coloanelor
  #facem un dataframe cu echivalenţele
  nume.partide <- c("UBBR", "ACDR", "PNDC", "U-Croati", "PRM", "Pro Europa", 
                    "RO.AS.IT", "U-Ucraineni", "FDGR", "Dom Polski", "PP-LC", 
                    "U-Elena", "PPOP", "PSR", "PDL", "MC-L", "PDL-PNTCD", 
                    "Al-Bacau", "BOTOSANI", "Al-Braila", "AP-C", "ACC", "AP", 
                    "MPNG", "USP", "M N MH", "A.P.E.L.", "Al-Teleorman", 
                    "Al-Vrancea", "UP", "PNL", "PNL-PC", "P.P.P.S.", "U.C.R.R.", 
                    "FC", "PCM", "PPE-DE", "UDMR", "PSDM", "PAS", "USE", "PUER", 
                    "PTT", "UDSCR", "A.M.R.", "PNG-CD", "PER", "UTM", 
                    "P.N.T.C.D.", "VERZII", "ApMS", "ApC", "PPMT", "SJ", "UDTR", 
                    "PC", "PP-DD", "PRE", "UNCR", "CRLR", "PSD", "USL", "UNPR", 
                    "UDTTMR", "USR", "ACA", "PSD+PNL MH", "P.PRO.", "FCER", 
                    "EMNP-MPP", "API", "ARH", "PSD-PNL VL", "PSD-PC VL", 
                    "APV PR", "USL CJ", "USL BR", "AP IL", "PSD+PC DJ", "UPBM", 
                    "UPMM", "APN", "P-Ren-R", "P-Rev-R", "NOLABEL", "FDR", 
                    "UIPS", "APM", "USPE", "AUT", "AEG", "AEF", "PSD+PNL AG", 
                    "APM1", "PNL+PP SV", "PRM-UNPR CV", "ACPC+PRM CV")
  coduri.partide <- paste("VVE.", 1:97, sep="")
  numar.partide <- (1:97)
  rep.col <- data.frame(nume = nume.partide, 
                        coduri = coduri.partide, numar = numar.partide, 
                        stringsAsFactors = FALSE)
  #punem coloanele lipsă
  for (i in 1:97) {
    colnames(work.transf) <- replace(colnames(work.transf), 
                                     colnames(work.transf) == rep.col$coduri[i], 
                                     rep.col$numar[i])
  }
  for (i in 1:97) {
    if(!(as.character(i) %in% colnames(work.transf))) {
      coloana <- matrix(nrow = nrow(work.transf), rep(0, nrow(work.transf)))
      colnames(coloana) <- as.character(i)
      work.transf <- cbind(work.transf, coloana)
      rm(coloana)
    } 
  }
  #acum ordonăm după codul partidului
  options(warn = -1) #ca să nu primesc warning aici
  work.transf <- work.transf[,order(as.numeric(colnames(work.transf)))]
  options(warn = 0) #revenim la setarea anterioară pentru warn
  #iar acum punem denumirile partidelor
  for (i in 1:97) {
    colnames(work.transf) <- replace(colnames(work.transf), 
                                     colnames(work.transf) == rep.col$numar[i], 
                                     rep.col$nume[i])
  }
  #unim tabelul cu datele de participare de cel cu voturile date partidelor
  tabel.transf <- merge(tabel.transf, work.transf, all.x = TRUE, sort = FALSE)
  rm(work.transf)
  ###
  #3. acum trebuie să adăugăm una sau mai multe coloane pentru independenţi
  ###
  #avem două cazuri, cu sau fără agregare independenţi
  if(indep.agreg == FALSE) {
    #colnames diferite în funcţie de nivel (judeţean sau local)
    if((indep$TIPPV[1] == 1) | (indep$TIPPV[1] == 2)) {
      indep.labels <- apply(indep, 1, function(qqq) paste(qqq[5],"_", qqq[2], 
                                                          "_", qqq[4], "+",
                                                          qqq[39], sep = ""))
    }
    else {
      indep.labels <- apply(indep, 1, function(qqq) paste(qqq[3],"_", qqq[2], 
                                                          "_", qqq[39], 
                                                          sep = ""))
    }
    indep <- cbind(indep, indep.labels)
    indep <- indep[, c("JUD", "CIRC", "SV", "VVE", "indep.labels")]
    indep <- reshape(indep, v.names = "VVE", idvar = c("JUD", "CIRC", "SV"), 
                     timevar = "indep.labels", direction = "wide")
    #eliminăm "VVE." din numele independenţilor
    colnames(indep) <- gsub("VVE.", "", colnames(indep))
    #printăm număr independenţi, aşa, ca chestie
    indep.labels <- unique(indep.labels)
    print(paste("Avem", length(indep.labels), "independenti."))
  }
  else {
    indep <- aggregate(indep$VVE, by = list(indep$JUD, indep$CIRC, indep$SV), 
                       sum)
    colnames(indep) <- c("JUD", "CIRC", "SV","INDEPENDENTI")
  }
  #unim tabelul de independenţi la tabelul general
  tabel.transf <- merge(tabel.transf, indep, sort = TRUE, all.x = TRUE)
  rm(indep)
  ###
  #4. suntem gata, mai trebuie doar să înlocuim NA cu zero şi să scoatem col.85
  ###
  tabel.transf[is.na(tabel.transf)] <- 0
  #numerotarea partidelor sare peste nr. 85 şi se creează o coloană goală
  tabel.transf$NOLABEL <- NULL 
  tabel.transf <- merge (tabel.transf, unique(nume.sec.loc.2012[,c(1:5)]), 
                         all.x = TRUE)
  #ordonăm coloanele  
  work1 <- tabel.transf[,c("JUD", "CIRC", "SV", "DEN_JUD", "DEN_CIRC", "siruta")]
  work2 <- subset(tabel.transf, select = -c(JUD, CIRC, SV, DEN_JUD, DEN_CIRC, siruta))
  tabel.transf <- cbind(work1, work2)
  rm(work1, work2)
  tabel.transf
}


AbsolutJudet = function (x, indep.agreg = FALSE) {
  #definirea funcţiei care prelucrează totalurile la nivel judeţean, 
  #acolo unde avem CIRC=0, adică la CJU şi PCJ
  #x e tabelul, indep.agreg ne zice dacă cumulăm independenţii
  work.judet <- x
  nume_tabel<- deparse(substitute(x))
  #separ datele la nivel de judet
  work.judet <- work.judet[work.judet$CIRC == 0,]
  #apelare funcţie TransformTabel(), cu sau fără cumulare independenţi
  if(indep.agreg == FALSE) {
    tabel <- TransformTabel(work.judet)
    nume_fisier <- paste(nume_tabel, "-judet.csv", sep ="")
  }
  else {
    tabel <- TransformTabel(work.judet, TRUE)
    nume_fisier <- paste(nume_tabel, "-judet-INDEP.AGREG.csv", sep ="")
  }
  #write.table(x=tabel,file=nume_fisier, sep = ";", row.names = FALSE)
  tabel
}

AbsolutCirc = function (x, indep.agreg = FALSE) {
  #definirea funcţiei care prelucrează totalurile la nivel de circumscripţie, 
  #CIRC != 0 && SV == 0
  #x e tabelul, indep.agreg ne zice dacă cumulăm independenţii
  work.circ <- x
  nume_tabel<- deparse(substitute(x))
  #separ date circumscripţie
  work.circ <- work.circ[work.circ$CIRC != 0 & work.circ$SV == 0,]
  #apelare funcţie TransformTabel(), cu sau fără cumulare independenţi
  if(indep.agreg == FALSE) {
    tabel <- TransformTabel(work.circ)
    nume_fisier <- paste(nume_tabel, "-circ.csv", sep ="")
  }
  else {
    tabel <- TransformTabel(work.circ, TRUE)
    nume_fisier <- paste(nume_tabel, "-circ-INDEP.AGREG.csv", sep ="")
  }
  #write.table(x=tabel,file=nume_fisier, sep = ";", row.names = FALSE)
  tabel
}

AbsolutSv = function (x, indep.agreg = FALSE) {
  #definirea funcţiei care prelucrează totalurile la nivel de secţie de votare,
  #SV != 0
  #x e tabelul, indep.agreg ne zice dacă cumulăm independenţii
  work.sv <- x
  nume_tabel<- deparse(substitute(x))
  #separ datele la nivel de secţie
  work.sv <- work.sv[work.sv$SV != 0,]
  #apelare funcţie TransformTabel(), cu sau fără cumulare independenţi
  if(indep.agreg == FALSE) {
    tabel <- TransformTabel(work.sv)
    nume_fisier <- paste(nume_tabel, "-sv.csv", sep ="")
  }
  else {
    tabel <- TransformTabel(work.sv, TRUE)
    nume_fisier <- paste(nume_tabel, "-sv-INDEP.AGREG.csv", sep ="")
  }
  #write.table(x=tabel,file=nume_fisier, sep = ";", row.names = FALSE)
  tabel
}

CumulatJudet = function (x){
  #definirea funcţiei de stabilire a rezultatelor cumulate pe judeţ acolo unde
  #nu avem înregistrări CIRC = 0, respectiv doar CLO şi PRI
  #obţinem mai întâi datele pe circumscripţie; cumulăm independenţii
  work <- AbsolutCirc(x, TRUE)
  nume_tabel<- deparse(substitute(x))
  #folosim funcţia de agregare
  work <- aggregate(subset(work, select = -c(JUD, DEN_JUD, DEN_CIRC, siruta)), 
                    by = list(work$JUD), sum)
  #după agregare mai sunt probleme cu unele coloane
  work$CIRC <- 0
  work$NR_CONS <- 0
  colnames(work)[1] <- "JUD"
  work$DEN_CIRC <- ""
  work$siruta <- NA
  work$DEN_JUD <- c("ALBA", "ARAD", "ARGEŞ", "BACAU", "BIHOR", 
                    "BISTRIŢA-NĂSĂUD", "BOTOŞANI", "BRAŞOV", "BRAILA", "BUZAU", 
                    "CARAŞ-SEVERIN", "CALARAŞI", "CLUJ", "CONSTANŢA", "COVASNA",
                    "DÂMBOVIŢA", "DOLJ", "GALAŢI", "GIURGIU", "GORJ", 
                    "HARGHITA", "HUNEDOARA", "IALOMIŢA", "IAŞI", "ILFOV", 
                    "MARAMUREŞ", "MEHEDINŢI", "MUREŞ", "NEAMŢ", "OLT", 
                    "PRAHOVA", "SATU MARE", "SALAJ", "SIBIU", "SUCEAVA", 
                    "TELEORMAN", "TIMIŞ", "TULCEA", "VASLUI", "VÂLCEA", 
                    "VRANCEA", "MUNICIPIUL BUCUREŞTI")
  work1 <- work[,c("JUD", "CIRC", "SV", "DEN_JUD", "DEN_CIRC", "siruta")]
  work2 <- subset(work, select = -c(JUD, CIRC, SV, DEN_JUD, DEN_CIRC, siruta))
  work <- cbind(work1, work2)
  rm(work1, work2)
  nume_fisier <- paste(nume_tabel, "-CumJud-INDEP.AGREG.csv", sep ="")
  #write.table(x=work, file=nume_fisier, sep = ";", row.names = FALSE)
  work
}

CumulatCirc = function (x){
  #definirea funcţiei de stabilire a rezultatelor cumulate pe circumscripţie 
  #acolo unde nu avem înregistrări CIRC != 0 & work.circ$SV == 0,
  #respectiv doar CJU şi PCJ
  #obţinem mai întâi datele pe secţie de votare; cumulăm independenţii
  cum.sv <- AbsolutSv(x, TRUE)
  nume_tabel<- deparse(substitute(x))
  #folosim funcţia de agregare
  work <- aggregate(subset(cum.sv, select = -c(JUD, CIRC, DEN_JUD, DEN_CIRC, 
                                               siruta)), 
                    by = list(cum.sv$JUD, cum.sv$CIRC), sum)
  #după agregare mai sunt probleme cu unele coloane
  work$SV <- 0
  colnames(work)[1] <- "JUD"
  colnames(work)[2] <- "CIRC"
  #facem merge ca să reintroducem DEN_JUD şi DEN_CIRC
  cum.sv <- cum.sv[,c("JUD", "CIRC", "DEN_JUD", "DEN_CIRC", "siruta")]
  cum.sv <- unique(cum.sv)
  work <- merge (work, cum.sv)
  rm(cum.sv)
  #ordonăm coloanele  
  work1 <- work[,c("JUD", "CIRC", "SV", "DEN_JUD", "DEN_CIRC", "siruta")]
  work2 <- subset(work, select = -c(JUD, CIRC, SV, DEN_JUD, DEN_CIRC, siruta))
  work <- cbind(work1, work2)
  rm(work1, work2)
  work <- work[ order(as.numeric(work[,1]), as.numeric(work[,2])), ]
  nume_fisier <- paste(nume_tabel, "-CumCirc-INDEP.AGREG.csv", sep ="")
  #write.table(x=work, file=nume_fisier, sep = ";", row.names = FALSE)
  work
}

#definirea funcţiei de trecere la procentaje
Procente = function (x){
  work <- x[,c("JUD", "CIRC", "SV", "DEN_JUD", "DEN_CIRC", "siruta", "TAP")]
  work$TAPU_P <- x$TAPU / x$TAP #participare la vot / lista perm.
  work$TVVE_P <- x$TVVE / x$TAPU #procentaj voturi valabile din total exprimate
  work$TVN_P <- x$TVN / x$TAPU #procentaj voturi nule din total exprimate
  work$TAPUP_P <- x$TAPUP / x$TAP #participare aleg. permanenti /lista perm.
  work$TAPUCO_P <- x$TAPUCO / x$TAP #participare aleg. UE /lista perm.
  work$TAPUSU_P <- x$TAPUSU / x$TAP #participare aleg. suplimentari /lista perm.
  work$TAPUSP_P <- x$TAPUSP / x$TAP #participare aleg. urnă spec. /lista perm.
  partide  <- x[-c(1:21)]
  partide <- partide / x$TVVE
  work <- cbind(work, partide)
  work
}

SimpLocale2012 = function (x){
  #definirea funcţiei de simplificare pentru locale2012
  #cumulare PDL cu alianţele în care a participat şi PDL
  #cumulare USL, PNL, PSD, PC şi alianţele aferente
  #cumulare minoritîţi, altele decât maghiară
  work <- x
  work$MINORITATI <- x$UBBR + x$'U-Croati' + x$RO.AS.IT + x$'U-Ucraineni'
  + x$FDGR + x$'Dom Polski' + x$'U-Elena' + x$U.C.R.R. + x$UDSCR +x$A.M.R.
  + x$UDTR + x$CRLR + x$UDTTMR + x$USR + x$ACA + x$FCER 
  work$ROMI <- x$'Pro Europa' + x$ACDR + x$UNCR
  work$allPDL <- x$PDL + x$'MC-L' + x$'PDL-PNTCD' + x$'Al-Bacau' +   x$BOTOSANI + 
    x$'Al-Braila' + x$'AP-C' + x$ACC + x$AP + x$MPNG + x$USP + x$'M N MH' + 
    x$A.P.E.L. + x$'Al-Teleorman' + x$'Al-Vrancea' + x$USE + x$ApMS + x$ApC + 
    x$ARH + x$'APV PR'+ x$'AP IL' + x$UPBM + x$UPMM + x$APN + x$APM1 + 
    x$'ACPC+PRM CV'
  work$allUSL <- x$PNL + x$'PNL-PC' + x$PC + x$PSD + x$USL + x$'PSD+PNL MH' + 
    x$'PSD-PNL VL' + x$'PSD-PC VL' + x$'USL CJ' + x$'USL BR' + x$'PSD+PC DJ' + 
    x$AEG + x$AEF + x$'PSD+PNL AG' + x$'PNL+PP SV'
  work$altele <- x$PNDC + x$PPOP + x$PSR + x$UP + x$P.P.P.S. + x$FC + x$'PPE-DE' +
    x$PSDM + x$PAS + x$PUER + x$PTT + x$'PNG-CD' + x$PRE + x$P.PRO. + x$'P-Ren-R'+
    x$'P-Rev-R' + x$FDR + x$USPE + x$AUT
  work$nonUDMR <- x$PCM + x$PPMT + x$'EMNP-MPP'
  work$allUNPR <- x$UNPR + x$API + x$'PRM-UNPR CV'
  work$allPNT <- x$UTM + x$P.N.T.C.D + x$SJ
  work$allVerzii <- x$VERZII + x$UIPS
  work$allPRM <- x$PRM + x$APM
  colnames(work) <- gsub("-| |\\+", "", colnames(work))
  work <- subset(work, select = -c(UBBR, UCroati, RO.AS.IT, UUcraineni, 
                                   FDGR, DomPolski, UElena, U.C.R.R., 
                                   UDSCR, A.M.R., UDTR, CRLR, UDTTMR, USR, ACA, 
                                   FCER, ProEuropa, ACDR, UNCR, PDL, MCL, 
                                   PDLPNTCD, AlBacau, BOTOSANI, 
                                   AlBraila, APC, ACC, AP, MPNG, USP, 
                                   MNMH, A.P.E.L., AlTeleorman, 
                                   AlVrancea, USE, ApMS, ApC, ARH, APVPR, 
                                   APIL, UPBM, UPMM, APN, APM1, ACPCPRMCV, 
                                   PNL, PNLPC, PC, PSD, USL, PSDPNLMH, 
                                   PSDPNLVL, PSDPCVL, USLCJ, 
                                   USLBR, PSDPCDJ, AEG, AEF, PSDPNLAG, PNLPPSV, 
                                   PNDC, PPOP, PSR, UP, P.P.P.S., FC, PPEDE, PSDM,
                                   PAS, PUER, PTT, PNGCD, PRE, P.PRO., PRenR,
                                   PRevR, FDR, USPE, PCM, PPMT, EMNPMPP, UTM, 
                                   P.N.T.C.D., SJ, UNPR, API, VERZII, UIPS, AUT,
                                   PRM, APM, PRMUNPRCV))
  work
}