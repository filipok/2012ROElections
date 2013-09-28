#CARE E SURSA DIFERENŢELOR DINTRE TAP ŞI SITUAŢIA PRE-ALEGERI?
#
#
#?funcţii pentru selectarea unui anumit judeţ sau comune din judeţ
#
#Acest script prelucrează rezultatele alegerilor locale din 2012 din România
#
#Pregătire: del cols C, E, G, I, AQ, AS, AU in x_baza.xlsx (din x_baza.zip) 
#& save as x_baza.csv
#
###################################
#Exemple de utilizare a funcţiilor#
###################################
#pe judeţ pentru cele unde avem CIRC = 0
#cjuAbsJud = AbsolutJudet(CJU)
#pcjAbsJud = AbsolutJudet(PCJ)
#pe judeţ pentru cele unde nu avem CIRC = 0, cu agregare independenţi
#cloCumJud = CumulatJudet(CLO) #cumulare pe bază de circumscripţie
#priCumJud = CumulatJudet(PRI) #cumulare pe bază de circumscripţie
#
#pe circumscripţie -> CIRC != 0 & SV == 0
#pentru cele cu date la nivel de circumscripţie
#cloAbsCirc = AbsolutCirc(CLO, TRUE)
#priAbsCirc = AbsolutCirc(PRI, TRUE)
#pentru cele fără date la nivel de circumscripţie
#cjuCumCirc = CumulatCirc(CJU) #cumulare pe bază de secţie de votare
#pcjCumCirc = CumulatCirc(PCJ) #cumulare pe bază de secţie de votare
#
#pe secţie de votare - pentru PRI şi CLO avem date doar pentru Bucureşti
#pcjAbsSv = AbsolutSv(PCJ)
#cjuAbsSv = AbsolutSv(CJU)
#cloAbsSvBuc = AbsolutSv(CLO, TRUE)
#priAbsSvBuc = AbsolutSv(PRI, TRUE)
#
#Procente(AbsolutJudet(CJU, TRUE)) #exemplu utilizare funcţie Procente
#SimpLocale2012(AbsolutSv(CJU)) #exemplu utilizare funcţie SimpLocale2012
#Procente(SimpLocale2012(AbsolutSv(CJU, TRUE))) #combinare funcţii
#
#Rezultate primării la nivel de circumscripţie TUR 1 + TUR 2
#priCirc = Procente(SimpLocale2012(AbsolutCirc(PRI, TRUE)))
#priCirc2 = TUR2[TUR2$TIPPV == 1,]
#priCirc2 = Procente(SimpLocale2012(AbsolutCirc(priCirc2, 
#                                                  TRUE)))
#priCircAll = rbind(priCirc, priCirc2)

#
#clear everything
#rm(list = ls(all = TRUE))
#
################################################################################
locale2012 = read.csv("2012locale/x_baza.csv", sep = ";", 
                       stringsAsFactors = FALSE)
################################################################################
locale2012$DEN_CIRC = gsub("23.aug", "23 AUGUST", locale2012$DEN_CIRC) #eroare
locale2012$CODU = as.numeric(locale2012$CODU)
tururi = split(locale2012, locale2012$TUR)
rm(locale2012)
#separate first and second round
TUR1 = tururi[[1]]
TUR2 = tururi[[2]]
TUR2$DEN_JUD = gsub("CĂLĂRAŞI", "CALARAŞI", TUR2$DEN_JUD) #unificare spelling
rm(tururi)
#separate first round: local council, county council, mayor and county president
tipuri = split(TUR1, TUR1$TIPPV)
rm(TUR1)
PCJ = tipuri[[1]]
PRI = tipuri[[2]]
CLO = tipuri[[3]]
CJU = tipuri[[4]]
rm(tipuri)
################################################################################
#încărcare statistica sectii pre-alegeri
alegLocale2012 = read.csv("2012locale/1. statistici/_statistica_alegatori_pe_sectii_de_vot_locale_2012.csv"
                       , sep = ";", stringsAsFactors = FALSE)
################################################################################
colnames(alegLocale2012)[1] = "CIRC"
colnames(alegLocale2012)[4] = "SV"
colnames(alegLocale2012)[3] = "DEN_JUD"
colnames(alegLocale2012)[5] = "Numar.alegatori"
alegLocale2012[,3] = toupper(alegLocale2012[,3])
alegLocale2012$DEN_JUD = gsub("BACĂU", "BACAU", alegLocale2012$DEN_JUD)
alegLocale2012$DEN_JUD = gsub("BRĂILA", "BRAILA", alegLocale2012$DEN_JUD)
alegLocale2012$DEN_JUD = gsub("BUZĂU", "BUZAU", alegLocale2012$DEN_JUD)
alegLocale2012$DEN_JUD = gsub("CĂLĂRAŞI", "CALARAŞI", alegLocale2012$DEN_JUD)
alegLocale2012$DEN_JUD = gsub("SĂLAJ", "SALAJ", alegLocale2012$DEN_JUD)
alegLocale2012$DEN_JUD = gsub("DÎMBOVIŢA", "DÂMBOVIŢA", alegLocale2012$DEN_JUD)
alegLocale2012$DEN_JUD = gsub("BUCUREŞTI", "MUNICIPIUL BUCUREŞTI", 
                              alegLocale2012$DEN_JUD)
alegLocale2012$Numar.alegatori = gsub("\\.","",alegLocale2012$Numar.alegatori)
alegLocale2012$Numar.alegatori = as.numeric(alegLocale2012$Numar.alegatori)
#test = merge(CJUtest[, c(1:6)], alegLocale2012, all.x = TRUE)
#
################################################################################
#încărcare denumiri secţii de votare locale 2012 şi schimbare ca la referend2012
numeSecLoc2012 = read.csv("2012locale/sv.csv", sep = ";", 
                              stringsAsFactors = FALSE)
################################################################################
# numeSecLoc2012$adresa = toupper(numeSecLoc2012$adresa)
# numeSecLoc2012$adresa = gsub("Â", "A", numeSecLoc2012$adresa)
# numeSecLoc2012$adresa = gsub("Ă", "A", numeSecLoc2012$adresa)
# numeSecLoc2012$adresa = gsub("Ţ", "T", numeSecLoc2012$adresa)
# numeSecLoc2012$adresa = gsub("Ş", "S", numeSecLoc2012$adresa)
# numeSecLoc2012$adresa = gsub (", STR\\. ", " STRADA: ", 
#                                   numeSecLoc2012$adresa)
# numeSecLoc2012$adresa = gsub (" TELEFON(.*)$", "", numeSecLoc2012$adresa)
colnames(numeSecLoc2012)[1] = "JUD"
colnames(numeSecLoc2012)[2] = "DEN_JUD"
colnames(numeSecLoc2012)[4] = "DEN_CIRC"
colnames(numeSecLoc2012)[5] = "CIRC"
colnames(numeSecLoc2012)[6] = "SV"
numeSecLoc2012$DEN_JUD = gsub("BACĂU", "BACAU", numeSecLoc2012$DEN_JUD)
numeSecLoc2012$DEN_JUD = gsub("BRĂILA", "BRAILA", numeSecLoc2012$DEN_JUD)
numeSecLoc2012$DEN_JUD = gsub("BUZĂU", "BUZAU", numeSecLoc2012$DEN_JUD)
numeSecLoc2012$DEN_JUD = gsub("CĂLĂRAŞI", "CALARAŞI", numeSecLoc2012$DEN_JUD)
numeSecLoc2012$DEN_JUD = gsub("SĂLAJ", "SALAJ", numeSecLoc2012$DEN_JUD)
#
################################################################################
#determinare partidul noilor primari
################################################################################
newPrim = PRI[PRI$MAN_1 == 1,c("JUD","DEN_JUD", "CIRC", "DEN_CIRC", "SV", 
                                 "CODU")]
TUR2Prim = TUR2[TUR2$TIPPV ==1 & TUR2$MAN_1 == 1, 
                  c("JUD", "DEN_JUD", "CIRC", "DEN_CIRC", "SV", "CODU")]
newPrim = rbind(newPrim, TUR2Prim)
echivalareCoduri = read.csv("2012locale/coduri-partide.csv", sep = ";", 
                              stringsAsFactors = FALSE)
newPrim = merge (newPrim, echivalareCoduri, all.x = TRUE)
newPrim = merge (newPrim, unique(numeSecLoc2012[,c(1:5)]), 
                       all.x = TRUE)
rm(TUR2Prim)
################################################################################
#FUNCŢII
################################################################################
TransformTabel = function (x, indepAgreg = FALSE){
  #funcţie de transformare în tabel
  workTransf = x
  #util dacă reactivez verificarea corectitudinii tabelului
  numeTabelTransf= deparse(substitute(x))
  ###
  #1. creez baza tabelului cu datele de participare din workTransf$CODU[i] == 0
  ###
  tabelTransf = workTransf[workTransf$CODU == 0, c("JUD", "DEN_JUD", "CIRC", 
                                                       "DEN_CIRC", "SV", "TA", 
                                                       "TAP", "TACO", "TASU", 
                                                       "TASP", "TAPU", "TAPUP", 
                                                       "TAPUCO", "TAPUSU", 
                                                       "TAPUSP", "TVVE", "TVN", 
                                                       "BVP", "BVA", "NR_CONS")]
  indep = workTransf[workTransf$CODU == 999,] #tabel separat cu independenţii
  ###
  #2. prelucrăm datele privind voturile date partidelor (VVE)
  ###
  #acum lucrăm doar cu rândurile cu CODU != 0 &  CODU != 999
  #în plus, păstrăm doar coloanele necesare pentru reshape
  workTransf = workTransf[workTransf$CODU != 0 & workTransf$CODU != 999, 
                             c("JUD", "CIRC", "SV", "CODU", "VVE")]
  workTransf = reshape(workTransf, v.names = "VVE", 
                         idvar = c("JUD", "CIRC", "SV"), timevar = "CODU", 
                         direction = "wide")
  #acum aranjăm numele coloanelor
  #facem un dataframe cu echivalenţele
  numePartide = c("UBBR", "ACDR", "PNDC", "U-Croati", "PRM", "Pro Europa", 
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
  coduriPartide = paste("VVE.", 1:97, sep="")
  numarPartide = (1:97)
  repCol = data.frame(nume = numePartide, 
                        coduri = coduriPartide, numar = numarPartide, 
                        stringsAsFactors = FALSE)
  #punem coloanele lipsă
  for (i in 1:97) {
    colnames(workTransf) = replace(colnames(workTransf), 
                                     colnames(workTransf) == repCol$coduri[i], 
                                     repCol$numar[i])
  }
  for (i in 1:97) {
    if(!(as.character(i) %in% colnames(workTransf))) {
      coloana = matrix(nrow = nrow(workTransf), rep(0, nrow(workTransf)))
      colnames(coloana) = as.character(i)
      workTransf = cbind(workTransf, coloana)
      rm(coloana)
    } 
  }
  #acum ordonăm după codul partidului
  options(warn = -1) #ca să nu primesc warning aici
  workTransf = workTransf[,order(as.numeric(colnames(workTransf)))]
  options(warn = 0) #revenim la setarea anterioară pentru warn
  #iar acum punem denumirile partidelor
  for (i in 1:97) {
    colnames(workTransf) = replace(colnames(workTransf), 
                                     colnames(workTransf) == repCol$numar[i], 
                                     repCol$nume[i])
  }
  #unim tabelul cu datele de participare de cel cu voturile date partidelor
  tabelTransf = merge(tabelTransf, workTransf, all.x = TRUE, sort = FALSE)
  rm(workTransf)
  ###
  #3. acum trebuie să adăugăm una sau mai multe coloane pentru independenţi
  ###
  #avem două cazuri, cu sau fără agregare independenţi
  if(indepAgreg == FALSE) {
    #colnames diferite în funcţie de nivel (judeţean sau local)
    if((indep$TIPPV[1] == 1) | (indep$TIPPV[1] == 2)) {
      indepLabels = apply(indep, 1, function(qqq) paste(qqq[5],"_", qqq[2], 
                                                          "_", qqq[4], "+",
                                                          qqq[39], sep = ""))
    }
    else {
      indepLabels = apply(indep, 1, function(qqq) paste(qqq[3],"_", qqq[2], 
                                                          "_", qqq[39], 
                                                          sep = ""))
    }
    indep = cbind(indep, indepLabels)
    indep = indep[, c("JUD", "CIRC", "SV", "VVE", "indepLabels")]
    indep = reshape(indep, v.names = "VVE", idvar = c("JUD", "CIRC", "SV"), 
                     timevar = "indepLabels", direction = "wide")
    #eliminăm "VVE." din numele independenţilor
    colnames(indep) = gsub("VVE.", "", colnames(indep))
    #printăm număr independenţi, aşa, ca chestie
    indepLabels = unique(indepLabels)
    print(paste("Avem", length(indepLabels), "independenti."))
  }
  else {
    indep = aggregate(indep$VVE, by = list(indep$JUD, indep$CIRC, indep$SV), 
                       sum)
    colnames(indep) = c("JUD", "CIRC", "SV","INDEPENDENTI")
  }
  #unim tabelul de independenţi la tabelul general
  tabelTransf = merge(tabelTransf, indep, sort = TRUE, all.x = TRUE)
  rm(indep)
  ###
  #4. suntem gata, mai trebuie doar să înlocuim NA cu zero şi să scoatem col.85
  ###
  tabelTransf[is.na(tabelTransf)] = 0
  #numerotarea partidelor sare peste nr. 85 şi se creează o coloană goală
  tabelTransf$NOLABEL = NULL 
  tabelTransf = merge (tabelTransf, unique(numeSecLoc2012[,c(1:5)]), 
                         all.x = TRUE)
  #ordonăm coloanele  
  work1 = tabelTransf[,c("JUD", "CIRC", "SV", "DEN_JUD", "DEN_CIRC", "siruta")]
  work2 = subset(tabelTransf, select = -c(JUD, CIRC, SV, DEN_JUD, DEN_CIRC, siruta))
  tabelTransf = cbind(work1, work2)
  rm(work1, work2)
  tabelTransf
}


AbsolutJudet = function (x, indepAgreg = FALSE) {
  #definirea funcţiei care prelucrează totalurile la nivel judeţean, 
  #acolo unde avem CIRC=0, adică la CJU şi PCJ
  #x e tabelul, indepAgreg ne zice dacă cumulăm independenţii
  workJudet = x
  numeTabel = deparse(substitute(x))
  #separ datele la nivel de judet
  workJudet = workJudet[workJudet$CIRC == 0,]
  #apelare funcţie TransformTabel(), cu sau fără cumulare independenţi
  if(indepAgreg == FALSE) {
    tabel = TransformTabel(workJudet)
    numeFisier = paste(numeTabel, "-judet.csv", sep ="")
  }
  else {
    tabel = TransformTabel(workJudet, TRUE)
    numeFisier = paste(numeTabel, "-judet-indepAgreg.csv", sep ="")
  }
  #write.table(x=tabel,file=numeFisier, sep = ";", row.names = FALSE)
  tabel
}

AbsolutCirc = function (x, indepAgreg = FALSE) {
  #definirea funcţiei care prelucrează totalurile la nivel de circumscripţie, 
  #CIRC != 0 && SV == 0
  #x e tabelul, indepAgreg ne zice dacă cumulăm independenţii
  workCirc = x
  numeTabel= deparse(substitute(x))
  #separ date circumscripţie
  workCirc = workCirc[workCirc$CIRC != 0 & workCirc$SV == 0,]
  #apelare funcţie TransformTabel(), cu sau fără cumulare independenţi
  if(indepAgreg == FALSE) {
    tabel = TransformTabel(workCirc)
    numeFisier = paste(numeTabel, "-circ.csv", sep ="")
  }
  else {
    tabel = TransformTabel(workCirc, TRUE)
    numeFisier = paste(numeTabel, "-circ-indepAgreg.csv", sep ="")
  }
  #write.table(x=tabel,file=numeFisier, sep = ";", row.names = FALSE)
  tabel
}

AbsolutSv = function (x, indepAgreg = FALSE) {
  #definirea funcţiei care prelucrează totalurile la nivel de secţie de votare,
  #SV != 0
  #x e tabelul, indepAgreg ne zice dacă cumulăm independenţii
  workSv = x
  numeTabel = deparse(substitute(x))
  #separ datele la nivel de secţie
  workSv = workSv[workSv$SV != 0,]
  #apelare funcţie TransformTabel(), cu sau fără cumulare independenţi
  if(indepAgreg == FALSE) {
    tabel = TransformTabel(workSv)
    numeFisier = paste(numeTabel, "-sv.csv", sep ="")
  }
  else {
    tabel = TransformTabel(workSv, TRUE)
    numeFisier = paste(numeTabel, "-sv-indepAgreg.csv", sep ="")
  }
  #write.table(x=tabel,file=numeFisier, sep = ";", row.names = FALSE)
  tabel
}

CumulatJudet = function (x){
  #definirea funcţiei de stabilire a rezultatelor cumulate pe judeţ acolo unde
  #nu avem înregistrări CIRC = 0, respectiv doar CLO şi PRI
  #obţinem mai întâi datele pe circumscripţie; cumulăm independenţii
  work = AbsolutCirc(x, TRUE)
  numeTabel = deparse(substitute(x))
  #folosim funcţia de agregare
  work = aggregate(subset(work, select = -c(JUD, DEN_JUD, DEN_CIRC, siruta)), 
                    by = list(work$JUD), sum)
  #după agregare mai sunt probleme cu unele coloane
  work$CIRC = 0
  work$NR_CONS = 0
  colnames(work)[1] = "JUD"
  work$DEN_CIRC = ""
  work$siruta = NA
  work$DEN_JUD = c("ALBA", "ARAD", "ARGEŞ", "BACAU", "BIHOR", 
                    "BISTRIŢA-NĂSĂUD", "BOTOŞANI", "BRAŞOV", "BRAILA", "BUZAU", 
                    "CARAŞ-SEVERIN", "CALARAŞI", "CLUJ", "CONSTANŢA", "COVASNA",
                    "DÂMBOVIŢA", "DOLJ", "GALAŢI", "GIURGIU", "GORJ", 
                    "HARGHITA", "HUNEDOARA", "IALOMIŢA", "IAŞI", "ILFOV", 
                    "MARAMUREŞ", "MEHEDINŢI", "MUREŞ", "NEAMŢ", "OLT", 
                    "PRAHOVA", "SATU MARE", "SALAJ", "SIBIU", "SUCEAVA", 
                    "TELEORMAN", "TIMIŞ", "TULCEA", "VASLUI", "VÂLCEA", 
                    "VRANCEA", "MUNICIPIUL BUCUREŞTI")
  work1 = work[,c("JUD", "CIRC", "SV", "DEN_JUD", "DEN_CIRC", "siruta")]
  work2 = subset(work, select = -c(JUD, CIRC, SV, DEN_JUD, DEN_CIRC, siruta))
  work = cbind(work1, work2)
  rm(work1, work2)
  numeFisier = paste(numeTabel, "-CumJud-indepAgreg.csv", sep ="")
  #write.table(x=work, file=numeFisier, sep = ";", row.names = FALSE)
  work
}

CumulatCirc = function (x){
  #definirea funcţiei de stabilire a rezultatelor cumulate pe circumscripţie 
  #acolo unde nu avem înregistrări CIRC != 0 & workCirc$SV == 0,
  #respectiv doar CJU şi PCJ
  #obţinem mai întâi datele pe secţie de votare; cumulăm independenţii
  cumSv = AbsolutSv(x, TRUE)
  numeTabel = deparse(substitute(x))
  #folosim funcţia de agregare
  work = aggregate(subset(cumSv, select = -c(JUD, CIRC, DEN_JUD, DEN_CIRC, 
                                               siruta)), 
                    by = list(cumSv$JUD, cumSv$CIRC), sum)
  #după agregare mai sunt probleme cu unele coloane
  work$SV = 0
  colnames(work)[1] = "JUD"
  colnames(work)[2] = "CIRC"
  #facem merge ca să reintroducem DEN_JUD şi DEN_CIRC
  cumSv = cumSv[,c("JUD", "CIRC", "DEN_JUD", "DEN_CIRC", "siruta")]
  cumSv = unique(cumSv)
  work = merge (work, cumSv)
  rm(cumSv)
  #ordonăm coloanele  
  work1 = work[,c("JUD", "CIRC", "SV", "DEN_JUD", "DEN_CIRC", "siruta")]
  work2 = subset(work, select = -c(JUD, CIRC, SV, DEN_JUD, DEN_CIRC, siruta))
  work = cbind(work1, work2)
  rm(work1, work2)
  work = work[ order(as.numeric(work[,1]), as.numeric(work[,2])), ]
  numeFisier = paste(numeTabel, "-CumCirc-indepAgreg.csv", sep ="")
  #write.table(x=work, file=numeFisier, sep = ";", row.names = FALSE)
  work
}

#definirea funcţiei de trecere la procentaje
Procente = function (x){
  work = x[,c("JUD", "CIRC", "SV", "DEN_JUD", "DEN_CIRC", "siruta", "TAP")]
  work$TAPU_P = x$TAPU / x$TAP #participare la vot / lista perm.
  work$TVVE_P = x$TVVE / x$TAPU #procentaj voturi valabile din total exprimate
  work$TVN_P = x$TVN / x$TAPU #procentaj voturi nule din total exprimate
  work$TAPUP_P = x$TAPUP / x$TAP #participare aleg. permanenti /lista perm.
  work$TAPUCO_P = x$TAPUCO / x$TAP #participare aleg. UE /lista perm.
  work$TAPUSU_P = x$TAPUSU / x$TAP #participare aleg. suplimentari /lista perm.
  work$TAPUSP_P = x$TAPUSP / x$TAP #participare aleg. urnă spec. /lista perm.
  partide  = x[-c(1:21)]
  partide = partide / x$TVVE
  work = cbind(work, partide)
  work
}

SimpLocale2012 = function (x){
  #definirea funcţiei de simplificare pentru locale2012
  #cumulare PDL cu alianţele în care a participat şi PDL
  #cumulare USL, PNL, PSD, PC şi alianţele aferente
  #cumulare minoritîţi, altele decât maghiară
  work = x
  work$MINORITATI = x$UBBR + x$'U-Croati' + x$RO.AS.IT + x$'U-Ucraineni'
  + x$FDGR + x$'Dom Polski' + x$'U-Elena' + x$U.C.R.R. + x$UDSCR +x$A.M.R.
  + x$UDTR + x$CRLR + x$UDTTMR + x$USR + x$ACA + x$FCER 
  work$ROMI = x$'Pro Europa' + x$ACDR + x$UNCR
  work$allPDL = x$PDL + x$'MC-L' + x$'PDL-PNTCD' + x$'Al-Bacau' +   x$BOTOSANI + 
    x$'Al-Braila' + x$'AP-C' + x$ACC + x$AP + x$MPNG + x$USP + x$'M N MH' + 
    x$A.P.E.L. + x$'Al-Teleorman' + x$'Al-Vrancea' + x$USE + x$ApMS + x$ApC + 
    x$ARH + x$'APV PR'+ x$'AP IL' + x$UPBM + x$UPMM + x$APN + x$APM1 + 
    x$'ACPC+PRM CV'
  work$allUSL = x$PNL + x$'PNL-PC' + x$PC + x$PSD + x$USL + x$'PSD+PNL MH' + 
    x$'PSD-PNL VL' + x$'PSD-PC VL' + x$'USL CJ' + x$'USL BR' + x$'PSD+PC DJ' + 
    x$AEG + x$AEF + x$'PSD+PNL AG' + x$'PNL+PP SV'
  work$altele = x$PNDC + x$PPOP + x$PSR + x$UP + x$P.P.P.S. + x$FC + x$'PPE-DE' +
    x$PSDM + x$PAS + x$PUER + x$PTT + x$'PNG-CD' + x$PRE + x$P.PRO. + x$'P-Ren-R'+
    x$'P-Rev-R' + x$FDR + x$USPE + x$AUT
  work$nonUDMR = x$PCM + x$PPMT + x$'EMNP-MPP'
  work$allUNPR = x$UNPR + x$API + x$'PRM-UNPR CV'
  work$allPNT = x$UTM + x$P.N.T.C.D + x$SJ
  work$allVerzii = x$VERZII + x$UIPS
  work$allPRM = x$PRM + x$APM
  colnames(work) = gsub("-| |\\+", "", colnames(work))
  work = subset(work, select = -c(UBBR, UCroati, RO.AS.IT, UUcraineni, 
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