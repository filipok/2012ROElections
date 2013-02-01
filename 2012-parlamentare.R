######################################################
#PRELUCRARE REZULTATE PARLAMENTARE 2012 DE LA BEC
######################################################

#Atenţie! Aici şterg tot ce e în memoria R.
#rm(list = ls(all = TRUE))
nume.sec.parl.2012 <- read.csv("2012AlegeriRomania/2012parlamentare/sv_bec.csv", sep = ";", 
                               stringsAsFactors = FALSE)
nume.sec.parl.2012 <- nume.sec.parl.2012[nume.sec.parl.2012$'Nr..Crt' != "" & 
  nume.sec.parl.2012$'Nr..Crt' != "TOTAL",]
colnames(nume.sec.parl.2012)[2:8] <- c("JUD", "siruta", "DEN_CIRC", "SV", 
                                       "COL_CD", "COL_SE", "Adresa.parl")
parlamentare2012 <- 
  read.csv("2012AlegeriRomania/2012parlamentare/RezultateNivelSectieParlamentare2012.csv", 
           sep = ";", stringsAsFactors = FALSE, encoding  = "UTF-8")
colnames(parlamentare2012)[1:4] <- c("JUD", "DEN_JUD", "SV", "DEN_CIRC")
camere <- split(parlamentare2012, parlamentare2012$numeTipColegiu)
cdep <- camere[[1]]
sen <- camere[[2]]
rm(parlamentare2012, camere)

#în continuare definesc câteva funcţii pentru prelucrare
#în principal, fac un reshape de la format long la wide
TransformTabelParl = function (x, indep.agreg = FALSE){
  #funcţie de transformare a datelor de la BEC în tabel de tip "wide"
  #1. creez baza tabelului cu datele de participare
  tabel.transf <- x[1:19]
  tabel.transf <- unique(tabel.transf)
  #2. fac tabel separat cu independenţii
  work.indep <- x[x$numeCompetitor == "NULL",] 
  #3. fac tabel cu datele privind voturile partidelor (doar coloanele necesare)
  work.partide <- x[x$numeCompetitor != "NULL", 
                    c("JUD", "DEN_CIRC", "SV", "numeCompetitor", 
                      "VoturiCandidat")]
  work.partide <- reshape(work.partide, v.names = "VoturiCandidat", 
                          idvar = c("JUD", "DEN_CIRC", "SV"), 
                          timevar = "numeCompetitor", direction = "wide")
  #4. unesc tabel cu date  participare cu tabel cu voturi partide
  tabel.transf <- merge(tabel.transf, work.partide, all.x = TRUE, sort = FALSE)
  rm(work.partide)
  #5. acum trebuie să adăugăm una sau mai multe coloane pentru independenţi
  #avem două cazuri, cu sau fără agregare independenţi
  if(indep.agreg == FALSE) {
    #numele coloanelor de independenţi: DEN_JUD + COLEGIU + NUME_CANDIDAT
    indep.labels <- apply(work.indep, 1, function(qqq) paste(qqq[2],"_", qqq[5], 
                                                             "_", qqq[20], 
                                                             sep = ""))  
    work.indep <- cbind(work.indep, indep.labels)
    work.indep <- work.indep[, c("JUD", "SV", "VoturiCandidat", "indep.labels")]
    work.indep <- reshape(work.indep, v.names = "VoturiCandidat", 
                          idvar = c("JUD", "SV"), timevar = "indep.labels", 
                          direction = "wide")
    #printăm număr independenţi, aşa, de verificare
    indep.labels <- unique(indep.labels)
    print(paste("Avem", length(indep.labels), "independenti."))
  }
  else {
    #dacă vrem să agregăm independenţii
    work.indep <- aggregate(work.indep$VoturiCandidat, 
                            by = list(work.indep$JUD, work.indep$SV), sum)
    colnames(work.indep) <- c("JUD", "SV","INDEPENDENTI")
  }
  #unim tabelul de independenţi la tabelul general
  tabel.transf <- merge(tabel.transf, work.indep, sort = TRUE, all.x = TRUE)
  rm(work.indep)
  #6. finalizare
  #eliminăm "VoturiCandidat." din numele coloanelor
  colnames(tabel.transf) <- gsub("VoturiCandidat.", "", colnames(tabel.transf))
  #înlocuim NA cu zero
  tabel.transf[is.na(tabel.transf)] <- 0
  #adăugăm adresa secţiei
  tabel.transf <- merge (tabel.transf, nume.sec.parl.2012[,c(2, 5, 8)], 
                         all.x = TRUE)
  tabel.transf
}

CumulatCirc = function(x){
  circ <- aggregate(subset(x, select = -c(JUD, SV, DEN_CIRC, DEN_JUD,
                                          numarColegiu, numeTipColegiu, 
                                          Adresa.parl)), 
                    by = list(x$JUD, x$DEN_JUD, x$DEN_CIRC), sum)
  colnames(circ)[1:3] <- c("JUD", "DEN_JUD", "DEN_CIRC")
  circ <- circ[ order(as.numeric(circ[,1]), circ[,3]), ]
  circ
}

CumulatColegiu = function(x){
  colegiu <- aggregate(subset(x, select = -c(JUD, SV, DEN_CIRC, DEN_JUD,
                                          numarColegiu, numeTipColegiu, 
                                          Adresa.parl)), 
                    by = list(x$JUD, x$DEN_JUD, x$numarColegiu, 
                              x$numeTipColegiu), sum)
  colnames(colegiu)[1:4] <- c("JUD", "DEN_JUD", "numarColegiu", 
                              "numeTipColegiu")
  colegiu <- colegiu[ order(as.numeric(colegiu[,1]), colegiu[,3]), ]
  colegiu
}

CumulatJudet = function(x){
  judet <- aggregate(subset(x, select = -c(JUD, SV, DEN_CIRC, DEN_JUD,
                                             numarColegiu, numeTipColegiu, 
                                             Adresa.parl)), 
                       by = list(x$JUD, x$DEN_JUD), sum)
  colnames(judet)[1:2] <- c("JUD", "DEN_JUD")
  judet
}

#Producerea datelor pentru fiecare nivel (secţie, localitate, colegiu, judeţ)
#date pe secţie de votare
sen.sv <- TransformTabelParl(sen)
cdep.sv <-TransformTabelParl(cdep)
#date pe localitate
sen.circ <- CumulatCirc(sen.sv)
cdep.circ <- CumulatCirc(cdep.sv)
#date pe colegiu
sen.colegiu <- CumulatColegiu(sen.sv)
cdep.colegiu <- CumulatColegiu(cdep.sv)
#date pe judeţ
sen.judet <- CumulatJudet(sen.sv)
cdep.judet <- CumulatJudet(cdep.sv)

######################################################
#IMPORT SECTII DE VOTARE PARLAMENTARE 2012 DE LA ROAEP
######################################################
library(xlsReadWrite) #Este necesară utilizarea pachetului xlsReadWrite
#Sursa datelor: http://www.roaep.ro/ro/section.php?id=25&l2=48&ids=121&an=2012
#(descărcate la 27.12.2012)
lista <- list.files("2012AlegeriRomania\\2012parlamentare\\SV_ROAEP\\judete") #A se schimba după caz
judete <- list()
#Fişierele ROAEP sunt în general similare, dar mai apar diferenţe în privinţa
#sheet-ului pe care sunt datele (sheet.ales) şi a rândului de la care încep
#datele propriu-zise (rand.initial)
for (i in lista){
  rand.initial <- 5
  sheet.ales <- 2
  if (i == "ag_parlam_2012_v2.xls" | i == "ar_parlam2012_v7.xls" | 
    i == "hr_parlam2012_v2.xls") {
    rand.initial <- 3}
  if (i == "ct_parlam_2012_final.xls" | i == "mm_parlam2012_v5.xls") {
    rand.initial <-6}
  if (i == "bn_parlam2012_v3.xls") {
    rand.initial <-4}
  if (i == "br_parlam2012_v2.xls" | i == "db_parlam2012_v3.xls" | 
    i == "sm_sectii_votare_v4.xls" | i == "tl_parlam2012_v1.xls") {
    rand.initial <-2}
  if (i == "mures_parlam_2012_modif.xls" | i == "sm_sectii_votare_v4.xls"){
    sheet.ales <- 3}
  if (i == "nt_parlam2012_v4.xls"){
    sheet.ales <- 1
    rand.initial <- 8}
  if (i == "tl_parlam2012_v1.xls"){
    sheet.ales <- 1}
  nume <- paste ("2012AlegeriRomania\\2012parlamentare\\SV_ROAEP\\judete\\", i, sep = "")
  judete[[i]] <- read.xls(nume, sheet = sheet.ales, from = rand.initial, 
                          stringsAsFactors = FALSE, colClasses = "character")
  if (i != "ct_parlam_2012_final.xls" & i != "vs_parlam2012_v3.xls"){
    judete[[i]] <- judete[[i]][-1,]} #mai scoatem un rând în plus
  
  colnames(judete[[i]])[1:17] <- c("JUD", "ColCD", "ColSE", "SV", "tipUAT", 
                                   "DEN_CIRC", "SirutaUAT", "tipComp", 
                                   "DenComp", "SirutaComp", "Sediu", 
                                   "Precizari", "TipArtera", "DenArtera", "Nr", 
                                   "FosteDenArtera", "Obs")
}

#Pregătim 3 judeţe pentru loop-ul următor
#Au pus un rând pentru fiecare Siruta, chiar dacă aveau aceeaşi secţie de votare
#Aşa că pun "" la valoarea secţiei, pentru a şterge rândurile la loop-ul
#următor
for (k in c(13, 27, 38)){
  for (i in (nrow(judete[[k]])-1):1){
    if (judete[[k]][i+1,4] ==judete[[k]][i,4]){
      judete[[k]][i+1,4] <- ""
    }
  }
}

#Aici scot: (a) rândurile goale de la final;
# (b) alte rânduri goale care apar atunci când în Excel au făcut mai multe
#rânduri pentru aceeaşi secţie, câte un rând pe cod Siruta; în situaţia asta,
#au completat cu nr. secţiei doar primul rând; am considerat că acest prim
#rând indică localizarea reală a secţiei; a se vedea, de exemplu, secţia 328
#din judeţul Bacău;
#(c) rândurile marcate la loop-ul de mai sus.
for (i in 1:42){
  judete[[i]] <- judete[[i]][judete[[i]]$SV != "",]}

#Corecturi manuale ale codurilor Siruta de la ROAEP pe baza Siruta2008
#Acolo unde erau mai multe coduri, l-am ales pe cel în care era efectiv secţia
#Acolo unde localitatea nu are cod Siruta am pus codul localităţii principale
#Unele modificări se puteau automatiza, dar am subestimat numărul lor.
judete[[1]][123,10] <- 146
judete[[2]][333,10] <- 1651
judete[[3]][1, 10] <- 927
judete[[3]][200, 10] <- 1003
judete[[3]][230, 10] <- 1038
judete[[3]][246, 10] <- 1061
judete[[3]][258, 10] <- 1074
judete[[3]][276, 10] <- 1096
judete[[3]][281, 10] <- 1104
judete[[3]][282, 10] <- 1106
judete[[3]][284, 10] <- 1116
judete[[3]][287, 10] <- 1107
judete[[3]][289, 10] <- 1118
judete[[3]][334, 10] <- 1177
judete[[3]][335, 10] <- 1183
judete[[3]][337, 10] <- 1181
judete[[3]][338, 10] <- 1185
judete[[3]][342, 10] <- 1191
judete[[3]][375, 10] <- 1229
judete[[3]][385, 10] <- 1241
judete[[5]][1,10] <- 2030
judete[[5]][125,10] <- 2088
judete[[5]][143,10] <- 2057
judete[[5]][179,10] <- 2078
judete[[5]][191,10] <- 2083
judete[[5]][208,10] <- 2217
judete[[5]][214,10] <- 2219
judete[[5]][215,10] <- 2221
judete[[5]][216,10] <- 2217
judete[[5]][220,10] <- 2092
judete[[5]][221,10] <- 2093
judete[[5]][222,10] <- 2094
judete[[5]][223,10] <- 2097
judete[[5]][429, 10] <- 2042
judete[[7]][1,10] <- 3240
judete[[7]][58,10] <- 3249
judete[[7]][69,10] <- 3255
judete[[7]][77,10] <- 3260
judete[[8]][172, 10] <- 4271
judete[[8]][173, 10] <- 4271
judete[[8]][254, 10] <- 4426
judete[[9]][221,10] <- 3706
judete[[9]][287,10] <- 3792 #BAZNOASA LUNCA
judete[[9]][290,10] <- 3794 #ZLATUNOAIA STANESTI
judete[[9]][293,10] <- 3803
judete[[9]][364,10] <- 3906
judete[[11]][110,10] <- 4792
judete[[11]][416, 10] <- 5046
judete[[11]][81:101,10] <- 4485
judete[[12]][203, 10] <- 10523
judete[[12]][204, 10] <- 10524
judete[[13]][1,10] <- 5498
judete[[13]][578,10] <- 5914
judete[[13]][550, 10] <- 5877
judete[[14]][331, 10] <- 5417
judete[[14]][332, 10] <- 5421
judete[[17]][92,10] <- 6569
judete[[17]][150,10] <- 6601
judete[[17]][151,10] <- 6601
judete[[17]][174,10] <- 10188 
judete[[17]][180,10] <- 6670
judete[[17]][181,10] <- 6670
judete[[17]][182,10] <- 6672
judete[[17]][183,10] <- 6672
judete[[17]][184,10] <- 6671
judete[[17]][185,10] <- 6671
judete[[17]][186,10] <- 6674
judete[[17]][187,10] <- 6674
judete[[17]][188,10] <- 6675
judete[[17]][206,10] <- 6704
judete[[17]][212,10] <- 6711
judete[[17]][225,10] <- 6726
judete[[17]][256,10] <- 6760
judete[[17]][266,10] <- 6771
judete[[17]][283,10] <- 6791
judete[[17]][286,10] <- 6793
judete[[17]][328,10] <- 6576
judete[[17]][330,10] <- 6579
judete[[17]][339,10] <- 6844
judete[[17]][347:349,10] <- c(6853, 6854, 6855)
judete[[17]][356,10] <- 6937
judete[[17]][372,10] <- 6881
judete[[17]][375:377,10] <- c(6889, 6890, 6891)
judete[[17]][395:398,10] <- c(6551, 6554, 6558, 6558)
judete[[17]][399:403,10] <- c(6907, 6916, 6915, 6914, 6912)
judete[[17]][416,10] <- 69367
judete[[17]][417:419,10] <- c(6940, 6943, 6942)
judete[[17]][145, 10] <- 6641
judete[[17]][224, 10] <- 6726
judete[[17]][373, 10] <- 10515
judete[[17]][374, 10] <- 10515
judete[[17]][410, 10] <- 6931
judete[[19]][208, 10] <- 8031
judete[[22]][1,10] <- 8669
judete[[22]][42,10] <- 8669
judete[[22]][128,10] <- 8764
judete[[22]][140,10] <- 8700
judete[[22]][166,10] <- 8718
judete[[22]][188,10] <- 8743
judete[[22]][222,10] <- 8708
judete[[22]][236,10] <- 8708
judete[[22]][238,10] <- 8767
judete[[22]][248,10] <- 8714
judete[[22]][259,10] <- 8785
judete[[22]][261,10] <- 8790
judete[[22]][264,10] <- 8786
judete[[22]][265,10] <- 8792
judete[[22]][266,10] <- 8793
judete[[22]][279,10] <- 8725
judete[[22]][303,10] <- 8860
judete[[22]][361,10] <- 8943
judete[[22]][363,10] <- 8951
judete[[22]][364,10] <- 8953
judete[[22]][365,10] <- 8952
judete[[22]][366,10] <- 8949
judete[[22]][374,10] <- 8969
judete[[22]][375,10] <- 8974
judete[[22]][376,10] <- 8970
judete[[22]][377,10] <- 8972
judete[[22]][382,10] <- 8986
judete[[22]][388,10] <- 8992
judete[[22]][443,10] <- 9088
judete[[22]][445,10] <- 9092
judete[[22]][448,10] <- 9095
judete[[22]][501,10] <- 9172
judete[[22]][501,10] <- 9210
judete[[22]][518,10] <- 9210
judete[[22]][326, 10] <- 8893
judete[[22]][453, 10] <- 9106
judete[[22]][356, 10] <- 8935
judete[[22]][405, 10] <- 9035
judete[[23]][91, 10] <- 8383
judete[[23]][210, 10] <- 8535
judete[[23]][211, 10] <- 8535
judete[[23]][287, 10] <- 8638
judete[[23]][138, 10] <- 8320
judete[[23]][143, 10] <- 8329
judete[[23]][169, 10] <- 8476
judete[[25]][100,10]<- 10093 #Frumuşica din Axintele, Ialomiţa fără Siruta?
judete[[25]][128,10]<- 9316 #Ghimpaţi din Ciulniţa, Ialomiţa fără Siruta?
judete[[26]][1,10] <- 9507
judete[[26]][237,10] <- 9540
judete[[26]][282,10] <- 9548
judete[[26]][388,10] <- 9676
judete[[26]][391,10] <- 9677
judete[[26]][444,10] <- 9741
judete[[26]][518,10] <- 9815
judete[[26]][520,10] <- 9817
judete[[26]][550:552,10] <- c(9861, 9862, 9863)
judete[[26]][567,10] <- 9526
judete[[26]][605,10] <- 9911
judete[[26]][606,10] <- 9915
judete[[26]][633,10] <- 9950
judete[[26]][675,10] <- 9601
judete[[26]][676,10] <- 9601
judete[[26]][677,10] <- 9602
judete[[26]][678,10] <- 9603
judete[[26]][679,10] <- 9604
judete[[26]][698,10] <- 10017
judete[[26]][686,10] <- 10009
judete[[26]][687,10] <- 10010
judete[[26]][385, 10] <- 9667
judete[[26]][386, 10] <- 9669
judete[[26]][413:414, 10] <- 9701
#Aici am pus codul pentru satul Bârnova, com. Bârnova din siruta2008
judete[[26]][436, 10] <- 9509 #com. Grajduri, sat Bârnova, spital Bârnova??
#
judete[[26]][491:494, 10] <- c(9773, 9774 , 9775, 9776)
judete[[26]][531, 10] <- 9831
judete[[26]][573, 10] <- 9878
judete[[26]][688, 10] <- 10011
judete[[26]][689, 10] <- 10012
judete[[26]][293, 10] <- 9567
judete[[27]][179,10] <- 11187 #Grădiniţa P.F. II, Gogoşu, MH fără Siruta
judete[[28]][274, 10] <- 10882
judete[[28]][353, 10] <- 10642
judete[[28]][354, 10] <- 10643
judete[[28]][370, 10] <- 10862
judete[[29]][414, 10] <- 11738
judete[[29]][415, 10] <- 11739
judete[[29]][428, 10] <- 11767
judete[[30]][146, 10] <- 12108
judete[[30]][172, 10] <- 12139
judete[[30]][226, 10] <- 12089
judete[[30]][1, 10] <- 12073
judete[[30]][38, 10] <- 12073
judete[[30]][65, 10] <- 12073
judete[[32]][285, 10] <- 13190
judete[[32]][379, 10] <- 13272
judete[[32]][489, 10] <- 13085
judete[[32]][550, 10] <- 13532
judete[[32]][551, 10] <- 13532 #Gârbeasca, Starchiojd, PH fără cod Siruta
judete[[32]][557, 10] <- 13549
judete[[32]][574,10] <- 13090
judete[[32]][575,10] <- 13090
judete[[33]][1:104, 10] <- 14346
judete[[34]][3,10] <- 13971
judete[[34]][56,10] <- 13989
judete[[34]][68,10] <- 14002
judete[[34]][69,10] <- 14003
judete[[34]][100,10] <- 14040
judete[[34]][106,10] <- 14051
judete[[34]][121,10] <- 14073
judete[[34]][138,10] <- 14092
judete[[34]][140,10] <- 14096
judete[[34]][143,10] <- 14099
judete[[34]][148,10] <- 14107
judete[[34]][155,10] <- 14116
judete[[34]][159,10] <- 14121
judete[[34]][167,10] <- 14132
judete[[34]][189,10] <- 14159
judete[[34]][191,10] <- 14163
judete[[34]][199,10] <- 14173
judete[[34]][212,10] <- 14189
judete[[34]][215,10] <- 14195
judete[[34]][241,10] <- 14229
judete[[34]][247,10] <- 14238
judete[[34]][289,10] <- 14290
judete[[34]][291,10] <- 14292
judete[[34]][302,10] <- 14304
judete[[34]][306,10] <- 14309
judete[[34]][307,10] <- 14312
judete[[34]][308,10] <- 14315
judete[[36]][169:170, 10] <- c(14642, 14641)
judete[[36]][405:406, 10] <- 14984
judete[[36]][482, 10] <- 15066
judete[[36]][483, 10] <- 15063
judete[[38]][314, 10] <- 15570
judete[[38]][316, 10] <- 15574
judete[[38]][335, 10] <- 15596
judete[[38]][338, 10] <- 15600
judete[[38]][396, 10] <- 15693
judete[[39]][116, 10] <- 15226
judete[[40]][1, 10] <- 16748
judete[[40]][88, 10] <- 16799
judete[[40]][109, 10] <- 16838
judete[[40]][117, 10] <- 16846
judete[[40]][129, 10] <- 16780
judete[[40]][135, 10] <- 16796 #Seaca, Călimăneşti
judete[[40]][136, 10] <- 16791
judete[[40]][144, 10] <- 16805
judete[[40]][152, 10] <- 16817 #am pus Siruta pentru Gura Suhaşului, Ocnele Mari
judete[[40]][153, 10] <- 16819
judete[[40]][392, 10] <- 17380
judete[[40]][393, 10] <- 17380
judete[[40]][364, 10] <- 17307
judete[[41]][99, 10] <- 17513
judete[[41]][101, 10] <- 17513
judete[[41]][100, 10] <- 17518
judete[[41]][102, 10] <- 17521
judete[[41]][103, 10] <- 17521
judete[[41]][123, 10] <- 17560
judete[[41]][124, 10] <- 17561
judete[[41]][125, 10] <- 17562
judete[[41]][126, 10] <- 17563
judete[[41]][127, 10] <- 17564
judete[[41]][128, 10] <- 17565
judete[[41]][129, 10] <- 17566
judete[[41]][151, 10] <- 17597
judete[[41]][229, 10] <- 17711
judete[[41]][230, 10] <- 17716
judete[[41]][234, 10] <- 17724 #Surlea, Păuneşti nu are cod Siruta
judete[[41]][235, 10] <- 17724 #Novăceşti, Păuneşti nu are cod Siruta
judete[[41]][236, 10] <- 17724 #Bostăneşti, Păuneşti nu are cod Siruta
judete[[41]][106, 10] <- 17523
judete[[41]][107, 10] <- 17527
judete[[41]][116, 10] <- 17544
judete[[41]][120, 10] <- 17551
judete[[41]][145, 10] <- 17589
judete[[41]][163, 10] <- 17616
judete[[41]][242, 10] <- 17727
judete[[41]][276:277, 10] <- 17767
judete[[41]][280, 10] <- 17777
judete[[41]][300, 10] <- 17805
judete[[41]][327, 10] <- 17848
judete[[42]][149, 10] <- 16220
judete[[42]][182, 10] <- 16260
judete[[42]][184, 10] <- 16263
judete[[42]][186, 10] <- 16264
judete[[42]][188, 10] <- 16271
judete[[42]][189, 10] <- 16277
judete[[42]][191, 10] <- 16276
judete[[42]][193, 10] <- 16280
judete[[42]][194, 10] <- 16283
judete[[42]][196, 10] <- 16281
judete[[42]][197, 10] <- 16288
judete[[42]][201, 10] <- 16296
judete[[42]][202, 10] <- 16294
judete[[42]][204, 10] <- 16529
judete[[42]][211, 10] <- 16306
judete[[42]][212, 10] <- 16307
judete[[42]][223, 10] <- 16341
judete[[42]][230, 10] <- 16327
judete[[42]][232, 10] <- 16333
judete[[42]][233, 10] <- 16334
judete[[42]][252, 10] <- 16350
judete[[42]][255, 10] <- 16356
judete[[42]][256, 10] <- 16365
judete[[42]][257, 10] <- 16362
judete[[42]][258, 10] <- 16366
judete[[42]][263, 10] <- 16374
judete[[42]][281, 10] <- 16391
judete[[42]][285, 10] <- 16397
judete[[42]][300, 10] <- 16421
judete[[42]][314, 10] <- 16443
judete[[42]][317, 10] <- 16446
judete[[42]][318, 10] <- 16453
judete[[42]][319, 10] <- 16451
judete[[42]][320, 10] <- 16442
judete[[42]][324, 10] <- 16465
judete[[42]][334, 10] <- 16478
judete[[42]][335, 10] <- 16476
judete[[42]][348, 10] <- 16495
judete[[42]][357, 10] <- 16514
judete[[42]][362, 10] <- 16521
judete[[42]][374, 10] <- 16545
judete[[42]][375, 10] <- 16457
judete[[42]][392, 10] <- 16549
judete[[42]][397, 10] <- 16570
judete[[42]][411, 10] <- 16579
judete[[42]][434, 10] <- 16606
judete[[42]][435, 10] <- 16607
judete[[42]][437, 10] <- 16608
judete[[42]][448, 10] <- 16629
judete[[42]][449, 10] <- 16632
judete[[42]][451, 10] <- 16638
judete[[42]][480, 10] <- 16681
judete[[42]][482, 10] <- 16685
judete[[42]][484, 10] <- 16688
judete[[42]][487, 10] <- 16696
judete[[42]][489, 10] <- 16693
judete[[42]][491, 10] <- 16699
judete[[42]][504, 10] <- 16716
judete[[4]][10] <- 17913 #pentru Bucureşti

#în unele judeţe au fost completate codurile Siruta doar la prima înregistrare
#cu localitatea respectivă. Pentru următoarele rămase goale am folosit loop-ul
# acesta. În altele nu erau deloc şi am completat pentru prima înregistrare 
#(în pasul de mai sus) şi pe urmă la fel. 
for (k in 1:42){
  for (i in 2:nrow(judete[[k]])){
    if (judete[[k]][i,10] == ""){
      judete[[k]][i,10] <- judete[[k]][i-1,10]
      
    }
  }
}
# La unele judeţe nu este trecut codul judeţului în toate înregistrările (sau 
#e greşit la unele înregistrări).
judete[[1]][1] <- 1
judete[[2]][1] <- 3
judete[[5]][1] <- 4
judete[[13]][1] <- 13
judete[[16]][1] <- 15
judete[[17]][1] <- 16
judete[[21]][1] <- 19
judete[[26]][1] <- 24
judete[[28]][1] <- 26
judete[[29]][1] <- 28
judete[[34]][1] <- 33
judete[[33]][1] <- 34
judete[[38]][1] <- 37
judete[[42]][1] <- 39

#Rânduri dublate şi alte rânduri în plus
judete[[4]] <- judete[[4]][-c(179, 180, 223, 224),]
judete[[10]] <- judete[[10]][-447,]

#Alte corecturi
judete[[5]][605,4] <- 605 #număr greşit SV


#o verificare
#for (i in 1:42){
#  print(paste("judetul",i, "incepe cu sectia",judete[[i]][1,4]))
#  print(paste("judetul",i, "se termina cu sectia", 
#              judete[[i]][nrow(judete[[i]]),4], "si are", nrow(judete[[i]]), 
#              "sectii"))
#}


#Mai sunt unele judeţe unde au folosit (şi) codul Siruta lung; scurtăm după caz
#Se poate folosi substr() în locul strtrim()
for (i in c(4, 29, 30, 32, 36, 41)){
  judete[[i]][10] <- sapply(judete[[i]][10], function(qqq) strtrim(qqq,5))
}
for (i in c(15, 22)){
judete[[i]][10] <- sapply(judete[[i]][10], function(qqq) strtrim(qqq,4))
}
#pentru judeţul 17, alea cu 6 scad de la 5 la 4 cifre, alea cu 1 de la 6 la 5
judete[[17]][10] <- sapply(judete[[17]][10], function(qqq) strtrim(qqq,5))
for (i in 1:nrow(judete[[17]][10])){
  if (strtrim(judete[[17]][i, 10], 1) == "6"){
    judete[[17]][i, 10] <- strtrim(judete[[17]][i, 10], 4)
  }
}
#pentru jud 26, alea cu 9 scad de la 5 la 4 cifre, alea cu 1 de la 6 la 5
judete[[26]][10] <- sapply(judete[[26]][10], function(qqq) strtrim(qqq,5))
for (i in 1:nrow(judete[[26]][10])){
  if (strtrim(judete[[26]][i, 10], 1) == "9"){
    judete[[26]][i, 10] <- strtrim(judete[[26]][i, 10], 4)
  }
}

# Unim cele 42 de judete[[x]]
romania <- judete[[1]][c("JUD", "SV", "SirutaComp")]
for (i in 2:42){
  romania <- rbind(romania, judete[[i]][c("JUD", "SV", "SirutaComp")])
}
#Facem un JOIN între cdep.sv/sen.sv şi judete pentru codul Siruta
cdep.sv <- merge(cdep.sv, romania, all.x = TRUE)
sen.sv <- merge(sen.sv, romania, all.x = TRUE)

##################
#Import Siruta2008
##################
#Fişierul este de la http://earth.unibuc.ro/download/romania-seturi-vectoriale
siruta2008 <- read.csv("2012AlegeriRomania/siruta2008/localitati2008_iso8859_2.csv", sep = ",", 
                      stringsAsFactors = FALSE)
#Scurtăm codul Siruta, pentru a corespunde formatului de la ROAEP
siruta2008[4] <- sapply(siruta2008[4], function(qqq) strtrim(qqq,nchar(qqq)-1))

#######################################################################
#Facem JOIN-ul final între cdep.sv/sen.sv şi siruta2008 pentru lat/long
#######################################################################
colnames(siruta2008)[4] <- "SirutaComp" #pregătire nume coloană pentru merge
cdep.sv <- merge(cdep.sv, siruta2008[c("X", "Y", "SirutaComp")], all.x = TRUE, 
                 sort = FALSE)
sen.sv <- merge(sen.sv, siruta2008[c("X", "Y", "SirutaComp")], all.x = TRUE, 
                 sort = FALSE)
#Verificări
#print(table(cdep.sv[is.na(cdep.sv$X) & cdep.sv$JUD != 42 & 
#                      cdep.sv$JUD != 43,]["DEN_JUD"]))
#print(table(sen.sv[is.na(sen.sv$X) & cdep.sv$JUD != 42 & 
#                       sen.sv$JUD != 43,]["DEN_JUD"]))
#pdf("2012-parlamentare.pdf")
#symbols(cdep.sv$X, cdep.sv$Y, 
#        circles=sqrt(cdep.sv$NrAlegatoriPrezentatiLaUrne)/2000, inches=FALSE,
#        main="Romania", xlab="longitudine", ylab="latitudine", 
#        bg = rgb(0,0,0,0.2), col = rgb(0,0,0,0.2))
#dev.off()
#
#pdf("2012-parlamentare-judete.pdf")
#for(judet in unique(cdep.sv$DEN_JUD[cdep.sv$DEN_JUD != "Strainatate (CE. 43)"])){
#  attach(cdep.sv[cdep.sv$DEN_JUD == judet,])
#  symbols(X, Y, circles=sqrt(NrAlegatoriPrezentatiLaUrne)/2000, inches=FALSE,
#          main=paste("Judeţul", judet), 
#          xlab="", ylab="", bg = rgb(0,0,0,0.2), col = rgb(0,0,0,0.2))
#  text(X, Y, labels = SV, cex = 0.7)
#    detach()
#}
#dev.off()

#ştergem variabile temporare
rm(romania, judete, i, k, lista, nume, rand.initial, sheet.ales)

###########
#EXPORT CSV
###########
#La nivel de secţie de votare, cu cod SIRUTA şi lat/long
#write.table(x=cdep.sv, file="2012AlegeriRomania/CSV/2012cdepsv.csv", row.names = FALSE)
#write.table(x=sen.sv, file="2012AlegeriRomania/CSV/2012sensv.csv", row.names = FALSE)
#La nivel de oraş/comună
#write.table(x=cdep.circ, file="2012AlegeriRomania/CSV/2012cdepcirc.csv", row.names = FALSE)
#write.table(x=sen.circ, file="2012AlegeriRomania/CSV/2012sencirc.csv", row.names = FALSE)
#La nivel de colegiu
#write.table(x=cdep.colegiu, file="2012AlegeriRomania/CSV/2012cdepcolegiu.csv", row.names = FALSE)
#write.table(x=sen.colegiu, file="2012AlegeriRomania/CSV/2012sencolegiu.csv", row.names = FALSE)
#La nivel de judeţ
#write.table(x=cdep.judet, file="2012AlegeriRomania/CSV/2012cdepjudet.csv", row.names = FALSE)
#write.table(x=sen.judet, file="2012AlegeriRomania/CSV/2012senjudet.csv", row.names = FALSE)
