rm(list = ls(all = TRUE))
################################################################################
#Apel cod pentru locale, referendum ºi parlamentare
source("2012AlegeriRomania/2012-locale-1.0.R")
source("2012AlegeriRomania/2012-referendum-0.3.R")
source("2012AlegeriRomania/2012-parlamentare-0.4.R")
################################################################################


#Un data frame pentru fiecare listã de secþii de votare, incl. nr. de alegãtori
a.loc <- merge(nume.sec.loc.2012, 
                            aleg.loc.2012[c("DEN_JUD", "CIRC", 
                                            "SV", "Numar.alegatori")], 
               sort = FALSE)
a.par <- nume.sec.parl.2012
a.ref <- ref.2012.sv[1:9]

#ªterg datele care nu-mi folosesc pe moment
rm(CJU, CLO, PCJ, PRI, TUR2, aleg.loc.2012, cdep, cdep.circ, cdep.colegiu, 
   cdep.judet, cdep.sv, echivalare.coduri, new.prim, nume.sec.loc.2012,
   nume.sec.parl.2012, ref.2012.circ, ref.2012.circ.pro, ref.2012.jud,
   ref.2012.jud.pro, ref.2012.nat, ref.2012.sv, ref.2012.sv.nat, 
   ref.2012.sv.pro, sen, sen.circ, sen.colegiu, sen.judet, sen.sv, AbsolutCirc, 
   AbsolutJudet, AbsolutSv, CumulatCirc, CumulatColegiu, CumulatJudet, Procente,
   Procente.Ref, SimpLocale2012, TransformTabel, TransformTabelParl)

Ghilimele = function(x){
  x <- gsub("„", "=",x)
  x <- gsub("”", "=",x)
  x <- gsub("“", "=",x)
  x <- gsub(",,", "=",x)
  x <- gsub("’’", "=",x)
  x <- gsub("\"", "=",x)
  x  
}

Prelucrari = function(x){
  x <- chartr("ãâîºþÃÂÎªÞ", "aaistAAist", x) #elimin diacriticele
  x <- toupper(x) #majuscule
  x <- gsub("^( +)", "", x)#spaþii de la începutul adresei
  x <- gsub(" ( +)", " ", x)#spaþii duble
  x <- Ghilimele(x)
  x <- gsub("([0-9A-Z])=([0-9A-Z])", "\\1 =\\2", x) #ghilimele fãrã spaþii înainte
}

#Prelucrari adresa
a.par$Adresa.parl <- Prelucrari(a.par$Adresa.parl)
a.loc$adresa <- Prelucrari(a.loc$adresa)
a.ref$adresa <- Prelucrari(a.ref$adresa)

#de individualizat coloanele SV ºi adresa
colnames(a.par)[5] <- "SV.par"
colnames(a.loc)[3] <- "SV.loc"
colnames(a.ref)[6] <- "SV.ref"
colnames(a.loc)[7] <- "adresa.loc"
colnames(a.par)[8] <- "adresa.par"
colnames(a.ref)[7] <- "adresa.ref"

#Prelucrãri DEN_CIRC
a.par$DEN_CIRC <- Prelucrari(a.par$DEN_CIRC)
a.ref$DEN_CIRC_R <- Prelucrari(a.ref$DEN_CIRC_R)
a.loc$DEN_CIRC <- Prelucrari(a.loc$DEN_CIRC)

#########AICI AM RÃMAS#############


# #Aici am început sã corectez denumirile localitãþilor de la parlamentare
# #în funcþie de ortografia de la referendum, dar pe urmã mi-am dat seama
# #cã pot folosi codul Siruta ca cheie comunã ºi nu am continuat. Au rãmas
# #oricum puþine denumiri diferite.
# a.par[a.par$DEN_CIRC == "ORAS CIMPENI",][4] <- "ORAS CAMPENI"
# a.par[a.par$DEN_CIRC == "CERU BACAINTI",][4] <- "CERU-BACAINTI"
# a.par[a.par$DEN_CIRC == "GIRBOVA",][4] <- "GARBOVA"
# a.par[a.par$DEN_CIRC == "GIRDA DE SUS",][4] <- "GARDA DE SUS"
# a.par[a.par$DEN_CIRC == "HOPIRTA",][4] <- "HOPARTA"
# a.par[a.par$DEN_CIRC == "SINCEL",][4] <- "SANCEL"
# a.par[a.par$DEN_CIRC == "BELETI-NEGRESTI",][4] <- "BELETI – NEGRESTI"
# a.par[a.par$DEN_CIRC == "CAPILNA",][4] <- "CAPALNA"
# a.par[a.par$DEN_CIRC == "CIMPANI",][4] <- "CAMPANI"
# a.par[a.par$DEN_CIRC == "SIMBATA",][4] <- "SAMBATA"
# a.par[a.par$DEN_CIRC == "SINNICOLAU ROMAN",][4] <- "SANNICOLAU ROMAN"
# a.par[a.par$DEN_CIRC == "MUNICIPIUL CAMPIA-TURZII",][4] <- "MUNICIPIUL CAMPIA TURZII"
# a.par[a.par$DEN_CIRC == "MAGURI RACATAU",][4] <- "MAGURI-RACATAU"
# a.par[a.par$DEN_CIRC == "GIURGIU",][4] <- "MUNICIPIUL GIURGIU"
# a.par[a.par$DEN_CIRC == "BOLINTIN-VALE",][4] <- "ORAS BOLINTIN-VALE"
# a.par[a.par$DEN_CIRC == "CAPILNITA",][4] <- "CAPALNITA"
# a.par[a.par$DEN_CIRC == "CIUCSINGEORGIU",][4] <- "CIUCSANGEORGIU"
# a.par[a.par$DEN_CIRC == "MUNTENI BUZAU",][4] <- "MUNTENI-BUZAU"
# a.par[a.par$DEN_CIRC == "ALEXANDRU IOAN CUZA",][4] <- "ALEXANDRU I. CUZA"
# a.par[a.par$DEN_CIRC == "BAIA MARE",][4] <- "MUNICIPIUL BAIA MARE"
# a.par[a.par$DEN_CIRC == "SIGHETU MARMATIEI",][4] <- "MUNICIPIUL SIGHETU MARMATIEI"
# a.par[a.par$DEN_CIRC == "BAIA SPRIE",][4] <- "ORAS BAIA SPRIE"
# a.par[a.par$DEN_CIRC == "CAVNIC",][4] <- "ORAS CAVNIC"
# a.par[a.par$DEN_CIRC == "SALISTEA DE SUS",][4] <- "ORAS SALISTEA DE SUS"
# a.par[a.par$DEN_CIRC == "SEINI",][4] <- "ORAS SEINI"
# a.par[a.par$DEN_CIRC == "SOMCUTA MARE",][4] <- "ORAS SOMCUTA MARE"
# a.par[a.par$DEN_CIRC == "TAUTII MAGHERAUS",][4] <- "ORAS TAUTII-MAGHERAUS"
# a.par[a.par$DEN_CIRC == "TARGU LAPUS",][4] <- "ORAS TARGU LAPUS"
# a.par[a.par$DEN_CIRC == "VISEU DE SUS",][4] <- "ORAS VISEU DE SUS"
# a.par[a.par$DEN_CIRC == "COPALNIC MANASTUR",][4] <- "COPALNIC-MANASTUR"
# a.par[a.par$DEN_CIRC == "FILIPESTII DE TIRG",][4] <- "FILIPESTII DE TARG"
# a.par[a.par$DEN_CIRC == "SINGERU",][4] <- "SANGERU"
# a.par[a.par$DEN_CIRC == "TIRGSORU VECHI",][4] <- "TARGSORU VECHI"
# a.par[a.par$DEN_CIRC == "VILCANESTI",][4] <- "VALCANESTI"
# a.par[a.par$DEN_CIRC == "MUNICIPIUL ROSIORII DE VEDE",][4] <- "MUNICIPIUL ROSIORI DE VEDE"
# a.par[a.par$DEN_CIRC == "DUDA EPURENI",][4] <- "DUDA-EPURENI"
# a.par[a.par$DEN_CIRC == "CAINENI",][4] <- "CIINENI"
# a.par[a.par$DEN_CIRC == "CRE_ENI",][4] <- "CRETENI"
# a.par[a.par$DEN_CIRC == "FARTA_ESTI",][4] <- "FIRTATESTI"
# a.par[a.par$DEN_CIRC == "ORASUL MARASESTI",][4] <- "ORAS MARASESTI"
# a.par[a.par$DEN_CIRC == "ORASUL ODOBESTI",][4] <- "ORAS ODOBESTI"
# a.par[a.par$DEN_CIRC == "ORASUL PANCIU",][4] <- "ORAS PANCIU"
# a.par[a.par$DEN_CIRC == "CORBI_A",][4] <- "CORBITA"
# a.par[a.par$DEN_CIRC == "GURA CALI_EI",][4] <- "GURA CALITEI"
# a.par[a.par$DEN_CIRC == "OBREJI_A",][4] <- "OBREJITA"
# a.par[a.par$DEN_CIRC == "PLOSCU_ENI",][4] <- "PLOSCUTENI"
# a.par[a.par$DEN_CIRC == "_IFESTI",][4] <- "TIFESTI"
# a.par[a.par$DEN_CIRC == "MUNICIPIUL BUCURESTI SECTORUL 1",][4] <- "BUCURESTI SECTOR 1"
# a.par[a.par$DEN_CIRC == "MUNICIPIUL BUCURESTI SECTORUL 2",][4] <- "BUCURESTI SECTOR 2"
# a.par[a.par$DEN_CIRC == "MUNICIPIUL BUCURESTI SECTORUL 3",][4] <- "BUCURESTI SECTOR 3"
# a.par[a.par$DEN_CIRC == "MUNICIPIUL BUCURESTI SECTORUL 4",][4] <- "BUCURESTI SECTOR 4"
# a.par[a.par$DEN_CIRC == "MUNICIPIUL BUCURESTI SECTORUL 5",][4] <- "BUCURESTI SECTOR 5"
# a.par[a.par$DEN_CIRC == "MUNICIPIUL BUCURESTI SECTORUL 6",][4] <- "BUCURESTI SECTOR 6"
# a.par[a.par$DEN_CIRC == "RIMETEA",][4] <- "RAMETEA"
# a.par[a.par$DEN_CIRC == "RIMET",][4] <- "RAMET"
# a.par[a.par$DEN_CIRC == "COMANDAU",][4] <- "COMANDÆU"
# #Denumire comunã schimbatã complet
# a.par[a.par$DEN_CIRC == "SANIOB",][4] <- "CIUHOI"


#Modificãri coduri Siruta
a.ref[a.ref$siruta == 81193, "siruta"] <- 81184
a.ref[a.ref$siruta == 78766, "siruta"] <- 78748
a.ref[a.ref$siruta == 78720, "siruta"] <- 78711
a.ref[a.ref$siruta == 999, "siruta"] <- 275

#scoatem judeþul 43 din parlamentare ºi referendum
a.par <- a.par[a.par$JUD != 43,]
a.ref <- a.ref[a.ref$JUD != 43,]

################################################################################
#Urmeazã verificãrile propriu-zise
################################################################################
#folosesc ca bazã parlamentarele, am cele mai multe secþii în RO

####################################################
#PARTEA PROASTÃ E CÃ MULTE SECÞII AU ACELAªI NUME
####################################################
timp1<- Sys.time()
baza <- a.par
a.ref.work <- a.ref
coloane <- matrix(rep(NA, 18456 * 3), ncol = 3)
baza <- cbind(baza, coloane)
colnames(baza)[11:13] <- c("SV.ref.echiv", "Adresa.ref.echiv","Aleg.ref.echiv")

contor <- 0 #contor pentru câte secþii a procesat
#coeficienþii pentru cãutare
distanta <- 0.3
distanta2 <- 0.4
distanta3 <- 0.5
procent <- 0.015
for(s in unique(a.par$siruta)){ #pt fiecare localitate facem câteva teste
  #1. Acum testãm în funcþie de adresã
  for(n in 1:nrow(baza[baza$siruta == s,])){ #pt fiecare secþie din localitate
    adresa.curenta <- baza[baza$siruta == s,][n,8] #adresa secþiei curente
    contor <- contor +1
    print(contor) #ca sã am idee pe unde sunt
    if(nchar(adresa.curenta) !=0){ #unele adrese nu sunt deloc ºi dãdea eroare
      adresa.gasita <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                             value = TRUE, max.distance = distanta)
      indice.gasit <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                            value = FALSE, max.distance = distanta)
      if(length(adresa.gasita) == 1){ #iau doar rãspunsurile unice (ºi nenule)
        #completãm adresa, nr. secþiei ºi nr. de alegãtori de la referendum
        baza[baza$siruta == s,][n, 12] <- adresa.gasita
        baza[baza$siruta == s,][n, 11] <- a.ref.work[a.ref.work$siruta == s,6][indice.gasit]
        baza[baza$siruta == s,][n, 13] <- a.ref.work[a.ref.work$siruta == s,9][indice.gasit]
        #Secþia din a.ref alocatã deja se scoate din listã
        a.ref.work[a.ref.work$siruta == s,][indice.gasit,] <- NA
        a.ref.work <- a.ref.work[!is.na(a.ref.work$JUD),]
      }
    }
  }
  #2. Acum testãm în funcþie de numãrul de alegãtori
  for(n in 1:nrow(baza[baza$siruta == s,])){
    if(is.na(baza[baza$siruta == s,][n, 11])){
      numar.curent <- baza[baza$siruta == s,][n, 10]
      vector.numere.ref <- a.ref.work[a.ref.work$siruta == s,9]
      vector.numere.ref <- abs((vector.numere.ref/numar.curent)-1)
      if (sum(vector.numere.ref < procent) == 1){ #condiþie suplimentarã, sã fie unul singur
        #Alocãm valoarea ce corespunde înregistrãrii cu diferenþã minimã - min(vector.numere.ref)
        baza[baza$siruta == s,][n,11:13] <- a.ref.work[a.ref.work$siruta == s,c(6,7,9)][which(vector.numere.ref == min(vector.numere.ref)),]
        #Secþia din a.ref alocatã deja se scoate din listã
        a.ref.work[a.ref.work$siruta == s,][which(vector.numere.ref == min(vector.numere.ref)),] <- NA
        a.ref.work <- a.ref.work[!is.na(a.ref.work$JUD),]
      }
    }
  }
  #3. Acum testãm iar în funcþie de adresã,cu un coeficient de distanþã mai mare
  for(n in 1:nrow(baza[baza$siruta == s,])){ #pt fiecare secþie din localitate
    if(is.na(baza[baza$siruta == s,][n, 11])){
      adresa.curenta <- baza[baza$siruta == s,][n,8] #adresa secþiei curente
      if(nchar(adresa.curenta) !=0){ #unele adrese nu sunt deloc ºi dãdea eroare
        adresa.gasita <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                               value = TRUE, max.distance = distanta2)
        indice.gasit <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                              value = FALSE, max.distance = distanta2)
        if(length(adresa.gasita) == 1){ #iau doar rãspunsurile unice (ºi nenule)
          #completãm adresa, nr. secþiei ºi nr. de alegãtori de la referendum
          baza[baza$siruta == s,][n, 12] <- adresa.gasita
          baza[baza$siruta == s,][n, 11] <- a.ref.work[a.ref.work$siruta == s,6][indice.gasit]
          baza[baza$siruta == s,][n, 13] <- a.ref.work[a.ref.work$siruta == s,9][indice.gasit]
          #Secþia din a.ref alocatã deja se scoate din listã
          a.ref.work[a.ref.work$siruta == s,][indice.gasit,] <- NA
          a.ref.work <- a.ref.work[!is.na(a.ref.work$JUD),]
        }
      }
    }
  }
  #   #4. Acum testãm iar în funcþie de nr. alegãtori, poate am mai scãpat de unele
  #   for(n in 1:nrow(baza[baza$siruta == s,])){
  #     if(is.na(baza[baza$siruta == s,][n, 11])){
  #       numar.curent <- baza[baza$siruta == s,][n, 10]
  #       vector.numere.ref <- a.ref.work[a.ref.work$siruta == s,9]
  #       vector.numere.ref <- abs((vector.numere.ref/numar.curent)-1)
  #       if (sum(vector.numere.ref < procent) == 1){ #condiþie suplimentarã, sã fie unul singur
  #         #Alocãm valoarea ce corespunde înregistrãrii cu diferenþã minimã - min(vector.numere.ref)
  #         baza[baza$siruta == s,][n,11:13] <- a.ref.work[a.ref.work$siruta == s,c(6,7,9)][which(vector.numere.ref == min(vector.numere.ref)),]
  #         #Secþia din a.ref alocatã deja se scoate din listã
  #         a.ref.work[a.ref.work$siruta == s,][which(vector.numere.ref == min(vector.numere.ref)),] <- NA
  #         a.ref.work <- a.ref.work[!is.na(a.ref.work$JUD),]
  #       }
  #     }
  #   }
  
  #   #5. Acum testãm iar în funcþie de adresã,cu coeficient de distanþã ºi mai mare
  #   for(n in 1:nrow(baza[baza$siruta == s,])){ #pt fiecare secþie din localitate
  #     if(is.na(baza[baza$siruta == s,][n, 11])){
  #       adresa.curenta <- baza[baza$siruta == s,][n,8] #adresa secþiei curente
  #       if(nchar(adresa.curenta) !=0){ #unele adrese nu sunt deloc ºi dãdea eroare
  #         adresa.gasita <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
  #                                value = TRUE, max.distance = distanta3)
  #         indice.gasit <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
  #                               value = FALSE, max.distance = distanta3)
  #         if(length(adresa.gasita) == 1){ #iau doar rãspunsurile unice (ºi nenule)
  #           #completãm adresa, nr. secþiei ºi nr. de alegãtori de la referendum
  #           baza[baza$siruta == s,][n, 12] <- adresa.gasita
  #           baza[baza$siruta == s,][n, 11] <- a.ref.work[a.ref.work$siruta == s,6][indice.gasit]
  #           baza[baza$siruta == s,][n, 13] <- a.ref.work[a.ref.work$siruta == s,9][indice.gasit]
  #           #Secþia din a.ref alocatã deja se scoate din listã
  #           a.ref.work[a.ref.work$siruta == s,][indice.gasit,] <- NA
  #           a.ref.work <- a.ref.work[!is.na(a.ref.work$JUD),]
  #         }
  #       }
  #     }
  #   }
  
  #   #6. Acum testãm iar în funcþie de nr. alegãtori, poate am mai scãpat de unele
  #   for(n in 1:nrow(baza[baza$siruta == s,])){
  #     if(is.na(baza[baza$siruta == s,][n, 11])){
  #       numar.curent <- baza[baza$siruta == s,][n, 10]
  #       vector.numere.ref <- a.ref.work[a.ref.work$siruta == s,9]
  #       vector.numere.ref <- abs((vector.numere.ref/numar.curent)-1)
  #       if (sum(vector.numere.ref < procent) == 1){ #condiþie suplimentarã, sã fie unul singur
  #         #Alocãm valoarea ce corespunde înregistrãrii cu diferenþã minimã - min(vector.numere.ref)
  #         baza[baza$siruta == s,][n,11:13] <- a.ref.work[a.ref.work$siruta == s,c(6,7,9)][which(vector.numere.ref == min(vector.numere.ref)),]
  #         #Secþia din a.ref alocatã deja se scoate din listã
  #         a.ref.work[a.ref.work$siruta == s,][which(vector.numere.ref == min(vector.numere.ref)),] <- NA
  #         a.ref.work <- a.ref.work[!is.na(a.ref.work$JUD),]
  #       }
  #     }
  #   }
  for(n in 1:nrow(baza[baza$siruta == s,])){
    ##########################
    #urmeazã 5 IF-uri; ar trebui unite, dacã se poate
    ##########################
    if(is.na(baza[baza$siruta == s,][n, 11])){
      #Mai întâi caut secþii cu nume asemãnãtor
      adresa.curenta <- baza[baza$siruta == s,][n,8] #adresa secþiei curente
      if(nchar(adresa.curenta) !=0){ #unele adrese nu sunt deloc ºi dãdea eroare
        adresa.gasita <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                               value = TRUE, max.distance = distanta)
        indice.gasit <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                              value = FALSE, max.distance = distanta)
        #Acum avem un vector în indice.gasit
        #Iau doar rãspunsurile nenule ºi identice
        # ºi gãsesc înregistrarea unicã cu nr. de secþie corespunzãtor
        if(length(adresa.gasita) > 0 & length(unique(adresa.gasita)) == 1){
          print(paste("am trecut de IF-ul 1 cu length(unique) == 1 ºi valoarea e",length(unique(adresa.gasita))))
          numar.curenta <- baza[baza$siruta == s,][n,5] #numar secþie parl
          #Urmãtorul IF verificã dacã numãrul secþiei de la parlamentare se
          #regãseºte în vectorul de secþii gãsite acum pentru referendum
          #De exemplu, dacã secþia la parlamentare e 366, iar vectorul de la
          #referendum e c(365, 366, 367) este ok.
          if(numar.curenta %in% a.ref.work[a.ref.work$siruta == s,6][indice.gasit]){
            #Iar urmãtorul IF verificã dacã numãrul de alegãtori e stabil
            #de la parlamentare la referendum
            if(abs((a.ref.work[a.ref.work$siruta == s 
                               & a.ref.work$SV.ref == 
                                 numar.curenta, 9] / baza[baza$siruta == s,][n,10])- 1) < procent){
              #Acum scriem echivalenþele
              #vectorul adresa.gasita e cu chestii identice ºi aplicãm (unique)
              baza[baza$siruta == s,][n, 12] <- unique(adresa.gasita) 
              #fiind acelaºi numãr de secþie, scriem nr. de la parlamentare
              baza[baza$siruta == s,][n, 11] <- baza[baza$siruta == s,][n, 5] 
              #Scriem numãrul de alegãtori, luat din înregistrarea cu 
              #codul siruta ºi numãrul de secþie corespunzãtoare
              baza[baza$siruta == s,][n, 13] <- 
                a.ref.work[a.ref.work$siruta == s 
                           & a.ref.work$SV.ref == baza[baza$siruta == s,][n, 5],9] 
              #Secþia din a.ref alocatã deja se scoate din listã
              a.ref.work[a.ref.work$siruta == s & a.ref.work$SV.ref == baza[baza$siruta == s,][n, 5],] <- NA
              a.ref.work <- a.ref.work[!is.na(a.ref.work$JUD),]
            }
          }
        }
      }
    }
    ##########################
    #se terminã cele 5 IF-uri
    ##########################
  }
  #########################################
  #Mai rulez încã o datã, cu distanta2=0.4
  #########################################
  for(n in 1:nrow(baza[baza$siruta == s,])){
    ##########################
    #urmeazã 5 IF-uri; ar trebui unite, dacã se poate
    ##########################
    if(is.na(baza[baza$siruta == s,][n, 11])){
      #Mai întâi caut secþii cu nume asemãnãtor
      adresa.curenta <- baza[baza$siruta == s,][n,8] #adresa secþiei curente
      if(nchar(adresa.curenta) !=0){ #unele adrese nu sunt deloc ºi dãdea eroare
        adresa.gasita <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                               value = TRUE, max.distance = distanta2)
        indice.gasit <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                              value = FALSE, max.distance = distanta2)
        #Acum avem un vector în indice.gasit
        #Iau doar rãspunsurile nenule ºi identice
        # ºi gãsesc înregistrarea unicã cu nr. de secþie corespunzãtor
        if(length(adresa.gasita) > 0 & length(unique(adresa.gasita)) == 1){
          print(paste("am trecut de IF-ul 2 cu length(unique) == 1 ºi valoarea e",length(unique(adresa.gasita))))
          numar.curenta <- baza[baza$siruta == s,][n,5] #numar secþie parl
          #Urmãtorul IF verificã dacã numãrul secþiei de la parlamentare se
          #regãseºte în vectorul de secþii gãsite acum pentru referendum
          #De exemplu, dacã secþia la parlamentare e 366, iar vectorul de la
          #referendum e c(365, 366, 367) este ok.
          if(numar.curenta %in% a.ref.work[a.ref.work$siruta == s,6][indice.gasit]){
            #Iar urmãtorul IF verificã dacã numãrul de alegãtori e stabil
            #de la parlamentare la referendum
            if(abs((a.ref.work[a.ref.work$siruta == s 
                               & a.ref.work$SV.ref == 
                                 numar.curenta, 9] / baza[baza$siruta == s,][n,10])- 1) < procent){
              #Acum scriem echivalenþele
              #vectorul adresa.gasita e cu chestii identice ºi aplicãm (unique)
              baza[baza$siruta == s,][n, 12] <- unique(adresa.gasita) 
              #fiind acelaºi numãr de secþie, scriem nr. de la parlamentare
              baza[baza$siruta == s,][n, 11] <- baza[baza$siruta == s,][n, 5] 
              #Scriem numãrul de alegãtori, luat din înregistrarea cu 
              #codul siruta ºi numãrul de secþie corespunzãtoare
              baza[baza$siruta == s,][n, 13] <- 
                a.ref.work[a.ref.work$siruta == s 
                           & a.ref.work$SV.ref == baza[baza$siruta == s,][n, 5],9] 
              #Secþia din a.ref alocatã deja se scoate din listã
              a.ref.work[a.ref.work$siruta == s & a.ref.work$SV.ref == baza[baza$siruta == s,][n, 5],] <- NA
              a.ref.work <- a.ref.work[!is.na(a.ref.work$JUD),]
            }
          }
        }
      }
    }
    ##########################
    #se terminã cele 5 IF-uri
    ##########################
  }
  #############################
  #aici încerc sã identific grupurile de secþii cu aceeaºi adresã, dar care
  #nu au acelaºi numãr, fiind decalate
  ############################
  for(n in 1:nrow(baza[baza$siruta == s,])){
    if(is.na(baza[baza$siruta == s,][n, 11])){
      #Mai întâi caut secþii cu nume asemãnãtor
      adresa.curenta <- baza[baza$siruta == s,][n,8] #adresa secþiei curente
      if(nchar(adresa.curenta) !=0){ #unele adrese nu sunt deloc ºi dãdea eroare
        adresa.gasita <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                               value = TRUE, max.distance = distanta2)
        indice.gasit <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                              value = FALSE, max.distance = distanta2)
        #Acum avem un vector în indice.gasit
        #Iau doar rãspunsurile nenule ºi identice
        # ºi gãsesc înregistrarea unicã cu nr. de secþie corespunzãtor
        if(length(adresa.gasita) > 0 & length(unique(adresa.gasita)) == 1){
          print(paste("am trecut de IF-ul 3 cu length(unique) == 1 ºi valoarea e",length(unique(adresa.gasita))))
          numar.curenta <- baza[baza$siruta == s,][n,5] #numar secþie parl
          #ok, avem în adresa.curenta denumirea secþiei de la parlamentare,
          #iar în unique(adresa.gasita) avem denumirea secþiei de la referendum
          #Verificãm cã avem acelaºi numãr de secþii
          sectiipar <- nrow(baza[baza$siruta == s & baza$adresa.par == adresa.curenta & is.na(baza$SV.ref.echiv),])
          sectiiref <- nrow(a.ref.work[a.ref.work$siruta == s & a.ref.work$adresa.ref == unique(adresa.gasita),])
          if(sectiipar == sectiiref & sectiipar != 0){ #nu ºtiu dacã pot fi egale cu zero, dar sã fie
            #Ok, avem acelaºi numãr de secþii. 
            #Vom presupune cã ordinea e aceeaºi.
            temporar <- as.data.frame(matrix(rep(NA, 3*sectiipar),sectiipar))
            for(z in 1:sectiipar){
              #testãm fiecare pereche de secþii parlamentar-referendum în
              #privinþa numãrului de alegãtori
              numarpar <- baza[baza$siruta == s & baza$adresa.par == adresa.curenta & is.na(baza$SV.ref.echiv),][z, 10]
              numarref <- a.ref.work[a.ref.work$siruta == s & a.ref.work$adresa.ref == unique(adresa.gasita),][z, 9]
              if(abs(numarref / numarpar - 1) < procent){
                #Putem introduce modificãrile. Adresa
                temporar[z, 2] <- unique(adresa.gasita)
                #baza[baza$siruta == s & baza$adresa.par == adresa.curenta & is.na(baza$SV.ref.echiv),][z, 12] <- unique(adresa.gasita)
                #Numãrul secþiei
                temporar[z, 1] <- a.ref.work[a.ref.work$siruta == s & a.ref.work$adresa.ref == unique(adresa.gasita),][z, 6]
                #baza[baza$siruta == s & baza$adresa.par == adresa.curenta & is.na(baza$SV.ref.echiv),][z, 11] <- a.ref.work[a.ref.work$siruta == s & a.ref.work$adresa.ref == unique(adresa.gasita),][z, 6]
                #Numãrul de alegãtori 
                temporar[z, 3] <- numarref
                #baza[baza$siruta == s & baza$adresa.par == adresa.curenta & is.na(baza$SV.ref.echiv),][z, 13] <- numarref
                }
              #Acum copiem df temporar
              baza[baza$siruta == s & baza$adresa.par == adresa.curenta & is.na(baza$SV.ref.echiv), 11:13] <- temporar
              #Secþiile din a.ref alocate deja se scot din listã
              a.ref.work[a.ref.work$siruta == s & a.ref.work$adresa.ref == unique(adresa.gasita),] <- NA
              a.ref.work <- a.ref.work[!is.na(a.ref.work$JUD),]              
            }
          }
        }
      }
    }
  }
}
timp2 <- Sys.time()
print(timp2 - timp1)

#au mai rãmas unele secþii decalate...!
#eventual separ oraºele mari ºi pe cuvinte cheie gen ºcoalã grãdiniþã etc
#eventual sã scot chestiile între egaluri (adicã ghilimele)?

# #ceva analizã distribuþie secþii gãsite pe siruta dupã 4 for-uri
# numar.sectii <- table(baza$siruta)
# numar.sectii <- as.data.frame(numar.sectii)
# gasite <- !is.na(baza$SV.ref.echiv)
# gasite <- cbind(baza$siruta, gasite)
# gasite <- as.data.frame(gasite)
# gasite$gasite <- as.numeric(gasite$gasite)-1
# agregare <- aggregate(gasite$gasite, by = list(gasite$V1), sum)
# colnames(agregare) <- c("siruta", "sectii.gasite")
# colnames(numar.sectii) <- c("siruta", "sectii.existente")
# statistica.sectii <- merge(numar.sectii, agregare)
# rm(numar.sectii, agregare, gasite)
# statistica.sectii$procentaj <- statistica.sectii$sectii.gasite/statistica.sectii$sectii.existente
# statistica.sectii$sectii.negasite <- statistica.sectii$sectii.existente - statistica.sectii$sectii.gasite
# #rezultate proaste avem, în mod previzibil, în localitãþile cu multe secþii
# #dar sunt ºi secþii mici fãrã rezultate bune
# plot(statistica.sectii$sectii.existente, statistica.sectii$procentaj)
# plot(log10(statistica.sectii$sectii.existente)+1, statistica.sectii$procentaj)
# #Alea unde a gãsit una din douã!
# View(statistica.sectii[statistica.sectii$sectii.existente == 2 & statistica.sectii$procentaj == 0.5,])
# #Distribuþia localitãþilor dupã nr. de secþii (log10)
# hist(log10(table(baza$siruta)+1))
# #Distribuþia secþiilor negãsite
# View(statistica.sectii[order(-statistica.sectii$sectii.negasite),])
# hist(log10(statistica.sectii$sectii.negasite))
# 
# #Compar secþiile din B/S3, sã vãd care e problema
# rstudio::viewData(baza[baza$siruta == 179169,])
# rstudio::viewData(a.ref.work[a.ref.work$siruta == 179169,])
# #Iaºi
# rstudio::viewData(baza[baza$siruta == 95060,])
# rstudio::viewData(a.ref.work[a.ref.work$siruta == 95060,])
# 
# rstudio::viewData(baza[baza$siruta == 179196,])
# rstudio::viewData(a.ref.work[a.ref.work$siruta == 179196,])
# 
# #condiþie triplã? denumire min.dist, nr +/- 0.015 ºi nr.secþie la fel?
# #eventual o condiþie suplimentarã, cu denumire identicã a celei dinainte sau de dupã?
# #sau dacã aia dinainte la parl e identicã, sã fie ºi la ref!!!******
# 
# 



#rm(contor, count, n, numar, s, adresa.curenta, adresa.gasita, indice.gasit, baza)




##JUNK##
# randuri <- matrix(rep(NA, 5661), ncol = 9)
# colnames(randuri) <- colnames(a.loc)
# a.unite <- rbind(a.loc, randuri)
# a.unite <- cbind(a.par, a.unite)
# a.unite[1] <- NULL
# write.table(x=a.unite, file="a.unite.csv", sep = ";", row.names = FALSE)

#eventual de verificat cu agrep sau adist?
#Separare a.par$adresa.parl pe ºcoalã-salã-adresã

# length(grep("CAMINUL CULTURAL", a.par$Adresa.par))
# length(grep("CASA DE CULTURA", a.par$Adresa.par))
# length(grep("COLEGIUL NATIONAL", a.par$Adresa.par))
# length(grep("COLEGIUL TEHNIC", a.par$Adresa.par))
# length(grep("GRADINITA", a.par$Adresa.par))
# length(grep("GRUP SCOLAR", a.par$Adresa.par))
# length(grep("GRUPUL SCOLAR", a.par$Adresa.par))
# length(grep("LICEUL", a.par$Adresa.par))
# length(grep("SCOALA", a.par$Adresa.par))
# length(grep("PRIMARIA", a.par$Adresa.par))
# length(grep("SALA DE SPORT", a.par$Adresa.par))
# length(grep("SAT ", a.par$Adresa.par))
# length(grep("SC. CLS.", a.par$Adresa.par))
# length(grep("SC. GEN.", a.par$Adresa.par))
# length(grep("SEDIUL", a.par$Adresa.par))
# length(grep("STR.", a.par$Adresa.par))
# length(grep("STR ", a.par$Adresa.par))
# length(grep(" STR ", a.par$Adresa.par))
# length(grep(" STR\\.", a.par$Adresa.par))
#idem BLD


