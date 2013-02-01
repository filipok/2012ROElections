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
#Folosesc ca bazã parlamentarele, am cele mai multe secþii în RO
################################################################################

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
  #4. Acum cãutãm secþii cu adresã asemãnãtoare, nr. identic ºi alegãtori +/-1.5%
  for(n in 1:nrow(baza[baza$siruta == s,])){
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
  }
  #5. Mai rulez încã o datã, cu distanta2=0.4
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
  }
  #6.aici încerc sã identific grupurile de secþii cu aceeaºi adresã, dar care
  #nu au acelaºi numãr, fiind decalate
  for(n in 1:nrow(baza[baza$siruta == s,])){
    #condiþiile de mai jos testeazã cã nu avem deja puse datele de la referendum
    #ºi cã adresa nu e nulã (cã-mi dã eroare)
    if(is.na(baza[baza$siruta == s,][n, 11]) & nchar(baza[baza$siruta == s,][n,8]) !=0){
      #Mai întâi caut secþii cu nume asemãnãtor
      adresa.curenta <- baza[baza$siruta == s,][n,8] #adresa secþiei curente
      adresa.gasita <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                             value = TRUE, max.distance = distanta)
      indice.gasit <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                            value = FALSE, max.distance = distanta)
      #Acum avem un vector în indice.gasit, iau doar rãspunsurile nenule ºi 
      #identice ºi gãsesc înregistrarea unicã cu nr. de secþie corespunzãtor,
      #dacã existã
      if(length(adresa.gasita) > 0 & length(unique(adresa.gasita)) == 1){
        print(paste("am trecut de IF-ul 3 cu length(unique) == 1 ºi valoarea e",length(unique(adresa.gasita))))
        numar.curenta <- baza[baza$siruta == s,][n,5] #numar secþie parl
        #ok, avem în adresa.curenta denumirea secþiei de la parlamentare,
        #iar în unique(adresa.gasita) avem denumirea secþiei de la referendum
        #Verificãm cã avem acelaºi numãr de secþii
        sectiipar <- nrow(baza[baza$siruta == s & baza$adresa.par == adresa.curenta & is.na(baza$SV.ref.echiv),])
        sectiiref <- nrow(a.ref.work[a.ref.work$siruta == s & a.ref.work$adresa.ref == unique(adresa.gasita),])
        if(sectiipar == sectiiref & sectiipar != 0){ #nu ºtiu dacã pot fi egale cu zero, dar sã fim siguri
          #Ok, avem acelaºi numãr de secþii ºi vom presupune cã ordinea e aceeaºi.
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
          }
          #Acum copiem df temporar
          baza[baza$siruta == s & baza$adresa.par == adresa.curenta & is.na(baza$SV.ref.echiv), 11:13] <- temporar
          #Secþiile din a.ref alocate deja se scot din listã
          #Atenþie, nu toate cu adresa.gasita, ci doar alea selectate în FOR-ul de mai sus!
          a.ref.work[a.ref.work$siruta == s & a.ref.work$adresa.ref == unique(adresa.gasita),][!is.na(temporar$V1),] <- NA
          a.ref.work <- a.ref.work[!is.na(a.ref.work$JUD),]            
        }
      }
    }
  }
  #7.Mai rulez încã o datã punctul #6 cu distanta = 0.2
  for(n in 1:nrow(baza[baza$siruta == s,])){
    #condiþiile de mai jos testeazã cã nu avem deja puse datele de la referendum
    #ºi cã adresa nu e nulã (cã-mi dã eroare)
    if(is.na(baza[baza$siruta == s,][n, 11]) & nchar(baza[baza$siruta == s,][n,8]) !=0){
      #Mai întâi caut secþii cu nume asemãnãtor
      adresa.curenta <- baza[baza$siruta == s,][n,8] #adresa secþiei curente
      adresa.gasita <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                             value = TRUE, max.distance = 0.2)
      indice.gasit <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                            value = FALSE, max.distance = 0.2)
      #Acum avem un vector în indice.gasit, iau doar rãspunsurile nenule ºi 
      #identice ºi gãsesc înregistrarea unicã cu nr. de secþie corespunzãtor,
      #dacã existã
      if(length(adresa.gasita) > 0 & length(unique(adresa.gasita)) == 1){
        print(paste("am trecut de IF-ul 3 cu length(unique) == 1 ºi valoarea e",length(unique(adresa.gasita))))
        numar.curenta <- baza[baza$siruta == s,][n,5] #numar secþie parl
        #ok, avem în adresa.curenta denumirea secþiei de la parlamentare,
        #iar în unique(adresa.gasita) avem denumirea secþiei de la referendum
        #Verificãm cã avem acelaºi numãr de secþii
        sectiipar <- nrow(baza[baza$siruta == s & baza$adresa.par == adresa.curenta & is.na(baza$SV.ref.echiv),])
        sectiiref <- nrow(a.ref.work[a.ref.work$siruta == s & a.ref.work$adresa.ref == unique(adresa.gasita),])
        if(sectiipar == sectiiref & sectiipar != 0){ #nu ºtiu dacã pot fi egale cu zero, dar sã fim siguri
          #Ok, avem acelaºi numãr de secþii ºi vom presupune cã ordinea e aceeaºi.
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
          }
          #Acum copiem df temporar
          baza[baza$siruta == s & baza$adresa.par == adresa.curenta & is.na(baza$SV.ref.echiv), 11:13] <- temporar
          #Secþiile din a.ref alocate deja se scot din listã
          #Atenþie, nu toate cu adresa.gasita, ci doar alea selectate în FOR-ul de mai sus!
          a.ref.work[a.ref.work$siruta == s & a.ref.work$adresa.ref == unique(adresa.gasita),][!is.na(temporar$V1),] <- NA
          a.ref.work <- a.ref.work[!is.na(a.ref.work$JUD),]            
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

#ceva analizã distribuþie secþii gãsite pe siruta dupã 4 for-uri
numar.sectii <- table(baza$siruta)
numar.sectii <- as.data.frame(numar.sectii)
gasite <- !is.na(baza$SV.ref.echiv)
gasite <- cbind(baza$siruta, gasite)
gasite <- as.data.frame(gasite)
gasite$gasite <- as.numeric(gasite$gasite)-1
agregare <- aggregate(gasite$gasite, by = list(gasite$V1), sum)
colnames(agregare) <- c("siruta", "sectii.gasite")
colnames(numar.sectii) <- c("siruta", "sectii.existente")
statistica.sectii <- merge(numar.sectii, agregare)
rm(numar.sectii, agregare, gasite)
statistica.sectii$procentaj <- statistica.sectii$sectii.gasite/statistica.sectii$sectii.existente
statistica.sectii$sectii.negasite <- statistica.sectii$sectii.existente - statistica.sectii$sectii.gasite
#rezultate proaste avem, în mod previzibil, în localitãþile cu multe secþii
# #dar sunt ºi secþii mici fãrã rezultate bune
# plot(statistica.sectii$sectii.existente, statistica.sectii$procentaj)
# plot(log10(statistica.sectii$sectii.existente)+1, statistica.sectii$procentaj)
# #Alea unde a gãsit una din douã!
# View(statistica.sectii[statistica.sectii$sectii.existente == 2 & statistica.sectii$procentaj == 0.5,])
# #Distribuþia localitãþilor dupã nr. de secþii (log10)
# hist(log10(table(baza$siruta)+1))
# #Distribuþia secþiilor negãsite
View(statistica.sectii[order(-statistica.sectii$sectii.negasite),])
# hist(log10(statistica.sectii$sectii.negasite))
# 
#Compar secþiile din B/S3, sã vãd care e problema
rstudio::viewData(baza[baza$siruta == 179169,])
rstudio::viewData(a.ref.work[a.ref.work$siruta == 179169,])
# #Iaºi
# rstudio::viewData(baza[baza$siruta == 95060,])
# rstudio::viewData(a.ref.work[a.ref.work$siruta == 95060,])
# 
# rstudio::viewData(baza[baza$siruta == 179196,])
# rstudio::viewData(a.ref.work[a.ref.work$siruta == 179196,])
# 
#Timiºoara multe adrese cu ortografii diferite
rstudio::viewData(baza[baza$siruta == 155243,])
rstudio::viewData(a.ref.work[a.ref.work$siruta == 155243,])
#Braºov
rstudio::viewData(baza[baza$siruta == 40198,])
rstudio::viewData(a.ref.work[a.ref.work$siruta == 40198,])

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


