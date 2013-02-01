rm(list = ls(all = TRUE))
################################################################################
#Apel cod pentru locale, referendum şi parlamentare
source("2012AlegeriRomania/2012-locale.R")
source("2012AlegeriRomania/2012-referendum.R")
source("2012AlegeriRomania/2012-parlamentare.R")
################################################################################


#Un data frame pentru fiecare listă de secţii de votare, incl. nr. de alegători
a.loc <- merge(nume.sec.loc.2012, 
                            aleg.loc.2012[c("DEN_JUD", "CIRC", 
                                            "SV", "Numar.alegatori")], 
               sort = FALSE)
a.par <- nume.sec.parl.2012
a.ref <- ref.2012.sv[1:9]

#Şterg datele care nu-mi folosesc pe moment
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
  x <- chartr("ăâîşţĂÂÎŞŢ", "aaistaaist", x) #elimin diacriticele
  x <- toupper(x) #majuscule
  x <- gsub("^( +)", "", x)#spaţii de la începutul adresei
  x <- gsub(" ( +)", " ", x)#spaţii duble
  x <- Ghilimele(x)
  x <- gsub("([0-9A-Z])=([0-9A-Z])", "\\1 =\\2", x) #ghilimele fără spaţii înainte
}

#Prelucrari adresa
a.par$Adresa.parl <- Prelucrari(a.par$Adresa.parl)
a.loc$adresa <- Prelucrari(a.loc$adresa)
a.ref$adresa <- Prelucrari(a.ref$adresa)

#de individualizat coloanele SV şi adresa
colnames(a.par)[5] <- "SV.par"
colnames(a.loc)[3] <- "SV.loc"
colnames(a.ref)[6] <- "SV.ref"
colnames(a.loc)[7] <- "adresa.loc"
colnames(a.par)[8] <- "adresa.par"
colnames(a.ref)[7] <- "adresa.ref"

#Prelucrări DEN_CIRC
a.par$DEN_CIRC <- Prelucrari(a.par$DEN_CIRC)
a.ref$DEN_CIRC_R <- Prelucrari(a.ref$DEN_CIRC_R)
a.loc$DEN_CIRC <- Prelucrari(a.loc$DEN_CIRC)

#Modificări coduri Siruta
a.ref[a.ref$siruta == 81193, "siruta"] <- 81184
a.ref[a.ref$siruta == 78766, "siruta"] <- 78748
a.ref[a.ref$siruta == 78720, "siruta"] <- 78711
a.ref[a.ref$siruta == 999, "siruta"] <- 275

#scoatem judeţul 43 din parlamentare şi referendum
a.par <- a.par[a.par$JUD != 43,]
a.ref <- a.ref[a.ref$JUD != 43,]

################################################################################
#Urmează verificările propriu-zise
#Folosesc ca bază parlamentarele, am cele mai multe secţii în RO
################################################################################

timp1<- Sys.time()
baza <- a.par
a.ref.work <- a.ref
coloane <- matrix(rep(NA, 18456 * 3), ncol = 3)
baza <- cbind(baza, coloane)
colnames(baza)[11:13] <- c("SV.ref.echiv", "Adresa.ref.echiv","Aleg.ref.echiv")

contor <- 0 #contor pentru câte secţii a procesat
#coeficienţii pentru căutare
distanta <- 0.3
distanta2 <- 0.4
distanta3 <- 0.5
procent <- 0.015
for(s in unique(a.par$siruta)){ #pt fiecare localitate facem câteva teste
  #1. Acum testăm în funcţie de adresă
  for(n in 1:nrow(baza[baza$siruta == s,])){ #pt fiecare secţie din localitate
    adresa.curenta <- baza[baza$siruta == s,][n,8] #adresa secţiei curente
    contor <- contor +1
    print(contor) #ca să am idee pe unde sunt
    if(nchar(adresa.curenta) !=0){ #unele adrese nu sunt deloc şi dădea eroare
      adresa.gasita <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                             value = TRUE, max.distance = distanta)
      indice.gasit <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                            value = FALSE, max.distance = distanta)
      if(length(adresa.gasita) == 1){ #iau doar răspunsurile unice (şi nenule)
        #completăm adresa, nr. secţiei şi nr. de alegători de la referendum
        baza[baza$siruta == s,][n, 12] <- adresa.gasita
        baza[baza$siruta == s,][n, 11] <- a.ref.work[a.ref.work$siruta == s,6][indice.gasit]
        baza[baza$siruta == s,][n, 13] <- a.ref.work[a.ref.work$siruta == s,9][indice.gasit]
        #Secţia din a.ref alocată deja se scoate din listă
        a.ref.work[a.ref.work$siruta == s,][indice.gasit,] <- NA
        a.ref.work <- a.ref.work[!is.na(a.ref.work$JUD),]
      }
    }
  }
  #2. Acum testăm în funcţie de numărul de alegători
  for(n in 1:nrow(baza[baza$siruta == s,])){
    if(is.na(baza[baza$siruta == s,][n, 11])){
      numar.curent <- baza[baza$siruta == s,][n, 10]
      vector.numere.ref <- a.ref.work[a.ref.work$siruta == s,9]
      vector.numere.ref <- abs((vector.numere.ref/numar.curent)-1)
      if (sum(vector.numere.ref < procent) == 1){ #condiţie suplimentară, să fie unul singur
        #Alocăm valoarea ce corespunde înregistrării cu diferenţă minimă - min(vector.numere.ref)
        baza[baza$siruta == s,][n,11:13] <- a.ref.work[a.ref.work$siruta == s,c(6,7,9)][which(vector.numere.ref == min(vector.numere.ref)),]
        #Secţia din a.ref alocată deja se scoate din listă
        a.ref.work[a.ref.work$siruta == s,][which(vector.numere.ref == min(vector.numere.ref)),] <- NA
        a.ref.work <- a.ref.work[!is.na(a.ref.work$JUD),]
      }
    }
  }
  #3. Acum testăm iar în funcţie de adresă,cu un coeficient de distanţă mai mare
  for(n in 1:nrow(baza[baza$siruta == s,])){ #pt fiecare secţie din localitate
    if(is.na(baza[baza$siruta == s,][n, 11])){
      adresa.curenta <- baza[baza$siruta == s,][n,8] #adresa secţiei curente
      if(nchar(adresa.curenta) !=0){ #unele adrese nu sunt deloc şi dădea eroare
        adresa.gasita <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                               value = TRUE, max.distance = distanta2)
        indice.gasit <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                              value = FALSE, max.distance = distanta2)
        if(length(adresa.gasita) == 1){ #iau doar răspunsurile unice (şi nenule)
          #completăm adresa, nr. secţiei şi nr. de alegători de la referendum
          baza[baza$siruta == s,][n, 12] <- adresa.gasita
          baza[baza$siruta == s,][n, 11] <- a.ref.work[a.ref.work$siruta == s,6][indice.gasit]
          baza[baza$siruta == s,][n, 13] <- a.ref.work[a.ref.work$siruta == s,9][indice.gasit]
          #Secţia din a.ref alocată deja se scoate din listă
          a.ref.work[a.ref.work$siruta == s,][indice.gasit,] <- NA
          a.ref.work <- a.ref.work[!is.na(a.ref.work$JUD),]
        }
      }
    }
  }
  #4. Acum căutăm secţii cu adresă asemănătoare, nr. identic şi alegători +/-1.5%
  for(n in 1:nrow(baza[baza$siruta == s,])){
    if(is.na(baza[baza$siruta == s,][n, 11])){
      #Mai întâi caut secţii cu nume asemănător
      adresa.curenta <- baza[baza$siruta == s,][n,8] #adresa secţiei curente
      if(nchar(adresa.curenta) !=0){ #unele adrese nu sunt deloc şi dădea eroare
        adresa.gasita <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                               value = TRUE, max.distance = distanta)
        indice.gasit <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                              value = FALSE, max.distance = distanta)
        #Acum avem un vector în indice.gasit
        #Iau doar răspunsurile nenule şi identice
        # şi găsesc înregistrarea unică cu nr. de secţie corespunzător
        if(length(adresa.gasita) > 0 & length(unique(adresa.gasita)) == 1){
          print(paste("am trecut de IF-ul 1 cu length(unique) == 1 şi valoarea e",length(unique(adresa.gasita))))
          numar.curenta <- baza[baza$siruta == s,][n,5] #numar sec?ie parl
          #Următorul IF verifică dacă numărul secţiei de la parlamentare se
          #regăseşte în vectorul de secţii găsite acum pentru referendum
          #De exemplu, dacă secţia la parlamentare e 366, iar vectorul de la
          #referendum e c(365, 366, 367), este ok.
          if(numar.curenta %in% a.ref.work[a.ref.work$siruta == s,6][indice.gasit]){
            #Iar următorul IF verifică dacă numărul de alegători e stabil
            #de la parlamentare la referendum
            if(abs((a.ref.work[a.ref.work$siruta == s 
                               & a.ref.work$SV.ref == 
                                 numar.curenta, 9] / baza[baza$siruta == s,][n,10])- 1) < procent){
              #Acum scriem echivalenţele
              #vectorul adresa.gasita e cu chestii identice şi aplicăm (unique)
              baza[baza$siruta == s,][n, 12] <- unique(adresa.gasita) 
              #fiind acelaşi număr de secţie, scriem nr. de la parlamentare
              baza[baza$siruta == s,][n, 11] <- baza[baza$siruta == s,][n, 5] 
              #Scriem numărul de alegători, luat din înregistrarea cu 
              #codul siruta şi numărul de secţie corespunzătoare
              baza[baza$siruta == s,][n, 13] <- 
                a.ref.work[a.ref.work$siruta == s 
                           & a.ref.work$SV.ref == baza[baza$siruta == s,][n, 5],9] 
              #Secţia din a.ref alocată deja se scoate din listă
              a.ref.work[a.ref.work$siruta == s & a.ref.work$SV.ref == baza[baza$siruta == s,][n, 5],] <- NA
              a.ref.work <- a.ref.work[!is.na(a.ref.work$JUD),]
            }
          }
        }
      }
    }
  }
  #5. Mai rulez încă o dată, cu distanta2=0.4
  for(n in 1:nrow(baza[baza$siruta == s,])){
    ##########################
    #urmează 5 IF-uri; ar trebui unite, dacă se poate
    ##########################
    if(is.na(baza[baza$siruta == s,][n, 11])){
      #Mai întâi caut secţii cu nume asemănător
      adresa.curenta <- baza[baza$siruta == s,][n,8] #adresa secţiei curente
      if(nchar(adresa.curenta) !=0){ #unele adrese nu sunt deloc şi dădea eroare
        adresa.gasita <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                               value = TRUE, max.distance = distanta2)
        indice.gasit <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                              value = FALSE, max.distance = distanta2)
        #Acum avem un vector în indice.gasit
        #Iau doar răspunsurile nenule şi identice
        # şi găsesc înregistrarea unică cu nr. de secţie corespunzător
        if(length(adresa.gasita) > 0 & length(unique(adresa.gasita)) == 1){
          print(paste("am trecut de IF-ul 2 cu length(unique) == 1 şi valoarea e",length(unique(adresa.gasita))))
          numar.curenta <- baza[baza$siruta == s,][n,5] #numar sec?ie parl
          #Următorul IF verifică dacă numărul secţiei de la parlamentare se
          #regăseşte în vectorul de secţii găsite acum pentru referendum
          #De exemplu, dacă secţia la parlamentare e 366, iar vectorul de la
          #referendum e c(365, 366, 367) este ok.
          if(numar.curenta %in% a.ref.work[a.ref.work$siruta == s,6][indice.gasit]){
            #Iar următorul IF verifică dacă numărul de alegători e stabil
            #de la parlamentare la referendum
            if(abs((a.ref.work[a.ref.work$siruta == s 
                               & a.ref.work$SV.ref == 
                                 numar.curenta, 9] / baza[baza$siruta == s,][n,10])- 1) < procent){
              #Acum scriem echivalenţele
              #vectorul adresa.gasita e cu chestii identice şi aplicăm (unique)
              baza[baza$siruta == s,][n, 12] <- unique(adresa.gasita) 
              #fiind acelaşi număr de secţie, scriem nr. de la parlamentare
              baza[baza$siruta == s,][n, 11] <- baza[baza$siruta == s,][n, 5] 
              #Scriem numărul de alegători, luat din înregistrarea cu 
              #codul siruta şi numărul de secţie corespunzătoare
              baza[baza$siruta == s,][n, 13] <- 
                a.ref.work[a.ref.work$siruta == s 
                           & a.ref.work$SV.ref == baza[baza$siruta == s,][n, 5],9] 
              #Secţia din a.ref alocată deja se scoate din listă
              a.ref.work[a.ref.work$siruta == s & a.ref.work$SV.ref == baza[baza$siruta == s,][n, 5],] <- NA
              a.ref.work <- a.ref.work[!is.na(a.ref.work$JUD),]
            }
          }
        }
      }
    }
  }
  #6.aici încerc să identific grupurile de secţii cu aceeaşi adresă, dar care
  #nu au acelaşi număr, fiind decalate
  for(n in 1:nrow(baza[baza$siruta == s,])){
    #condiţiile de mai jos testează că nu avem deja puse datele de la referendum
    #şi că adresa nu e nulă (că-mi dă eroare)
    if(is.na(baza[baza$siruta == s,][n, 11]) & nchar(baza[baza$siruta == s,][n,8]) !=0){
      #Mai întâi caut secţii cu nume asemănător
      adresa.curenta <- baza[baza$siruta == s,][n,8] #adresa secţiei curente
      adresa.gasita <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                             value = TRUE, max.distance = distanta)
      indice.gasit <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                            value = FALSE, max.distance = distanta)
      #Acum avem un vector în indice.gasit, iau doar răspunsurile nenule şi 
      #identice şi găsesc înregistrarea unică cu nr. de secţie corespunzător,
      #dacă există
      if(length(adresa.gasita) > 0 & length(unique(adresa.gasita)) == 1){
        print(paste("am trecut de IF-ul 3 cu length(unique) == 1 şi valoarea e",length(unique(adresa.gasita))))
        numar.curenta <- baza[baza$siruta == s,][n,5] #numar secţie parl
        #ok, avem în adresa.curenta denumirea secţiei de la parlamentare,
        #iar în unique(adresa.gasita) avem denumirea secţiei de la referendum
        #Verificăm că avem acelaşi număr de secţii
        sectiipar <- nrow(baza[baza$siruta == s & baza$adresa.par == adresa.curenta & is.na(baza$SV.ref.echiv),])
        sectiiref <- nrow(a.ref.work[a.ref.work$siruta == s & a.ref.work$adresa.ref == unique(adresa.gasita),])
        if(sectiipar == sectiiref & sectiipar != 0){ #nu ştiu dacă pot fi egale cu zero, dar să fim siguri
          #Ok, avem acelaşi număr de secţii şi vom presupune că ordinea e aceeaşi.
          temporar <- as.data.frame(matrix(rep(NA, 3*sectiipar),sectiipar))
          for(z in 1:sectiipar){
            #testăm fiecare pereche de secţii parlamentar-referendum în
            #privinţa numărului de alegători
            numarpar <- baza[baza$siruta == s & baza$adresa.par == adresa.curenta & is.na(baza$SV.ref.echiv),][z, 10]
            numarref <- a.ref.work[a.ref.work$siruta == s & a.ref.work$adresa.ref == unique(adresa.gasita),][z, 9]
            if(abs(numarref / numarpar - 1) < procent){
              #Putem introduce modificările. Adresa
              temporar[z, 2] <- unique(adresa.gasita)
              #Num?rul sec?iei
              temporar[z, 1] <- a.ref.work[a.ref.work$siruta == s & a.ref.work$adresa.ref == unique(adresa.gasita),][z, 6]
              #Num?rul de aleg?tori 
              temporar[z, 3] <- numarref
              }
          }
          #Acum copiem df temporar
          baza[baza$siruta == s & baza$adresa.par == adresa.curenta & is.na(baza$SV.ref.echiv), 11:13] <- temporar
          #Secţiile din a.ref alocate deja se scot din listă
          #Atenţie, nu toate cu adresa.gasita, ci doar alea selectate în FOR-ul de mai sus!
          a.ref.work[a.ref.work$siruta == s & a.ref.work$adresa.ref == unique(adresa.gasita),][!is.na(temporar$V1),] <- NA
          a.ref.work <- a.ref.work[!is.na(a.ref.work$JUD),]            
        }
      }
    }
  }
  #7.Mai rulez încă o dată punctul #6 cu distanta = 0.2
  for(n in 1:nrow(baza[baza$siruta == s,])){
    #condiţiile de mai jos testează că nu avem deja puse datele de la referendum
    #şi că adresa nu e nulă (că-mi dă eroare)
    if(is.na(baza[baza$siruta == s,][n, 11]) & nchar(baza[baza$siruta == s,][n,8]) !=0){
      #Mai întâi caut secţii cu nume asem?n?tor
      adresa.curenta <- baza[baza$siruta == s,][n,8] #adresa secţiei curente
      adresa.gasita <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                             value = TRUE, max.distance = 0.2)
      indice.gasit <- agrep(adresa.curenta, a.ref.work[a.ref.work$siruta == s,7], 
                            value = FALSE, max.distance = 0.2)
      #Acum avem un vector în indice.gasit, iau doar răspunsurile nenule şi 
      #identice şi găsesc înregistrarea unică cu nr. de secţie corespunzător,
      #dacă există
      if(length(adresa.gasita) > 0 & length(unique(adresa.gasita)) == 1){
        print(paste("am trecut de IF-ul 4 cu length(unique) == 1 şi valoarea e",length(unique(adresa.gasita))))
        numar.curenta <- baza[baza$siruta == s,][n,5] #numar secţie parl
        #ok, avem în adresa.curenta denumirea secţiei de la parlamentare,
        #iar în unique(adresa.gasita) avem denumirea secţiei de la referendum
        #Verificăm că avem acelaşi număr de secţii
        sectiipar <- nrow(baza[baza$siruta == s & baza$adresa.par == adresa.curenta & is.na(baza$SV.ref.echiv),])
        sectiiref <- nrow(a.ref.work[a.ref.work$siruta == s & a.ref.work$adresa.ref == unique(adresa.gasita),])
        if(sectiipar == sectiiref & sectiipar != 0){ #nu ştiu dacă pot fi egale cu zero, dar să fim siguri
          #Ok, avem acelaşi număr de secţii şi vom presupune că ordinea e aceeaşi.
          temporar <- as.data.frame(matrix(rep(NA, 3*sectiipar),sectiipar))
          for(z in 1:sectiipar){
            #testăm fiecare pereche de secţii parlamentar-referendum în
            #privinţa num?rului de alegători
            numarpar <- baza[baza$siruta == s & baza$adresa.par == adresa.curenta & is.na(baza$SV.ref.echiv),][z, 10]
            numarref <- a.ref.work[a.ref.work$siruta == s & a.ref.work$adresa.ref == unique(adresa.gasita),][z, 9]
            if(abs(numarref / numarpar - 1) < procent){
              #Putem introduce modificările. Adresa
              temporar[z, 2] <- unique(adresa.gasita)
              #Num?rul sec?iei
              temporar[z, 1] <- a.ref.work[a.ref.work$siruta == s & a.ref.work$adresa.ref == unique(adresa.gasita),][z, 6]
              #Num?rul de aleg?tori 
              temporar[z, 3] <- numarref
              }
          }
          #Acum copiem df temporar
          baza[baza$siruta == s & baza$adresa.par == adresa.curenta & is.na(baza$SV.ref.echiv), 11:13] <- temporar
          #Secţiile din a.ref alocate deja se scot din listă
          #Atenţie, nu toate cu adresa.gasita, ci doar alea selectate în FOR-ul de mai sus!
          a.ref.work[a.ref.work$siruta == s & a.ref.work$adresa.ref == unique(adresa.gasita),][!is.na(temporar$V1),] <- NA
          a.ref.work <- a.ref.work[!is.na(a.ref.work$JUD),]            
        }
      }
    }
  }
}
timp2 <- Sys.time()
print(timp2 - timp1)

#au mai r?mas unele sec?ii decalate...!
#eventual separ ora?ele mari ?i pe cuvinte cheie gen ?coal? gr?dini?? etc
#eventual s? scot chestiile ?ntre egaluri (adic? ghilimele)?

#ceva analiz? distribu?ie sec?ii g?site pe siruta dup? 4 for-uri
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
#rezultate proaste avem, ?n mod previzibil, ?n localit??ile cu multe sec?ii
# #dar sunt ?i sec?ii mici f?r? rezultate bune
# plot(statistica.sectii$sectii.existente, statistica.sectii$procentaj)
# plot(log10(statistica.sectii$sectii.existente)+1, statistica.sectii$procentaj)
# #Alea unde a g?sit una din dou?!
# View(statistica.sectii[statistica.sectii$sectii.existente == 2 & statistica.sectii$procentaj == 0.5,])
# #Distribu?ia localit??ilor dup? nr. de sec?ii (log10)
# hist(log10(table(baza$siruta)+1))
# #Distribu?ia sec?iilor neg?site
View(statistica.sectii[order(-statistica.sectii$sectii.negasite),])
# hist(log10(statistica.sectii$sectii.negasite))
# 
#Compar sec?iile din B/S3, s? v?d care e problema
rstudio::viewData(baza[baza$siruta == 179169,])
rstudio::viewData(a.ref.work[a.ref.work$siruta == 179169,])
# #Ia?i
# rstudio::viewData(baza[baza$siruta == 95060,])
# rstudio::viewData(a.ref.work[a.ref.work$siruta == 95060,])
# 
# rstudio::viewData(baza[baza$siruta == 179196,])
# rstudio::viewData(a.ref.work[a.ref.work$siruta == 179196,])
# 
#Timi?oara multe adrese cu ortografii diferite
rstudio::viewData(baza[baza$siruta == 155243,])
rstudio::viewData(a.ref.work[a.ref.work$siruta == 155243,])
#Bra?ov
rstudio::viewData(baza[baza$siruta == 40198,])
rstudio::viewData(a.ref.work[a.ref.work$siruta == 40198,])

# #condi?ie tripl?? denumire min.dist, nr +/- 0.015 ?i nr.sec?ie la fel?
# #eventual o condi?ie suplimentar?, cu denumire identic? a celei dinainte sau de dup??
# #sau dac? aia dinainte la parl e identic?, s? fie ?i la ref!!!******
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
#Separare a.par$adresa.parl pe ?coal?-sal?-adres?

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


