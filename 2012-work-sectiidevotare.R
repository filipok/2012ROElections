beep <- function(n = 3){
  #source: http://stackoverflow.com/questions/3365657/is-there-a-way-to-make-r-beep-play-a-sound-at-the-end-of-a-script
  for(i in seq(n)){
    system("rundll32 user32.dll,MessageBeep -1")
    Sys.sleep(.5)
  }
}

testadresa <- function(lista1, lista2, distanta){
  #funcţie de testare pe baza adresei
  for(n in 1:nrow(lista1)){ #pt fiecare secţie din localitate
    adresa.curenta <- lista1[n,8] #adresa secţiei curente
    if(nchar(adresa.curenta) !=0){ #unele adrese nu sunt deloc şi dădea eroare
      adresa.gasita <- agrep(adresa.curenta, lista2[,7], 
                             value = TRUE, max.distance = distanta)
      indice.gasit <- agrep(adresa.curenta, lista2[,7], 
                            value = FALSE, max.distance = distanta)
      if(length(adresa.gasita) == 1){ #iau doar răspunsurile unice (şi nenule)
        #completăm adresa, nr. secţiei şi nr. de alegători de la referendum
        lista1[n, 12] <- adresa.gasita
        lista1[n, 11] <- lista2[,6][indice.gasit]
        lista1[n, 13] <- lista2[,9][indice.gasit]
        #Secţia din a.ref alocată deja se scoate din listă
        lista2[indice.gasit,] <- NA
        lista2 <- lista2[!is.na(lista2[,1]),]
      }
    }
  }
  listoi <- vector ("list", 2)
  listoi[[1]] <- lista1
  listoi[[2]] <- lista2
  listoi
}

testnraleg <- function(lista1, lista2){
  #funcţie de testare pe baza numărului de alegători
  for(n in 1:nrow(lista1)){
    if(is.na(lista1[n, 11])){
      numar.curent <- lista1[n, 10]
      vector.numere.ref <- lista2[,9]
      vector.numere.ref <- abs((vector.numere.ref/numar.curent)-1)
      if (sum(vector.numere.ref < procent) == 1){ #condiţie suplimentară, să fie unul singur
        #Alocăm valoarea ce corespunde înregistrării cu diferenţă minimă - min(vector.numere.ref)
        lista1[n,11:13] <- lista2[,c(6,7,9)][which(vector.numere.ref == min(vector.numere.ref)),]
        #Secţia din a.ref alocată deja se scoate din listă
        lista2[which(vector.numere.ref == min(vector.numere.ref)),] <- NA
        lista2 <- lista2[!is.na(lista2$JUD),]
      }
    }
  }
  listoi <- vector ("list", 2)
  listoi[[1]] <- lista1
  listoi[[2]] <- lista2
  listoi
}

testcomplex <- function(lista1, lista2, distanta, procent){
  #Căutăm secţii cu adresă asemănătoare, nr. identic şi alegători +/-1.5%
  for(n in 1:nrow(lista1)){
    if(is.na(lista1[n, 11])){
      #Mai întâi caut secţii cu nume asemănător
      adresa.curenta <- lista1[n,8] #adresa secţiei curente
      if(nchar(adresa.curenta) !=0){ #unele adrese nu sunt deloc şi dădea eroare
        adresa.gasita <- agrep(adresa.curenta, lista2[,7], 
                               value = TRUE, max.distance = distanta)
        indice.gasit <- agrep(adresa.curenta, lista2[,7], 
                              value = FALSE, max.distance = distanta)
        #Acum avem un vector în indice.gasit
        #Iau doar răspunsurile nenule şi identice
        # şi găsesc înregistrarea unică cu nr. de secţie corespunzător
        if(length(adresa.gasita) > 0 & length(unique(adresa.gasita)) == 1){
          #print(paste("am trecut de IF-ul 1 cu length(unique) == 1 şi valoarea e",length(unique(adresa.gasita))))
          numar.curenta <- lista1[n,5] #numar secţie parl
          #Următorul IF verifică dacă numărul secţiei de la parlamentare se
          #regăseşte în vectorul de secţii găsite acum pentru referendum
          #De exemplu, dacă secţia la parlamentare e 366, iar vectorul de la
          #referendum e c(365, 366, 367), este ok.
          if(numar.curenta %in% lista2[,6][indice.gasit]){
            #Iar următorul IF verifică dacă numărul de alegători e stabil
            #de la parlamentare la referendum
            if(abs((lista2[lista2$SV.ref == 
                             numar.curenta, 9] / lista1[n,10])- 1) < procent){
              #Acum scriem echivalenţele
              #vectorul adresa.gasita e cu chestii identice şi aplicăm (unique)
              lista1[n, 12] <- unique(adresa.gasita) 
              #fiind acelaşi număr de secţie, scriem nr. de la parlamentare
              lista1[n, 11] <- lista1[n, 5] 
              #Scriem numărul de alegători, luat din înregistrarea cu 
              #codul siruta şi numărul de secţie corespunzătoare
              lista1[n, 13] <- 
                lista2[lista2$SV.ref == lista1[n, 5],9] 
              #Secţia din a.ref alocată deja se scoate din listă
              lista2[lista2$SV.ref == lista1[n, 5],] <- NA
              lista2 <- lista2[!is.na(lista2$JUD),]
            }
          }
        }
      }
    }
  }
  listoi <- vector ("list", 2)
  listoi[[1]] <- lista1
  listoi[[2]] <- lista2
  listoi
}

testdecalat <- function(lista1, lista2, distanta, procent){
  #Caut grupuri de secţii cu aceeaşi adresă, dar cu numere diferite, decalate
  for(n in 1:nrow(lista1)){
    #testez că nu avem deja datele din lista2 şi că adresa nu e nulă (dă eroare)
    if(is.na(lista1[n, 11]) & nchar(lista1[n,8]) !=0){
      #Caut secţii cu nume asemănător
      adresa.curenta <- lista1[n,8] #adresa secţiei curente
      adresa.gasita <- agrep(adresa.curenta, lista2[,7], 
                             value = TRUE, max.distance = distanta)
      indice.gasit <- agrep(adresa.curenta, lista2[,7], 
                            value = FALSE, max.distance = distanta)
      #Am vectori cu adrese şi indici adrese, caut înregistrarea unică, dacă e
      if(length(adresa.gasita) > 0 & length(unique(adresa.gasita)) == 1){
        numar.curenta <- lista1[n,5] #numar secţie parl
        #Verificăm că avem acelaşi număr de secţii pentru adresa din lista1 şi
        #cea din lista 2
        sectiipar <- nrow(lista1[lista1$adresa.par == adresa.curenta & is.na(lista1$SV.ref.echiv),])
        sectiiref <- nrow(lista2[lista2$adresa.ref == unique(adresa.gasita),])
        if(sectiipar == sectiiref & sectiipar != 0){ #pot fi egale cu zero?
          #Avem acelaşi număr de secţii; vom presupune că ordinea e aceeaşi.
          temporar <- as.data.frame(matrix(rep(NA, 3*sectiipar),sectiipar))
          for(z in 1:sectiipar){
            #testăm fiecare pereche de secţii lista1-lista2 în privinţa
            #numărului de alegători
            numar1 <- lista1[lista1$adresa.par == adresa.curenta & is.na(lista1$SV.ref.echiv),][z, 10]
            numar2 <- lista2[lista2$adresa.ref == unique(adresa.gasita),][z, 9]
            if(abs(numar2 / numar1 - 1) < procent){
              #Putem introduce modificările. Adresa
              temporar[z, 2] <- unique(adresa.gasita)
              #Numărul secţiei
              temporar[z, 1] <- lista2[lista2$adresa.ref == unique(adresa.gasita),][z, 6]
              #Numărul de alegători 
              temporar[z, 3] <- numar2
            }
          }
          #Acum copiem df temporar
          lista1[lista1$adresa.par == adresa.curenta & is.na(lista1$SV.ref.echiv), 11:13] <- temporar
          #Secţiile din lista2 alocate deja se scot din listă
          #Atenţie, nu toate cu adresa gasita, ci doar alea selectate în FOR-ul de mai sus!
          lista2[lista2$adresa.ref == unique(adresa.gasita),][!is.na(temporar$V1),] <- NA
          lista2 <- lista2[!is.na(lista2$JUD),]            
        }
      }
    }
  }
  listoi <- vector ("list", 2)
  listoi[[1]] <- lista1
  listoi[[2]] <- lista2
  listoi
}

rm(list = ls(all = TRUE))
################################################################################
#Apel cod pentru locale, referendum şi parlamentare
source("2012-locale.R")
source("2012-referendum.R")
source("2012-parlamentare.R")
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


baza <- a.par
a.ref.work <- a.ref
coloane <- matrix(rep(NA, 18456 * 3), ncol = 3)
baza <- cbind(baza, coloane)
colnames(baza)[11:13] <- c("SV.ref.echiv", "Adresa.ref.echiv","Aleg.ref.echiv")

timp1<- Sys.time()
#coeficienţii pentru căutare
distanta <- 0.3
distanta2 <- 0.4
distanta3 <- 0.5
procent <- 0.015

#separ dataframe-ul în câte unul pentru fiecare cod Siruta
listapar <- vector ("list", length(unique(a.par$siruta)))
listaref <- vector ("list", length(unique(a.par$siruta)))
pb <- txtProgressBar(min = 0, max = length(unique(a.par$siruta)), style = 3, 
                     char = "*")
for (i in 1:length(unique(a.par$siruta))){
  setTxtProgressBar(pb, i)
  listapar[[i]] <- baza[baza$siruta == unique(a.par$siruta)[i],]
  listaref[[i]] <- a.ref.work[a.ref.work$siruta == unique(a.par$siruta)[i],]
}
close(pb)

pb <- txtProgressBar(min = 0, max = length(unique(a.par$siruta)), style = 3)
for(i in 1:length(unique(a.par$siruta))){ #pt fiecare localitate facem câteva teste
  setTxtProgressBar(pb, i)
  #1. Acum testăm în funcţie de adresă
  listoi <- testadresa(listapar[[i]], listaref[[i]], distanta)
  listapar[[i]] <- listoi[[1]]
  listaref[[i]] <- listoi[[2]]
  #2. Acum testăm în funcţie de numărul de alegători
  listoi <- testnraleg(listapar[[i]], listaref[[i]])
  listapar[[i]] <- listoi[[1]]
  listaref[[i]] <- listoi[[2]]
  #3. Acum testăm iar în funcţie de adresă,cu un coeficient de distanţă mai mare
  listoi <- testadresa(listapar[[i]], listaref[[i]], distanta2)
  listapar[[i]] <- listoi[[1]]
  listaref[[i]] <- listoi[[2]]
  #4. Acum căutăm secţii cu adresă asemănătoare, nr. identic şi alegători +/-1.5%
  listoi <- testcomplex(listapar[[i]], listaref[[i]], distanta, procent)
  listapar[[i]] <- listoi[[1]]
  listaref[[i]] <- listoi[[2]]
  #5. Mai rulez încă o dată, cu distanta2=0.4
  listoi <- testcomplex(listapar[[i]], listaref[[i]], distanta2, procent)
  listapar[[i]] <- listoi[[1]]
  listaref[[i]] <- listoi[[2]]
  #6.aici încerc să identific grupurile de secţii cu aceeaşi adresă, dar care
  #nu au acelaşi număr, fiind decalate
  listoi <- testdecalat(listapar[[i]], listaref[[i]], distanta, procent)
  listapar[[i]] <- listoi[[1]]
  listaref[[i]] <- listoi[[2]]
  #7.Mai rulez încă o dată punctul #6 cu distanta = 0.2
  listoi <- testdecalat(listapar[[i]], listaref[[i]], 0.2, procent)
  listapar[[i]] <- listoi[[1]]
  listaref[[i]] <- listoi[[2]]  
}

close(pb)

baza <- listapar[[1]]
a.ref.work <- listaref[[1]]
pb <- txtProgressBar(min = 0, max = length(unique(a.par$siruta)), style = 3, 
                     char = "+")
for(j in 2:length(unique(a.par$siruta))){
  setTxtProgressBar(pb, j)
  baza <- rbind(baza, listapar[[j]])
  a.ref.work <- rbind(a.ref.work, listaref[[j]])
}
close(pb)

#au mai rămas unele secţii decalate...!
#eventual separ oraşele mari şi pe cuvinte cheie gen şcoală grădiniţă etc
#eventual să scot chestiile între egaluri (adică ghilimele)?

#ceva analiză distribuţie secţii găsite pe siruta după 4 for-uri
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
#rezultate proaste avem, în mod previzibil, în localităţile cu multe secţii
# #dar sunt şi secţii mici fără rezultate bune
# plot(statistica.sectii$sectii.existente, statistica.sectii$procentaj)
# plot(log10(statistica.sectii$sectii.existente)+1, statistica.sectii$procentaj)
# #Alea unde a găsit una din două!
# View(statistica.sectii[statistica.sectii$sectii.existente == 2 & statistica.sectii$procentaj == 0.5,])
# #Distribuţia localităţilor după nr. de secţii (log10)
# hist(log10(table(baza$siruta)+1))
# #Distribuţia secţiilor negăsite
View(statistica.sectii[order(-statistica.sectii$sectii.negasite),])
sum(statistica.sectii$sectii.negasite)

timp2 <- Sys.time()
print(timp2 - timp1)
beep(10)

# hist(log10(statistica.sectii$sectii.negasite))
# 
#Compar secţiile din B/S3, să văd care e problema
rstudio::viewData(baza[baza$siruta == 179169,])
rstudio::viewData(a.ref.work[a.ref.work$siruta == 179169,])
# #Iaşi
# rstudio::viewData(baza[baza$siruta == 95060,])
# rstudio::viewData(a.ref.work[a.ref.work$siruta == 95060,])
# 
# rstudio::viewData(baza[baza$siruta == 179196,])
# rstudio::viewData(a.ref.work[a.ref.work$siruta == 179196,])
# 
#Timişoara multe adrese cu ortografii diferite
rstudio::viewData(baza[baza$siruta == 155243,])
rstudio::viewData(a.ref.work[a.ref.work$siruta == 155243,])
#Braşov
rstudio::viewData(baza[baza$siruta == 40198,])
rstudio::viewData(a.ref.work[a.ref.work$siruta == 40198,])

# #condiţie triplă denumire min.dist, nr +/- 0.015 şi nr.secţie la fel?
# #eventual o condiţie suplimentară, cu denumire identică a celei dinainte sau de după?
# #sau dacă aia dinainte la parl e identică, să fie şi la ref!!!******
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
#Separare a.par$adresa.parl pe şcoală-sală-adresă

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


