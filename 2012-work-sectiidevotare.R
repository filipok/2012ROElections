rm(list = ls(all = TRUE))
source("2012-functiigenerale.R")
#Apel cod pentru locale, referendum şi parlamentare
source("2012-locale.R")
source("2012-referendum.R")
source("2012-parlamentare.R")

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

#Urmează verificările propriu-zise
#Folosesc ca bază parlamentarele, am cele mai multe secţii în RO

timp1<- Sys.time()
baza <- a.par
arefwork <- a.ref
coloane <- matrix(rep(NA, 18456 * 3), ncol = 3)
baza <- cbind(baza, coloane)
rm(coloane)
colnames(baza)[11:13] <- c("SV.ref.echiv", "Adresa.ref.echiv","Aleg.ref.echiv")
baza[,11] <- as.integer(NA)
baza[,12] <- as.character(NA)
baza[,13] <- as.integer(NA)

rm(statss)
#coeficienţii pentru căutare
distanta <- 0.3
distanta2 <- 0.4
distanta3 <- 0.5
procent <- 0.05

#bucată de activat dacă folosim data.table
# baza <- as.data.table(baza)
# arefwork <- as.data.table(arefwork)

#splitare date pe siruta
listapar <- split(baza, as.numeric(baza$siruta))
listaref <- split(arefwork, arefwork$siruta)

timp3<- Sys.time()
pb <- txtProgressBar(min = 0, max = length(unique(a.par$siruta)), style = 3)
source("2012-work-functiicomparare.R") #încărcăm funcţiile apelate în loop
for(i in 1:length(unique(a.par$siruta))){ #pt fiecare localitate facem câteva teste
  setTxtProgressBar(pb, i)
  #1. Acum testăm în funcţie de adresă
  listoi <- testadresa(listapar[[i]], listaref[[i]], distanta)
  #2. Acum testăm în funcţie de numărul de alegători
  listoi <- testnraleg(listoi[[1]], listoi[[2]], procent)
  #3. Acum testăm iar în funcţie de adresă,cu un coeficient de distanţă mai mare
  listoi <- testadresa(listoi[[1]], listoi[[2]], distanta2)
  #4. Acum căutăm secţii cu adresă asemănătoare, nr. identic şi alegători +/-5%
  listoi <- testcomplex(listoi[[1]], listoi[[2]], distanta, procent)
  #5. Mai rulez încă o dată, cu distanta2=0.4
  listoi <- testcomplex(listoi[[1]], listoi[[2]], distanta2, procent)
  #6.aici încerc să identific grupurile de secţii cu aceeaşi adresă, dar care
  #nu au acelaşi număr, fiind decalate
  listoi <- testdecalat(listoi[[1]], listoi[[2]], distanta, procent)
  #7.Mai rulez încă o dată punctul #6 cu distanta = 0.2
  listoi <- testdecalat(listoi[[1]], listoi[[2]], 0.2, procent)
  #8. Căutăm secţii cu adresă asemănătoare, nr. identic şi alegători +/5%
  #(diferenţa faţă de cealaltă funcţie testcomplex e că nu cere ca adresele
  #găsite să fie identice, existând mici typos uneori)
  listoi <- testcomplex2(listoi[[1]], listoi[[2]], distanta, procent)
  
  #9. Căutăm secţii cu adresă asemănătoare, nr. identic, alegători +/-10%
  listoi <- testcomplex(listoi[[1]], listoi[[2]], distanta, 0.1)
  #10.Identific grupurile de secţii cu aceeaşi adresă, dar care
  #nu au acelaşi număr, fiind decalate; acum procent = 10%
  listoi <- testdecalat(listoi[[1]], listoi[[2]], distanta, 0.1)
  #11.Identific grupurile de secţii cu aceeaşi adresă, dar care nu au acelaşi
  #număr, fiind decalate; adresele găsite nu e musai să fie identice (typos)
  listoi <- testdecalat2(listoi[[1]], listoi[[2]], distanta, procent)
  #12. Teste speciale pentru codurile siruta cu cele mai multe secţii negăsite
  if(listapar[[i]][1,3] == 54975){
    listoi <- testdecalat2(listoi[[1]], listoi[[2]], distanta, 0.3)
  }
  if(listapar[[i]][1,3] %in% c(95060, 155243, 60419, 179169)){
    listoi <- testcomplex(listoi[[1]], listoi[[2]], distanta, 0.3)
  }
  if(listapar[[i]][1,3] %in% c(40198, 9262, 143450, 114319, 42682, 20297, 106318)){
    listoi <- testdecalat(listoi[[1]], listoi[[2]], distanta, 0.3)
  }
  if(listapar[[i]][1,3] %in% c(130534, 13169, 167473)){
    listoi <- testdecalat(listoi[[1]], listoi[[2]], 0.5, procent)
  }
  if(listapar[[i]][1,3] %in% c(197196, 20563, 106318, 114319, 159614, 60847, 
                               139704, 1017, 130534)){
    listoi <- testdecalat(listoi[[1]], listoi[[2]], 0.5, 0.5)
  }
  if(listapar[[i]][1, 3] %in% c(13169)){
    listoi <- testdecalat(listoi[[1]], listoi[[2]], 0.7, procent)
  }
    
  #13. Test special pentru cele cu mai puţin de 10 negăsite, unde numărul de
  #secţii negăsite de la lista1 e acelaşi cu cel de la lista2
  if(nrow(listoi[[2]]) < 11 & nrow(listoi[[2]]) == nrow(listoi[[1]][is.na(listoi[[1]]$Adresa.ref.echiv),])){
    listoi <- testdecalat(listoi[[1]], listoi[[2]], 0.5, 0.3)
  }
  
  #14. Teste speciale pentru cele cu 2 sau 3 negăsite; distanţă mică, fiindcă au
  #nume asemănătoare
  if(nrow(listoi[[2]]) %in% c(2, 3) & nrow(listoi[[2]]) == nrow(listoi[[1]][is.na(listoi[[1]]$Adresa.ref.echiv),])){
    listoi <- testdecalat(listoi[[1]], listoi[[2]], 0.1, procent)
  }
  if(nrow(listoi[[2]]) %in% c(2, 3) & nrow(listoi[[2]]) == nrow(listoi[[1]][is.na(listoi[[1]]$Adresa.ref.echiv),])){
    listoi <- testdecalat(listoi[[1]], listoi[[2]], 0.05, procent)
  }
  listapar[[i]] <- listoi[[1]]
  listaref[[i]] <- listoi[[2]]
}

close(pb)
timp4<- Sys.time()


baza <- do.call("rbind", listapar)
arefwork <- do.call("rbind", listaref)
rm(listapar, listaref)

#apelare cod modificări manuale
source("2012-par-ref-manual.R")

#siru <- 1213; vezi(baza[baza$siruta == siru,])
#vezi(arefwork[arefwork$siruta == siru,])


#analiză distribuţie secţii găsite
numar.sectii <- table(baza[, 3])
numar.sectii <- as.data.frame(numar.sectii)
gasite <- !is.na(baza[, 11])
gasite <- cbind(baza[, 3], gasite)
gasite <- as.data.frame(gasite)
gasite$gasite <- as.numeric(gasite$gasite)-1
agregare <- aggregate(gasite$gasite, by = list(gasite$V1), sum)
colnames(agregare) <- c("siruta", "sectii.gasite")
colnames(numar.sectii) <- c("siruta", "existpar")
statss <- merge(numar.sectii, agregare)
rm(numar.sectii, agregare, gasite)
statss$procentaj <- statss$sectii.gasite/statss$existpar
statss$nepar <- statss$existpar - statss$sectii.gasite
statss <- merge(statss, table(arefwork[,4]), by.x = "siruta", by.y = "Var1")
colnames(statss)[6] <- "neref"
#View(statss[order(-statss$nepar),])
vezi(with(statss, table(nepar, neref)))

print(paste("mai am", sum(statss$nepar), "secţii negăsite"))
timp2 <- Sys.time()

print(paste("a durat", timp2 - timp1, "minute, din care loop-ul", timp4 - timp3))
beep(10)


# vezi(statss[statss$nepar == 1 & statss$neref == 1,])
# vezi(statss[statss$nepar == 2 & statss$neref == 2,])
# vezi(statss[statss$nepar == 3 & statss$neref == 3,])
# vezi(statss[statss$nepar == 4 & statss$neref == 4,])
# vezi(statss[statss$nepar == 113 & statss$neref == 89,])
# 
# vezi(statss[statss$nepar == 2 & statss$neref == 1,])
# vezi(statss[statss$nepar == 1 & statss$neref == 2,])

#rezultate proaste avem, în mod previzibil, în localităţile cu multe secţii
# #dar sunt şi secţii mici fără rezultate bune
# plot(statss$existpar, statss$procentaj)
# plot(log10(statss$existpar)+1, statss$procentaj)
# #Alea unde a găsit una din două!
# View(statss[statss$existpar == 2 & statss$procentaj == 0.5,])
# #Distribuţia localităţilor după nr. de secţii (log10)
# hist(log10(table(baza$siruta)+1))
# #Distribuţia secţiilor negăsite
vezi(statss[order(-statss$nepar),])

# hist(log10(statss$nepar))
