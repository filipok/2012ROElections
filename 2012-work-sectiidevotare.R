library("R2HTML")
library("data.table")

vezi = function (x) { 
  #de la Georgian
  file.remove( 'test.html');
  HTML(x, file='test.html', row.names=TRUE, innerBorder=1);
  system(' "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe" "C:\\Users\\Filip\\Dropbox\\R_Work\\2012AlegeriRomania\\test.html"  ', wait=FALSE )
}

beep <- function(n = 3){
  #source: http://stackoverflow.com/questions/3365657/is-there-a-way-to-make-r-beep-play-a-sound-at-the-end-of-a-script
  for(i in seq(n)){
    system("rundll32 user32.dll,MessageBeep -1")
    Sys.sleep(.5)
  }
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


timp1<- Sys.time()
baza <- a.par
a.ref.work <- a.ref
coloane <- matrix(rep(NA, 18456 * 3), ncol = 3)
baza <- cbind(baza, coloane)
colnames(baza)[11:13] <- c("SV.ref.echiv", "Adresa.ref.echiv","Aleg.ref.echiv")
baza[,11] <- as.integer(NA)
baza[,12] <- as.character(NA)
baza[,13] <- as.integer(NA)

rm(statistica.sectii)
#coeficienţii pentru căutare
distanta <- 0.3
distanta2 <- 0.4
distanta3 <- 0.5
procent <- 0.015

#bucată de activat dacă folosim data.table
# baza <- as.data.table(baza)
# a.ref.work <- as.data.table(a.ref.work)

#splitare date pe siruta
listapar <- split(baza, as.numeric(baza$siruta))
listaref <- split(a.ref.work, a.ref.work$siruta)

timp3<- Sys.time()
pb <- txtProgressBar(min = 0, max = length(unique(a.par$siruta)), style = 3)
source("2012-work-functiicomparare.R") #încărcăm funcţiile apelate în loop
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
  #8. Căutăm secţii cu adresă asemănătoare, nr. identic şi alegători +/-1.5%
  #(diferenţa faţă de cealaltă funcţie testcomplex e că nu cere ca adresele
  #găsite să fie identice, existând mici typos uneori)
  listoi <- testcomplex2(listapar[[i]], listaref[[i]], distanta, procent)
  listapar[[i]] <- listoi[[1]]
  listaref[[i]] <- listoi[[2]]
}

close(pb)
timp4<- Sys.time()

baza <- do.call("rbind", listapar)
a.ref.work <- do.call("rbind", listaref)

#au mai rămas unele secţii decalate...!
#eventual separ oraşele mari şi pe cuvinte cheie gen şcoală grădiniţă etc
#eventual să scot chestiile între egaluri (adică ghilimele)?

#analiză distribuţie secţii găsite
#nu funcţionează dacă folosim data tables
numar.sectii <- table(baza[, 3])
numar.sectii <- as.data.frame(numar.sectii)
gasite <- !is.na(baza[, 11])
gasite <- cbind(baza[, 3], gasite)
gasite <- as.data.frame(gasite)
gasite$gasite <- as.numeric(gasite$gasite)-1
agregare <- aggregate(gasite$gasite, by = list(gasite$V1), sum)
colnames(agregare) <- c("siruta", "sectii.gasite")
colnames(numar.sectii) <- c("siruta", "sectii.existente")
statistica.sectii <- merge(numar.sectii, agregare)
rm(numar.sectii, agregare, gasite)
statistica.sectii$procentaj <- statistica.sectii$sectii.gasite/statistica.sectii$sectii.existente
statistica.sectii$sectii.negasite <- statistica.sectii$sectii.existente - statistica.sectii$sectii.gasite
View(statistica.sectii[order(-statistica.sectii$sectii.negasite),])

print(paste("mai am", sum(statistica.sectii$sectii.negasite), "secţii negăsite"))
timp2 <- Sys.time()
print(paste("a durat", timp2 - timp1, "minute, din care loop-ul", timp4 - timp3))
beep(10)

#rezultate proaste avem, în mod previzibil, în localităţile cu multe secţii
# #dar sunt şi secţii mici fără rezultate bune
# plot(statistica.sectii$sectii.existente, statistica.sectii$procentaj)
# plot(log10(statistica.sectii$sectii.existente)+1, statistica.sectii$procentaj)
# #Alea unde a găsit una din două!
# View(statistica.sectii[statistica.sectii$sectii.existente == 2 & statistica.sectii$procentaj == 0.5,])
# #Distribuţia localităţilor după nr. de secţii (log10)
# hist(log10(table(baza$siruta)+1))
# #Distribuţia secţiilor negăsite
vezi(statistica.sectii[order(-statistica.sectii$sectii.negasite),])
vezi(baza[baza$siruta == 54975,])
vezi(a.ref.work[a.ref.work$siruta == 54975,])
# hist(log10(statistica.sectii$sectii.negasite))
# 
#Compar secţiile din B/S3, să văd care e problema
rstudio::viewData(baza[baza$siruta == 155243,])
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


