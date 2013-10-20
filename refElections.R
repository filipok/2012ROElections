#Source the file below:
#source("loadDump.R")
#or if you prefer the raw data:
#source("get_data.R")

#am siruta la referendum şi am şi la sv.xlsx în locale 2012
#
# cumulare la nivel de CIRC/SIRUTA (similar funcţiei CumulatCirc din locale2012)
work = ref2012Sv[ref2012Sv$tara == "",]
ref2012Circ = aggregate(subset(work, 
                                  select = -c(JUD, DEN_JUD, tara, siruta, SV, 
                                              DEN_CIRC_R, adresa, sector, mediu, 
                                              sortare, moc)), 
                           by = list(work$JUD, work$siruta), sum)
colnames(ref2012Circ)[1] = "JUD"
colnames(ref2012Circ)[2] = "siruta"
coloane = work[, c("JUD", "DEN_JUD", "siruta", "DEN_CIRC_R", "mediu",
                    "sortare",   "moc")]
coloane = unique(coloane)
ref2012Circ = merge(ref2012Circ, coloane)
ref2012Circ1 = ref2012Circ[,c("JUD", "DEN_JUD", "siruta", "DEN_CIRC_R")]
ref2012Circ2 = subset(ref2012Circ, select = -c(JUD, siruta, DEN_JUD, DEN_CIRC_R))
ref2012Circ = cbind(ref2012Circ1, ref2012Circ2)
rm(ref2012Circ1, ref2012Circ2)
#cumulare la nivel de judeţ
work = ref2012Sv[ref2012Sv$tara == "",]
ref2012Jud = aggregate(subset(work, 
                                 select = -c(JUD, DEN_JUD, tara, siruta, SV, 
                                             DEN_CIRC_R, adresa, sector, mediu, 
                                             sortare, moc)), 
                          by = list(work$JUD), sum)
colnames(ref2012Jud)[1] = "JUD"
coloane = work[, c("JUD", "DEN_JUD")]
coloane = unique(coloane)
ref2012Jud = merge(ref2012Jud, coloane)
ref2012Jud1 = ref2012Jud[,c("JUD", "DEN_JUD")]
ref2012Jud2 = subset(ref2012Jud, select = -c(JUD, DEN_JUD))
ref2012Jud = cbind(ref2012Jud1, ref2012Jud2)
rm(ref2012Jud1, ref2012Jud2, coloane)
#cumulare la nivel naţional
ref2012Nat = aggregate(subset(ref2012Sv, select = -c(mediu, sortare, moc, 
                                                             sector, adresa, SV, 
                                                             DEN_CIRC_R, siruta, tara, 
                                                             DEN_JUD, JUD)), 
                          by = list(ref2012Sv$tara), FUN = sum)
colnames(ref2012Nat)[1] = "Tara"
ref2012Nat[1,1] = "ROMANIA"
rm(work)
#funcţie procente
ProcenteRef = function (x){
  work = subset(x, select = -c(tapu, vveda, vvenu, tvn, nvc))
  work$TAPU_P_Ref = x$tapu/x$ta
  work$TVVE_P_Ref = (x$tapu - x$tvn)/x$tapu
  work$TVN_P_Ref = x$tvn/x$tapu
  work$VVEDA = x$vveda/(x$tapu - x$tvn)
  work$VVENU = x$vvenu/(x$tapu - x$tvn)
  work
}

ref2012SvPro = ProcenteRef(ref2012Sv)
ref2012CircPro = ProcenteRef(ref2012Circ)
ref2012JudPro = ProcenteRef(ref2012Jud)
ref2012SvNat = ProcenteRef(ref2012Nat)

#vizualizare participare maximă
#q = ref2012Sv.pro[ref2012Sv.pro$TAPU_P > 1 & ref2012Sv.pro$tara == "",]
#View(q[order(q$TAPU_P),])