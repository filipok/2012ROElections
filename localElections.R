#Source the file below:
#source("loadDump.R")
#or if you prefer the raw data:
#source("get_data.R")

#Load the required functions
source("localFunctions.R")

#Examples:

#pe judeţ pentru cele unde avem CIRC = 0
cjuAbsJud = AbsolutJudet(CJU)
pcjAbsJud = AbsolutJudet(PCJ)
#pe judeţ pentru cele unde nu avem CIRC = 0, cu agregare independenţi
cloCumJud = CumulatJudet(CLO) #cumulare pe bază de circumscripţie
priCumJud = CumulatJudet(PRI) #cumulare pe bază de circumscripţie

#pe circumscripţie -> CIRC != 0 & SV == 0
#pentru cele cu date la nivel de circumscripţie
cloAbsCirc = AbsolutCirc(CLO, TRUE)
priAbsCirc = AbsolutCirc(PRI, TRUE)
#pentru cele fără date la nivel de circumscripţie
cjuCumCirc = CumulatCirc(CJU) #cumulare pe bază de secţie de votare
pcjCumCirc = CumulatCirc(PCJ) #cumulare pe bază de secţie de votare

#pe secţie de votare - pentru PRI şi CLO avem date doar pentru Bucureşti
pcjAbsSv = AbsolutSv(PCJ)
cjuAbsSv = AbsolutSv(CJU)
cloAbsSvBuc = AbsolutSv(CLO, TRUE)
priAbsSvBuc = AbsolutSv(PRI, TRUE)

Procente(AbsolutJudet(CJU, TRUE)) #exemplu utilizare funcţie Procente
SimpLocale2012(AbsolutSv(CJU)) #exemplu utilizare funcţie SimpLocale2012
Procente(SimpLocale2012(AbsolutSv(CJU, TRUE))) #combinare funcţii
#
#Rezultate primării la nivel de circumscripţie TUR 1 + TUR 2
priCirc = Procente(SimpLocale2012(AbsolutCirc(PRI, TRUE)))
priCirc2 = TUR2[TUR2$TIPPV == 1,]
priCirc2 = Procente(SimpLocale2012(AbsolutCirc(priCirc2, TRUE)))
priCircAll = rbind(priCirc, priCirc2)