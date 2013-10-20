********************************************************************************
Fişierul parlElections.R generează un folder CSV cu fişiere csv.
Fişierele CSV din subfolderul  CSV conţin rezultatele la alegerile parlamentare 
din 9 decembrie 2012, obţinute din următoarele surse:
********************************************************************************
- BEC, www.becparlamentare2012, pentru rezultatele la alegeri la nivel de secţie
de votare; fişierele BEC se găsesc în subfolderul /parlamentare2012, din care
am folosit fişierul RezultateNivelSectieParlamentare2012.csv şi fişierul 
sv_bec.xls (convertit în sv_bec.csv), acesta din urmă pentru adresele secţiilor;
- Autoritatea Electorală Permanentă, www.roaep.ro, pentru codurile Siruta*
aferente adresei fiecărei secţii de votare; fişierele ROAEP se găsesc în 
subfolderul /parlamentare2012/SV_ROAEP/judete şi provin de la adresa 
http://www.roaep.ro/ro/section.php?id=25&l2=48&ids=121&an=2012;
- EARTH.UNIBUC.RO, http://earth.unibuc.ro/download/romania-seturi-vectoriale,
pentru coordonatele geografice ale tuturor localităţilor din România 
identificate pe baza codului Siruta; fişierul utilizat se găseşte în subfolderul
/siruta2008.

*Detalii despre codurile Siruta se găsesc la:
http://colectaredate.insse.ro/senin/classifications.htm?selectedClassification=&action=&classificationName=SIRUTA 
şi http://ro.wikipedia.org/wiki/SIRUTA

********************************************************************************
Datele la nivel de secţie de votare sunt incluse în fişierele:
********************************************************************************
- 2012cdepsv.csv pentru Camera Deputaţilor
- 2012sensv.csv pentru Senat
Datele la nivl de localitate (municipiu/oraş/comună) sunt incluse în:
- 2012cdepcirc.csv pentru Camera Deputaţilor
- 2012sencirc.csv pentru Senat
Datele la nivel de colegiu sunt incluse în:
- 2012cdepcolegiu.csv pentru Camera Deputaţilor
- 2012sensv.csv pentru Senat
Datele la nivel de judeţ sunt incluse în:
- 2012cdepjudet.csv pentru Camera Deputaţilor
- 2012senjudet.csv pentru Senat

Toate fişierele au fost obţinute prin prelucrarea fişierului
RezultateNivelSectieParlamentare2012.csv de la www.becparlamentare2012.ro

********************************************************************************
Fişierele 2012cdepsv.csv şi 2012sensv.csv (cele cu rezultatele la nivel de 
secţie) au următoarea structură:
********************************************************************************

1. Pentru Camera Deputaţilor:

Codul Siruta (scurt, fără cifra de control) al localităţii în care e secţia
de votare
 [1] "SirutaComp"                                                        
Judeţul secţiei de votare
[2] "JUD"                                                               
Numărul secţiei de votare
[3] "SV"                                                                
Denumirea localităţii (municipiu/oraş/comună)
[4] "DEN_CIRC"                                                          
Denumirea judeţului
[5] "DEN_JUD"                                                           
Numărul colegiului
[6] "numarColegiu"                                                      
Tipul colegiului (senat/cameră)
[7] "numeTipColegiu"                                                    

Număr total de alegători înscrişi pe liste ( = 9 + 10 + 11)
[8] "NrAlegatoriListe"                                                  
Număr de alegători înscrişi pe listele permanente
[9] "NrAlegatoriListePermanent"                                         
Număr de alegători înscrişi pe listele suplimentare
[10] "NrAlegatoriListeSuplimentare"                                      
Număr de alegători înscrişi pe listele speciale
[11] "NrAlegatoriUrnaSpeciala"                                           
Număr total alegători prezentaţi la urne (= 13 + 14 + 15)
[12] "NrAlegatoriPrezentatiLaUrne"                                       
Număr de alegători prezenţi de pe lista permanentă
[13] "NrAlegatoriPrezentiLaUrneListaPermanenta"                          
Număr de alegători prezenţi de pe lista suplimentară
[14] "NrAlegatoriPrezentiLaUrneListaSuplimentara"                        
Număr de alegători prezenţi de pe lista specială
[15] "NrAlegatoriPrezentiLaUrneListaSpeciala"                            

Numărul de buletine de vot primite ( = 17 + 18 + 19 + 20)
[16] "NrBuletineVotPrimite"                                              
Număr de buletine de vot anulate
[17] "NrBuletineVotAnulate"                                              
Total voturi valabil exprimate
[18] "TotalVoturiValabilExprimateLaNivelSectie"                          
Număr de voturi nule
[19] "NrVoturiNule"                                                      
Număr de voturi albe
[20] "NrVoturiAlbe"                                                      

Partide
[21] "UNIUNEA SOCIAL LIBERALĂ"                                           
[22] "UNIUNEA DEMOCRATĂ MAGHIARĂ DIN ROMÂNIA"                            
[23] "ALIANŢA ROMÂNIA DREAPTĂ"                                           
[24] "PARTIDUL ROMÂNIA MARE"                                             
[25] "PARTIDUL POPORULUI - DAN DIACONESCU"                               
[26] "ASOCIAŢIA ITALIENILOR DIN ROMÂNIA - RO.AS.IT."                     
[27] "FEDERAŢIA COMUNITĂŢILOR EVREIEŞTI DIN ROMÂNIA"                     
[28] "UNIUNEA POLONEZILOR DIN ROMÂNIA"                                   
[29] "ASOCIAŢIA LIGA ALBANEZILOR DIN ROMÂNIA"                            
[30] "COMUNITATEA RUŞILOR LIPOVENI DIN ROMÂNIA"                          
[31] "UNIUNEA DEMOCRATICĂ A SLOVACILOR ŞI CEHILOR DIN ROMÂNIA"           
[32] "UNIUNEA UCRAINENILOR DIN ROMÂNIA"                                  
[33] "UNIUNEA BULGARĂ DIN BANAT - ROMÂNIA"                               
[34] "FORUMUL DEMOCRAT AL GERMANILOR DIN ROMÂNIA"                        
[35] "ASOCIAŢIA PARTIDA ROMILOR PRO-EUROPA"                              
[36] "UNIUNEA CROAŢILOR DIN ROMÂNIA"                                     
[37] "UNIUNEA DEMOCRATĂ TURCĂ DIN ROMÂNIA"                               
[38] "UNIUNEA ELENĂ DIN ROMÂNIA"                                         
[39] "UNIUNEA DEMOCRATĂ A TĂTARILOR TURCO-MUSULMANI DIN ROMÂNIA"         
[40] "UNIUNEA ARMENILOR DIN ROMÂNIA"                                     
[41] "UNIUNEA CULTURALĂ A RUTENILOR DIN ROMÂNIA"                         
[42] "ASOCIAŢIA MACEDONENILOR DIN ROMÂNIA"                               
[43] "UNIUNEA SÂRBILOR DIN ROMÂNIA"                                      
[44] "ERDÉLYI MAGYAR NÉPPÁRT - PARTIDUL POPULAR MAGHIAR DIN TRANSILVANIA"
[45] "PARTIDUL ECOLOGIST ROMÂN"                                          
[46] "PARTIDUL ALIANŢA SOCIALISTĂ"                                       
[47] "PARTIDUL POPULAR"                                                  
[48] "PARTIDUL POPULAR ŞI AL PROTECŢIEI SOCIALE"                         
[49] "PARTIDUL SOCIAL DEMOCRAT AL MUNCITORILOR"                          
[50] "PARTIDUL NAŢIONAL DEMOCRAT CREŞTIN"                                

Independenţi Camera Deputaţilor (variabile de forma JUDEŢ_NRCOLEGIU_NUME)
[51] "BIHOR_ 5_NOANE GHEORGHE-VIOREL"                                    
[52] "BRAŞOV_ 1_HUTOPILĂ VIOLETA"                                        
[53] "CLUJ_ 1_ALEXA LIVIU-DORU-ALIN"                                     
[54] "DÂMBOVIŢA_ 8_PETRE IONEL"                                          
[55] "HUNEDOARA_ 2_POPA GHEORGHE-GABRIEL"                                
[56] "IAŞI_ 6_RAŞCU BENONI"                                              
[57] "OLT_ 3_DRĂGHICESCU DRAGOŞ-NICOLAE"                                 
[58] "OLT_ 2_STAN NICOLAE"                                               
[59] "TIMIŞ_ 4_GOCIU ADRIAN"                                             
[60] "Mun. BUCUREŞTI_16_PIPEREA GHEORGHE"                                
[61] "Mun. BUCUREŞTI_17_FILIP CONSTANTIN"                                

Adresa secţiei de votare
[62] "Adresa.parl"    
Coordonatele geografice ale localităţii identificate cu codul Siruta (lat/long)
[63] "X"                                                                 
[64] "Y"

2. Pentru Senat

Codul Siruta (scurt, fără cifra de control) al localităţii în care e secţia
de votare
 [1] "SirutaComp"                                                        
Judeţul secţiei de votare
[2] "JUD"                                                               
Numărul secţiei de votare
[3] "SV"                                                                
Denumirea localităţii (municipiu/oraş/comună)
[4] "DEN_CIRC"                                                          
Denumirea judeţului
[5] "DEN_JUD"                                                           
Numărul colegiului
[6] "numarColegiu"                                                      
Tipul colegiului (senat/cameră)
[7] "numeTipColegiu"                                                    

Număr total de alegători înscrişi pe liste ( = 9 + 10 + 11)
[8] "NrAlegatoriListe"                                                  
Număr de alegători înscrişi pe listele permanente
[9] "NrAlegatoriListePermanent"                                         
Număr de alegători înscrişi pe listele suplimentare
[10] "NrAlegatoriListeSuplimentare"                                      
Număr de alegători înscrişi pe listele speciale
[11] "NrAlegatoriUrnaSpeciala"                                           
Număr total alegători prezentaţi la urne (= 13 + 14 + 15)
[12] "NrAlegatoriPrezentatiLaUrne"                                       
Număr de alegători prezenţi de pe lista permanentă
[13] "NrAlegatoriPrezentiLaUrneListaPermanenta"                          
Număr de alegători prezenţi de pe lista suplimentară
[14] "NrAlegatoriPrezentiLaUrneListaSuplimentara"                        
Număr de alegători prezenţi de pe lista specială
[15] "NrAlegatoriPrezentiLaUrneListaSpeciala"                            

Numărul de buletine de vot primite ( = 17 + 18 + 19 + 20)
[16] "NrBuletineVotPrimite"                                              
Număr de buletine de vot anulate
[17] "NrBuletineVotAnulate"                                              
Total voturi valabil exprimate
[18] "TotalVoturiValabilExprimateLaNivelSectie"                          
Număr de voturi nule
[19] "NrVoturiNule"                                                      
Număr de voturi albe
[20] "NrVoturiAlbe"                                                   

Partide
[21] "UNIUNEA SOCIAL LIBERALĂ"                                           
[22] "UNIUNEA DEMOCRATĂ MAGHIARĂ DIN ROMÂNIA"                            
[23] "ALIANŢA ROMÂNIA DREAPTĂ"                                           
[24] "PARTIDUL ROMÂNIA MARE"                                             
[25] "PARTIDUL POPORULUI - DAN DIACONESCU"                               
[26] "ERDÉLYI MAGYAR NÉPPÁRT - PARTIDUL POPULAR MAGHIAR DIN TRANSILVANIA"
[27] "PARTIDUL ECOLOGIST ROMÂN"                                          
[28] "PARTIDUL SOCIAL DEMOCRAT AL MUNCITORILOR"                          
[29] "PARTIDUL NAŢIONAL DEMOCRAT CREŞTIN"                                
[30] "PARTIDUL POPULAR"                                                  
[31] "PARTIDUL POPULAR ŞI AL PROTECŢIEI SOCIALE"                         
[32] "PARTIDUL ALIANŢA SOCIALISTĂ"                                       

Independenţi  (variabile de forma JUDEŢ_NRCOLEGIU_NUME)
[33] "BOTOŞANI_3_CONŢAC CONSTANTIN"                                      

Adresa secţiei de votare
[34] "Adresa.parl"    
Coordonatele geografice ale localităţii identificate cu codul Siruta (lat/long)
[35] "X"                                                                 
[36] "Y"

********************************************************************************
Fişierele la nivel de colegiu, localitate (municipiu/oraş/comună) şi judeţ au
aceleaşi variabile, cu următoarele excepţii:
********************************************************************************

1. Pentru rezultatele la nivel de localitate (municipiu/oraş/comună), faţă de 
rezultatele la nivel de secţie lipsesc datele referitoare la codul Siruta 
("SirutaComp"), la secţia de votare ("SV"), la numărul colegiului 
(numarColegiu"), la tipul colegiului ("numeTipColegiu"), la adresa secţiei
de votare ("Adresa.parl") şi la coordonatele geografice ("X" şi "Y").
***Ca fapt divers, unele oraşe sunt împărţite între mai multe colegii.***

2. Pentru rezultatele la nivel de colegiu, faţă de rezultatele la nivel de
localitate(municipiu/oraş/comună) lipsesc datele referitoare la localitate 
("DEN_CIRC") şi reapar datele referitoare la numărul colegiului 
(numarColegiu") şi tipul colegiului ("numeTipColegiu").

3. Pentru rezultatele la nivel de judeţ, faţă de rezultatele la nivel de colegiu
lipsesc datele referitoare la numărul colegiului (numarColegiu") şi tipul 
colegiului ("numeTipColegiu").

********************************************************************************
Codul R utilizat pentru prelucrarea datelor se găseşte în fişierul
parlamentare2012v0.4.R.
Pentru rulare este necesar şi pachetul xlsReadWrite. 
Codul presupune că porneşte din directorul de lucru al R, iar subfolderele cu 
date se găsesc tot acolo. Fişierele CSV sunt generate în subfolderul CSV, care 
trebuie să existe.
********************************************************************************