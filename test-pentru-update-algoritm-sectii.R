listapar <- vector ("list", length(unique(a.par$siruta)))
listaref <- vector ("list", length(unique(a.par$siruta)))
for (i in 1:length(unique(a.par$JUD))){
  listapar[[i]] <- baza[baza$siruta == unique(a.par$siruta)[i],]
  listaref[[i]] <- a.ref[a.ref$siruta == unique(a.par$siruta)[i],]
}

# listapar <- vector ("list", length(unique(a.par$JUD)))
# listaref <- vector ("list", length(unique(a.par$JUD)))
# for (i in 1:length(unique(a.par$JUD))){
#   listapar[[i]] <- baza[baza$JUD == unique(a.par$JUD)[i],]
#   listaref[[i]] <- a.ref[a.ref$JUD == unique(a.par$JUD)[i],]
# }

# rm(listapar, listaref)

