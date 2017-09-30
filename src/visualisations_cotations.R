setwd("C:/Users/claude/Desktop/R folder/eclipse/workspace/rpure/src")

source(file="connectionDB.R",, echo=TRUE)

actions <- dbGetQuery(con, "SELECT * from stocks order by id")
q <- dbGetQuery(con, "SELECT distinct date from quotes where date > '2014-01-01' order by date asc")
nom <-c("date")
for (id in actions$id){
	nom <- c(nom,trimws(actions$code[id]))
	x <- dbGetQuery(con, paste(paste("SELECT close from quotes where date > '2014-01-01'and id_stock = ",id,sep=""),"order by date asc",sep=" "))
	q <- cbind(q,x)
}
names(q) <- nom
v <- q[,-1] # suppression de la colonne date
# Recherche des NA dans la table quotes (nécessité d'ajustements de la tables)
vx <- unlist(v)
vx <- unname(vx)
vx[!complete.cases(vx)]
# transformation en une matrice pour l'analyse
vm <- as.matrix(v)
# mise en base 100
nbc <- ncol(vm)
for( i in seq(1:nbc)){
	vm[,i]<-round(100*vm[,i]/vm[1,i],3)
}

sortVector <- sort(vm[nrow(vm),],decreasing=TRUE)
sortNoms <- names(sortVector)
sortMatrix <- matrix(sort(sortVector,decreasing=TRUE))
rownames(sortMatrix) <- sortNoms
sortMatrix
plot(q[,1],vm[,1],cex=.5,type="l",ylim=c(min(vm),max(vm)))
nbc <- ncol(vm)
for( i in seq(2:nbc)){
	lines(q[,1],vm[,i],type="l",col=palette()[i%%8])
}

