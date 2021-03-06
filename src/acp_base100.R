setwd("C:/Users/claude/Desktop/R folder/eclipse/workspace/rpure/src")

source(file="connectionDB.R",, echo=TRUE)

actions <- dbGetQuery(con, "SELECT * from stocks where actived order by id")
q <- dbGetQuery(con, "SELECT distinct date from quotes where date > '2014-01-01' order by date asc")
nom <-c("date")
for (id in actions$id){
	nom <- c(nom,trimws(actions$code[id]))
	x <- dbGetQuery(con, paste(paste("SELECT close from quotes where date > '2014-01-01'and id_stock = ",id,sep=""),"order by date asc",sep=" "))
	q <- cbind(q,x)
}
names(q) <- nom
v <- q[,-1] # suppression de la colonne date
# Recherche des NA dans la table quotes (n�cessit� d'ajustements de la tables)
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
# analyse en composantes principales
pca <- prcomp(vm)
# rotation des axes orth. de l'espace (actions,cotations) pour mettre
# en �vidence les composntes principales
round(pca$rotation,2)
# la fraction d'information (sdev="standard deviation") contenue
# dans chacune des composantes
round(pca$sdev,2)
# proportions de variance de chaque composante
round(100 * pca$sdev^2 / sum(pca$sdev^2),2)
#variance totale des deux 1�res composantes
round(sum(100 * (pca$sdev^2)[1:2] / sum(pca$sdev^2)),2)
# visualisation de la r�partition des variances
# plot(pca)
# projection de cotations dans le plan des 2 principales composantes
# plot(pca$x[,1:2], col=vm[,c(5,6)]) # pca$x[,1:2] pour les 2 1�res comp., vm[,c(5,6)] pour les actions 5 et 6
# autre visualisation
biplot(pca,cex=c(.1,.5))
title(main="ACP BASE 100",cex.main=1.)





