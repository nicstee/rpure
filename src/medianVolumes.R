setwd("C:/Users/claude/Desktop/R folder/eclipse/workspace/rpure/src")

source(file="connectionDB.R",, echo=TRUE)

actions <- dbGetQuery(con, "SELECT id,code, (select close from quotes where id_stock = t1.id order by date desc limit 1) as last
 from stocks t1 order by id")
## actions <- dbGetQuery(con, "SELECT * from stocks order by id")
q <- dbGetQuery(con, "SELECT distinct date from quotes where date > '2014-01-01' order by date asc")
nom <-c("date")
for (id in actions$id){
	nom <- c(nom,trimws(actions$code[id]))
	x <- dbGetQuery(con, paste(paste(
	"SELECT volume from quotes where date > '2014-01-01'and id_stock = ",id,sep=""),"order by date asc",
	sep=" "))
	q <- cbind(q,x)
}
names(q) <- nom
v <- q[,-1] # suppression de la colonne date
vm <- as.matrix(v)
# calcul des moyennes
nbc <- ncol(vm)
moy <- c()
q05 <- c()
for( i in seq(1:nbc)){
	moy<-append(moy,(round(mean(vm[,i],na.rm=TRUE),0)))
	q05<-append(q05,(round(quantile(vm[,i],.05,na.rm=TRUE),0)))
}
mm <-rbind(moy,q05)
colnames(mm)<- nom[-1]
nbc <- ncol(mm)
for (id in actions$id){
	mm[,id]<-round(actions$last[id]*mm[,id]/100000)
}
mm
