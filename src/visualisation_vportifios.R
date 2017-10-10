setwd("C:/Users/claude/Desktop/R folder/eclipse/workspace/rpure/src")

source(file="connectionDB.R",, echo=TRUE)

# RANDOM FULL
q <- dbGetQuery(con, paste("select id_portfolio,date,rendannuel,vportfolio from rendementsmovements",
				" where name ='random' order by id_portfolio,date",sep=""))
q[,1]<-q[,1] - min(q[,1]) + 1
nc<-max(q[,1])
nr<-length(q[(q[,1]==1),1])
qm <- matrix(0.,ncol=nc,nrow=nr)
qr <- matrix(0.,ncol=nc,nrow=nr)
for(i in seq(1,nc))qm[,i]<-q[(q[,1]==i),3] # rendement annuel
for(i in seq(1,nc))qr[,i]<-q[(q[,1]==i),4] # val. portefeuille
qmmrandom <- c(rep(0.,nr))
qvmrandom <- c(rep(0.,nr))
for(i in seq(1,nr))qmmrandom[i]<-mean(qm[i,])
for(i in seq(1,nr))qvmrandom[i]<-mean(qr[i,])

colmax<-which(qr[nr,]==max(qr[nr,]))
colmin<-which(qr[nr,]==min(qr[nr,]))
qrmax<-qr[,colmax]
qrmin<-qr[,colmin]
# MINIMAX
q <- dbGetQuery(con, paste("select id_portfolio,date,rendannuel,vportfolio from rendementsmovements",
				" where name ='minimax' order by id_portfolio,date",sep=""))
# VISUALISATION
plot(q[,2],qvmrandom,type="l")
lines(q[,2],q[,4],col=2)
lines(q[,2],qrmax,col=3)
lines(q[,2],qrmin,col=4)


