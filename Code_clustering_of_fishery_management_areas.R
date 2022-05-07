#Input Data
data<-read.csv(file.choose(),header=T,sep=";",dec=",")
attach(data)
str(data)

#PENGUJIAN ASUMSI
#Non-Multikolinieritas
q=cbind(Cumi.cumi,Ikan.Demersal,Ikan.Karang,Ikan.Pelagis.Besar,Ikan.Pelagis.Kecil,Kepiting,Lobster,Rajungan,Udang.Penaeid)
f=cor(q)
vif=diag(solve(f))
vif

#MENENTUKAN UKURAN JARAK ANTAR DATA
Komoditas = data.frame(Cumi.cumi,Ikan.Demersal,Ikan.Karang,Ikan.Pelagis.Besar,Ikan.Pelagis.Kecil,Kepiting,Lobster,Rajungan,Udang.Penaeid)
d<-dist(Komoditas, method="euclidean")
d

#PROSES KLASTER DENGAN METODE HIERARKI
par(mfrow=c(2,2))

#Single Linkage
fit1<-hclust(d, method="single")
plot(fit1,main="Dendrogram Single Linkage")

cop1<-cophenetic(fit1)
cor(d,cop1)

#Complete Linkage
fit2<-hclust(d, method="complete")
plot(fit2,main="Dendrogram Complete Linkage")

cop2<-cophenetic(fit2)
cor(d,cop2)

#Average Linkage
fit3<-hclust(d, method="average")
plot(fit3,main="Dendrogram Average Linkage")

cop3<-cophenetic(fit3)
cor(d,cop3)

#Ward's Linkage
fit4<-hclust(d, method="ward.D")
plot(fit4,main="Dendrogram Ward's Method")

cop4<-cophenetic(fit4)
cor(d,cop4)

#Bandingkan hasil korelasi, yang paling mendekati 1 = metode bagus dan tepat

#MENENTUKAN METODE TERBAIK DAN KLASTER YANG TERBENTUK
par(mfrow=c(1,3))
fit3<-hclust(d, method="average")
plot(fit3,main="Dendrogram Average Linkage")
rect.hclust(fit3,k=4,border=2:6)

#PROFILING KLASTER
groups<-cutree(fit3,k=2)
klaster<-data.frame(Komoditas,groups)
klaster
klaster1<-data.frame(klaster[which(groups==1),1:9])
klaster2<-data.frame(klaster[which(groups==2),1:9])
klaster3<-data.frame(klaster[which(groups==3),1:9])
summary(klaster1)
summary(klaster2)
summary(klaster3)
