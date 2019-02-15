aderencia<-function(dados1,dados2,dire=NA,alvo1)
{
options(scipen=999)
library(xlsx)
library("Hmisc")
if(is.na(dire)!=TRUE)
{
dir.create(paste("C:/Resultados_bivariadas_",dire,"/ADERENCIA",sep=""))
}else{
dir.create("C:/Resultados_bivariadas")

}
cont<-c()
alvo<-dados1[,which(names(dados1) %in% alvo1)]
dados1<-dados1[,-which(names(dados1) %in% alvo1)]
#alvo<-sample(c(0,1),nrow(dados1),prob=c(0.7,0.3),replace=T)


variaveis<-names(dados1)
fx<-list()
indexcc<-c()
cont100<-0
cont110<-0
guardaindex<-c()
for(i in 1:ncol(dados1))
 {
   if(is.numeric(dados1[,i])=="TRUE")
    {
     a<-quantile(dados1[,i],probs = seq(0,1,by=0.2),na.rm=TRUE)
     a<-a[!duplicated(a)]
     z <- cut(dados1[,i],breaks=a)
     b<-as.matrix(table(z,alvo,useNA = "ifany"))
   cont[i-1]<-length(rownames(b))
     }else{
  cont[i-1]<-length(levels(dados1[,i]))
}
print(i)
}
lista<-matrix(nrow=ncol(dados1),ncol=6)
ks2<-c()

for(j in 1:ncol(dados1))
 {
 #j<-1
 if(is.numeric(dados1[,j])=="TRUE")
 {



#j=4

b2<-table(dados1[,j],alvo)
freq_bom<-b2[,1]/sum(b2[,1])
freq_mau<-b2[,2]/sum(b2[,2])
freq_ac_bom<-cumsum( freq_bom )
freq_ac_mau<-cumsum( freq_mau )
diff<-round(abs(freq_ac_bom-freq_ac_mau),3)

ks2[j]<-max(diff)


a<-quantile(dados1[,j],probs = seq(0,1,by=0.05),na.rm=TRUE)
a<-c(min(dados1[,j],na.rm=T),a,max(dados1[,j],na.rm=T))
a<-a[!duplicated(a)]


a1<-a[-c(1,length(a))]
a1<-c(min(dados2[,j],na.rm=T),a1,max(dados2[,j],na.rm=T))
a1<-a1[!duplicated(a1)]
if(length(which(a1>max(dados2[,j],na.rm=T)))>0)
{
a1<-a1[-which(a1>max(dados2[,j],na.rm=T))]
}
z <- cut(dados1[,j],breaks=a,include.lowest = TRUE,dig.lab = 10)
z1<-cut(dados2[,j],breaks=a1,include.lowest = TRUE,dig.lab = 10)
levels(z1)[1]<-levels(z)[1]
levels(z1)[length(levels(z1))]<-levels(z)[length(levels(z1))]

b<-as.matrix(table(z,useNA = "ifany")) ###mudar o 2 por j
d<-rownames(b)
b1<-as.matrix(table(z1,useNA = "ifany")) ###mudar o 2 por j
b.1<-b
if(sum(is.na(rownames(b1))==TRUE)==1 & sum(is.na(rownames(b))==TRUE)==0)
{
b<-rbind(b,0)
rownames(b)[length(rownames(b))]<-NA
}
if(nrow(b)>nrow(b1))
{
 if(sum(is.na(rownames(b))==TRUE)>0)
{
  g101<-rownames(b)
 g101[which(is.na(g101)==TRUE)]<-"Missing"
 }else{
 g101<-rownames(b)
 }
 if(sum(is.na(rownames(b1))==TRUE)>0)
{
  g102<-rownames(b1)
 g102[which(is.na(g102)==TRUE)]<-"Missing"
 }else{
 g102<-rownames(b1)
 }
 
  b.1<-b
 for(kkl in 1:nrow(b))
 {
  if(length(which(g101[kkl]==g102))>0)
   {
 b.1[kkl,1]<-as.numeric(b1[which(g101[kkl]==g102),1])
   }else{
 b.1[kkl,1]<-0
 }
 }
b1<-b.1
}


total<-c(sum(b[,1]))
b<-rbind(b,total)

total<-c(sum(b1[,1]))
b1<-rbind(b1,total)

freq<-c()
for(i in 1:nrow(b1))
{
freq[i]<-b1[i,1]/b1[nrow(b),1]
}
b1<-cbind(b1,freq)


freq<-c()
for(i in 1:nrow(b))
{
freq[i]<-b[i,1]/b[nrow(b),1]
}
b<-cbind(b,freq)


freq_ac<-c()
for(i in 1:nrow(b1))
{
if(i==1)
{
freq_ac[i]<-b1[1,2]
}else{
freq_ac[i]<-freq_ac[i-1]+b1[i,2]
}
}
b1<-cbind(b1,freq_ac)


freq_ac<-c()
for(i in 1:nrow(b))
{
if(i==1)
{
freq_ac[i]<-b[1,2]
}else{
freq_ac[i]<-freq_ac[i-1]+b[i,2]
}
}
b<-cbind(b,freq_ac)

diff<-round(abs(b[,3]-b1[,3]),3)

ks1<-ks.test(dados1[,j],dados2[,j])$statistic

if(sum(is.na(rownames(b))==TRUE)>0)
{
 g100<-rownames(b)
 g100[which(is.na(g100)==TRUE)]<-"Missing"
  rownames(b)<-g100
 }
rownames(b1)<-rownames(b)
e<-data.frame(b,b1,diff,ks1,variaveis[j])

flag<-ifelse(ks1>=0.06,"MAU","BOM")
ad<-c(variaveis[j],flag,ks1,"NULL","NULL",ks2[j])
lista[j,]<-ad

coln<-c("Freq_teste","perc_teste","perc_ac_teste","Freq_online","perc_online","perc_ac_online","diff","KS1")

colnames(e)<-coln

if(ks1>0.1)
{

cont110<-cont110+1
ppt1<-rownames(e[which(abs(e$perc_teste-e$perc_online)<0.02),])
ppt1<-ppt1[-length(ppt1)]

fx[[cont110]]<-ppt1
guardaindex[cont110]<-j
}

if(is.na(dire)!=TRUE)
{
write.xlsx(e,paste(paste("C:/Resultados_bivariadas_",dire,"/ADERENCIA/",sep=""),variaveis[j],".xlsx",sep=""),col.names=TRUE, row.names=TRUE)
}else{
write.xlsx(e,paste("C:/Resultados_bivariadas/",variaveis[j],".xlsx",sep=""),col.names=TRUE, row.names=TRUE)
}

}else{############variaveis categoricas##########

ks2[j]<-0
if(length(levels(dados1[,j]))>=length(levels(dados2[,j])))
{
b<-as.matrix(table(dados1[,j],useNA = "ifany"))
d<-rownames(b)
b1<-c()

for(i in 1:length(d))
{
 b1[i]<-sum(dados2[,j]==d[i],na.rm=T)

}
b.1<-as.matrix(table(dados2[,j],useNA = "ifany"))
if(length(b.1)==1 & sum(is.na(rownames(b.1)))==1)
{

b<-rbind(b,0)
rownames(b)[which(rownames(b)=="")]<-NA

d<-rownames(b)
b1<-c(b1,as.numeric(b.1))
}



}else{
b.1<-as.matrix(table(dados2[,j],useNA = "ifany"))

d<-rownames(b.1)
b1<-c()

for(i in 1:length(d))
{
 b1[i]<-sum(dados1[,j]==d[i],na.rm=T)

}
b=b1
b<-t(cbind(b))

b1<-b.1
colnames(b)<-rownames(b1)
b<-t(cbind(b))
b1<-as.vector(b1)


}
b1<-matrix(b1)
rownames(b1)<-d


total<-c(sum(b[,1]))
b<-rbind(b,total)

total<-c(sum(b1[,1]))
b1<-rbind(b1,total)

freq<-c()
for(i in 1:nrow(b1))
{
freq[i]<-b1[i,1]/b1[nrow(b),1]
}
b1<-cbind(b1,freq)


freq<-c()
for(i in 1:nrow(b))
{
freq[i]<-b[i,1]/b[nrow(b),1]
}
b<-cbind(b,freq)

diff1<-abs(b1[,2]-b[,2])


alerta<-c()
for(i in 1:length(diff1))
{
if(diff1[i]>0.05)
{
 alerta[i]<-"RUIM"
}else{
alerta[i]<-"BOM"
}
}
if(sum(is.na(rownames(b))==TRUE)>0)
{
 g100<-rownames(b)
 g100[which(is.na(g100)==TRUE)]<-"Missing"
  rownames(b)<-g100
 }
 
 if(sum(is.na(rownames(b1))==TRUE)>0)
{
 g100<-rownames(b1)
 g100[which(is.na(g100)==TRUE)]<-"Missing"
  rownames(b1)<-g100
 }

e<-data.frame(b,b1,diff1,alerta,variaveis[j])
count<-sum(e[,5]>0.05)
flag1<-ifelse(count>0,"MAU","BOM")
tot<-nrow(e)-1
ad1<-c(variaveis[j],flag1,"NULL",count,tot,ks2[j])


lista[j,]<-ad1

coln<-c("Freq_teste","perc_teste","Freq_online","perc_online","diff","Alerta","Variavel")
colnames(e)<-coln


ppt1<-rownames(e[which(abs(e$perc_teste-e$perc_online)<0.02),])
ppt1<-ppt1[-length(ppt1)]
cont110<-cont110+1
fx[[cont110]]<-ppt1
guardaindex[cont110]<-j

if(is.na(dire)!=TRUE)
{
write.xlsx(e,paste(paste("C:/Resultados_bivariadas_",dire,"/ADERENCIA/",sep=""),variaveis[j],".xlsx",sep=""),col.names=TRUE, row.names=TRUE)
}else{
write.xlsx(e,paste("C:/Resultados_bivariadas/",variaveis[j],".xlsx",sep=""),col.names=TRUE, row.names=TRUE)
}

}

print(j)
}

names(fx)<-variaveis[guardaindex]

colnames(lista)<-c("Variaveis","SITUACAO","KS1","DIFF NIVEIS >5%","TOTAL DE NIVEIS","KS2")
if(is.na(dire)!=TRUE)
{
write.xlsx(lista,paste(paste("C:/Resultados_bivariadas_",dire,"/ADERENCIA/",sep=""),"relatorios",".xlsx",sep=""),col.names=TRUE, row.names=TRUE)
}else{
write.xlsx(lista,paste("C:/Resultados_bivariadas/","relatorios",".xlsx"),col.names=TRUE, row.names=TRUE)
}

return(fx)

}

