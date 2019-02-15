
Testa_dados<-function(dados1)
{
f<-c()
cont<-0
ggg100<-which(sapply(dados1, function(x) !all(is.na(as.Date(as.character(x),format="%d/%m/%Y"))))==TRUE)
ggg101<-which(sapply(dados1, function(x) !all(is.na(as.Date(as.character(x),format="%Y-%m-%d"))))==TRUE)
for(j in 1:ncol(dados1))
{
if(sum(is.na(dados1[,j])==TRUE)==nrow(dados1))
{
 cont<-cont+1
    f[cont]<-j
}

 if(is.numeric(dados1[,j])==TRUE)
 {
  g<-unique(dados1[,j])
  g<-g[-which(is.na(g)==TRUE)]
  if(length(g)==1)
   {
   cont<-cont+1
    f[cont]<-j
   }
  if(sum(is.na(dados1[,j]))/nrow(dados1)==0)
  {
 if(sd(dados1[,j])==0)
  {
    cont<-cont+1
    f[cont]<-j
   }
}else{
if(sum(is.na(dados1[,j]))==nrow(dados1))
  {
  cont<-cont+1
  f[cont]<-j
  }
}

}

if(length(unique(dados1[,j]))==nrow(dados1))
{
cont<-cont+1
f[cont]<-j
}

if(length(unique(dados1[,j]))==1)
{
cont<-cont+1
f[cont]<-j
}

print(j)
}
if(length(ggg100)>0)
{
f<-c(f,ggg100)
f<-f[!duplicated(f)]
}
if(length(ggg101)>0)
{
f<-c(f,ggg101)
f<-f[!duplicated(f)]
}
f<-f[!duplicated(f)]
return(f)
}

#dados1<-read.csv("D:/Users/CLP124604/Desktop/bank/bank-full.csv",sep=";",h=T)
#var_select="education"
#alvo=dados1[,"id"]
#manual=FALSE
#bivariada_table(dados1,alvo,var_select)
bivariada_table<-function(dados1,alvo,var_select,manual=FALSE)
{
library(stringr)
guarda_niv<-list()
guarda_var<-list()
cont2<-0
j=var_select
 if(is.numeric(dados1[,j])=="TRUE")
 {
a<-quantile(dados1[,j], probs = seq(0,1,by=0.2),na.rm=TRUE)
a<-a[!duplicated(a)]
if((sum(dados1[,j]==0,na.rm=TRUE)/length(dados1[,j]))>0.05)
{
if(sum(a==0)==1)
{
a<-c(a,0.0001)
}else{
a<-c(a,0,0.0001)
}
a<-sort(a)
}

if((sum(dados1[,j]==-1,na.rm=TRUE)/length(dados1[,j]))>0.05)
{
if(sum(a==0)==1)
{
a<-c(a,-1)
}else{
a<-c(a,0,-1)
}
a<-sort(a)
}

a<-a[!duplicated(a)]


z <- cut(dados1[,j],breaks=as.numeric(a),include.lowest = TRUE,right=FALSE,dig.lab = 10)
fd<-ifelse(summary(z)/length(z)>0.68,1,0)
fd1<-ifelse(sum(is.na(z)==TRUE)/length(z)>0.23,1,0)
max1<-max(a)
if(sum(fd)==0 & fd1==0)
{
if(length(a)<=4)
{
flag<-"true"
ef<-0.001
while(flag=="true")
{
ef<-ef+0.01
a<-quantile(dados1[,j], probs = seq(0,1,by=ef),na.rm=TRUE)
a<-a[!duplicated(a)]
if(length(a)<=5)
{ 
 flag<-"false"
}
}
}
}
a<-c(a,max1)

if((sum(dados1[,j]==0,na.rm=TRUE)/length(dados1[,j]))>0.05)
{
if(sum(a==0)==1)
{
a<-c(a,0.0001)
}else{
a<-c(a,0,0.0001)
}
a<-sort(a)
}


if((sum(dados1[,j]==-1,na.rm=TRUE)/length(dados1[,j]))>0.05)
{
if(sum(a==0)==1)
{
a<-c(a,-1)
}else{
a<-c(a,0,-1)
}
a<-sort(a)
}

a<-a[!duplicated(a)]




if(manual==TRUE)
{
z <- cut(dados1[,j],breaks=as.numeric(a),include.lowest = TRUE,right=FALSE,dig.lab = 10) 
b<-as.matrix(table(z,alvo,useNA = "ifany"))
total<-c()
for(i in 1:nrow(b))
{
total[i]<-sum(b[i,])
}
b<-cbind(b,total)
total<-c(sum(b[,1]),sum(b[,2]),sum(b[,1:2]))
b<-rbind(b,total)
freq<-c()
for(i in 1:nrow(b))
{
freq[i]<-b[i,3]/b[nrow(b),3]
}
freq<-round(freq,3)
b<-cbind(b,freq)
rr<-c()
for(i in 1:nrow(b))
{
rr[i]<-(b[i,1]/b[nrow(b),1])/(b[i,2]/b[nrow(b),2])
}
rr<-round(rr,3)
b<-cbind(b,rr)
flag_manu<-TRUE
fg<-rownames(b)
plot(rr[-length(rr)],type="l",col="red")
et<-0
ap1<-a
while(flag_manu==TRUE)
{
 print(b)
 et<-et+1
 if(et>1)
 {
 print(b1)
 }
 cat("A variavel é:",var_select,"\n")
 cat("Você concorda com este intervalo?", "Se sim digite 1", ",c.c. digite 2.","\n")
 ap<-readLines(n=1)
 ap<-as.numeric(ap)
 if(ap==1)
 {  
   flag_manu<-"FALSE"
}else{
 if(et>1)
 {
 print(b1)
 }else{
 print(b)
}
 print(ap1)
 cat("Digite a quantidade a ser adicionada","\n")
 af<-readLines(n=1)
 af<-as.numeric(af)
cat("Digite o novo intervalo!" ,"\n")
 ap1<-readLines(n=af) 
 ap1<-as.numeric(ap1) 
 ap1<-c(min(a),ap1,max(a))
z <- cut(dados1[,j],breaks=as.numeric(ap1),include.lowest = TRUE,right=FALSE,dig.lab = 10) 
b1<-as.matrix(table(z,alvo,useNA = "ifany"))
total<-c()
for(i in 1:nrow(b1))
{
total[i]<-sum(b1[i,])
}
b1<-cbind(b1,total)
total<-c(sum(b1[,1]),sum(b1[,2]),sum(b1[,1:2]))
b1<-rbind(b1,total)
for(i in 1:nrow(b1))
{
freq[i]<-b1[i,3]/b1[nrow(b1),3]
}
freq<-round(freq,3)
b1<-cbind(b1,freq)
rr<-c()
for(i in 1:nrow(b1))
{
rr[i]<-(b1[i,1]/b1[nrow(b1),1])/(b1[i,2]/b1[nrow(b1),2])
}
rr<-round(rr,3)
b1<-cbind(b1,rr)
flag_manu<-TRUE
#print(b)
fg<-rownames(b1)
plot(rr[-length(rr)],type="l",col="red") 


}


}#end while


a<-ap1
}

a<-a[!duplicated(a)]
cont2<-cont2+1
guarda_niv[[cont2]]<-a
guarda_var[[cont2]]<-var_select

z <- cut(dados1[,j],breaks=as.numeric(a),include.lowest = TRUE,right=FALSE,dig.lab = 10)




b<-as.matrix(table(z,alvo,useNA = "ifany")) ###mudar o 2 por j
 #b<-b[-1,]
total<-c()
for(i in 1:nrow(b))
{
total[i]<-sum(b[i,])
}
b<-cbind(b,total)
total<-c(sum(b[,1]),sum(b[,2]),sum(b[,1:2]))
b<-rbind(b,total)

freq<-c()
for(i in 1:nrow(b))
{
freq[i]<-b[i,3]/b[nrow(b),3]
}
b<-cbind(b,freq)


tx_maus<-c()
for(i in 1:nrow(b))
{
tx_maus[i]<-b[i,2]/b[i,3]
}
b<-cbind(b,tx_maus)

rr<-c()
for(i in 1:nrow(b))
{
rr[i]<-(b[i,1]/b[nrow(b),1])/(b[i,2]/b[nrow(b),2])
}
b<-cbind(b,rr)

b1<-matrix(nrow=nrow(b),ncol=ncol(b))
for(k in 1:nrow(b))
 {
  b1[k,]<-b[k,]
 }

entropia_raiz<--(b1[nrow(b1),1]/b1[nrow(b1),3])*(log2(b1[nrow(b1),1]/b1[nrow(b1),3]))-(b1[nrow(b1),2]/b1[nrow(b1),3])*log2((b1[nrow(b1),2]/b1[nrow(b1),3]))
entropia_niv<-c()
for(i in 1:(nrow(b1)-1))
{
 if(b1[i,1]==0)
  {
 entropia_niv[i]<--(b1[i,2]/b1[i,3])*log2((b1[i,2]/b1[i,3]))
 }
 if(b1[i,2]==0)
  {
  entropia_niv[i]<--(b1[i,1]/b1[i,3])*(log2(b1[i,1]/b1[i,3]))
 }
 if(b1[i,1]>0 & b1[i,2]>0)
  {
  entropia_niv[i]<--(b1[i,1]/b1[i,3])*(log2(b1[i,1]/b1[i,3]))-(b1[i,2]/b1[i,3])*log2((b1[i,2]/b1[i,3]))
  }
}
media_entrop<-c()
for(i in 1:(nrow(b1)-1))
{
media_entrop[i]<-(b1[i,3]/b1[nrow(b1),3])*entropia_niv[i]
}
media_entrop<-sum(media_entrop)
ganho_inf<-entropia_raiz-media_entrop
entropia_niv<-c(entropia_niv,entropia_raiz)
b1<-cbind(b1,entropia_niv)
b1<-cbind(b1,ganho_inf)

b2<-table(dados1[,j],alvo)
freq_bom<-b2[,1]/sum(b2[,1])
freq_mau<-b2[,2]/sum(b2[,2])
freq_ac_bom<-cumsum( freq_bom )
freq_ac_mau<-cumsum( freq_mau )
diff<-round(abs(freq_ac_bom-freq_ac_mau),3)

ks1<-max(diff)


b1<-data.frame(b1,ks1)


ef<-is.na(z)
if(sum(ef==TRUE)>0)
{
rown<-c(levels(z),"Missing","Total")
}else{
rown<-c(levels(z),"Total")
}
coln<-c(as.character(unique(alvo)[1]),as.character(unique(alvo)[2]),"Total","FREQ","TX_MAUS","RR","ENTROPIA","GANHO_INFORMACAO","KS")
colnames(b1)<-coln
rownames(b1)<-rown

b29<-rownames(b1)
ind24<-c(which(b29=="Missing"),which(b29=="Total"))
niv30<-b29[-ind24]


f30<-str_locate_all(niv30[1], ",")
n30<-str_length(niv30[1])
g30<-str_sub(niv30[1], f30[[1]][1,1]+1,n30)
h30<-paste("[<",g30,sep="")

f31<-str_locate_all(niv30[length(niv30)], ",")
g31<-str_sub(niv30[length(niv30)],2,f31[[1]][1,1]-1)
h31<-paste("[>=",g31,"]",sep="")

niv30[1]<-h30
niv30[length(niv30)]<-h31

niv31<-c(niv30,b29[ind24])
#rownames(b1)<-niv31
b1<-data.frame(niv31,b1)
coln<-c(var_select,as.character(unique(alvo)[1]),as.character(unique(alvo)[2]),"Total","FREQ","TX_ALVO","RISCO_RELATIVO","ENTROPIA","GANHO_INFORMACAO","KS")
colnames(b1)<-coln
rownames(b1)<-c(1:nrow(b1))

b1$FREQ<-paste(100*round(b1$FREQ,4),"%",sep="")
b1$TX_ALVO<-paste(100*round(b1$TX_ALVO,4),"%",sep="")
b1$RISCO_RELATIVO<-round(b1$RISCO_RELATIVO,4)
b1$ENTROPIA<-round(b1$ENTROPIA,4)
b1$GANHO_INFORMACAO<-round(b1$GANHO_INFORMACAO,4)
b1$KS<-round(b1$KS,4)

#b1<-b1[,-which(names(b1) %in% c("ENTROPIA","GANHO_INFORMACAO"))]

#write.xlsx(b1,paste("C:/Resultados_bivariadas_",projeto,"/",var_select,".xlsx",sep=""),col.names=TRUE, row.names=TRUE)

}else{###############################

#j<-2
b<-as.matrix(table(dados1[,j],alvo,useNA = "ifany")) ###mudar o 2 por j
 #b<-b[-1,]
total<-c()
for(i in 1:nrow(b))
{
total[i]<-sum(b[i,])
}
b<-cbind(b,total)
total<-c(sum(b[,1]),sum(b[,2]),sum(b[,1:2]))
b<-rbind(b,total)

freq<-c()
for(i in 1:nrow(b))
{
freq[i]<-b[i,3]/b[nrow(b),3]
}
b<-cbind(b,freq)


tx_maus<-c()
for(i in 1:nrow(b))
{
tx_maus[i]<-b[i,2]/b[i,3]
}
b<-cbind(b,tx_maus)

rr<-c()
for(i in 1:nrow(b))
{
rr[i]<-(b[i,1]/b[nrow(b),1])/(b[i,2]/b[nrow(b),2])
}
b<-cbind(b,rr)

b1<-matrix(nrow=nrow(b),ncol=ncol(b))
for(k in 1:nrow(b))
 {
  b1[k,]<-b[k,]
 }

entropia_raiz<--(b1[nrow(b1),1]/b1[nrow(b1),3])*(log2(b1[nrow(b1),1]/b1[nrow(b1),3]))-(b1[nrow(b1),2]/b1[nrow(b1),3])*log2((b1[nrow(b1),2]/b1[nrow(b1),3]))
entropia_niv<-c()
for(i in 1:(nrow(b1)-1))
{
 if(b1[i,1]==0)
  {
 entropia_niv[i]<--(b1[i,2]/b1[i,3])*log2((b1[i,2]/b1[i,3]))
 }
 if(b1[i,2]==0)
  {
  entropia_niv[i]<--(b1[i,1]/b1[i,3])*(log2(b1[i,1]/b1[i,3]))
 }
 if(b1[i,1]>0 & b1[i,2]>0)
  {
  entropia_niv[i]<--(b1[i,1]/b1[i,3])*(log2(b1[i,1]/b1[i,3]))-(b1[i,2]/b1[i,3])*log2((b1[i,2]/b1[i,3]))
  }
}
media_entrop<-c()
for(i in 1:(nrow(b1)-1))
{
media_entrop[i]<-(b1[i,3]/b1[nrow(b1),3])*entropia_niv[i]
}
media_entrop<-sum(media_entrop)
ganho_inf<-entropia_raiz-media_entrop
entropia_niv<-c(entropia_niv,entropia_raiz)
b1<-cbind(b1,entropia_niv)
b1<-cbind(b1,ganho_inf)
KS2<-0
b1<-data.frame(b1,KS2)
ef2<-rownames(table(dados1[,j],alvo,useNA = "ifany"))
for(i in 1:length(ef2))
{
 if(is.na(ef2[i])=="TRUE")
 {
  ef2[i]<-"MISSING"
}
}

rown<-c(ef2,"Total")
b1<-data.frame(rown,b1)
coln<-c(var_select,as.character(unique(alvo)[1]),as.character(unique(alvo)[2]),"Total","FREQ","TX_ALVO","RISCO_RELATIVO","ENTROPIA","GANHO_INFORMACAO","KS")
colnames(b1)<-coln
rownames(b1)<-c(1:nrow(b1))

b1$FREQ<-paste(100*round(b1$FREQ,4),"%",sep="")
b1$TX_ALVO<-paste(100*round(b1$TX_ALVO,4),"%",sep="")
b1$RISCO_RELATIVO<-round(b1$RISCO_RELATIVO,4)
b1$ENTROPIA<-round(b1$ENTROPIA,4)
b1$GANHO_INFORMACAO<-round(b1$GANHO_INFORMACAO,4)
b1$KS<-round(b1$KS,4)

#b1<-b1[,-which(names(b1) %in% c("ENTROPIA","GANHO_INFORMACAO"))]

#write.xlsx(b1,paste("C:/Resultados_bivariadas_",projeto,"/",var_select,".xlsx",sep=""),col.names=TRUE, row.names=TRUE)

}#end if

return(b1)
}
