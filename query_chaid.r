
#dados<-read.csv("D:/Users/CLP124604/Desktop/AUTO_MODEL/TRAIN.csv",sep=";",h=T)

#ind<-sample(1:nrow(dados),0.75*nrow(dados),replace=F)

#dados_train<-dados[ind,]
#dados_test<-dados[-ind,]


#alvo<-"y"
#target_class<-"yes"
#min_freq_leaf<-30
#max_split_tree<-4
#lift1<-1.3




rules_generate<-function(dados_train=NA,alvo,target_class,min_freq_leaf,max_split_tree,lift1,type="SQL SAP HANA",projeto)
{
#install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(CHAID)
library(woe)
library(stringr)
dir.create(paste("C:/Resultados_regras_",projeto,sep=""))
alvo_real<-dados_train[,alvo]


dados_train[,alvo]<-ifelse(dados_train[,alvo]==target_class,1,0)
dados1<-dados_train
sink(file=paste("C:/Resultados_regras_",projeto,"/Pre_proc_data_rules.sql",sep=""), type="output")
for(i in which(!(names(dados_train) %in% alvo)))
{
if(i==1)
{

cat("Select \n")
}
if(is.numeric(dados_train[,i])==TRUE)
{
 cat("case \n")
 if(sum(is.na(dados_train[,i])==TRUE)>0)
 {
 
  ep<- woe(Data=dados_train[-which(is.na(dados_train[,i])==TRUE),],names(dados_train)[i],TRUE,alvo,10,Bad=0,Good=1)
  ep<-ep[!duplicated(paste(ep$MAX,ep$MIN,sep="")),]
   for(j in 1:nrow(ep))
 {
  if(j==1)
  {
   dados1[,i]<-ifelse(dados_train[,i]<=ep$MAX[j],paste("<=",ep$MAX[j],sep=""),"")
  cat("when ","\"",names(dados_train)[i],"\""," <= ",ep$MAX[j]," then ",paste("'<=",ep$MAX[j],"'",sep=""),"\n",sep="")
  }
  if(j>1 & j<nrow(ep))
  {
  dados1[,i]<-ifelse(dados_train[,i]>ep$MIN[j] & dados_train[,i]<=ep$MAX[j],paste("(",ep$MIN[j],",",ep$MAX[j],"]",sep=""), dados1[,i])
  cat("when ","\"",names(dados_train)[i],"\""," > ",ep$MIN[j]," and ","\"",names(dados_train)[i],"\""," <= ",ep$MAX[j]," then ",paste("'(",ep$MIN[j],",",ep$MAX[j],"]'",sep=""),"\n",sep="")
  }
   if(j==nrow(ep))
  {
  dados1[,i]<-ifelse(dados_train[,i]>ep$MIN[j],paste(">",ep$MIN[j],sep=""), dados1[,i])
 cat("when ","\"",names(dados_train)[i],"\""," > ",ep$MIN[j]," then ",paste("'>",ep$MIN[j],"'",sep=""),"\n",sep="")
  }
  }
  dados1[which(is.na(dados_train[,i])==TRUE),i]<-"MISSING"
  dados1[,i]<-factor(dados1[,i])
    if(i==max(which(!(names(dados_train) %in% alvo))))
{
  cat("when ","\"",names(dados_train)[i],"\""," is null then 'MISSING' end as ","\"",names(dados_train)[i],"\"","\n",sep="")
  }else{
  cat("when ","\"",names(dados_train)[i],"\""," is null then 'MISSING' end as ","\"",names(dados_train)[i],"\"",",\n",sep="")
  }

 
  
 }else{
 ep<- woe(Data=dados_train,names(dados_train)[i],TRUE,alvo,10,Bad=0,Good=1)
 ep<-ep[!duplicated(paste(ep$MAX,ep$MIN,sep="")),]
  for(j in 1:nrow(ep))
 {
  if(j==1)
  {
   dados1[,i]<-ifelse(dados_train[,i]<=ep$MAX[j],paste("<=",ep$MAX[j],sep=""),"")
   cat("when ","\"",names(dados_train)[i],"\""," <= ",ep$MAX[j]," then ",paste("'<=",ep$MAX[j],"'",sep=""),"\n",sep="")
  }
  if(j>1 & j<nrow(ep))
  {
  dados1[,i]<-ifelse(dados_train[,i]>ep$MIN[j] & dados_train[,i]<=ep$MAX[j],paste("(",ep$MIN[j],",",ep$MAX[j],"]",sep=""), dados1[,i])
  cat("when ","\"",names(dados_train)[i],"\""," > ",ep$MIN[j]," and ","\"",names(dados_train)[i],"\""," <= ",ep$MAX[j]," then ",paste("'(",ep$MIN[j],",",ep$MAX[j],"]'",sep=""),"\n",sep="")
  }
   if(j==nrow(ep))
  {
  dados1[,i]<-ifelse(dados_train[,i]>ep$MIN[j],paste(">",ep$MIN[j],sep=""), dados1[,i])
  if(i==max(which(!(names(dados_train) %in% alvo))))
{
 cat("when ","\"",names(dados_train)[i],"\""," > ",ep$MIN[j]," then ",paste("'>",ep$MIN[j],"'",sep="")," end as ","\"",names(dados_train)[i],"\"", "\n",sep="")
 }else{
  cat("when ","\"",names(dados_train)[i],"\""," > ",ep$MIN[j]," then ",paste("'>",ep$MIN[j],"'",sep="")," end as ","\"",names(dados_train)[i],"\"", ",\n",sep="")
 }
  }
  }
 dados1[,i]<-factor(dados1[,i])

 
 }
 
 }else{
 
 cat("\"",names(dados_train)[i],"\"",",\n",sep="")
 }
if(i==max(which(!(names(dados_train) %in% alvo))))
{
cat("FROM TABLE")

}

}
sink()

dados1[,alvo]<-factor(alvo_real)





ff<-formula(paste(alvo,"~.",sep=""))
print("Read chaid")
dt.chaid  <- chaid(ff , 
                   control = chaid_control(minprob = min_freq_leaf/nrow(dados1),
                                           minsplit = 200,minbucket = 40,maxheight=max_split_tree),
                   data=dados1)
				   
				   
print("Finish chaid")			
			   
regras<-partykit:::.list.rules.party(dt.chaid) 				   



freq<-c()
tx_alvo<-c()
lift<-c()
id_regra<-c()
#leaf<-c()
#confidence_less<-c()
#confidence_greater<-c()
attach(dados1)
for(i in 1:length(regras))
{

indice<-eval(parse(text=regras[i]))


tx_alvo[i]<-mean(ifelse(dados1[indice==TRUE,alvo]==target_class,1,0))
freq[i]<-sum(indice==TRUE)/nrow(dados1)
lift[i]<-mean(ifelse(dados1[indice==TRUE,alvo]==target_class,1,0))/mean(ifelse(dados1[,alvo]==target_class,1,0))
#leaf[i]<-mean(ifelse(dados1[indice==TRUE,alvo]==target_class,1,0))*log(1/(1-mean(ifelse(dados1[indice==TRUE,alvo]==target_class,1,0))))
id_regra[i]<-names(regras)[i]
}					   
d3<-data.frame(regras,tx_alvo,freq,lift,id_regra)	
if(length(which(d3$freq<min_freq_leaf/nrow(dados1)))>0)
{	
regras<-regras[-which(d3$freq<min_freq_leaf/nrow(dados1))]  		   
d3<-d3[-which(d3$freq<min_freq_leaf/nrow(dados1)),] 

}
colnames(d3)<-c("REGRAS","ACERTO","FREQUÊNCIA","LIFT","ID REGRA")

if(length(which(d3$LIFT>=lift1))>0)
{	
regras<-regras[which(d3$LIFT>=lift1)] 		   
d3<-d3[which(d3$LIFT>=lift1),]   

}
if(nrow(d3)>0)
{

sink(file=paste("C:/Resultados_regras_",projeto,"/Query_rules_production.sql",sep=""), type="output")
for(k in 1:length(regras))
{
regras1<-regras[k]
regras1<-str_replace_all(regras1,"%in%","in")
regras1<-str_replace_all(regras1,"c\\(","\\(")
regras1<-str_replace_all(regras1,"\\&","and")
regras1<-str_replace_all(regras1,"\"","'")
if(k==1)
{
cat("Select *, case \n")
}
if(k>=1 & k<length(regras))
{
cat("when ",regras1," then ",names(regras)[k],"\n",sep="")
}
if(k==length(regras))
{
cat("when ",regras1," then ",names(regras)[k]," end as REGRAS FROM TABLE\n",sep="")
cat("Where ","\n")

for(k1 in 1:length(regras))
{
regras1<-regras[k1]
regras1<-str_replace_all(regras1,"%in%","in")
regras1<-str_replace_all(regras1,"c\\(","\\(")
regras1<-str_replace_all(regras1,"\\&","and")
regras1<-str_replace_all(regras1,"\"","'")
if(k1==1)
{
cat("(",regras1,")\n",sep="")
}else{
cat("OR\n")
cat("(",regras1,")\n",sep="")
}

}

}
}
sink()

write.table(d3,paste("C:/Resultados_regras_",projeto,"/Output_rules.csv",sep=""),sep=";",col.names=T,row.names=F,dec=",")
}
}
