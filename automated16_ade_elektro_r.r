generate_doc<-function(parp,projeto,dados_75,dados_25,ade_prod,ks2,ks1,ks3,maximo_y,df100,PROD)
{

library(openxlsx)
library(ReporteRs)
library(stringr)
# Create a word document to contain R outputs
doc <- docx()

doc<-addDocument(doc,"C:/Users/valter.e.junior/Desktop/AUTO_MODEL/Capa.docx")
# Add a title to the document
doc <- addPageBreak(doc)
doc <- addTitle(doc, "Sumário",level=1)
doc <- addTOC(doc)

doc <- addPageBreak(doc)
doc <- addTitle(doc, paste("Modelo Automatizado ",projeto,sep=""), level=1)
# Add a paragraph of text into the Word document 

doc <- addParagraph(doc,"\n Este documento evidência todos os resultados gerado pelo melhor modelo de classificação selecionado pela ferramenta AutoML, nesta ferramenta se encontram os seguintes modelos:")
doc <- addParagraph(doc, value= c("Rede Neural (MLP)", "GBM (Gradient Boosting Machine)", "XGBoost (Extreme Gradient Boosting)"),
          par.properties =  parProperties(list.style = 'unordered'))
doc <- addParagraph(doc,paste("\n O melhor modelo selecionado para esse projeto foi ",ifelse(ks2==parp,"MLP",ifelse(ks1==parp,"XGBoost",ifelse(ks3==parp,"GBM",""))),", nos próximos tópicos será demonstrado os resultados deste modelo na base de teste de referência.",sep=""))


doc <- addTitle(doc, "Análise Bivariada", level = 1) 

f<-read.xlsx(paste("C:/Resultados_bivariadas_",projeto,"/bivariada_full.xlsx",sep=""),sheet="Sheet1")
f<-f[,-1]
f<-f[,-ncol(f)]


f1<-f[which(as.numeric(f$KS2)==max(as.numeric(f$KS2),na.rm=T)),]
colnames(f1)<-as.character(f[which(as.numeric(f$KS2)==max(as.numeric(f$KS2),na.rm=T))[1]-1,])
f1$freq<-round(as.numeric(f1$freq),4)
f1$tx_maus<-round(as.numeric(f1$tx_maus),4)
f1$RR<-round(as.numeric(f1$RR),4)
f1$entropia<-round(as.numeric(f1$entropia),4)
f1$ganho_inf<-round(as.numeric(f1$ganho_inf),4)
f1$KS2<-round(as.numeric(f1$KS2),4)
f1<-f1[,-which(names(f1) %in% c("entropia","ganho_inf"))]

doc <- addParagraph(doc,paste("\n Este tópico demonstra a relevância e comportamento das variáveis explicativas em relação ao alvo objetivo do projeto, a Tabela 1 mostra a tabela bivariada da variável ","\"",names(f1)[1],"\""," com o maior valor no teste estatístico não-paramétrico KS (Kolmogorov-Smirnov) que será chamado KS1 para medir a aderência e KS2 para desempenho, este teste observa a máxima diferença absoluta entre a função de distribuição acumulada entre duas distribuições, ou seja, o valor da seu teste varia entre 0 e 1, quanto mais proximo de 1 maior é a separação entre as distribuições.\n",sep=""))




##doc <- addPageBreak(doc)  #break page


MyFTable <- vanilla.table(f1)
MyFTable <- setZebraStyle(MyFTable, odd = '#eeeeee', even = 'white')
doc <- addFlexTable( doc, MyFTable)



doc <- addParagraph(doc,paste("\n O RR é um indicador do quanto cada determinação pode contribuir isoladamente para classificação do risco dos clientes. Quanto mais afastado do 1, mais discriminante é a característica. Segue a referência do risco relativo:",sep=""))

doc <- addParagraph(doc, value= c("<= 0.5  Muito Ruim", "0.5 a 0.90  Ruim", "0.90 a 1.10  Neutro","1.10 a 2.00  Bom","> 2.00  Muito Bom"),
          par.properties =  parProperties(list.style = 'unordered'))


doc <- addTitle(doc, "Referência dos dados", level = 1) 

doc <- addParagraph(doc,paste(" As referências apresentadas será de extrema importância, é nela que o modelo processa a otimização buscando um desempenho maior que apresenta o acerto nos dados do projeto, segue abaixo os valores de referência:",sep=""))


dep<-data.frame(ACERTO_TRN=paste(100*round(mean(dados_75$alvo),4),"%",sep=""),VOLUME_TRN=nrow(dados_75),ACERTO_TST=paste(100*round(mean(dados_25$alvo),4),"%",sep=""),VOLUME_TST=nrow(dados_25))

#doc <- addPageBreak(doc)
colnames(dep)<-str_replace(names(dep),"_"," ")
MyFTable <- vanilla.table(dep)
MyFTable <- setZebraStyle(MyFTable, odd = '#eeeeee', even = 'white')
doc <- addFlexTable( doc, MyFTable)

doc <- addTitle(doc, paste("Resultado do Modelo ",ifelse(ks2==parp,"MLP",ifelse(ks1==parp,"XGBoost",ifelse(ks3==parp,"GBM",""))),sep=""), level = 1) 

doc <- addParagraph(doc, "Essa seção irá mostrar os resultados do modelo em 2 subtópicos: \n") 

doc <- addParagraph(doc, value= c("Importância das Variáveis", "Análise de Desempenho"),
          par.properties =  parProperties(list.style = 'unordered'))

doc <- addTitle(doc, "Importância das Variáveis", level = 2) 

doc <- addParagraph(doc, " A Tabela abaixo demonstra o Rank ou a importância das variáveis entrantes do modelo, quanto maior o Rank maior é a importância da variável no modelo.\n")

imp_f<-read.csv(paste("C:/Resultados_bivariadas_",projeto,"/Import_variaveis.csv",sep=""),sep=";",h=T,dec=",")  
colnames(imp_f)<-c("Variáveis","Rank")
imp_f$Rank<-round(imp_f$Rank,4)
MyFTable <- vanilla.table(imp_f)
MyFTable <- setZebraStyle(MyFTable, odd = '#eeeeee', even = 'white')
doc <- addFlexTable( doc, MyFTable)

doc <- addParagraph(doc, " Variáveis com Rank igual a 0 significa que esta variável não entrou na aprendizagem pelo fato desta variável apresentar pouco poder de predição.\n")


doc <- addTitle(doc, "Análise de Desempenho", level = 2) 

doc <- addParagraph(doc, paste(" O desempenho do modelo será médido na base de teste pela técnica estatística não paramétrica KS (Kolmogorov-Smirnov), este teste compara se duas amostras vêm de uma mesma população. O intuito é inferir se as duas amostras de clientes (bons e maus) provêm de populações distintas.Desta forma obtém-se evidências que o modelo está cumprindo seu objetivo de separar os dois grupos. Assim como o coeficiente de Gini, o KS varia entre 0 e 1 e valores mais altos indicam uma melhor performance. A Figura abaixo ilustra a separação das classes pelo score gerado pelo modelo. O valor do KS foi de ",round(maximo_y,3),".",sep=""))


doc <- addImage(doc,paste("C:/Resultados_bivariadas_",projeto,"/ks_plot.jpg",sep=""), width = 5, height = 4)

doc <- addParagraph(doc,"A analise de assertividade for faixa de escore na base de teste é extremamente importante, nela extraimos a inferência do quanto esperamos acerta o alvo de interesse em produção a uma certa quantidade de registros em percentual, lembrando que quanto maior o score maior é a chance de acerta a classe alvo. O Grafico abaixo ilustra o comportamento de acerto versus a massa de dados por faixa de score.")
 
doc <- addImage(doc,paste("C:/Resultados_bivariadas_",projeto,"/ANALISE_TESTE_plot.jpg",sep=""), width = 5, height = 4) 

doc <- addParagraph(doc,paste("A ultima faixa de score temos uma percentual esperado de registros em produção de ",paste(round(as.numeric(df100[nrow(df100),2]),4),"%",sep="")," com uma assertividade esperada de ",paste(100*round(as.numeric(df100[nrow(df100),3]),4),"%",sep=""),", ou seja, o nivel de assertividade esta ",round(as.numeric(df100[nrow(df100),3])/mean(dados_75$alvo),2), ifelse(as.numeric(df100[nrow(df100),3])>=mean(dados_75$alvo)," acima"," abaixo")," da média de referência da base de teste que é ",paste(100*round(mean(dados_25$alvo),4),"%",sep=""),".",sep="")) 

if(is.null(nrow(PROD))!=TRUE)
{
doc <- addParagraph(doc,paste("Toda a analise de inferência só é valida se o modelo final apresentar uma boa aderência em produção, se esse ponto não for validado toda inferência não será consistente! O Gráfico abaixo mostra a aderência entre o score do teste e de produção, a técnica utiliza para medir a aderência é o KS, porém comparando as duas distribuições do score de teste e produção é considerado uma boa aderência quando o KS esta abaixo de 10, o valor da estatística nesse teste foi de ",100*round(ade_prod,4)," que é uma ",ifelse(100*round(ade_prod,4)<10,"boa","baixa")," aderência!"))
doc <- addImage(doc,paste("C:/Resultados_bivariadas_",projeto,"/analise_aderencia.jpg",sep=""), width = 6, height = 4) 

}
doc <- addPageBreak(doc)
doc <- addTitle(doc, "Sugestões", level = 1) 

doc <- addParagraph(doc,"Caso queira opinar com sugestões para melhorias relevantes, segue meu email e telefone abaixo, agradeço desde já!") 
doc <- addParagraph(doc, value= c("E-mail: valteresj2@gmail.com", "Telefone: +55 81 997741891"),
          par.properties =  parProperties(list.style = 'unordered'))

writeDoc( doc, file = paste("C:/Resultados_bivariadas_",projeto,"/RESULTADOS MODELO.docx",sep="") )
detach(package:openxlsx, unload = TRUE)
}

SQL_PRE_PROC_sql<-function(data,var1,file,input.table,id)
{


library(stringr)

sink(file, type="output")


 cat(paste("SELECT ",id,",","\n\n",sep=""))

for(i in 1:length(var1))
{

if(is.numeric(data[,names(var1)[i]])==TRUE & sum(str_detect(var1[[i]],"Range")==TRUE)==0)
    {
    if(length(var1[[i]])>0)
	  {
	  cat("CASE ","\n")
      for(k in 1:length(var1[[i]]))
	   {
	      f<-var1[[i]][k]
	p=str_sub(f,1,1)
    p1=as.numeric(str_sub(f,2,str_locate(f,"\\,")[1,1]-1))
    p2=as.numeric(str_sub(f,str_locate(f,"\\,")[1,1]+1,str_length(f)-1))
    p3=str_sub(f,str_length(f),str_length(f))
	
	 if(k<length(var1[[i]]))
	 {
	 	if(p=="(" & p3=="]")
{
cat(" when ",names(var1)[i]," > ",p1," and ",names(var1)[i]," <=",p2," then ",names(var1)[i],"\n")

}

if(p=="[" & p3=="]")
{
#cat(" when ",names(var1)[i]," >= ",p1," and ",names(var1)[i]," <=",p2," then 1 else 0 end as ",paste("\"",names(var1)[i],var1[[i]][k],"\"",",",sep=""),"\n")

cat(" when ",names(var1)[i]," >= ",p1," and ",names(var1)[i]," <=",p2," then ",names(var1)[i],"\n")

}
if(p=="(" & p3==")")
{
#cat(" when ",names(var1)[i]," > ",p1," and ",names(var1)[i]," <",p2," then 1 else 0 end as ",paste("\"",names(var1)[i],var1[[i]][k],"\"",",",sep=""),"\n")

cat(" when ",names(var1)[i]," > ",p1," and ",names(var1)[i]," <",p2," then ",names(var1)[i],"\n")

}
if(p=="[" & p3==")")
{
#cat(" when ",names(var1)[i]," >= ",p1," and ",names(var1)[i]," <",p2," then 1 else 0 end as ",paste("\"",names(var1)[i],var1[[i]][k],"\"",",",sep=""),"\n")

cat(" when ",names(var1)[i]," >= ",p1," and ",names(var1)[i]," <",p2," then ",names(var1)[i],"\n")

}
if(f=="Missing")
{
#cat(" when ",names(var1)[i]," is null then 1 else 0 end as ",paste("\"",names(var1)[i],f,"\"",",",sep=""),"\n")

cat(" when ",names(var1)[i]," is null then ",names(var1)[i],"\n")
}
	
}else{

 	if(p=="(" & p3=="]")
{
cat(" when ",names(var1)[i]," > ",p1," and ",names(var1)[i]," <=",p2," then ",names(var1)[i]," else ",min(data[,names(var1)[i]],na.rm=T)-100," end as ",names(var1)[i],",\n")

}

if(p=="[" & p3=="]")
{
#cat(" when ",names(var1)[i]," >= ",p1," and ",names(var1)[i]," <=",p2," then 1 else 0 end as ",paste("\"",names(var1)[i],var1[[i]][k],"\"",",",sep=""),"\n")

cat(" when ",names(var1)[i]," >= ",p1," and ",names(var1)[i]," <=",p2," then ",names(var1)[i]," else ",min(data[,names(var1)[i]],na.rm=T)-100," end as ",names(var1)[i],",\n")

}
if(p=="(" & p3==")")
{
#cat(" when ",names(var1)[i]," > ",p1," and ",names(var1)[i]," <",p2," then 1 else 0 end as ",paste("\"",names(var1)[i],var1[[i]][k],"\"",",",sep=""),"\n")

cat(" when ",names(var1)[i]," > ",p1," and ",names(var1)[i]," <",p2," then ",names(var1)[i]," else ",min(data[,names(var1)[i]],na.rm=T)-100," end as ",names(var1)[i],",\n")

}
if(p=="[" & p3==")")
{
#cat(" when ",names(var1)[i]," >= ",p1," and ",names(var1)[i]," <",p2," then 1 else 0 end as ",paste("\"",names(var1)[i],var1[[i]][k],"\"",",",sep=""),"\n")

cat(" when ",names(var1)[i]," >= ",p1," and ",names(var1)[i]," <",p2," then ",names(var1)[i]," else ",min(data[,names(var1)[i]],na.rm=T)-100," end as ",names(var1)[i],",\n")

}
if(f=="Missing")
{
#cat(" when ",names(var1)[i]," is null then 1 else 0 end as ",paste("\"",names(var1)[i],f,"\"",",",sep=""),"\n")

cat(" when ",names(var1)[i]," is null then ",names(var1)[i]," else ",min(data[,names(var1)[i]],na.rm=T)-100," end as ",names(var1)[i],",\n")
}




}	   
	   
	   
	   
	   }
	
	
	 }
	 
	 
	 
	 
	}
	
	if(is.numeric(data[,names(var1)[i]])!=TRUE & sum(str_detect(var1[[i]],"Range")==TRUE)==0)
    {
     if(length(var1[[i]])>0)
	  {
	  	
		cat("case when ", names(var1)[i]," in ('",paste(var1[[i]],collapse="','"),"') then ",names(var1)[i]," else 'OUT_RANGE' end as ",names(var1)[i],",\n",sep="")
		
}
	  
	  
	  }
	
	
	   if(i==length(var1))
   {
  cat(paste(names(data)[which(!(names(data) %in% names(var1)))],collapse=",\n")) 
   cat(paste("FROM ",input.table))
}
	
	
	 }

	 
  sink()	 

}



SQL_PRE_PROC<-function(data,var1,file,input.table,id)
{


library(stringr)

sink(file, type="output")


 #cat(paste("SELECT ",id,",","\n\n",sep=""))
 
 cat(paste("pre_proc<-function(dados){","\n\n",sep=""))

for(i in 1:length(var1))
{

if(is.numeric(data[,names(var1)[i]])==TRUE & sum(str_detect(var1[[i]],"Range")==TRUE)==0)
    {
    if(length(var1[[i]])>0)
	  {
	#  cat("CASE ","\n")
	   cat("dados$",names(var1)[i],"[which(!(1:nrow(dados) %in% which(","\n",sep="")
      for(k in 1:length(var1[[i]]))
	   {
	      f<-var1[[i]][k]
	p=str_sub(f,1,1)
    p1=as.numeric(str_sub(f,2,str_locate(f,"\\,")[1,1]-1))
    p2=as.numeric(str_sub(f,str_locate(f,"\\,")[1,1]+1,str_length(f)-1))
    p3=str_sub(f,str_length(f),str_length(f))
	
	 if(k<length(var1[[i]]))
	 {
	 	if(p=="(" & p3=="]")
{
#cat(" when ",names(var1)[i]," > ",p1," and ",names(var1)[i]," <=",p2," then ",names(var1)[i],"\n")
cat("(dados$",names(var1)[i]," > ",p1," & ","dados$",names(var1)[i]," <=",p2,") |\n",sep="")

}

if(p=="[" & p3=="]")
{
#cat(" when ",names(var1)[i]," >= ",p1," and ",names(var1)[i]," <=",p2," then 1 else 0 end as ",paste("\"",names(var1)[i],var1[[i]][k],"\"",",",sep=""),"\n")

#cat(" when ",names(var1)[i]," >= ",p1," and ",names(var1)[i]," <=",p2," then ",names(var1)[i],"\n")
cat("(dados$",names(var1)[i]," >= ",p1," & ","dados$",names(var1)[i]," <=",p2,") |\n",sep="")

}
if(p=="(" & p3==")")
{
#cat(" when ",names(var1)[i]," > ",p1," and ",names(var1)[i]," <",p2," then 1 else 0 end as ",paste("\"",names(var1)[i],var1[[i]][k],"\"",",",sep=""),"\n")

#cat(" when ",names(var1)[i]," > ",p1," and ",names(var1)[i]," <",p2," then ",names(var1)[i],"\n")
cat("(dados$",names(var1)[i]," > ",p1," & ","dados$",names(var1)[i]," <",p2,") |\n",sep="")

}
if(p=="[" & p3==")")
{
#cat(" when ",names(var1)[i]," >= ",p1," and ",names(var1)[i]," <",p2," then 1 else 0 end as ",paste("\"",names(var1)[i],var1[[i]][k],"\"",",",sep=""),"\n")

#cat(" when ",names(var1)[i]," >= ",p1," and ",names(var1)[i]," <",p2," then ",names(var1)[i],"\n")

cat("(dados$",names(var1)[i]," >= ",p1," & ","dados$",names(var1)[i]," <",p2,") |\n",sep="")

}
if(f=="Missing")
{
#cat(" when ",names(var1)[i]," is null then 1 else 0 end as ",paste("\"",names(var1)[i],f,"\"",",",sep=""),"\n")

#cat(" when ",names(var1)[i]," is null then ",names(var1)[i],"\n")

cat("(is.na(dados$",names(var1)[i],")==TRUE )|\n",sep="")
}
	
}else{

 	if(p=="(" & p3=="]")
{
#cat(" when ",names(var1)[i]," > ",p1," and ",names(var1)[i]," <=",p2," then ",names(var1)[i]," else ",max(data[,names(var1)[i]],na.rm=T)+100," end as ",names(var1)[i],",\n")
cat("(dados$",names(var1)[i]," > ",p1," & ","dados$",names(var1)[i]," <=",p2,"))) )]<-",max(data[,names(var1)[i]],na.rm=T)+100,"\n",sep="")
}

if(p=="[" & p3=="]")
{
#cat(" when ",names(var1)[i]," >= ",p1," and ",names(var1)[i]," <=",p2," then 1 else 0 end as ",paste("\"",names(var1)[i],var1[[i]][k],"\"",",",sep=""),"\n")

#cat(" when ",names(var1)[i]," >= ",p1," and ",names(var1)[i]," <=",p2," then ",names(var1)[i]," else ",max(data[,names(var1)[i]],na.rm=T)+100," end as ",names(var1)[i],",\n")
cat("(dados$",names(var1)[i]," >= ",p1," & ","dados$",names(var1)[i]," <=",p2,"))) )]<-",max(data[,names(var1)[i]],na.rm=T)+100,"\n",sep="")
}
if(p=="(" & p3==")")
{
#cat(" when ",names(var1)[i]," > ",p1," and ",names(var1)[i]," <",p2," then 1 else 0 end as ",paste("\"",names(var1)[i],var1[[i]][k],"\"",",",sep=""),"\n")

#cat(" when ",names(var1)[i]," > ",p1," and ",names(var1)[i]," <",p2," then ",names(var1)[i]," else ",max(data[,names(var1)[i]],na.rm=T)+100," end as ",names(var1)[i],",\n")

cat("(dados$",names(var1)[i]," > ",p1," & ","dados$",names(var1)[i]," <",p2,"))) )]<-",max(data[,names(var1)[i]],na.rm=T)+100,"\n",sep="")
}
if(p=="[" & p3==")")
{
#cat(" when ",names(var1)[i]," >= ",p1," and ",names(var1)[i]," <",p2," then 1 else 0 end as ",paste("\"",names(var1)[i],var1[[i]][k],"\"",",",sep=""),"\n")

#cat(" when ",names(var1)[i]," >= ",p1," and ",names(var1)[i]," <",p2," then ",names(var1)[i]," else ",max(data[,names(var1)[i]],na.rm=T)+100," end as ",names(var1)[i],",\n")
cat("(dados$",names(var1)[i]," >= ",p1," & ","dados$",names(var1)[i]," <",p2,"))) )]<-",max(data[,names(var1)[i]],na.rm=T)+100,"\n",sep="")
}
if(f=="Missing")
{
#cat(" when ",names(var1)[i]," is null then 1 else 0 end as ",paste("\"",names(var1)[i],f,"\"",",",sep=""),"\n")

#cat(" when ",names(var1)[i]," is null then ",names(var1)[i]," else ",max(data[,names(var1)[i]],na.rm=T)+100," end as ",names(var1)[i],",\n")

cat("(is.na(dados$",names(var1)[i],")==TRUE ))) )]<-",max(data[,names(var1)[i]],na.rm=T)+100,"\n",sep="")
}




}	   
	   
	   
	   
	   }
	
	
	 }
	 
	 
	 
	 
	}
	
	if(is.numeric(data[,names(var1)[i]])!=TRUE & sum(str_detect(var1[[i]],"Range")==TRUE)==0)
    {
     if(length(var1[[i]])>0)
	  {
	  	
		#cat("case when ", names(var1)[i]," in ('",paste(var1[[i]],collapse="','"),"') then ",names(var1)[i]," else 'OUT_RANGE' end as ",names(var1)[i],",\n",sep="")
		#cat("dados$",names(var1)[i]," >= ",p1," & ","dados$",names(var1)[i]," <",p2,") )\n",sep="")
		cat("dados$",names(var1)[i],"<-as.character(","dados$",names(var1)[i],") \n",sep="")
		cat("dados[which(!(dados$",names(var1)[i]," %in% ", "c('",paste(var1[[i]],collapse="','"),"'))),","\"",names(var1)[i],"\"","]<-'OUT_RANGE'","\n",sep="")
		cat("dados$",names(var1)[i],"<-factor(","dados$",names(var1)[i],") \n",sep="")
}
	  
	  
	  }
	
	
	   if(i==length(var1))
   {
  # cat(paste("FROM ",input.table))
  cat(" return(dados) } \n",sep="")
}
	
	
	 }

	 
  sink()	 

}


var_fx<-function(x,f)
{
library(stringr)

p=str_sub(f,1,1)
p1=as.numeric(str_sub(f,2,str_locate(f,"\\,")[1,1]-1))
p2=as.numeric(str_sub(f,str_locate(f,"\\,")[1,1]+1,str_length(f)-1))
p3=str_sub(f,str_length(f),str_length(f))

if(p=="(" & p3=="]")
{
#x2<-ifelse(x>p1 & x<=p2,1,0)
x2<-which(x>p1 & x<=p2)
}

if(p=="[" & p3=="]")
{
#x2<-ifelse(x>=p1 & x<=p2,1,0)
x2<-which(x>=p1 & x<=p2)
}
if(p=="(" & p3==")")
{
#x2<-ifelse(x>p1 & x<p2,1,0)
x2<-which(x>p1 & x<p2)
}
if(p=="[" & p3==")")
{
#x2<-ifelse(x>=p1 & x<p2,1,0)
x2<-which(x>=p1 & x<p2)
}
if(f=="Missing")
{
#x2<-ifelse(is.na(x)==TRUE,1,0)
x2<-which(is.na(x)==TRUE)
}

return(x2)

}


###AUCRF
AUCRF1<-function (formula, data, k0 = 1, pdel = 0.2, ranking = c("MDG", 
    "MDA"), ...) 
{
    AUC.randomForest <- function(rf, clase = 1) {
        r <- rank(rf$votes[, as.character(clase)])
        rd <- mean(r[rf$y == clase])
        nd <- sum(rf$y == clase)
        nnd <- length(rf$y) - nd
        return((rd - nd/2 - 0.5)/nnd)
    }
    MDGRanking <- function(formula, data, ...) {
        fitRF <- randomForest(formula, data = data,ntree=50, ...)
        mdgRanking <- sort(fitRF$importance[, "MeanDecreaseGini"], 
            decreasing = TRUE)
        return(mdgRanking)
    }
    MDARanking <- function(formula, data, ...) {
        fitRF <- randomForest(formula, data = data,ntree=50, importance = TRUE, 
            ...)
        mdaRanking <- sort(fitRF$importance[, "MeanDecreaseAccuracy"], 
            decreasing = TRUE)
        return(mdaRanking)
    }
    t <- 0
    cl <- match.call()
    mf <- match("formula", names(cl), 0L)
    y <- eval(eval(cl[[mf]])[[2]], data)
    if (!is.factor(y) && length(levels(y)) != 2) 
        stop("Outcome must be a factor with two levels")
    if (pdel < 0 || pdel >= 1) 
        stop("pdel must be in the interval [0,1)")
    ranking <- match.arg(ranking)
    switch(ranking, MDG = {
        ranking <- MDGRanking(formula, data, ...)
        ImpMes <- "MDG"
    }, MDA = {
        ranking <- MDARanking(formula, data, ...)
        ImpMes <- "MDA"
    }, stop("Not valid ranking"))
    mf <- match("formula", names(cl), 0L)
    yname <- as.character(eval(cl[[mf]])[[2]])
    vars <- names(ranking)
    AUCcurve <- data.frame()
    auxThres <- 0
    auxMaxAUC <- 0
    k <- length(vars)
    while (k >= k0) {
        fitRF <- randomForest(formula, data = data[, c(yname, 
            vars[1:k])],do.trace=T,ntree=50, ...)
        getAUC <- AUC.randomForest(fitRF)
        if (getAUC >= auxMaxAUC) {
            auxMaxAUC <- getAUC
            auxThres <- auxMaxAUC - t
        }
        if (getAUC >= auxThres) 
            RFopt <- fitRF
        AUCcurve <- rbind(c(k, getAUC), AUCcurve)
        k <- k - as.integer(pdel * k) - 1
    }
    colnames(AUCcurve) <- c("k", "AUC")
    maxAUC <- max(AUCcurve$AUC)
    opthreshold <- maxAUC - t
    optimal <- AUCcurve[AUCcurve$AUC >= opthreshold, ][1, ]
    objectList <- list()
    objectList$call <- cl
    objectList$data <- data
    objectList$ranking <- ranking
    objectList$Xopt <- names(ranking)[1:(optimal$k)]
    objectList$"OOB-AUCopt" <- optimal$AUC
    objectList$Kopt <- optimal$k
    objectList$AUCcurve <- AUCcurve
    objectList$RFopt <- RFopt
    objectList$ImpMeasure <- ImpMes
    class(objectList) <- "AUCRF"
    return(objectList)
}




###### AUC
getROC_AUC = function(probs, true_Y){
    probsSort = sort(probs, decreasing = TRUE, index.return = TRUE)
    val = unlist(probsSort$x)
    idx = unlist(probsSort$ix)  

    roc_y = true_Y[idx];
    stack_x = cumsum(roc_y == 2)/sum(roc_y == 2)
    stack_y = cumsum(roc_y == 1)/sum(roc_y == 1)    

    auc = sum((stack_x[2:length(roc_y)]-stack_x[1:length(roc_y)-1])*stack_y[2:length(roc_y)])
    return(list(stack_x=stack_x, stack_y=stack_y, auc=auc))
}

################ GBM
SQL_DATA_SET_LOG<-function(data,var1,file,input.table,id,newgrupo1,guarda_outros)
{
library(stringr)

sink(file, type="output")
 cat(paste("SELECT ",id,",","\n\n",sep=""))
for(i in 1:length(var1))
{

 
 if(is.numeric(data[,which(names(data)==var1[i])])==TRUE)
  {  
 cat(paste("coalesce(",paste("\"",var1[i],"\"",sep=""),",", min(data[,which(names(data)==var1[i])]),")"," AS ",paste("\"",var1[i],"\"",sep=""),", \n",sep=""))
  }
   
    if(is.numeric(data[,which(names(data)==var1[i])])!=TRUE & length(which(var1[i]==names(guarda_outros)))>0 & length(which(var1[i]==names(newgrupo1)))==0)
   {
   cat("case when ","coalesce(",paste("\"",var1[i],"\"",sep=""),",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),paste("\"",var1[i],"\"",sep=""), "end as ",paste("\"",var1[i],"\"",sep=""),",\n"  )
 
	}

	if(is.numeric(data[,which(names(data)==var1[i])])!=TRUE & length(which(var1[i]==names(guarda_outros)))==0 & length(which(var1[i]==names(newgrupo1)))==0)
	{
	 #vett<-newgrupo1[[which(var1[i]==names(newgrupo1))]]
	if( length(which(unlist(lapply(data[,i],function(x){length(unique(x))}))>1))==0)
   {
	
	#nv<-levels(data[,which(names(data)==var1[i])])
    #for(j in 1:length(nv))
	# {
	 #cat("(case when ",paste("\"",var1[i],"\"",sep="")," = ",nv[j]," then 1 else 0 end) as",gsub("[ [:punct:]]", "",paste(var1[i],nv[j],sep="")))
	 cat("coalesce(",paste("\"",var1[i],"\"",sep=""),",'MISSING') as ",paste("\"",var1[i],"\"",sep=""),",\n") 
	#}
	
	}
	}
	
	 if(is.numeric(data[,which(names(data)==var1[i])])!=TRUE & length(which(var1[i]==names(guarda_outros)))>0  & length(which(var1[i]==names(newgrupo1)))==0)
	 {
	 vett<-guarda_outros[[which(var1[i]==names(guarda_outros))]]
	 
	 cat("case when ",paste("coalesce(",paste("\"",var1[i],"\"",sep=""),",'MISSING')",sep="")," not in ('",paste(vett,collapse="','"),"') then 'OUTROS' else ",paste("coalesce(",paste("\"",var1[i],"\"",sep=""),",'MISSING')",sep="")," end as ",paste("\"",var1[i],"\"",sep=""),",",sep="")
	 }
	
	 if(is.numeric(data[,which(names(data)==var1[i])])!=TRUE & length(which(var1[i]==names(guarda_outros)))==0  & length(which(var1[i]==names(newgrupo1)))>0)
	 {
	  vett<-newgrupo1[[which(var1[i]==names(newgrupo1))]]
     if( length(which(unlist(lapply(vett,function(x){length(unique(x))}))>1))>0)
     {
     #nv<-levels(data[,which(names(data)==var1[i])])
     #for(j in 1:length(nv))
	 #{
	#cat("case when \n\n")
	
       vett<-newgrupo1[[which(var1[i]==names(newgrupo1))]]
	   contc<-which(unlist(lapply(vett,function(x){length(unique(x))}))>1)
	   contc<-contc[length(contc)]
	   cont<-0
	     if(max(unlist(lapply(vett,function(x){length(x)})))>1)
	 {
	# cat("case when \n\n")
	   for(k in 1:length(vett))
     {
	 if(length(vett[[k]])>1)
	  {
	   cont<-cont+1
	   if(cont==1)
	   {
	   
	  cat("(case when ","coalesce(",paste("\"",var1[i],"\"",sep=""),",'MISSING')",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"\n" )
       }
	   if(cont>1 & k<=contc)
	   {
	   cat("when ","coalesce(",paste("\"",var1[i],"\"",sep=""),",'MISSING')",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"\n" )
	   }
	   if(k==contc )
	   {
	   #cat("when (case when ","coalesce(",paste("\"",var1[i],"\"",sep=""),",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),paste("\"",var1[i],"\"",sep=""),"END)",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"ELSE ",paste("\"",var1[i],"\"",sep="")," END as ",paste("\"",var1[i],"\"",sep=""),",\n" )
	   #cat("when ","coalesce(",paste("\"",var1[i],"\"",sep=""),",'MISSING')",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"ELSE ",paste("\"",var1[i],"\"",sep="")," END) as ",paste("\"",var1[i],"\"",sep=""),",\n" )
	    cat("ELSE ","coalesce(",paste("\"",var1[i],"\"",sep=""),",'MISSING') end) as ",paste("\"",var1[i],"\"",sep=""),",\n" )
	  }
	  }
	 
	
	
	}
	}
	
	#else{
	#cat("coalesce(",paste("\"",var1[i],"\"",sep=""),",'MISSING') as ",paste("\"",var1[i],"\"",sep=""),",\n")
	#}
	 #cat(") =",paste("'",nv[j],"'",sep="")," then 1 else 0 end as",gsub("[ [:punct:]]", "",paste(var1[i],nv[j],sep="")),",\n" )
     
     #}
	   
	   
	
	}else{
	cat("coalesce(",paste("\"",var1[i],"\"",sep=""),",'MISSING') as ",paste("\"",var1[i],"\"",sep=""),",\n")
	}
	}
	
	
	 if(is.numeric(data[,which(names(data)==var1[i])])!=TRUE & length(which(var1[i]==names(guarda_outros)))>0 & length(which(var1[i]==names(newgrupo1)))>0)
     {
	  vett<-newgrupo1[[which(var1[i]==names(newgrupo1))]]
	 if(length(which(unlist(lapply(vett,function(x){length(unique(x))}))>1))>0)	 
   {
     #nv<-levels(data[,which(names(data)==var1[i])])
     #for(j in 1:length(nv))
	 #{
	  
	   #cat("case when \n\n")
	
       vett<-newgrupo1[[which(var1[i]==names(newgrupo1))]]
	   contc<-which(unlist(lapply(vett,function(x){length(unique(x))}))>1)
	   contc<-contc[length(contc)]
	   cont<-0
	   
   for(k in 1:length(vett))
    {
	 if(length(vett[[k]])>1)
	  {
	 # cat("case when \n\n")
	   cont<-cont+1
	   if(cont==1)
	   {
	   
	  cat("(case when (case when ","coalesce(",paste("\"",var1[i],"\"",sep=""),",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),paste("\"",var1[i],"\"",sep=""),"END)",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"\n" )
       }
	   if(cont>1 & k<=contc)
	   {
	   cat("when (case when ","coalesce(",paste("\"",var1[i],"\"",sep=""),",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),paste("\"",var1[i],"\"",sep=""),"END)",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"\n" )
	   }
	   if(k==contc)
	   {
	   cat(" ELSE ",paste("\"",var1[i],"\"",sep="")," END) as ",paste("\"",var1[i],"\"",sep=""),",\n" )
	   #cat("when (case when ","coalesce(",paste("\"",var1[i],"\"",sep=""),",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),paste("\"",var1[i],"\"",sep=""),"END)",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"ELSE ",paste("\"",var1[i],"\"",sep="")," END) as ",paste("\"",var1[i],"\"",sep=""),",\n" )
	   #cat("when (case when ","coalesce(",paste("\"",var1[i],"\"",sep=""),",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),paste("\"",var1[i],"\"",sep=""),"END)",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"ELSE ",paste("\"",var1[i],"\"",sep="")," END )","\n" )
	   }
	  }
	  
	 
	
	
	}
	#cat("ELSE ",paste("\"",var1[i],"\"",sep=""), " end) as ",paste("\"",var1[i],"\"",sep=""),",\n"  )
	 #cat(") =",paste("'",nv[j],"'",sep="")," then 1 else 0 end as",gsub("[ [:punct:]]", "",paste(var1[i],nv[j],sep="")),",\n" )
     
     #}
    }else{
	
	cat("case when ","coalesce(",paste("\"",var1[i],"\"",sep=""),",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),paste("\"",var1[i],"\"",sep=""), "end as ",paste("\"",var1[i],"\"",sep=""),",\n"  )
 
	}
	}
   
#print(i)
  } #end for
   

  if(i==length(var1))
   {
   cat(paste("FROM ",input.table))
}
 sink()
}




#SQL_DATA_SET_LOG(data=dados_75_r,var1=var1,file="D:/Users/clp124604/Documents/AUTOMATED_R/model_output_data_set_logistic.SQL",input.table="source_table",id="ZCGINSTALL",newgrupo1=newgrupo1,guarda_outros=guarda_outros)

############################
sql.output.logistic<-function(fit,pred, file, input.table="source_table",id="id")
{
beta<-fit$coefficients
var1<-attr(fit$terms,"dataClasses")
library(stringr)
data<-fit$data
aa<- round(quantile(c(1:10000),seq(0,1,by=150/10000)))
  aa<-aa[-1]
  aa<-aa[-length(aa)]
  aa1<-aa-1

cont<-0


sink(file, type="output")

 cat("Select ",id,", case when (POND_LOGISTIC-",min(pred),")/(",max(pred),"-",min(pred),") >1 then 1 \n")
  cat("when (POND_LOGISTIC-",min(pred),")/(",max(pred),"-",min(pred),") <0 then 0 \n")
   cat("else (POND_LOGISTIC-",min(pred),")/(",max(pred),"-",min(pred),") end as","Pred \n")
   cat("from (\n")

cat("SELECT ",id, ", 1-(1/(1+EXP(case when POND_LOGISTIC>300 then 300 else POND_LOGISTIC end))) as PROD0 from ","\n")

if(sum((names(beta) %in% "(Intercept)")==TRUE)>0)
{
if(length(beta)<=150)
{
cat("(SELECT ",id,", (",beta[1],"\n")
}else{
cat("(SELECT ",id,", ((",beta[1],"\n")
}

}else{
if(length(beta)<=150)
{
cat("(SELECT ",id,", (","\n")
}else{
cat("(SELECT ",id,", ((","\n")
}
}
for(i in 2:length(var1))
{
cont<-cont+1



if(var1[i]=="numeric")
{
if(cont %in% aa1)
{
cat(paste(")+((",names(var1)[i],"*",beta[which(names(var1)[i]==names(beta))],")",sep=""),"\n") 
}else{
cat(paste("+(",names(var1)[i],"*",beta[which(names(var1)[i]==names(beta))],")",sep=""),"\n") 
}
}
if(var1[i]=="factor")
{
nv<-levels(data[,names(var1)[i]])
for(j in 1:length(nv))
{
if(length(which(paste(names(var1)[i],nv[j],sep="")==names(beta)))>0)
{
cont<-cont+1
if(cont %in% aa1)
{
cat(paste(")+((case when ",names(var1)[i]," = ","\'",nv[j],"\'"," then ",beta[which(paste(names(var1)[i],nv[j],sep="")==names(beta))]," else 0 end)",sep=""),"\n")
}else{
cat(paste("+(case when ",names(var1)[i]," = ","\'",nv[j],"\'"," then ",beta[which(paste(names(var1)[i],nv[j],sep="")==names(beta))]," else 0 end)",sep=""),"\n")
}
}
}

}
if(i==length(var1))
{
cat(")) as POND_LOGISTIC","\n")
cat("FROM ",input.table,"))")
}



}



# close the file
  sink()
}



#sql.output.logistic(fit=fit0,pred=pred2,file="D:/Users/clp124604/Documents/AUTOMATED_R/sql_model_logistic.sql", input.table="source_table",id="ZCGINSTALL")



##########################GBM


sql.export.gbm <- function (g,pred, file, input.table="source_table", n.trees=NULL, id="id", trees.per.query=1, variant="generic") {
  require (gbm, quietly=TRUE)
  
  if (class(g) != "gbm") {
    stop ("Expected a GBM object")
    return
  }
  
  if (g$distribution$name == "multinomial") {
    stop (paste("Unsupported distribution ", g$distribution$name))
  } else if (!(g$distribution$name %in% c("gaussian", "bernoulli", "laplace"))) {
    # Untested but should work: "tdist","huberized","poisson","coxph","quantile","pairwise", "gamma","tweedie"
    warning (paste("Untested distribution", g$distribution$name, "- please validate your model works as expected"))
  }
  
  # TODO: leverage gbm.pref() to find the optimal number of trees?
  if (is.null(n.trees)) {
    n.trees <- gbm.perf(g, method="cv")
  }
  
  indent = ""  
  
  sink(file, type="output")
  
  cat("Select ",id,", case when (Pred-",min(pred),")/(",max(pred),"-",min(pred),") >1 then 1 \n")
  cat("when (Pred-",min(pred),")/(",max(pred),"-",min(pred),") <0 then 0 \n")
   cat("else (Pred-",min(pred),")/(",max(pred),"-",min(pred),") end as","Pred \n")
   cat("from (\n")
  aa<- round(quantile(c(1:n.trees),seq(0,1,by=150/n.trees)))
  aa<-aa[-1]
  if(n.trees%% 2==0)
   {
   aa<-aa[-length(aa)]
   }
  aa1<-aa-1

  for(jl in 1:(n.trees-1))
  { 
   if(jl==1)
   {
    if(g$distribution$name=="bernoulli")
    {
   cat(paste("SELECT \"",id,"\",", "(1/(1+EXP(-(", g$initF ,"+(tree",jl,"+\n",sep=""))
    }
    if(g$distribution$name=="laplace")
    {
    cat(paste("SELECT \"",id,"\",", "((((", g$initF ,"+(tree",jl,"+\n",sep=""))
    }
   }



   if(jl>1  & jl<(n.trees-1))
   {
  if((jl %in% aa)==TRUE)
   {
 cat(paste(")+(tree",jl,"+\n",sep=""))
   }
   if((jl %in% aa)!=TRUE & (jl %in% aa1)!=TRUE)
    {
   cat(paste("tree",jl,"+\n",sep=""))
    }
    if((jl %in% aa1)==TRUE)
    {
  cat(paste("tree",jl,"\n",sep=""))
   
     }
   }



   if(jl==(n.trees-1))
   {
   cat(paste("tree",jl,"))))) AS Pred\n",sep=""))
   cat("FROM ")
   }
}
  
  cat("(SELECT \"",id,"\",",sep="")
  
  recurse.gbm.tree <- function (tree, leaf=0, indent="",cont) {
    if (leaf > -1) {
      
      split.var <- tree[leaf+1,"SplitVar"]
      split.var.name <- g$var.names[split.var+1]
      
      if (tree[leaf+1,"SplitVar"] == -1) {
        cat(tree[leaf+1,"Prediction"])
      } else {
       if (leaf == 0) {
          cat("\n", indent, "(CASE ")  
        } else {
          cat("\n", indent, "(CASE ")  
        }
        if (attr(g$Terms, "dataClasses")[[split.var.name]] == "factor") {
          val.index <- tree[leaf+1,"SplitCodePred"]
          categories <- unlist(g$var.levels[split.var+1])
          if (tree[leaf+1,"LeftNode"] != -1) {
            cat("WHEN", paste("\"",split.var.name,"\"",sep=""), "in (", 
                paste("'", categories[g$c.split[[val.index+1]]==-1], "'", sep="", collapse=","), ") THEN ")
            recurse.gbm.tree(tree, tree[leaf+1,"LeftNode"], paste(indent," "))  
            indent = paste(indent," ")
          }
          if (tree[leaf+1,"RightNode"] != -1) {
            cat("\n", indent, "WHEN", paste("\"",split.var.name,"\"",sep=""), "in (", 
                paste("'", categories[g$c.split[[val.index+1]]==1], "'", sep="", collapse=","), ") THEN ")
            recurse.gbm.tree(tree, tree[leaf+1,"RightNode"], paste(indent," "))  
          }          
          if (tree[leaf+1,"MissingNode"] != -1) {
            cat("\n", indent, "ELSE ")
            recurse.gbm.tree(tree, tree[leaf+1,"MissingNode"], paste(indent," "))  
          }
        } else {
          if (tree[leaf+1,"MissingNode"] != -1) {
            cat("WHEN", paste("\"",split.var.name,"\"",sep=""), "IS NULL THEN ")
            recurse.gbm.tree(tree, tree[leaf+1,"MissingNode"], paste(indent," "))  
            indent = paste(indent," ")
          }
          if (tree[leaf+1,"LeftNode"] != -1) {
            cat("\n", indent, "WHEN", paste("\"",split.var.name,"\"",sep=""), "< ", tree[leaf+1,"SplitCodePred"], " THEN ")
            recurse.gbm.tree(tree, tree[leaf+1,"LeftNode"], paste(indent," "))  
          }
          if (tree[leaf+1,"RightNode"] != -1) {
            cat("\n", indent, "WHEN", paste("\"",split.var.name,"\"",sep=""), ">= ", tree[leaf+1,"SplitCodePred"], " THEN ")
            recurse.gbm.tree(tree, tree[leaf+1,"RightNode"], paste(indent," "))  
          }
        }
        cat(" END)")
      }
    }
  }
  print.breaks <- unique(c(seq(1,n.trees,trees.per.query), n.trees))
  lapply(1:(length(print.breaks)-1), function (print.tree.grp) {
    if ((print.tree.grp+1) == length(print.breaks)) {
      last.tree <- print.breaks[print.tree.grp+1]
    } else {
      last.tree <- print.breaks[print.tree.grp+1] - 1
    }
    invisible(lapply(print.breaks[print.tree.grp]:last.tree, function (which.tree) {
      recurse.gbm.tree(pretty.gbm.tree(g, which.tree), 0, indent,cont=print.tree.grp) 
      cat (" + ")
    }))
    if(print.tree.grp+1<n.trees)
    {
    cat("0 as tree", print.tree.grp, ",\n", sep="")
    }else{
    cat("0 as tree", print.tree.grp, "\n", sep="")
    }
    if(print.tree.grp+1==n.trees)
    {
    cat("FROM ", input.table, "))\n\n", sep="")
    }
    
  })
  
  
 
  
  # close the file
  sink()
  
  if (!is.null(attr(g$Terms, "offset"))) {
    warning("offset not implemented")
  }
}

#sql.export.gbm(gbm.model,pred=pred3,file="D:/Users/clp124604/Documents/AUTOMATED_R/sql_model_gbm.sql", input.table="source_table", n.trees=NULL, id="ZCINSTALL", trees.per.query=1, variant="generic")





########################## XGBOOST


SQL_DATA_SET_xgb<-function(data,var1,file,input.table,id,newgrupo1,guarda_outros)
{
library(stringr)

sink(file, type="output")
 cat(paste("SELECT ",id,",","\n\n",sep=""))
for(i in 1:length(var1))
{


 if(is.numeric(data[,which(names(data)==var1[i])])==TRUE)
  {  
 cat(paste("coalesce(",var1[i],",", min(data[,which(names(data)==var1[i])]),")"," AS ",paste("\"",gsub("[ [:punct:]]", "",var1[i]),"\"",sep=""),", \n",sep=""))
  }
   
    if(is.numeric(data[,which(names(data)==var1[i])])!=TRUE & length(which(var1[i]==names(guarda_outros)))>0 & length(which(var1[i]==names(newgrupo1)))==0)
   {
   nv<-levels(data[,which(names(data)==var1[i])])
	for(j in 1:length(nv))
	 {
	cat("case when \n\n")
   cat("(case when ","coalesce(",var1[i],",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),var1[i],"END) =", paste("'",nv[j],"'",sep=""),"\n")
    cat(" then 1 else 0 end as ","\"",gsub("[ [:punct:]]", "",paste(var1[i],nv[j],sep="")),"\"",",\n")
	}
	}
	
	if(is.numeric(data[,which(names(data)==var1[i])])!=TRUE & length(which(var1[i]==names(guarda_outros)))==0 & length(which(var1[i]==names(newgrupo1)))==0)
   {
	
	nv<-levels(data[,which(names(data)==var1[i])])
     for(j in 1:length(nv))
	 {
	  cat("(case when ",var1[i]," = ",paste("'",nv[j],"'",sep="")," then 1 else 0 end) as",paste(" \"",gsub("[ [:punct:]]", "",paste("\"",var1[i],nv[j],"\"",sep="")),"\"",sep=""),",\n")
	}
	
	}
	
	
	 if(is.numeric(data[,which(names(data)==var1[i])])!=TRUE & length(which(var1[i]==names(guarda_outros)))==0 & length(which(var1[i]==names(newgrupo1)))>0)
   {
     nv<-levels(data[,which(names(data)==var1[i])])
	 vett<-newgrupo1[[which(var1[i]==names(newgrupo1))]]
	 if(length(nv)>length(vett))
	 {
	 contpp<-0
	 for(kk in 1:length(nv))
	 {
	  
		 if( length(which(nv[kk] %in% unlist(vett)))==0 & str_detect(nv[kk],"GRUPO")!=TRUE)
		  {
		  contpp<-contpp+1
		vett[[length(vett)+contpp]]<-nv[kk]
		  }
		}
	 
	 }
	 
	 
     for(j in 1:length(nv))
	 {
	cat("case when \n\n")
	
       
	   contc<-which(unlist(lapply(vett,function(x){length(unique(x))}))>1)
	   contc<-contc[length(contc)]
	   cont<-0
	   if(max(unlist(lapply(vett,function(x){length(x)})))>1)
	   {
	   for(k in 1:length(vett))
     {
	 if(length(vett[[k]])>1)
	  {
	   cont<-cont+1
	   if(cont==1)
	   {
	   
	  cat("(case when ","coalesce(",var1[i],",'MISSING')",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"\n" )
       }
	   if(cont>1 & k<=contc)
	   {
	   cat("when ","coalesce(",var1[i],",'MISSING')",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"\n" )
	   }
	   if(k==contc)
	   {
	   #cat("when (case when ","coalesce(",var1[i],",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),var1[i],"END)",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"ELSE ",var1[i]," END as ",var1[i],",\n" )
	  #cat("when ","coalesce(",var1[i],",'MISSING')",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"ELSE ",var1[i]," END ","\n" )
       cat(" ELSE ",var1[i],"END","\n" )	  
	  }
	  }
	 
	
	
	}
	
	}else{
	cat("(",var1[i] )
	
	}
	 cat(") =",paste("'",nv[j],"'",sep="")," then 1 else 0 end as ",paste("\"",gsub("[ [:punct:]]", "",paste(var1[i],nv[j],sep="")),"\"",sep=""),",\n" )
     
     }
	   
	   
	
	}
	
	 if(is.numeric(data[,which(names(data)==var1[i])])!=TRUE & length(which(var1[i]==names(guarda_outros)))>0 & length(which(var1[i]==names(newgrupo1)))>0)
   {
     nv<-levels(data[,which(names(data)==var1[i])])
	  vett<-newgrupo1[[which(var1[i]==names(newgrupo1))]]
	 if(length(nv)>length(vett))
	 {
	 contpp<-0
	 for(kk in 1:length(nv))
	 {
	  
		 if( length(which(nv[kk] %in% unlist(vett)))==0 & str_detect(nv[kk],"GRUPO")!=TRUE)
		  {
		  contpp<-contpp+1
		vett[[length(vett)+contpp]]<-nv[kk]
		  }
		}
	 
	 }
     for(j in 1:length(nv))
	 {
	  
	   
	
      # vett<-newgrupo1[[which(var1[i]==names(newgrupo1))]]
	   contc<-which(unlist(lapply(vett,function(x){length(unique(x))}))>1)
	   contc<-contc[length(contc)]
	   contc<-ifelse(length(contc)==0,length(vett),contc)
	   cont<-0
	  #  if(length(vett[[j]])>1)
	 #{
	   
	 # cat("case when \n")
	   
	 # }
	   
	   
   for(k in 1:length(vett))
    {
	 if(length(vett[[k]])>1)
	  {
	   if(length(vett[[k]])>1)
	 {
	   
	  cat("case when \n")
	   
	  }
	   cont<-cont+1
	   if(cont==1)
	   {
	   
	  cat("(case when (case when ","coalesce(",var1[i],",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),var1[i],"END)",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"\n" )
       }
	   if(cont>1 & k<=contc)
	   {
	   cat("when (case when ","coalesce(",var1[i],",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),var1[i],"END)",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"\n" )
	   }
	   if(k==contc )
	   {
	   #cat("when (case when ","coalesce(",var1[i],",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),var1[i],"END)",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"ELSE ",var1[i]," END as ",var1[i],",\n" )
	   #cat("when (case when ","coalesce(",var1[i],",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),var1[i],"END)",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"ELSE ",var1[i]," END ","\n" )
	   cat(" ELSE ",var1[i],"END","\n" )
	   }
	  }
	  
	 
	 }
	
	
	 if(length(vett[[j]])>=1 & cont>0)
	 {
	 cat(") =",paste("'",nv[j],"'",sep="")," then 1 else 0 end as ",paste("\"",gsub("[ [:punct:]]", "",paste(var1[i],nv[j],sep="")),"\"",sep=""),",\n" )
	 }
	 
	
	 if(cont==0){
	  cat("(case when (case when ","coalesce(",var1[i],",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),var1[i],"END)",paste("in ('", paste(vett[[j]],collapse="','"),"')",sep=""),paste("then 1 else 0 end) as ",sep=""),paste("\"",gsub("[ [:punct:]]", "",paste(var1[i],vett[[j]],sep="")),"\"",sep=""),",\n" )
	  }
	  
	 
     
     }
  
	}
 
  
  } #end for
   

  if(i==length(var1))
   {
   cat(paste("FROM ",input.table))
}
 sink()
}




#SQL_DATA_SET_xgb(data=dados_75_r,pred,var1=var1,file="D:/Users/clp124604/Documents/AUTOMATED_R/model_output_data_set_xgb.SQL",input.table="source_table",id="ZCGINSTALL")

############################
sql.xgb<-function(x,pred,bst,file,table,id)
{
library(stringr)
options(scipen=999)
g<-xgb.model.dt.tree(colnames(x), model = bst)
ntree<-unique(g$Tree)
n.tree<-length(ntree)

sink(file, type="output")

cat("Select ",id,", case when (Pred-",min(pred),")/(",max(pred),"-",min(pred),") >1 then 1 \n")
  cat("when (Pred-",min(pred),")/(",max(pred),"-",min(pred),") <0 then 0 \n")
   cat("else (Pred-",min(pred),")/(",max(pred),"-",min(pred),") end as","Pred \n")
   cat("from (\n")

  aa<- round(quantile(c(1:n.tree),seq(0,1,by=150/n.tree)))
  aa<-aa[-1]
  #aa<-aa[-length(aa)]
  aa1<-aa-1
###################################
 for(jl in 1:(n.tree))
  { 
   if(jl==1)
   {
   cat("SELECT ","\"",id,"\",","(1/(1+EXP(-S))) AS Pred FROM (",sep="")
   cat(paste("SELECT \"",id,"\",", "(boosting",jl,"+\n",sep=""))
    }


   if(jl>1  & jl<(n.tree))
   {
   if((jl %in% aa)==TRUE)
    {
  cat(paste(")+(boosting",jl,"+\n",sep=""))
    }
   if((jl %in% aa)!=TRUE & (jl %in% aa1)!=TRUE)
    {
   cat(paste("boosting",jl,"+\n",sep=""))
    }
    if((jl %in% aa1)==TRUE)
    {
   cat(paste("boosting",jl,"\n",sep=""))
     }
   }



   if(jl==(n.tree))
   {
   cat(paste("boosting",jl,") AS S\n",sep=""))
   cat("FROM ")
   }
}
  
  cat("(SELECT \"",id,"\",",sep="")
###################################


for(i in 1:length(ntree))
{
 g1<-g[g$Tree==ntree[i]]
 g100<-which(str_detect(g1$Feature,"Leaf")==TRUE)
 cond<-c()
 cat("(CASE ")
 for(j in 1:length(g100))
  {
    g10<-list()
    if(length(which(g1[g100[j]]$ID==g1$Yes))>0)
     {
    g10[[1]]<-g1[which(g1[g100[j]]$ID==g1$Yes)]
     cond[1]<-" < "
     }
     if(length(which(g1[g100[j]]$ID==g1$No))>0)
     {
     g10[[1]]<-g1[which(g1[g100[j]]$ID==g1$No)]
     cond[1]<-" >= "
     }
    kl=0
    flag=TRUE
    while(flag==TRUE)
    {
     kl=kl+1
     #g10[[kl+1]]<-g10[[kl]]$ID
    if(length(which(g10[[kl]]$ID==g1$Yes))>0)
     {
    
   g10[[kl+1]]=g1[which(g10[[kl]]$ID==g1$Yes)]
     cond[kl+1]<-" < "
     }
    if(length(which(g10[[kl]]$ID==g1$No))>0)
     {
    
   g10[[kl+1]]=g1[which(g10[[kl]]$ID==g1$No)]
     cond[kl+1]<-" >= "
     }
     #print(kl)
     if(length(which(g10[[kl]]$ID==g1$Yes))==0 & length(which(g10[[kl]]$ID==g1$No))==0)
     {
     if(g10[[kl]]$ID==g1$ID[1])
     {
      flag=FALSE
      }
      }else{
      if(g10[[kl+1]]$ID==g1$ID[1])
      {
       flag=FALSE
      }
     }
}

value=g1[g100[j]]$Quality
cat("WHEN ")
for(jj in length(g10):1)
{ 
value1<-g10[[jj]]$Split
if(str_detect(value1,"e")==TRUE)
   {
   value1<-as.numeric(value1)
   }else{
   value1<-as.numeric(value1)
   }


if(jj==length(g10))
    {
   kk<-paste("\"",gsub("[ [:punct:]]", "",g10[[jj]]$Feature),"\"",cond[jj],value1,sep="")
    }else{
   kk<-paste(kk, " AND ","\"",gsub("[ [:punct:]]", "",g10[[jj]]$Feature),"\"",cond[jj],value1,sep="")
   }
 if(jj==1)
  {
   kk<-paste(kk," THEN ",value,sep="")
   }
 

}

if(j<length(g100))
 {
  cat(kk,"\n")
  }
  if(j==length(g100) & i<length(ntree))
   {
  cat(kk," END) + 0 as boosting",i,",","\n",sep="") 
  }
 if(j==length(g100) & i==length(ntree))
   {
  cat(kk," END) + 0 as boosting",i,"\n",sep="") 
  }

}

}
cat("FROM ",table,")))",sep="")
sink()

}#### end function


#sql.xgb(x=x,pred=pred,bst=bst,file="D:/Users/clp124604/Documents/AUTOMATED_R/sql_model_xgb.sql",table="table",id="ZCGINSTALL")


###################MLP 
SQL_DATA_SET_RN<-function(data,var1,file,input.table,id,newgrupo1,guarda_outros)
{
library(stringr)

sink(file, type="output")


 cat(paste("SELECT ",id,",","\n\n",sep=""))
for(i in 1:length(var1))
{


 if(is.numeric(data[,which(names(data)==var1[i])])==TRUE)
  { 
  cat("case when ","(",paste("coalesce(",var1[i],",", min(data[,which(names(data)==var1[i])]),")",sep=""),"-",min(data[,which(names(data)==var1[i])]),")/(",max(data[,which(names(data)==var1[i])]),"-",min(data[,which(names(data)==var1[i])]),")",">1 then 1 \n")  
    cat("when ","(",paste("coalesce(",var1[i],",", min(data[,which(names(data)==var1[i])]),")",sep=""),"-",min(data[,which(names(data)==var1[i])]),")/(",max(data[,which(names(data)==var1[i])]),"-",min(data[,which(names(data)==var1[i])]),")","<0 then 0 \n")
   cat("else ","(",paste("coalesce(",var1[i],",", min(data[,which(names(data)==var1[i])]),")",sep=""),"-",min(data[,which(names(data)==var1[i])]),")/(",max(data[,which(names(data)==var1[i])]),"-",min(data[,which(names(data)==var1[i])]),")","END AS ",gsub("[ [:punct:]]", "",var1[i]),", \n")
  }
   
    if(is.numeric(data[,which(names(data)==var1[i])])!=TRUE & length(which(var1[i]==names(guarda_outros)))>0 & length(which(var1[i]==names(newgrupo1)))==0)
   {
    nv<-levels(data[,which(names(data)==var1[i])])
	for(j in 1:length(nv))
	 {
	cat("case when \n\n")
   cat("(case when ","coalesce(",var1[i],",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),var1[i],"END) =", paste("'",nv[j],"'",sep=""),"\n")
    cat(" then 1 else 0 end as ",paste("\"",gsub("[ [:punct:]]", "",paste(var1[i],nv[j],sep="")),"\"",sep=""),",\n")
	}
	}
	
	if(is.numeric(data[,which(names(data)==var1[i])])!=TRUE & length(which(var1[i]==names(guarda_outros)))==0 & length(which(var1[i]==names(newgrupo1)))==0)
   {
	
	nv<-levels(data[,which(names(data)==var1[i])])
     for(j in 1:length(nv))
	 {
	 cat("(case when ",var1[i]," = ",paste("'",nv[j],"'",sep="")," then 1 else 0 end) as",paste("\"",gsub("[ [:punct:]]", "",paste("\"",var1[i],nv[j],"\"",sep="")),"\"",sep=""),",\n")
	}
	
	}
	
	
	 if(is.numeric(data[,which(names(data)==var1[i])])!=TRUE & length(which(var1[i]==names(guarda_outros)))==0 & length(which(var1[i]==names(newgrupo1)))>0)
   {
     nv<-levels(data[,which(names(data)==var1[i])])
     for(j in 1:length(nv))
	 {
	cat("case when \n\n")
	
       vett<-newgrupo1[[which(var1[i]==names(newgrupo1))]]
	   contc<-which(unlist(lapply(vett,function(x){length(unique(x))}))>1)
	   contc<-contc[length(contc)]
	   cont<-0
	    if(max(unlist(lapply(vett,function(x){length(x)})))>1)
	   {
	   for(k in 1:length(vett))
     {
	 if(length(vett[[k]])>1)
	  {
	   cont<-cont+1
	   if(cont==1)
	   {
	   
	  cat("(case when ","coalesce(",var1[i],",'MISSING')",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"\n" )
       }
	   if(cont>1 & k<=contc)
	   {
	   cat("when ","coalesce(",var1[i],",'MISSING')",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"\n" )
	   }
	   if(k==contc)
	   {
	   #cat("when (case when ","coalesce(",var1[i],",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),var1[i],"END)",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"ELSE ",var1[i]," END as ",var1[i],",\n" )
	  # cat("when ","coalesce(",var1[i],",'MISSING')",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"ELSE ",var1[i]," END ","\n" )
	   cat(" ELSE ",var1[i],"END","\n" )
	   }
	  }
	 
	
	
	}
	
	}else{
	cat("(",var1[i] )
	}
	 cat(") =",paste("'",nv[j],"'",sep="")," then 1 else 0 end as",paste("\"",gsub("[ [:punct:]]", "",paste(var1[i],nv[j],sep="")),"\"",sep=""),",\n" )
     
     }
	   
	   
	
	}
	
	 if(is.numeric(data[,which(names(data)==var1[i])])!=TRUE & length(which(var1[i]==names(guarda_outros)))>0 & length(which(var1[i]==names(newgrupo1)))>0)
   {
     nv<-levels(data[,which(names(data)==var1[i])])
	 
	
	 
	 
	 
     for(j in 1:length(nv))
	 {
	  
	   
	
       vett<-newgrupo1[[which(var1[i]==names(newgrupo1))]]
	    if(length(nv)>length(vett))
	 {
	  for(inju in 1:length(nv))
	   {
	    if(inju>length(vett))
		{
		vett[[inju]]<-nv[inju]
		
		}
	   
	   }
	 
	 }
	   contc<-which(unlist(lapply(vett,function(x){length(unique(x))}))>1)
	   contc<-contc[length(contc)]
	   cont<-0
	 #   if(length(vett[[j]])>1)
	#  {
	   
	#   cat("case when \n\n")
	   
	#   }
	   
   for(k in 1:length(vett))
    {
	 if(length(vett[[k]])>1)
	  {
	   cont<-cont+1
	   if(cont==1)
	   {
	       if(length(vett[[k]])>1)
	 {
	   
	  cat("case when \n")
	   
	  }
	   
	  cat("(case when (case when ","coalesce(",var1[i],",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),var1[i],"END)",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"\n" )
       }
	   if(cont>1 & k<=contc)
	   {
	   cat("when (case when ","coalesce(",var1[i],",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),var1[i],"END)",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"\n" )
	   }
	   if(k==contc )
	   {
	   #cat("when (case when ","coalesce(",var1[i],",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),var1[i],"END)",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"ELSE ",var1[i]," END as ",var1[i],",\n" )
	   #cat("when (case when ","coalesce(",var1[i],",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),var1[i],"END)",paste("in ('", paste(vett[[k]],collapse="','"),"')",sep=""),paste("then 'GRUPO",k,"'",sep=""),"ELSE ",var1[i]," END ","\n" )
	  cat(" ELSE ",var1[i],"END","\n" )
	  }
	  }
	  
	 
	
	
	}
	 if(length(vett[[j]])>=1 & cont>0)
	 {
	 cat(") =",paste("'",nv[j],"'",sep="")," then 1 else 0 end as",paste("\"",gsub("[ [:punct:]]", "",paste(var1[i],nv[j],sep="")),"\"",sep=""),",\n" )
	 }
	 
	
	 if(cont==0){
	  cat("(case when (case when ","coalesce(",var1[i],",'MISSING') not in ", paste("('",paste(guarda_outros[[which(var1[i]==names(guarda_outros))]],collapse="','"),"') then 'OUTROS' ELSE",sep=""),var1[i],"END)",paste("in ('", paste(vett[[j]],collapse="','"),"')",sep=""),paste("then 1 else 0 end) as ",sep=""),paste("\"",gsub("[ [:punct:]]", "",paste(var1[i],vett[[j]],sep="")),"\"",sep=""),",\n" )
	  }
	
	
     
     }
  
	}
 

  } #end for
   

  if(i==length(var1))
   {
   cat(paste("FROM ",input.table))
}
 sink()
}




#SQL_DATA_SET_RN(data=dados_75,var1=var1,file="D:/Users/clp124604/Documents/AUTOMATED_R/model_output_data_set_RN.SQL",input.table="source_table",id="ZCGINSTALL")

####################### SQL MLP

SQL_MLP<-function(fit,pred,id,file)
{
aa<- round(quantile(c(1:nrow(fit$wh)),seq(0,1,by=150/nrow(fit$wh))))
  aa<-aa[-1]
  #aa<-aa[-length(aa)]
  aa1<-aa-1

sink(file, type="output")

cat("Select ",id,", case when (SCORE_MLP-",min(pred),")/(",max(pred),"-",min(pred),") >1 then 1 \n")
  cat("when (SCORE_MLP-",min(pred),")/(",max(pred),"-",min(pred),") <0 then 0 \n")
   cat("else (SCORE_MLP-",min(pred),")/(",max(pred),"-",min(pred),") end as","Pred \n")
   cat("from (\n")

library(stringr)
for(j in 1:ncol(fit$wh)  )

{

 if(j==1)
  {
cat(paste("SELECT ",id," , 1/(1+EXP(-(",fit$bout_val,"*(Hidden_Layer_",j,sep=""),"\n")
 }
if(j>1 & j<ncol(fit$wh))
{
cat(paste( "+Hidden_Layer_",j,sep=""),"\n")

}
if(j==ncol(fit$wh))
{
cat(paste( "+Hidden_Layer_",j,")))) as SCORE_MLP from ",sep=""),"\n")

}



}

for(j in 1:ncol(fit$wh))
   {
if(j==1)
{
cat("(SELECT ",id,",\n")

}

for(i in 1:nrow(fit$wh))
{
 if(i==1)
  {
cat(fit$wout[j,1],"+(1/(1+EXP(-((",gsub("[ [:punct:]]", "",rownames(fit$wh)[i]),"*",fit$wh[i,j],"\n")
 }

if((i %in% aa)!=TRUE)
{

if(i>1 & i<nrow(fit$wh))
{
cat("+",gsub("[ [:punct:]]", "",rownames(fit$wh)[i]),"*",fit$wh[i,j],"\n")
}
if(i==nrow(fit$wh) & j<ncol(fit$wh))
{
 cat(paste("+",gsub("[ [:punct:]]", "",rownames(fit$wh)[i]),"*",fit$wh[i,j],"))))) as Hidden_Layer_",j,",\n\n",sep=""))
}
if(i==nrow(fit$wh) & j==ncol(fit$wh))
{
cat(paste("+",gsub("[ [:punct:]]", "",rownames(fit$wh)[i]),"*",fit$wh[i,j],"))))) as Hidden_Layer_",j,"\n from table))",sep=""))

}

}else{

if(i>1 & i<nrow(fit$wh))
{
cat(")+(",gsub("[ [:punct:]]", "",rownames(fit$wh)[i]),"*",fit$wh[i,j],"\n")
}

}   
 
    
} ## end for i
} ## end for j

  sink()

}## end function
#SQL_MLP(fit,id="ZCGINSTALL",pred=pred1,file="D:/Users/clp124604/Documents/AUTOMATED_R/MLP_SQL.sql")




######################################################################################
#dados3<-read.csv("D:/Users/clp124604/Desktop/bank/bank-full.csv",sep=";",h=T)
#alvo<-dados3[,"y"]
#dados3<-dados3[,-c(1,ncol(dados3))]
#dados1<-dados3
#mau="yes"

#alvo<-dados[,"ACERTO"]
#dados3<-dados[,-which(names(dados)=="ACERTO")]
#dados1<-dados3
#mau=1

######pacotes necessarios###

#install.packages("stringr")
#install.packages("xgboost")
#install.packages("randomForest")
#install.packages("xlsx")
#install.packages("gbm")
#install.packages("FSelector")
#install.packages("ROSE")


################# REde Neural (MLP BACkPROPAGATION)############

sigmoid<-function(x){
1/(1+exp(-x))
}

# derivative of sigmoid function
derivatives_sigmoid<-function(x){
x*(1-x)
}
MLP<-function(X,X1,Y,Y1,epoch,lr,hiddenlayer_neurons)
{
inputlayer_neurons=ncol(X)
output_neurons=1
#weight and bias initialization
wh=matrix( rnorm(inputlayer_neurons*hiddenlayer_neurons,mean=0,sd=1), inputlayer_neurons, hiddenlayer_neurons)
bias_in=runif(hiddenlayer_neurons)
bias_in_temp=rep(bias_in, nrow(X))
bias_in_temp_val=rep(bias_in, nrow(X1))
bh=matrix(bias_in_temp, nrow = nrow(X), byrow = FALSE)
bh_val=matrix(bias_in_temp_val, nrow = nrow(X1), byrow = FALSE)
wout=matrix( rnorm(hiddenlayer_neurons*output_neurons,mean=0,sd=1), hiddenlayer_neurons, output_neurons)

bias_out=runif(output_neurons)
bias_out_temp=rep(bias_out,nrow(X))
bias_out_temp_val=rep(bias_out,nrow(X1))
bout=matrix(bias_out_temp,nrow = nrow(X),byrow = FALSE)
bout_val=matrix(bias_out_temp_val,nrow = nrow(X1),byrow = FALSE)
# forward propagation
err<-rep(NA,epoch/2)
cont<-0
for(i in 1:epoch){

###########treinamento
hidden_layer_input1= X%*%wh
hidden_layer_input=hidden_layer_input1+bh
hidden_layer_activations=sigmoid(hidden_layer_input)
output_layer_input1=hidden_layer_activations%*%wout
output_layer_input=output_layer_input1+bout
output= sigmoid(output_layer_input)

########## validação
hidden_layer_input1_val= X1%*%wh
hidden_layer_input_val=hidden_layer_input1_val#+bh_val
hidden_layer_activations_val=sigmoid(hidden_layer_input_val)
output_layer_input1_val=hidden_layer_activations_val%*%wout
output_layer_input_val=output_layer_input1_val+bout_val
output_val= sigmoid(output_layer_input_val)




# Back Propagation

E=Y-output
slope_output_layer=derivatives_sigmoid(output)
slope_hidden_layer=derivatives_sigmoid(hidden_layer_activations)
d_output=E*slope_output_layer
Error_at_hidden_layer=d_output%*%t(wout)
d_hiddenlayer=Error_at_hidden_layer*slope_hidden_layer
wout= wout + (t(hidden_layer_activations)%*%d_output)*lr
bout= bout+rowSums(d_output)*lr
wh = wh +(t(X)%*%d_hiddenlayer)*lr
bh = bh + rowSums(d_hiddenlayer)*lr
if(i%%2==0)
{
print(i)
print(mean(ifelse((Y1-output_val)>0.5,1,0)))
cont<-cont+1
err[cont]<-mean(ifelse((Y1-output_val)>0.5,1,0))
plot(err,type="l",col="red")
}
}

return(list(wh=wh,wout=wout,bout_val=bout_val[1]))
}



######predict test

predict_MLP<-function(fit,newdata)
{
wh<-fit[[1]]
wout<-fit[[2]]
bout_val<-fit[[3]]
hidden_layer_input1_test= newdata%*%wh
hidden_layer_activations_test=sigmoid(hidden_layer_input1_test)
output_layer_input1_test=hidden_layer_activations_test%*%wout
output_layer_input_test=output_layer_input1_test+bout_val
output_test= sigmoid(output_layer_input_test)
return(output_test)
}




###############################calculo da curva do KS#####################




Entropy<-function( vls ) {
  res<-vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0]<- 0
  -sum(res)
}

InformationGain<-function( tble ) {
  tble<-as.data.frame.matrix(tble)
  entropyBefore<-Entropy(colSums(tble))
  s<-rowSums(tble)
  entropyAfter<-sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy ))
  informationGain<-entropyBefore - entropyAfter
  return (informationGain)
}
information.gain1<-function(formula=alvo~.,data=dados_75_r)
{
 GI<-c()
 for(i in 1:(ncol(data)-1))
  {
   tb<-table(data[,i],data[,as.character(formula[[2]])])
   GI[i]<-InformationGain(tb)
   }
  GI<-data.frame(attr_importance=cbind(GI))
  rownames(GI)<-names(data)[-ncol(data)]
  return(GI)
}

calc_ecdf <- function(x) {
	u <- sort(unique(x))
	n <- length(u)
	cdf <- vector(mode='numeric', n)
	for (i in 1:n) {
		cdf[i] = sum(x < u[i])/length(x)
	}
	df <- data.frame(c(0,u,1),c(0,cdf,1))
	names(df) <- c('x','y')
	return(df)
}


calc_ecdf_dif <- function(e0, e1) {
	n0 <- nrow(e0)
	n1 <- nrow(e1)

	i0 = 1
	i1 = 1
	
	x = c()
	y = c()

	while ((i0 <= n0) & (i1 <= n1)) {
		x0 = e0[i0,'x']
		y0 = e0[i0,'y']

		x1 = e1[i1,'x']
		y1 = e1[i1,'y']

		if (x0 < x1) {
			x <- c(x, x0)
			i0 <- i0 + 1
		} else if (x0 == x1) {
			x <- c(x,x0)
			i0 <- i0 + 1
			i1 <- i1 + 1
		} else {
			x <- c(x, x1)
			i1 <- i1 + 1
		}

		y <- c(y, y0 - y1)
	}

	return(data.frame(x, y))
}

calc_auc_ks2 <- function(dif) {
	n <- nrow(dif)

	auc <- double(1)

	for (i in 1:(n-1)) {
		amplitude_x <- dif[i+1,'x'] - dif[i,'x']
		amplitude_y <- dif[i+1,'y'] - dif[i,'y']
		area_retangulo <- amplitude_x*dif[i,'y']
		area_triangulo <- .5 * amplitude_y * amplitude_y
		auc = area_retangulo + area_triangulo
	}
	return(auc)
}

####################################################################




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

library(ggplot2)

bivariada<-function(dados1=NA,alvo1,formula=alvo~.,chave1=NA,analise="FALSE",manual="FALSE",mau=NA,bom=NA,id="id",projeto="",data_model=NA,data_teste=NA,balanced=TRUE,PROD=NA,ADE=FALSE,type="SQL SAP HANA")
{

if(length(which(is.na(data_model)!=TRUE))>0)
{
dados1<-rbind(data_model,data_teste)
}
if(length(which(is.na(chave1)!=TRUE))>0)
{
chave<-dados1[,which(names(dados1) %in% chave1)]
dados1<-dados1[,-which(names(dados1) %in% chave1)]
}
alvo<-dados1[,which(names(dados1) %in% alvo1)]
dados1<-dados1[,-which(names(dados1) %in% alvo1)]

f<-Testa_dados(dados1)
if(length(f)>0)
{
dados1<-dados1[,-unique(f)]
}


library(stringr)
guarda_niv<-list()
guarda_var<-list()
cont2<-0
###################juntar excel's
juntar_xlsx <- function(pasta, colunas, coluna_variavel, arquivo_de_saida) {
	wd_atual <- getwd()
        library(stringr)
	require(xlsx)
	setwd(pasta)

	wb <- createWorkbook()
	sheet <- createSheet(wb, "Sheet1")

	linha <- 2

	cs1 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border(position=c("TOP", "BOTTOM"))

	message(paste("Processando a pasta:", pasta))
        et<-grep("*.xlsx", dir(), value=TRUE)
        if(str_detect(et[length(et)],"~")==TRUE)
        {
         et<-et[-length(et)]
        }
	for (i in et) {
		message(paste("\t", i))
		tabela <- read.xlsx(i, 1, encoding="UTF-8", stringsAsFactors = FALSE)
		nome_primeira_variavel <- tabela[1,coluna_variavel]

		tabela <- tabela[,colunas]
		novas_variaveis <- colunas
		novas_variaveis[1] <- nome_primeira_variavel 
		names(tabela) <- novas_variaveis


		addDataFrame(tabela, sheet, startRow=linha, startColumn=1, colnamesStyle=cs1)

		linha <- linha + nrow(tabela) + 4
	}

	saveWorkbook(wb, arquivo_de_saida)

	message("Pronto!")
	setwd(wd_atual)
}
#########################################################
variaveis<-names(dados1)
library(xlsx)
#library("Hmisc")
dir.create(paste("C:/Resultados_bivariadas_",projeto,sep=""))
ganho_inf<-c()
matrix_ganho_inf<-matrix(0,ncol=2,nrow=ncol(dados1))
for(j in 1:ncol(dados1))
 {
 #j<-1
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
 cat("A variavel é:",variaveis[j],"\n")
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
guarda_var[[cont2]]<-variaveis[j]

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
ganho_inf[j]<-entropia_raiz-media_entrop
entropia_niv<-c(entropia_niv,entropia_raiz)
b1<-cbind(b1,entropia_niv)
b1<-cbind(b1,ganho_inf[j])

b2<-table(dados1[,j],alvo)
freq_bom<-b2[,1]/sum(b2[,1])
freq_mau<-b2[,2]/sum(b2[,2])
freq_ac_bom<-cumsum( freq_bom )
freq_ac_mau<-cumsum( freq_mau )
diff<-round(abs(freq_ac_bom-freq_ac_mau),3)

ks1<-max(diff)


b1<-data.frame(b1,ks1,variaveis[j])


ef<-is.na(z)
if(sum(ef==TRUE)>0)
{
rown<-c(levels(z),"Missing","total")
}else{
rown<-c(levels(z),"total")
}
coln<-c("BOM","MAU","total","freq","tx_maus","RR","entropia","ganho_inf","KS2","variaveis")
colnames(b1)<-coln
rownames(b1)<-rown

b29<-rownames(b1)
ind24<-c(which(b29=="Missing"),which(b29=="total"))
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
rownames(b1)<-niv31
write.xlsx(b1,paste("C:/Resultados_bivariadas_",projeto,"/",variaveis[j],".xlsx",sep=""),col.names=TRUE, row.names=TRUE)

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
ganho_inf[j]<-entropia_raiz-media_entrop
entropia_niv<-c(entropia_niv,entropia_raiz)
b1<-cbind(b1,entropia_niv)
b1<-cbind(b1,ganho_inf[j])
KS2<-0
b1<-data.frame(b1,KS2,variaveis[j])
ef2<-rownames(table(dados1[,j],alvo,useNA = "ifany"))
for(i in 1:length(ef2))
{
 if(is.na(ef2[i])=="TRUE")
 {
  ef2[i]<-"MISSING"
}
}

rown<-c(ef2,"total")
coln<-c("BOM","MAU","total","freq","tx_maus","RR","entropia","ganho_inf","KS2","variaveis")
colnames(b1)<-coln
rownames(b1)<-rown

write.xlsx(b1,paste("C:/Resultados_bivariadas_",projeto,"/",variaveis[j],".xlsx",sep=""),col.names=TRUE, row.names=TRUE)

}#end if
ed<-colnames(dados1)

matrix_ganho_inf[j,]<-c(ganho_inf[j],ed[j])
print(j)

} #end for

colunas <- c("NA.", "BOM", "MAU", "total", "freq", "tx_maus", "RR","entropia","ganho_inf","KS2","variaveis")
coluna_variavel <- "variaveis"

pasta <- paste("C:/Resultados_bivariadas_",projeto,sep="")
arquivo_de_saida <- paste("C:/Resultados_bivariadas_",projeto,"/bivariada_full.xlsx",sep="")
juntar_xlsx(pasta, colunas, coluna_variavel, arquivo_de_saida)
write.xlsx(matrix_ganho_inf,paste("C:/Resultados_bivariadas_",projeto,"/ganho_inf2.xlsx",sep=""),col.names=TRUE, row.names=TRUE)

len<-c()
for(m in 1:length(guarda_niv))
{
 len[m]<-length(guarda_niv[[m]])
}
g21<-matrix(nrow=length(guarda_niv),ncol=(max(len)+1))

for(m in 1:length(guarda_niv))
{
 g21[m,1]<-guarda_var[[m]]
 g21[m,2:(len[m]+1)]<-guarda_niv[[m]]
}
write.table(g21,paste("C:/Resultados_bivariadas_",projeto,"/faixas.txt",sep=""),col.names=FALSE, row.names=FALSE,sep="\t")


##################################analise=TRUE
if(analise=="TRUE")
{
options(scipen=999)
data_teste_f<-data_teste
data_model_f<-data_model
PROD_f<-PROD


contkkls1<-0
varcontkkls1<-c()
for(kkls1 in 1:ncol(data_model_f))
{
if(is.factor(data_model_f[,kkls1])==TRUE)
{
contkkls1<-contkkls1+1
varcontkkls1[contkkls1]<-names(data_model_f)[kkls1]

}


}

if(length(varcontkkls1)>0)
{
write.table(varcontkkls1,
paste("C:/Resultados_bivariadas_",projeto,"/var_factor.csv",sep=""),sep=";",col.names=T,row.names=F,dec=",")

}



if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{
PROD<-PROD[,names(data_teste)]

var_exclu_ade<-aderencia(data_teste[,-which(names(data_teste) %in% c(chave1))],PROD[,-which(names(PROD) %in% c(chave1,alvo1))],dire=projeto,alvo1=alvo1)
}


if(ADE==TRUE)
{

var_dummy<-names(var_exclu_ade)
var_dummy_m<-c()
contgg100<-0
contgg101<-0
var_exclu_df<-c()
for(kkj1 in 1:length(var_exclu_ade))
{
 if(length(var_exclu_ade[[kkj1]])>0)
  {
 for(kkj2 in 1:length(var_exclu_ade[[kkj1]]))
   {
       contgg100<-contgg100+1 
      if(is.numeric(data_model[,var_dummy[kkj1]])==TRUE  & sum(str_detect(var_exclu_ade[[kkj1]],"Range")==TRUE)==0)
      {	  
      flag_dummy<-var_fx(data_model[,var_dummy[kkj1]],var_exclu_ade[[kkj1]][kkj2])
	  flag_dummyt<-var_fx(data_teste[,var_dummy[kkj1]],var_exclu_ade[[kkj1]][kkj2])
	  flag_dummyp<-var_fx(PROD[,var_dummy[kkj1]],var_exclu_ade[[kkj1]][kkj2])
	   }
	   if(is.numeric(data_model[,var_dummy[kkj1]])!=TRUE)
	   {
	    if(var_exclu_ade[[kkj1]][kkj2]!="Missing")
		{
	   flag_dummy<-which(data_model[,var_dummy[kkj1]]==var_exclu_ade[[kkj1]][kkj2])
	   flag_dummyt<-which(data_teste[,var_dummy[kkj1]]==var_exclu_ade[[kkj1]][kkj2])
	   flag_dummyp<-which(PROD[,var_dummy[kkj1]]==var_exclu_ade[[kkj1]][kkj2])
	    }else{
		flag_dummy<-which(is.na(data_model[,var_dummy[kkj1]])==TRUE)
	   flag_dummyt<-which(is.na(data_teste[,var_dummy[kkj1]])==TRUE)
	   flag_dummyp<-which(is.na(PROD[,var_dummy[kkj1]])==TRUE)
		}
	   }
	   if(contgg100==1)
	   {
	   flag_dummy1<-flag_dummy
	   flag_dummyt1<-flag_dummyt
	   flag_dummyp1<-flag_dummyp
	   }else{
	   flag_dummy1<-c(flag_dummy1,flag_dummy)
	   flag_dummyt1<-c(flag_dummyt1,flag_dummyt)
	    flag_dummyp1<-c(flag_dummyp1,flag_dummyp)
	   }
	  
   }
   
    if(length(flag_dummy1)>0)
	   {
	   if(is.numeric(data_model[,var_dummy[kkj1]])==TRUE & sum(str_detect(var_exclu_ade[[kkj1]],"Range")==TRUE)==0)
      {
	   which(!(1:nrow(data_model) %in% flag_dummy1))
	    ppttx<-max(data_model[,var_dummy[kkj1]],na.rm=T)
       	data_model[which(!(1:nrow(data_model) %in% flag_dummy1)),var_dummy[kkj1]] <-  ppttx+100
		data_teste[which(!(1:nrow(data_teste) %in% flag_dummyt1)),var_dummy[kkj1]] <-  ppttx+100
		PROD[which(!(1:nrow(PROD) %in% flag_dummyp1)),var_dummy[kkj1]] <-  ppttx+100
		}
		if(is.numeric(data_model[,var_dummy[kkj1]])!=TRUE)
	   {
		data_model[,var_dummy[kkj1]]<-as.character(data_model[,var_dummy[kkj1]])
		data_teste[,var_dummy[kkj1]]<-as.character(data_teste[,var_dummy[kkj1]])		
		PROD[,var_dummy[kkj1]]<-as.character(PROD[,var_dummy[kkj1]])
		data_model[which(!(1:nrow(data_model) %in% flag_dummy1)),var_dummy[kkj1]] <- "OUT_RANGE"
		data_teste[which(!(1:nrow(data_teste) %in% flag_dummyt1)),var_dummy[kkj1]] <- "OUT_RANGE"
		
		PROD[which(!(1:nrow(PROD) %in% flag_dummyp1)),var_dummy[kkj1]] <- "OUT_RANGE"
		
		data_model[,var_dummy[kkj1]]<-factor(data_model[,var_dummy[kkj1]])
		data_teste[,var_dummy[kkj1]]<-factor(data_teste[,var_dummy[kkj1]])
		PROD[,var_dummy[kkj1]]<-factor(PROD[,var_dummy[kkj1]])
		
		}
   flag_dummy1<-NA
   flag_dummyt1<-NA
   flag_dummyp1<-NA
   }
    
print(kkj1)
}else{
contgg101<-contgg101+1
var_exclu_df[contgg101]<-var_dummy[kkj1]

}


}

if(length(var_exclu_df)>0)
{
data_model<-data_model[,-which(names(data_model) %in% var_exclu_df)]
data_teste<-data_teste[,-which(names(data_teste) %in% var_exclu_df)]
PROD<-PROD[,-which(names(PROD) %in% var_exclu_df)]
write.table(var_exclu_df,
paste("C:/Resultados_bivariadas_",projeto,"/var_excluidas_ade.csv",sep=""),sep=";",col.names=T,row.names=F,dec=",")

}


contgg101<-0
indextt1001<-c()
for(kkj3 in 1:ncol(data_model))
{
if(length(unique(data_model[,kkj3]))==1)
{
contgg101<-contgg101+1
indextt1001[contgg101]<-kkj3


}
if(length(unique(data_model[,kkj3]))==2 & sum(is.na(data_model[,kkj3])==TRUE)>0)
{
contgg101<-contgg101+1
indextt1001[contgg101]<-kkj3
}

}

if(length(indextt1001)>0)
{
write.table(names(data_model)[indextt1001],
paste("C:/Resultados_bivariadas_",projeto,"/var_excluidas_filtro.csv",sep=""),sep=";",col.names=T,row.names=F,dec=",")
data_model<-data_model[,-indextt1001]
data_teste<-data_teste[,-indextt1001]
 if(ADE==TRUE)
{
PROD<-PROD[,-indextt1001]
}
}

}


if(length(which(is.na(data_model)!=TRUE))>0)
{
alvo_trn<-data_model[,which(names(data_model) %in% alvo1)]
alvo_tst<-data_teste[,which(names(data_teste) %in% alvo1)]
data_model1<-data_model[,-which(names(data_model) %in% c(chave1,alvo1))]
data_teste1<-data_teste[,-which(names(data_teste) %in% c(chave1,alvo1))]
 if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{
PROD<-PROD[,-which(names(PROD) %in% c(chave1,alvo1))]
}
data_model1<-data.frame(data_model1,alvo=alvo_trn)
data_teste1<-data.frame(data_teste1,alvo=alvo_tst)

f101<-Testa_dados(data_model1)
if(length(f101)>0)
{
write.table(names(data_model1)[f101],
paste("C:/Resultados_bivariadas_",projeto,"/var_excluidas_filtro1.csv",sep=""),sep=";",col.names=T,row.names=F,dec=",")
if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{
PROD<-PROD[,-which(names(PROD) %in% names(data_model1)[f101])]
}
data_model1<-data_model1[,-f101]
data_teste1<-data_teste1[,-f101]
 
}

dados_full<-rbind(data_model1,data_teste1)


dados_75<-data_model1
dados_25<-data_teste1
rm(data_model1,data_teste1)
index_tst<-c((nrow(dados_75)+1):nrow(dados_full))
}else{

dados_full<-data.frame(dados1,alvo)

ind_m<-which(dados_full[,"alvo"]==bom)
ind_b<-which(dados_full[,"alvo"]==mau)

p_b<-length(ind_b)/nrow(dados_full)
p_m<-1-p_b



ind_75_b<-sample(ind_b,nrow(dados_full)*0.75*p_b,replace=FALSE)
ind_75_m<-sample(ind_m,nrow(dados_full)*0.75*p_m,replace=FALSE)
dados_75<-dados_full[c(ind_75_b,ind_75_m),]
dados_25<-dados_full[-c(ind_75_b,ind_75_m),]
index_tst<-1:nrow(dados_full)
index_tst<-index_tst[-c(ind_75_b,ind_75_m)]


}


contjl<-0
varjl<-list()
varnamejl<-c()
for(jl in 1:(ncol(dados_75)-1))
 {
 
  if(is.numeric(dados_75[,jl])==TRUE & sum(is.na(dados_75[,jl])==TRUE)>=1)
   {
     
     dados_75[is.na(dados_75[,jl])==TRUE,jl]<-min(dados_75[,jl],na.rm=TRUE)
	 
   }
     if(is.numeric(dados_25[,jl])==TRUE & sum(is.na(dados_25[,jl])==TRUE)>=1)
   {
     dados_25[is.na(dados_25[,jl])==TRUE,jl]<-min(dados_75[,jl],na.rm=TRUE)
   }
    if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{  
 if(is.numeric(PROD[,jl])==TRUE)
 {
  contjl<-contjl+1
  varjl[[contjl]]<-min(dados_75[,jl],na.rm=TRUE)
	 varnamejl[[contjl]]<-names(dados_75)[jl]
 }
 }
   if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{
   if(is.numeric(PROD[,jl])==TRUE & sum(is.na(PROD[,jl])==TRUE)>=1 )
   {
  
     PROD[is.na(PROD[,jl])==TRUE,jl]<-min(dados_75[,jl],na.rm=TRUE)
	
   }
   }
  if(is.numeric(dados_75[,jl])!=TRUE & sum(is.na(dados_75[,jl])==TRUE)>=1)
   {
     dados_75[,jl]<-as.character(dados_75[,jl])
     dados_75[is.na(dados_75[,jl])==TRUE,jl]<-"MISSING"
     dados_75[,jl]<-factor(dados_75[,jl])
   }
     if(is.numeric(dados_25[,jl])!=TRUE & sum(is.na(dados_25[,jl])==TRUE)>=1)
   {
      dados_25[,jl]<-as.character(dados_25[,jl])
      dados_25[is.na(dados_25[,jl])==TRUE,jl]<-"MISSING"
      dados_25[,jl]<-factor(dados_25[,jl])
   }
   if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{
      if(is.numeric(PROD[,jl])!=TRUE & sum(is.na(PROD[,jl])==TRUE)>=1 )
   {
      PROD[,jl]<-as.character(PROD[,jl])
      PROD[is.na(PROD[,jl])==TRUE,jl]<-"MISSING"
      PROD[,jl]<-factor(PROD[,jl])
   }
}
}

if(length(varjl)>0)
{
names(varjl)<-varnamejl
save(varjl,file =paste("C:/Resultados_bivariadas_",projeto,"/var_nu_missing.RData",sep=""))

}

####var_import###




rp1<-information.gain1(formula=alvo~.,data=dados_75)

rp1<-data.frame(var=rownames(rp1),importance=rp1$GI)
rp1<-rp1[order(rp1[,2],decreasing = T),]
#var_imp<-as.character(rp1[1:round((ncol(dados_75)-1)*0.7),1])
var_imp<-as.character(rp1[which(rp1[,2]>rp1[1,2]*0.03),1])
var_imp<-c(var_imp,"alvo")


#rp2<-AUCRF1(alvo~., data=dados_75,k0=(ncol(dados_75)-1)*0.5)

#dados_75<-dados_75[,which(names(dados_75) %in% var_imp)]

#dados_25<-dados_25[,which(names(dados_25) %in% var_imp)]

#PROD<-PROD[,which(names(PROD) %in% var_imp)]


################# agrupar variáveis categoricas
guarda_outros<-list()
newgrupo1<-list()
contvar<-0
contvar1<-0
var_name<-c()
var_name1<-c()
for(i in 1:(ncol(dados_75)-1))
{
if(is.numeric(dados_75[,i])!=TRUE)
{

if(length(which(table(dados_75[,i])<30))>1)
{
contvar<-contvar+1
guarda_outros[[contvar]]<-names(table(dados_75[,i]))[which(table(dados_75[,i])>=30)]
levels(dados_75[,i])[which(table(dados_75[,i])<30)]<-"OUTROS"
#levels(dados_25[,i])[which(table(dados_75[,i])<30)]<-"OUTROS"
levels(dados_25[,i])[-which(levels(dados_25[,i]) %in% names(table(dados_75[,i]))[which(table(dados_75[,i])>=30)])]<-"OUTROS"
if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{
levels(PROD[,i])[-which(levels(PROD[,i]) %in% names(table(dados_75[,i]))[which(table(dados_75[,i])>=30)])]<-"OUTROS"
}
if(contvar==1)
{
var_name<-names(dados_75)[i]
}else{
var_name<-c(var_name,names(dados_75)[i])
}
}
if(length(var_name)>0)
{
names(guarda_outros)<-var_name
}


if(length(unique(dados_75[,i]))>2)
{

grup<-cbind(taxa=table(dados_75[,i],dados_75$alvo)[,2]/apply(table(dados_75[,i],dados_75$alvo),1,sum),
entropy=apply(table(dados_75[,i],dados_75$alvo),1,Entropy))
nvv<-(length(grup)/2)
grup1<-grup
newgrupo<-list()
nv10<-rownames(grup)
flag=TRUE
k<-0
while(flag==TRUE)
{
 k<-k+1
 
  # newgrupo[[k]]<-names(which( grup[,1]>=grup[k,1]*0.95 & grup[,1]<=grup[k,1]*1.05))
   newgrupo[[k]]<-names(which( grup[,1]>=grup[1,1]*0.95 & grup[,1]<=grup[1,1]*1.05))
   grup<-grup[-which( grup[,1]>=grup[1,1]*0.95 & grup[,1]<=grup[1,1]*1.05),]
   grup<-rbind(grup)
    if(nrow(grup)==0)
   {
   contvar1<-contvar1+1
   flag=FALSE
  
   }else{
 if(length(which(nv10 %in% newgrupo[[k]]))>0)
  {
  nv10<-nv10[-which(nv10 %in% newgrupo[[k]])]
  }
 
  
  if(k==(nvv-1))
  {
  newgrupo[[k+1]]<-names(which( grup1[,1]==grup[1,1]))
  }

 
 if(length(nv10) %in% c(0,1) | (length(grup)/2) %in% c(0,1) )
  {
   contvar1<-contvar1+1
  flag=FALSE
  }
  
  }

}

if(contvar1==1)
{
var_name1<-names(dados_75)[i]
}else{
var_name1<-c(var_name1,names(dados_75)[i])
}

if(length(var_name1)>0)
{
newgrupo<-newgrupo[which(duplicated(newgrupo)=="FALSE")]
newgrupo1[[contvar1]]<-newgrupo
names(newgrupo1)<-var_name1
}



for(k in 1:length(newgrupo))
{
  if(length(newgrupo[[k]])>1)
  {   
 levels(dados_75[,i])[which(levels(dados_75[,i]) %in% newgrupo[[k]])]<-paste("GRUPO",k,sep="")
 levels(dados_25[,i])[which(levels(dados_25[,i]) %in% newgrupo[[k]])]<-paste("GRUPO",k,sep="")
 if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{
 levels(PROD[,i])[which(levels(PROD[,i]) %in% newgrupo[[k]])]<-paste("GRUPO",k,sep="")
 }
  }else{
levels(dados_75[,i])[which(levels(dados_75[,i]) %in% newgrupo[[k]])]<-newgrupo[[k]]
 levels(dados_25[,i])[which(levels(dados_25[,i]) %in% newgrupo[[k]])]<-newgrupo[[k]]
 if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{
 levels(PROD[,i])[which(levels(PROD[,i]) %in% newgrupo[[k]])]<-newgrupo[[k]]
 }

}  

}

}#end if

}


if(i==(ncol(dados_75)-1) & length(guarda_outros)==0)
{
guarda_outros<-NA
}
if(i==(ncol(dados_75)-1) & length(newgrupo1)==0)
{
newgrupo1<-NA
}
print(i)
}

#################

if(nrow(dados_75)>30000)
{
ind_gbm1<-sample(1:nrow(dados_75),30000,replace=F)
dados_75<-dados_75[ind_gbm1,]
}

for(i in 1:(ncol(dados_75)-1))
{
if(is.numeric(dados_75[,i])!=TRUE)
{
if(length(unique(as.character(dados_75[,i])))!=length(unique(as.character(dados_25[,i]))))
{
dados_25[,i]<-as.character(dados_25[,i])
dados_25[-which(dados_25[,i] %in% unique(dados_75[,i])),i] <- names(table(dados_75[,i]))[which(table(dados_75[,i])==max(table(dados_75[,i])))]
dados_25[,i]<-factor(dados_25[,i])
print(i)
}
if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{
if(length(unique(as.character(dados_75[,i])))!=length(unique(as.character(PROD[,i]))) )
{
PROD[,i]<-as.character(PROD[,i])
PROD[-which(PROD[,i] %in% unique(dados_75[,i])),i] <- names(table(dados_75[,i]))[which(table(dados_75[,i])==max(table(dados_75[,i])))]
PROD[,i]<-factor(PROD[,i])
print(i)
}
}

}

}
contikjlm<-0
idxkj<-c()
for(ikjlm in 1:(ncol(dados_75)-1))
{
if(length(unique(dados_75[,ikjlm]))==1)
{
contikjlm<-contikjlm+1
idxkj[contikjlm]<-ikjlm 
}
if(length(unique(dados_25[,ikjlm]))==1)
{
contikjlm<-contikjlm+1
idxkj[contikjlm]<-ikjlm 
}
if(is.null(nrow(PROD))!=TRUE)
{
if(length(unique(PROD[,ikjlm]))==1)
{
contikjlm<-contikjlm+1
idxkj[contikjlm]<-ikjlm 
}
}
}


if(length(idxkj)>0)
{
dados_75<-dados_75[,-idxkj]
dados_25<-dados_25[,-idxkj]
if(is.null(nrow(PROD))!=TRUE)
{
PROD<-PROD[,-idxkj]
}
}


#Rede neural

dados_75_rn<-dados_75

dados_25_rn<-dados_25

if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{
dados_prod_rn<-PROD
}

for(i in 1:(ncol(dados_75_rn)-1))
{
 if(is.numeric(dados_75_rn[,i])==TRUE)
  {
   dados_25_rn[,i]<-(dados_25_rn[,i]-min(dados_75_rn[,i]))/(max(dados_75_rn[,i])-min(dados_75_rn[,i]))
   dados_25_rn[which(dados_25_rn[,i]>1),i]<-1
   dados_25_rn[which(dados_25_rn[,i]<0),i]<-0
   if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
   {
   dados_prod_rn[,i]<-(dados_prod_rn[,i]-min(dados_75_rn[,i]))/(max(dados_75_rn[,i])-min(dados_75_rn[,i]))
   dados_prod_rn[which(dados_prod_rn[,i]>1),i]<-1
  dados_prod_rn[which(dados_prod_rn[,i]<0),i]<-0
  }
   
   dados_75_rn[,i]<-(dados_75_rn[,i]-min(dados_75_rn[,i]))/(max(dados_75_rn[,i])-min(dados_75_rn[,i]))
  
}
print(i)

}


 ind_m<-which(dados_75_rn[,"alvo"]==mau)
 ind_b<-which(dados_75_rn[,"alvo"]==bom) 

 

p_b<-length(ind_b)/nrow(dados_75_rn)
p_m<-1-p_b

ind_67_b<-sample(ind_b,nrow(dados_75_rn)*p_b*0.67,replace=FALSE)
ind_67_m<-sample(ind_m,nrow(dados_75_rn)*p_m*0.67,replace=FALSE)
X<-dados_75_rn[c(ind_67_b,ind_67_m),]
X1<-dados_75_rn[-c(ind_67_b,ind_67_m),]


if(is.numeric(X[,"alvo"])=="TRUE")
 {
  ind_m<-which(X[,"alvo"]==max(X[,"alvo"]))
  ind_b<-which(X[,"alvo"]==min(X[,"alvo"])) 
}else{
  g1<-levels(X[,"alvo"])
  ind_m<-which(X[,"alvo"]==g1[2])
  ind_b<-which(X[,"alvo"]==g1[1])
}


if(balanced==TRUE)
{
if(length(ind_b)>length(ind_m))
{
 ind_rep<-sample(ind_m,length(ind_b),replace=TRUE)
X<-X[c(ind_b,ind_rep),]
}else{
ind_rep<-sample(ind_b,length(ind_m),replace=TRUE)
X<-X[c(ind_m,ind_rep),]
}
}



  ind_m<-which(X1[,"alvo"]==mau)
  ind_b<-which(X1[,"alvo"]==bom) 


if(balanced==TRUE)
{
if(length(ind_b)>length(ind_m))
{
 ind_rep<-sample(ind_m,length(ind_b),replace=TRUE)
X1<-X1[c(ind_b,ind_rep),]
}else{
ind_rep<-sample(ind_b,length(ind_m),replace=TRUE)
X1<-X1[c(ind_m,ind_rep),]
}
}

Y<-ifelse(X$alvo==mau,1,0)
Y1<-ifelse(X1$alvo==mau,1,0)



X<-model.matrix(alvo~-1+.,data=X)
X1<-model.matrix(alvo~-1+.,data=X1)
X3<-model.matrix(alvo~-1+.,data=dados_25_rn)
if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{
X4<-model.matrix(~-1+.,data=dados_prod_rn)
}

contgg<-0
indttg<-c()
for(at in 1:ncol(X))
{
if(length(which(colnames(X)[at]==colnames(X3)))>0)
{
 contgg<-contgg+1
 indttg[contgg]<-at
}

}
if(length(indttg)>0)
{
X<-X[,indttg]
X1<-X1[,indttg]
}

colnames(X)<-gsub("[ [:punct:]]", "",colnames(X))
colnames(X1)<-gsub("[ [:punct:]]", "",colnames(X1))
colnames(X3)<-gsub("[ [:punct:]]", "",colnames(X3))
if( is.null(nrow(PROD))!=TRUE)
{
colnames(X4)<-gsub("[ [:punct:]]", "",colnames(X4))
}
varxxt<-colnames(X) 
varxxt<-varxxt[which((varxxt %in% colnames(X1))) ]
varxxt<-varxxt[which((varxxt %in% colnames(X3))) ]
if( is.null(nrow(PROD))!=TRUE)
{
varxxt<-varxxt[which((varxxt %in% colnames(X4))) ]
}

X<-X[,varxxt]
X1<-X1[,varxxt]
X3<-X3[,varxxt]
if( is.null(nrow(PROD))!=TRUE)
{
X4<-X4[,varxxt]
}
fit<-MLP(X=X,X1=X1,Y=Y,Y1=Y1,epoch=3000,lr=0.001,hiddenlayer_neurons=5)
pred1<-predict_MLP(fit,newdata=X3[,which(colnames(X3) %in% colnames(X))])
if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{
pred1p<-predict_MLP(fit,newdata=X4[,which(colnames(X4) %in% colnames(X))])
}

########### GBM

dados_75_gbm<-dados_75



  ind_m<-which(dados_75_gbm[,"alvo"]==mau)
  ind_b<-which(dados_75_gbm[,"alvo"]==bom) 
  

if(length(which(dados_75$alvo==mau))<=500 & length(which(dados_75$alvo==mau))>70 & round(length(which(dados_75$alvo==bom))/length(which(dados_75$alvo==mau)))>1)
{
Ntot<-round(length(which(dados_75$alvo==bom))/length(which(dados_75$alvo==mau)))
ktt<-c()
g_modelo<-list()
bst_iter<-list()
indexb1<-list()
for(kkl in 1:Ntot)
{
 if(kkl==1)
 {
 
   indexb<-sample(ind_b,length(ind_m),replace=F)
 
 dados_75_gbm1<-dados_75_gbm[c(ind_m,indexb),]
 }else{
 indexb<-sample(ind_b[-which(ind_b %in% indexb)],length(ind_m),replace=F)
 dados_75_gbm1<-dados_75_gbm[c(ind_m,indexb),]
 }
 indexb1[[kkl]]<-indexb
 dados_75_gbm1$alvo<-ifelse(dados_75_gbm1$alvo==mau,1,0)

library(gbm)
gbm.model = gbm(alvo~., data=dados_75_gbm1, shrinkage=0.001, distribution = 'bernoulli', cv.folds=5, n.trees=5000, verbose=F,interaction.depth = 3,n.cores =1)
best.iter = gbm.perf(gbm.model, method="cv")
##write.table(summary(gbm.model),"D:/Users/clp124604/Desktop/bank/MODELO_IRREGULARIDADE/var_import.csv",sep=";",col.names=T,row.names=F,quote=F,dec=",")
pred3 = predict(gbm.model, dados_25, na.action = best.iter,type="response")
b2<-table(pred3,dados_25[,"alvo"])
freq_bom<-b2[,1]/sum(b2[,1])
freq_mau<-b2[,2]/sum(b2[,2])
freq_ac_bom<-cumsum( freq_bom )
freq_ac_mau<-cumsum( freq_mau )
diff<-round(abs(freq_ac_bom-freq_ac_mau),3)

ktt[kkl]<-max(diff)
g_modelo[[kkl]]<-gbm.model
bst_iter[[kkl]]<-best.iter
print(paste("GBM iter_",kkl,", com KS de ",ktt[kkl],sep=""))
}

indtx<-which(ktt==max(ktt))[1]

gbm.model<-g_modelo[[indtx]]
best.iter<-bst_iter[[indtx]]
pred3 = predict(gbm.model, dados_25, na.action = best.iter,type="response")


}
  
  


if(length(ind_b)>length(ind_m)  & (length(which(dados_75$alvo==mau))>500 |  length(which(dados_75$alvo==mau))<=70) | (round(length(which(dados_75$alvo==bom))/length(which(dados_75$alvo==mau)))==1 & length(which(dados_75$alvo==mau))<=500 & length(ind_b)>length(ind_m))) 
{
if(balanced==TRUE)
{
 ind_rep<-sample(ind_b,length(ind_m),replace=FALSE)
dados_75_gbm<-dados_75_gbm[c(ind_m,ind_rep),]
}
dados_75_gbm$alvo<-ifelse(dados_75_gbm$alvo==mau,1,0)

library(gbm)
gbm.model = gbm(alvo~., data=dados_75_gbm, shrinkage=0.001, distribution = 'bernoulli', cv.folds=5, n.trees=5000, verbose=F,interaction.depth = 3,n.cores =1)
best.iter = gbm.perf(gbm.model, method="cv")
##write.table(summary(gbm.model),"D:/Users/clp124604/Desktop/bank/MODELO_IRREGULARIDADE/var_import.csv",sep=";",col.names=T,row.names=F,quote=F,dec=",")
pred3 = predict(gbm.model, dados_25, na.action = best.iter,type="response")
}
if(length(ind_b)<length(ind_m) & (length(which(dados_75$alvo==mau))>500 |  length(which(dados_75$alvo==mau))<=70) | (round(length(which(dados_75$alvo==bom))/length(which(dados_75$alvo==mau)))==1 & length(which(dados_75$alvo==mau))<=500 & length(ind_b)<length(ind_m)))
{
if(balanced==TRUE)
{
ind_rep<-sample(ind_m,length(ind_b),replace=FALSE)
dados_75_gbm<-dados_75_gbm[c(ind_b,ind_rep),]
}
dados_75_gbm$alvo<-ifelse(dados_75_gbm$alvo==mau,1,0)

library(gbm)
gbm.model = gbm(alvo~., data=dados_75_gbm, shrinkage=0.001, distribution = 'bernoulli', cv.folds=5, n.trees=5000, verbose=F,interaction.depth = 3,n.cores =1)
best.iter = gbm.perf(gbm.model, method="cv")
##write.table(summary(gbm.model),"D:/Users/clp124604/Desktop/bank/MODELO_IRREGULARIDADE/var_import.csv",sep=";",col.names=T,row.names=F,quote=F,dec=",")
pred3 = predict(gbm.model, dados_25, na.action = best.iter,type="response")
}

if(length(ind_b)==length(ind_m))
{
library(gbm)
gbm.model = gbm(alvo~., data=dados_75_gbm, shrinkage=0.001, distribution = 'bernoulli', cv.folds=5, n.trees=5000, verbose=F,interaction.depth = 3,n.cores =1)
best.iter = gbm.perf(gbm.model, method="cv")
##write.table(summary(gbm.model),"D:/Users/clp124604/Desktop/bank/MODELO_IRREGULARIDADE/var_import.csv",sep=";",col.names=T,row.names=F,quote=F,dec=",")
pred3 = predict(gbm.model, dados_25, na.action = best.iter,type="response")

}
if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{
pred3p = predict(gbm.model, PROD, na.action = best.iter,type="response")
}




###########xgboost
library(unbalanced)
library(xgboost)
library(caret)
x<-dados_75[,-ncol(dados_75)]
x<-model.matrix(~.,data=x)
x<-x[,-1]
x<-data.matrix(x)

#x1<-dados_25[,-ncol(dados_25)]
#x1<-model.matrix(~.,data=x1)
#x1<-x1[,-1]
#x1<-data.matrix(x1)

y<-dados_75[,ncol(dados_75)]
y<-ifelse(y==mau,1,0)

dt<-ubENN(X=data.frame(x), Y=factor(y), k = 3, verbose = TRUE)

if(length(dt$id.rm)>0)
{
x<-x[-dt$id.rm,]
y<-y[-dt$id.rm]
}

# melhorar esse ponto
#bbced<-ubUnder(X=x, Y= y, perc = 50, method = "percPos")

#if(length(bbced$id.rm)>0)
#{
#x<-x[-bbced$id.rm,]
#y<-y[-bbced$id.rm]

#}

#y1<-dados_25[,ncol(dados_25)]
#y1<-ifelse(y1==mau,1,0)

#xgb.data.train <- xgb.DMatrix(x, label = y)
#xgb.data.test <- xgb.DMatrix(x1, label = y1)
cvFolds <- createFolds(y, k=5, list=TRUE, returnTrain=FALSE)

			  param <- list(booster="gbtree",
              objective="binary:logistic",
              eta = 0.02,
              gamma = 1,
              max_depth = 6,
              min_child_weight = 1,
              subsample = 0.8,
              colsample_bytree = 0.8
)
# Run model
dtrain <- xgb.DMatrix(data=x, label=y)
xgb_cv <- xgb.cv(data = dtrain,
                  params = param,
                  nrounds = 5000,
                  eval_metric = auc,
                  maximize = TRUE,
                  prediction = TRUE,
                  folds = cvFolds,
                  print_every_n = 25,
                  early_stopping_round = 30)
 nround <- xgb_cv$best_iteration


#nround=2000

dtrain <- xgb.DMatrix(x, label = data.matrix(y))

bst <- xgb.train(param, dtrain, nrounds = nround)



#bst = xgboost(param=param, data = x, label = data.matrix(y), nrounds=nround)

pred <- predict(bst, model.matrix(alvo~.,dados_25)[,-1])
if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{
predp <- predict(bst, model.matrix(~.,PROD)[,-1])
if(type=="R")
{
write.table(data.frame(chave=PROD_f[1:10,chave1],PROD[1:10,]),paste("C:/Resultados_bivariadas_",projeto,"/PROD_XGBOOST.csv",sep=""),sep=";",col.names=T,row.names=F,dec=",")
}
}


b2<-table(pred,dados_25[,"alvo"])
freq_bom<-b2[,1]/sum(b2[,1])
freq_mau<-b2[,2]/sum(b2[,2])
freq_ac_bom<-cumsum( freq_bom )
freq_ac_mau<-cumsum( freq_mau )
diff<-round(abs(freq_ac_bom-freq_ac_mau),3)

ks1<-max(diff)
AUC1<-getROC_AUC(pred,ifelse(dados_25[,"alvo"]==mau,1,2))

b2<-table(pred1,dados_25[,"alvo"])
freq_bom<-b2[,1]/sum(b2[,1])
freq_mau<-b2[,2]/sum(b2[,2])
freq_ac_bom<-cumsum( freq_bom )
freq_ac_mau<-cumsum( freq_mau )
diff<-round(abs(freq_ac_bom-freq_ac_mau),3)

ks2<-max(diff)
AUC2<-getROC_AUC(pred1,ifelse(dados_25[,"alvo"]==mau,1,2))

b2<-table(pred3,dados_25[,"alvo"])
freq_bom<-b2[,1]/sum(b2[,1])
freq_mau<-b2[,2]/sum(b2[,2])
freq_ac_bom<-cumsum( freq_bom )
freq_ac_mau<-cumsum( freq_mau )
diff<-round(abs(freq_ac_bom-freq_ac_mau),3)

ks3<-max(diff)
AUC3<-getROC_AUC(pred3,ifelse(dados_25[,"alvo"]==mau,1,2))

 

#c(AUC0$auc,AUC1$auc,AUC2$auc,AUC3$auc)
#if((length(which(dados_75$alvo==mau))/nrow(dados_75))>=0.1 & nrow(dados_75)==20000)
#{
parp<-max(c(ks2,ks3))
#}else{
#parp<-max(c(ks2,ks0,ks3))
#}
#parp<-max(c(ks3))

if(ks2==parp)
{
print("Melhor modelo foi a Rede Neural (MLP)")
var_import_rn<-apply(fit[[1]],1,mean)
var_import_rn<-data.frame(Variaveis=names(var_import_rn),Rank=unlist(abs(var_import_rn))/max(unlist(abs(var_import_rn))))
var_import_rn<-var_import_rn[order(var_import_rn[,2],decreasing=T),]
write.table(var_import_rn,paste("C:/Resultados_bivariadas_",projeto,"/Import_variaveis.csv",sep=""),sep=";",col.names=T,row.names=F,dec=",")

pred_val_esc<-pred1

#var1<-names(attr(fit0$terms,"dataClasses"))
var1<-names(dados_75)
var1<-var1[-which(var1=="alvo")]
#save(fit,file =paste("C:/Resultados_bivariadas/","modelo_randomForest.RData",sep=""))

if(ADE==TRUE)
{
if(type=="R")
{
SQL_PRE_PROC(data=data_model_f,var1=var_exclu_ade,file=paste("C:/Resultados_bivariadas_",projeto,"/data_pre_set_rn.r",sep=""),input.table="source_table",id=id)
}
if(type=="SQL SAP HANA")
{
SQL_PRE_PROC_sql(data=data_model_f,var1=var_exclu_ade,file=paste("C:/Resultados_bivariadas_",projeto,"/data_pre_set_rn.sql",sep=""),input.table="source_table",id=id)
}
}
if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{
pred_ade_esc<-pred1p

ade_prod<-ks.test(pred1,pred1p)$statistic
}
if(type=="R")
{
save(fit,file =paste("C:/Resultados_bivariadas_",projeto,"/modelo_MLP.RData",sep=""))
save(newgrupo1,file =paste("C:/Resultados_bivariadas_",projeto,"/newgrupo.RData",sep=""))
save(guarda_outros,file =paste("C:/Resultados_bivariadas_",projeto,"/guarda_outros.RData",sep=""))
}
if(type=="SQL SAP HANA")
{
SQL_DATA_SET_RN(data=dados_75,var1=var1,file=paste("C:/Resultados_bivariadas_",projeto,"/data_set_RN.SQL",sep=""),input.table="source_table",id=id,newgrupo1=newgrupo1,guarda_outros=guarda_outros)
SQL_MLP(fit,id=id,pred=pred1,file=paste("C:/Resultados_bivariadas_",projeto,"/sql_model_RN.SQL",sep=""))
}

}
if(ks1==parp) 
{
print("Melhor modelo foi o XGBoost")
#var1<-names(attr(fit0$terms,"dataClasses"))

Importance_matrix <- data.frame(xgb.importance(names(dados_75[,-ncol(dados_75)]), model = bst))
Importance_matrix[,4]<-Importance_matrix[,4]/max(Importance_matrix[,4])
Importance_matrix<-Importance_matrix[,c(1,4)]
Importance_matrix<-Importance_matrix[order(Importance_matrix[,2],decreasing=T),]
write.table(Importance_matrix,paste("C:/Resultados_bivariadas_",projeto,"/Import_variaveis.csv",sep=""),sep=";",col.names=T,row.names=F,dec=",")

var1<-names(dados_75)
var1<-var1[-which(var1=="alvo")]
pred_val_esc<-pred

#xgb.save(bst, paste("C:/Resultados_bivariadas/","modelo_xgboost.model",sep=""))
if(ADE==TRUE)
{
if(type=="R")
{
SQL_PRE_PROC(data=data_model_f,var1=var_exclu_ade,file=paste("C:/Resultados_bivariadas_",projeto,"/data_pre_set_xgb.r",sep=""),input.table="source_table",id=id)
}
if(type=="SQL SAP HANA")
{
SQL_PRE_PROC_sql(data=data_model_f,var1=var_exclu_ade,file=paste("C:/Resultados_bivariadas_",projeto,"/data_pre_set_xgb.sql",sep=""),input.table="source_table",id=id)
}
}

if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{
pred_ade_esc<-predp

ade_prod<-ks.test(pred,predp)$statistic
}
if(type=="R")
{
xgb.save(bst, paste("C:/Resultados_bivariadas_",projeto,"/modelo_xgboost.model",sep=""))
save(newgrupo1,file =paste("C:/Resultados_bivariadas_",projeto,"/newgrupo.RData",sep=""))
save(guarda_outros,file =paste("C:/Resultados_bivariadas_",projeto,"/guarda_outros.RData",sep=""))
}
if(type=="SQL SAP HANA")
{
SQL_DATA_SET_xgb(data=dados_75,var1=var1,file=paste("C:/Resultados_bivariadas_",projeto,"/data_set_xgb.SQL",sep=""),input.table="source_table",id=id,newgrupo1=newgrupo1,guarda_outros=guarda_outros)
sql.xgb(x=x,bst=bst,pred=pred,file=paste("C:/Resultados_bivariadas_",projeto,"/sql_model_xgb.SQL",sep=""),table="table",id=id)
}


}

if(ks3==parp)
{
print("Melhor modelo foi o GBM")

et45<-summary(gbm.model)
colnames(et45)<-c("Variáveis","Rank")
et45[,2]<-et45[,2]/max(et45[,2])
write.table(et45,paste("C:/Resultados_bivariadas_",projeto,"/Import_variaveis.csv",sep=""),sep=";",col.names=T,row.names=F,dec=",")

pred_val_esc<-pred3

var1<-names(dados_75_gbm)
var1<-var1[-which(var1=="alvo")]
#save(fit0,file =paste("C:/Resultados_bivariadas/","modelo_logistico.RData",sep=""))
if(ADE==TRUE)
{
if(type=="R")
{
SQL_PRE_PROC(data=data_model_f,var1=var_exclu_ade,file=paste("C:/Resultados_bivariadas_",projeto,"/data_pre_set_gbm.r",sep=""),input.table="source_table",id=id)
}
if(type=="SQL SAP HANA")
{
SQL_PRE_PROC_sql(data=data_model_f,var1=var_exclu_ade,file=paste("C:/Resultados_bivariadas_",projeto,"/data_pre_set_gbm.sql",sep=""),input.table="source_table",id=id)
}
}
if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{
pred_ade_esc<-pred3p

ade_prod<-ks.test(pred3,pred3p)$statistic
}
if(type=="R")
{
save(gbm.model,file =paste("C:/Resultados_bivariadas_",projeto,"/modelo_gbm.RData",sep=""))
save(newgrupo1,file =paste("C:/Resultados_bivariadas_",projeto,"/newgrupo.RData",sep=""))
save(guarda_outros,file =paste("C:/Resultados_bivariadas_",projeto,"/guarda_outros.RData",sep=""))
}
if(type=="SQL SAP HANA")
{
SQL_DATA_SET_LOG(data=dados_75_gbm,var1=var1,file=paste("C:/Resultados_bivariadas_",projeto,"/data_set_GBM.SQL",sep=""),input.table="source_table",id=id,newgrupo1=newgrupo1,guarda_outros=guarda_outros)
sql.export.gbm(gbm.model,pred=pred3,file=paste("C:/Resultados_bivariadas_",projeto,"/sql_model_GBM.SQL",sep=""), input.table="source_table", n.trees=NULL, id=id, trees.per.query=1, variant="generic")
}

}





#eb<-data.frame(variaveis=rownames(fit$importance)[order(fit$importance[,1],decreasing=T)],importance=fit$importance[order(fit$importance[,1],decreasing=T)]/max(fit$importance[order(fit$importance[,1],decreasing=T)]))
#write.table(eb,paste("C:/Resultados_bivariadas/","variaveis_selecionadas_modelo.txt"),col.names=T, row.names=FALSE,quote=F,sep="\t")

pred_val_esc1<-(pred_val_esc-min(pred_val_esc))/(max(pred_val_esc)-min(pred_val_esc))

if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{
pred_ade_esc1<-(pred_ade_esc-min(pred_val_esc))/(max(pred_val_esc)-min(pred_val_esc))

pred_ade_esc1<-ifelse(pred_ade_esc1>1,1,ifelse(pred_ade_esc1<0,0,pred_ade_esc1))

write.table(data.frame(PROD_f,Score_orig=pred_ade_esc,Score_norm=pred_ade_esc1),
paste("C:/Resultados_bivariadas_",projeto,"/dados_prod_scorado.csv",sep=""),sep=";",col.names=T,row.names=F,dec=",")
}

write.table(data.frame(data_teste_f,Score_orig=pred_val_esc,Score_norm=pred_val_esc1),
paste("C:/Resultados_bivariadas_",projeto,"/dados_teste_scorado.csv",sep=""),sep=";",col.names=T,row.names=F,dec=",")



pred_val_esc1<-round(pred_val_esc1,3)
          data_tot<-data.frame(pred_val_esc1,alvo=factor(dados_25[,"alvo"]))
         p_bom<-data_tot[data_tot[,2]==levels(data_tot[,"alvo"])[1],1]
         p_mau<-data_tot[data_tot[,2]==levels(data_tot[,"alvo"])[2],1]
         ecdf_bom <- calc_ecdf(p_bom)
	ecdf_mau <- calc_ecdf(p_mau)
	dif <- calc_ecdf_dif(ecdf_bom, ecdf_mau)
	maximo_y <- max(abs(dif[,'y']))
	maximo_x <- dif$x[dif$y == maximo_y]
        ############PLOT KS##################



		
		colnames(ecdf_bom)<-c("SCORE","KS2")
	cols <- c("red" = "red", "blue" = "blue", "green" = "green")
	ks_plot<-	ggplot(data=ecdf_bom,
       aes(x=SCORE, y=KS2,colour="blue")) +
       geom_line()+
	   geom_line(aes(x, y, colour="red"), ecdf_mau)+
	   geom_line(aes(x, y, colour="green"), abs(dif))+
      geom_hline(yintercept = maximo_y) + 
        annotate("text", maximo_x, maximo_y, vjust = -1, label = paste("KS2=",round(maximo_y,3),sep=""))+
		 theme(legend.position="top",legend.title=element_blank())+
		 scale_colour_manual(values=cols,
  breaks = c("blue", "green", "red"),
  labels = c("FAC_BOM", "KS2", "FAC_MAU")
)
ggsave(filename=paste("C:/Resultados_bivariadas_",projeto,"/ks_plot.jpg",sep=""), plot=ks_plot,width = 200, height = 100, units = "mm")		 
save(ks_plot,file =paste("C:/Resultados_bivariadas_",projeto,"/Plot_KS.RData",sep=""))   
###############################################


     
		  
		 
		   a<-seq(0,1,by=0.2)
		    cuts <- cut(pred_val_esc1, breaks=a)
	
       
         counts <- table(dados_25[,"alvo"],cuts)
         g100<-(apply(counts,2,sum)/sum(apply(counts,2,sum)))*100
		  counts1<-c()
         for(j in 1:ncol(counts))
         {
         counts1[j]<-counts[2,j]/sum(counts[,j])
         }
         counts1[is.na(counts1)]<-0
		 
		 df100<-data.frame(SCORE=names(g100),FREQUENCIA=round(unlist(g100),2),ACERTO=counts1)
		 rownames(df100)<-c(1:nrow(df100))
	 
		plot_err<- ggplot(df100, aes(x = SCORE)) +
  geom_col(aes( y = FREQUENCIA, fill="redfill")) +
  geom_text(aes(y = FREQUENCIA, label = paste(FREQUENCIA,"%",sep="")), fontface = "bold", vjust = 1.4, color = "black", size = 4) +
  geom_line(aes(y = round(ACERTO * 100,2), group = 1, color = 'blackline')) +
  geom_text(aes(y = round(ACERTO * 100,2), label = paste(round(ACERTO * 100,2),"%",sep="")),fontface = "bold", vjust = 1.4, color = "black", size = 3) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . / 1)) +
  scale_fill_manual('', labels = 'FREQUENCIA', values = "green") +
  scale_color_manual('', labels = 'ACERTO', values = 'black') +
   theme(legend.position="top",legend.title=element_blank())

ggsave(filename=paste("C:/Resultados_bivariadas_",projeto,"/ANALISE_TESTE_plot.jpg",sep=""), plot=plot_err,width = 200, height = 100, units = "mm")		 
save(plot_err,file =paste("C:/Resultados_bivariadas_",projeto,"/Plot_erro.RData",sep=""))	

if(ADE==TRUE || is.null(nrow(PROD))!=TRUE)
{

a110<-seq(0,1,by=0.1)



dt_plot<-rbind(data.frame(aggregate(rep(1,length(pred_ade_esc1)),list(cut(pred_ade_esc1,breaks=as.numeric(a110),include.lowest = TRUE,right=FALSE,dig.lab = 10)),length),REF="PRODUCAO"),
data.frame(aggregate(rep(1,length(pred_val_esc1)),list(cut(pred_val_esc1,breaks=as.numeric(a110),include.lowest = TRUE,right=FALSE,dig.lab = 10)),length),REF="TESTE"))
colnames(dt_plot)<-c("SCORE","FREQUENCIA","REF")

dt_plot[which(dt_plot$REF=="TESTE"),"FREQUENCIA"]<-100*round(dt_plot[which(dt_plot$REF=="TESTE"),"FREQUENCIA"]/sum(dt_plot[which(dt_plot$REF=="TESTE"),"FREQUENCIA"]),4)
dt_plot[which(dt_plot$REF=="PRODUCAO"),"FREQUENCIA"]<-100*round(dt_plot[which(dt_plot$REF=="PRODUCAO"),"FREQUENCIA"]/sum(dt_plot[which(dt_plot$REF=="PRODUCAO"),"FREQUENCIA"]),4)

myplot<-ggplot(data=dt_plot, aes(x=SCORE, y=FREQUENCIA, fill=REF)) +
    geom_bar(stat="identity", position=position_dodge(), colour="black")+
	 ggtitle(paste("KS1= ",100*round(ade_prod,4),sep=""))+
	 geom_text(aes(label=paste(FREQUENCIA,"%",sep="")),size = 4, position=position_dodge(width=0.9), vjust=-0.25)+
	 theme(legend.position="top",legend.title=element_blank())+
	 annotate("label", x = 5, y = max(dt_plot$FREQUENCIA)+10, label = paste("DATA REF=",Sys.Date(),sep=""))	 
	 
save(myplot,file =paste("C:/Resultados_bivariadas_",projeto,"/Plot_ader.RData",sep=""))	 
	 #


ggsave(filename=paste("C:/Resultados_bivariadas_",projeto,"/analise_aderencia.jpg",sep=""), plot=myplot,width = 300, height = 100, units = "mm")

generate_doc(parp,projeto,dados_75,dados_25,ade_prod,ks2,ks1,ks3,maximo_y,df100,PROD)
}else{

generate_doc(parp,projeto,dados_75,dados_25,ade_prod=NULL,ks2,ks1,ks3,maximo_y,df100,PROD=NULL)
}
}#end if


#return(list(myplot,plot_err,ks_plot))

} #end fuction

#f1<-bivariada(dados1,alvo,manual=FALSE,analise="TRUE",mau="yes",id="ZCGINSTALL")



