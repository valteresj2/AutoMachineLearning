##Install packages ##

#install.packages("shinydashboard")
#install.packages("shiny")
#install.packages("data.table")
#install.packages("DT")
#install.packages("shinycssloaders")
#install.packages("shinyjs")
#install.packages("ggplot2")
#install.packages("xlsx")
#install.packages("Hmisc")
#install.packages("gbm")
#install.packages("xgboost")
#install.packages("unbalanced")
#install.packages("caret")
#install.packages("stringr")
#install.packages("openxlsx")
#install.packages("ReporteRs")

## app.R ##
library(shinydashboard)
library(shiny)
library(data.table)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(ggplot2)
source("C:/Users/valter.e.junior/Desktop/AUTO_MODEL/bivariada_table.r")
source("C:/Users/valter.e.junior/Desktop/AUTO_MODEL/automated16_ade_elektro_r.r", encoding = "latin2//TRANSLIT")
source("C:/Users/valter.e.junior/Desktop/AUTO_MODEL/aderencia4_todas.r")
source("C:/Users/valter.e.junior/Desktop/AUTO_MODEL/query_chaid.r")
options(shiny.maxRequestSize=8000*1024^2) 

Logged = FALSE
my_username <- c("valteresj","CLP124051","ETP021273")
my_password <- c("z1z2","leleo@2021","Meunome")


ui <- dashboardPage(
  dashboardHeader(title = "AutoML"),
   dashboardSidebar(
    sidebarMenu(
	id = "tabs",
      menuItem("Up Load Data Set",tabName = "p2",icon = icon("database",lib = "font-awesome")), 
      menuItem("Select Features", tabName = "SF", icon = icon("columns",lib = "font-awesome")),
	  menuItem("Induction Rules", tabName = "IR", icon = icon("sitemap",lib = "font-awesome")),
	  menuItem("Automated CLassification", tabName = "AC", icon = icon("cogs",lib = "font-awesome")),
	  menuItem("Results", tabName = "R", icon = icon("line-chart")),
	  menuItem("ReadMe", tabName = "readme", icon=icon("mortar-board"))
    )
  ),
  dashboardBody(
  
   tabItems(
   
    tabItem(tabName = "readme",
            withMathJax(), 
            includeMarkdown("C:/Users/valter.e.junior/Desktop/AUTO_MODEL/readMe.Rmd")
    ),
   
   tabItem(tabName = "p2",
	  
	sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file ----
      fileInput("filetr", "Choose Train CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
	  fileInput("filets", "Choose Teste CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
	  fileInput("filepr1", "Choose Production CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),					 

      # Horizontal line ----
      tags$hr(),

      # Input: Checkbox if file has header ----
      checkboxInput("header1", "Header", TRUE),

      # Input: Select separator ----
      radioButtons("sep1", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
	 radioButtons("dec1", "Decimal",
                   choices = c(Comma = ",",
                               Dot = "."),
                   selected = "."),		   

      # Input: Select quotes ----
      radioButtons("quote1", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = ""),

      # Horizontal line ----
      #tags$hr(),

      # Input: Select number of rows to display ----
      radioButtons("disp1", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
	  radioButtons("key1", "Key",
                   choices = c(Yes = "yes",
                               No = "no"),
                   selected = "yes")			   

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Data file ----
      tableOutput("contents1"),
	   tableOutput("contents2"),
	   tableOutput("contents3")

    )

  )),
      
	  
	  tabItem(tabName = "SF",
	  selectInput("vars","Select a variable Numeric:", choices=NULL,multiple=TRUE  ),
	  selectInput("vars1","Select a variable Categoric:", choices=htmlOutput("varselect"),multiple=TRUE  ),
	  selectInput("vars2","Select a variable Target:", choices=htmlOutput("varselect"),multiple=FALSE  ),
	  selectInput("vars3","Select a variable Key:", choices=htmlOutput("varselect"),multiple=FALSE  ),
	  column(3,downloadButton("downloadData1", label = "Download List Features")),
	  column(3,fileInput("filevar", "Load Old List Features",accept=c('.RData'))),
	  column(3,actionButton("go2", "Reset List Features")),
	   selectInput("vars0","Analyse Variables:", choices=NULL,multiple=F  ),
	 # numericInput("obs", "Percentage of Training:", 0.75, min = 0.5, max = 1),
	  withSpinner(DT::dataTableOutput("mytable0"))
	  
	  
	  ),
	  tabItem(tabName = "AC",
	  numericInput("obs", "Percentage of Training:", 0.75, min = 0.5, max = 1),
	  selectInput("vars4","Select Class Target:", choices=htmlOutput("varselect"),multiple=FALSE ),
	  column(3,selectInput("vars5","Apply Transformation Adherence:", choices=c("TRUE","FALSE"),multiple=FALSE ),
	  wellPanel(checkboxInput(inputId = "checkbox",
                 label = "Multiply Model?",
                 value = FALSE),uiOutput("conditionalInput"),uiOutput("conditionalInput0")
                 )),
	  selectInput("vars6","Save Results in Format:", choices=c("R","SQL SAP HANA"),multiple=FALSE ),
	  textInput("caption", "Name Project:", NULL),
	  actionButton("go", "Go Train"),
	 withSpinner(tableOutput("contents4"))
	
	  ),
	  tabItem(tabName = "IR",
	  numericInput("Min", "Minimum quantity per leaf:", 30, min = 1, max = 100000),
	  numericInput("Max", "Maximum division of the tree:", 4, min = 1, max = 100000),
	  numericInput("lift", "Add value lift of the target:", 1.1, min = 1, max = 100000),
	  selectInput("vars10","Select Class Target:", choices=htmlOutput("varselect"),multiple=FALSE ),
	   column(3, wellPanel(checkboxInput(inputId = "checkbox1",
                 label = "Multiply Rules?",
                 value = FALSE),uiOutput("conditionalInput2"),uiOutput("conditionalInput02")
                 )),
	  selectInput("vars7","Save Results in Format:", choices=c("R","SQL SAP HANA"),multiple=FALSE ),
	  textInput("caption1", "Name Project:", NULL),
	  actionButton("go1", "Go Rules"),
	 withSpinner(tableOutput("contents7"))
	  ),
	   tabItem(tabName = "R",
	    column(3,downloadButton("downloadData", label = "Download Model"),
		downloadButton("downloadDataR", label = "Download Rules")),
		uiOutput("conditionalInput1"),
	   column(width = 8,
                     box(  width = NULL, plotOutput("plot",height="500px"), collapsible = TRUE,
                           title = "KS", status = "primary", solidHeader = TRUE),
					 box(  width = NULL, plotOutput("EFIC",height="500px"), collapsible = TRUE,
                           title = "Massa X Acerto", status = "primary", solidHeader = TRUE),
					box(  width = NULL, plotOutput("ADER",height="500px"), collapsible = TRUE,
                           title = "Aderência Teste X Produção", status = "primary", solidHeader = TRUE)	   
              )
		)	  
  
  )
)

)

server <- function(session,input, output) {

values <- reactiveValues(authenticated = FALSE)

# Return the UI for a modal dialog with data selection input. If 'failed' 
# is TRUE, then display a message that the previous value was invalid.
dataModal <- function(failed = FALSE) {
  modalDialog(
    textInput("username", "Username:"),
    passwordInput("password", "Password:"),
    footer = tagList(
      # modalButton("Cancel"),
      actionButton("ok", "OK")
    )
  )
}

# Show modal when button is clicked.  
# This `observe` is suspended only whith right user credential

obs1 <- observe({
  showModal(dataModal())
})

# When OK button is pressed, attempt to authenticate. If successful,
# remove the modal. 

obs2 <- observe({
  req(input$ok)
  isolate({
    Username <- input$username
    Password <- input$password
  })
  Id.username <- which(my_username %in% Username)
  Id.password <- which(my_password %in% Password)
  if (length(Id.username) > 0 & length(Id.password) > 0) {
    if (Id.username == Id.password) {
      Logged <<- TRUE
        values$authenticated <- TRUE
        obs1$suspend()
        removeModal()

    } else {
      values$authenticated <- FALSE
    }     
  }
  })



#### fim da senha


	
	 dfp<-reactive({
	
	if(is.null(input$filepr1)!=TRUE)
	 {
	  if(input$key1=="yes")
	 {
	  df <- fread(input$filepr1$datapath,
             header = input$header1,
             sep = input$sep1,
             quote = input$quote1,dec=input$dec1)
    df<-data.frame(df)
	 
	 }else{
	df <- fread(input$filepr1$datapath,
             header = input$header1,
             sep = input$sep1,
             quote = input$quote1,dec=input$dec1)
    df<-data.frame(KEY=1:nrow(df),df)
	
	
	}
	 }
	 
	 
	 })
     
	 dfr<-reactive({
	if(is.null(input$filetr)!=TRUE)
	 {
	 if(input$key1=="yes")
	 {
    df <- fread(input$filetr$datapath,
             header = input$header1,
             sep = input$sep1,
             quote = input$quote1,dec=input$dec1)
    df<-data.frame(df)
	}else{
	df <- fread(input$filetr$datapath,
             header = input$header1,
             sep = input$sep1,
             quote = input$quote1,dec=input$dec1)
    df<-data.frame(KEY=1:nrow(df),df)
	
	
	}
	
	
	 }
	 })
	 
	 
	
	
	 	
	 
	 dfts<-reactive({
	 
	 if( is.null(input$filets)!=TRUE)
	 {
	 if(input$key1=="yes")
	 {
     dft <- fread(input$filets$datapath,
             header = input$header1,
             sep = input$sep1,
             quote = input$quote1,dec=input$dec1)
    dft<-data.frame(dft)

	}else{
	
		dft <- fread(input$filets$datapath,
             header = input$header1,
             sep = input$sep1,
             quote = input$quote1,dec=input$dec1)
    dft<-data.frame(KEY=1:nrow(dft),dft)
	
	}
	 
	 
	 }
	
})

 
  
   output$contents1 <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
   if(is.null(input$filetr)!=TRUE)
   {
   df<-dfr()
	
    if(input$disp1 == "head") {
      return(head(df))
    }
    else {
      return(head(df))
    }
   }
  })
  
     output$contents2 <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
   if(is.null(input$filets)!=TRUE)
   {
   df<-dfts()
	
    if(input$disp1 == "head") {
      return(head(df))
    }
    else {
      return(head(df))
    }
   }
  })
  
  output$contents3 <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
   if(is.null(input$filepr1)!=TRUE)
   {
   df<-dfp()
	
    if(input$disp1 == "head") {
      return(head(df))
    }
    else {
      return(head(df))
    }
   }
  })
  
 output$mytable0<- DT::renderDataTable({
 df<-dfr()
 if(is.null(input$vars1)!=TRUE)
	 {
	 df[input$vars1]<-lapply(df[input$vars1], as.factor)
	 
	 }
	 if(is.null(input$vars)!=TRUE)
	 {
	 df[input$vars]<-lapply(df[input$vars], as.numeric)
	 
	 }
 
 
 req(input$vars2)
 
  if(length(unique(df[,input$vars2]))==2)
  {
 table1<-bivariada_table(df,alvo=df[,input$vars2],var_select=input$vars0)
  }else{
  table1<-data.frame(MESSAGE="Target variable must be binary") 
  }
 

 
 DT::datatable(table1,
filter = 'bottom',
extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'Scroller'),
 options = list(
          dom = 'Bfrtip',
          searching = T,
          pageLength = 25,
          searchHighlight = TRUE,
          colReorder = TRUE,
          fixedHeader = FALSE,
          filter = 'top',
          buttons = c('copy', 'csv', 'excel', 'print'),
          paging    = TRUE,
          deferRender = TRUE,
          scroller = TRUE,
          scrollX = TRUE,
          scrollY = 550
        ))	
 })
  

 #names(dfr())[which(sapply(dfr(),class) %in% c("integer","numeric","logical"))]
# names(dfr())[which(sapply(dfr(),class) %in% c("factor","character"))]
  observe({
    
	 if(length(listvar())>2)
	  {
	   updateSelectInput(session, inputId="vars",
                      choices=names(dfr()), selected=listvar()$Numeric)
	updateSelectInput(session, inputId="vars1",
                      choices=names(dfr()), selected=listvar()$Categoric)
	updateSelectInput(session, inputId="vars2",
                      choices=names(dfr()), selected=listvar()$Target)
	updateSelectInput(session, inputId="vars3",
                      choices=names(dfr()), selected=listvar()$Key)
	  
	  
	  }else{
   
    updateSelectInput(session, inputId="vars",
                      choices=names(dfr()), selected=NULL)
	updateSelectInput(session, inputId="vars1",
                      choices=names(dfr()), selected=NULL)
	updateSelectInput(session, inputId="vars2",
                      choices=names(dfr()), selected=NULL)
	updateSelectInput(session, inputId="vars3",
                      choices=names(dfr()), selected=NULL)
	 updateSelectInput(session, inputId="vars0",
                      choices=names(dfr()), selected=NULL)
	
     }	
					  				  
  })
  
  
  
  observe({
  df<-dfr()
  if(length(which(names(df) %in% input$vars2))>0 )
	{
	  if(length(unique(df[,input$vars2]))==2)
	   {
	   updateSelectInput(session, inputId="vars4",
                      choices=as.character(unique(df[,input$vars2])), selected=as.character(unique(df[,input$vars2]))[1])	
		 }else{
      updateSelectInput(session, inputId="vars4",
                      choices=c("0","1"), selected="1")	

    }
	
    }
  
   
   })
   
   
    observe({
  df<-dfr()
  if(length(which(names(df) %in% input$vars2))>0 )
	{
	  if(length(unique(df[,input$vars2]))==2)
	   {
	   updateSelectInput(session, inputId="vars10",
                      choices=as.character(unique(df[,input$vars2])), selected=as.character(unique(df[,input$vars2]))[1])	
		 }else{
      updateSelectInput(session, inputId="vars10",
                      choices=c("0","1"), selected="1")	

    }
	
    }
  
   
   })
  	
randomVals <-  eventReactive(input$go, {
   if(is.null(input$filetr)!=TRUE)
   {
   train<-dfr()
     if(length(which(names(train) %in% input$vars2))>0 & length(which(names(train) %in% input$vars3))>0 & length(which(names(train) %in% input$vars))>0 & length(which(names(train) %in% input$vars1))>0)
	{
	if(input$checkbox)
	{
	part_train<-unlist(train[,input$typeInput])
	}
   train<-train[,which(names(train) %in% c(input$vars3,input$vars,input$vars1,input$vars2))]
   }
   }else{
   train<-NA
   }
     if(is.null(input$filets)!=TRUE)
   {
   test<-dfts()
     if(length(which(names(test) %in% input$vars2))>0 & length(which(names(test) %in% input$vars3))>0 & length(which(names(test) %in% input$vars))>0 & length(which(names(test) %in% input$vars1))>0)
	{
	if(input$checkbox)
	{
	part_test<-unlist(test[,input$typeInput])
	}
    test<-test[,which(names(test) %in% c(input$vars3,input$vars,input$vars1,input$vars2))]
	 }
   }else{
   test<-NA
   }
    if(is.null(input$filepr1)!=TRUE)
   {
   PROD<-dfp()
    if(length(which(names(PROD) %in% input$vars2))>0 & length(which(names(PROD) %in% input$vars3))>0 & length(which(names(PROD) %in% input$vars))>0 & length(which(names(PROD) %in% input$vars1))>0)
	{
	if(input$checkbox)
	{
	part_prod<-unlist(PROD[,input$typeInput])
	}
   PROD<-PROD[,which(names(PROD) %in% c(input$vars3,input$vars,input$vars1,input$vars2))]
   }
   }else{
   PROD<-NA
   }
   
     if(is.null(nrow(train))!=TRUE & is.null(nrow(test))!=TRUE & is.null(nrow(PROD))!=TRUE)
   {
     if(input$checkbox)
	{
	 if(length(which(input$typeInput0 %in% "ALL"))>0)
	 {
	 qtd_loop<-length(unique(part_train))
	 nv<-unique(part_train)
	 }else{
	 qtd_loop<-length(unique(part_train[which(as.character(part_train) %in% input$typeInput0)]))
	 nv<-unique(part_train[which(as.character(part_train) %in% input$typeInput0)])
	 }
	
	}else{
	qtd_loop<-1
	
	}

	 for(k in 1:qtd_loop)
	 {
	 train2<-train
	 test2<-test
	 PROD2<-PROD
	  if(qtd_loop>=1 & input$checkbox==TRUE)
	  {
	  indextrain<-which(part_train==nv[k])
	  indextest<-which(part_test==nv[k])
	  indexPROD<-which(part_prod==nv[k])
	  nome_projeto<-paste(input$caption,"_",nv[k],sep="")
	  }
	  if(qtd_loop==1 & input$checkbox!=TRUE)
	  {
	  indextrain<-1:nrow(train)
	  indextest<-1:nrow(test)
	  indexPROD<-1:nrow(PROD)
	  nome_projeto<-input$caption
	  }
	  
      if(is.null(input$vars1)!=TRUE)
	 {
	 train2[input$vars1]<-lapply(train2[input$vars1], as.factor)
	 
	 }
	 if(is.null(input$vars)!=TRUE)
	 {
	 train2[input$vars]<-lapply(train2[input$vars], as.numeric)
	 
	 }
	 
	 
	    if(is.null(input$vars1)!=TRUE)
	 {
	 test2[input$vars1]<-lapply(test2[input$vars1], as.factor)
	 
	 }
	 if(is.null(input$vars)!=TRUE)
	 {
	 test2[input$vars]<-lapply(test2[input$vars], as.numeric)
	 
	 }
	 
	 
	     if(is.null(input$vars1)!=TRUE)
	 {
	 PROD2[input$vars1]<-lapply(PROD2[input$vars1], as.factor)
	 
	 }
	 if(is.null(input$vars)!=TRUE)
	 {
	 PROD2[input$vars]<-lapply(PROD2[input$vars], as.numeric)
	 
	 }
	 PROD2<-PROD2[indexPROD,]
     train2<-train2[indextrain,]
	 test2<-test2[indextest,]
     index<-c()
     cont<-0
      for(i in 1:ncol(train2))
       {
   if(length(unique(train2[,i]))==1)
     {
       cont<-cont+1
           index[cont]<-i

        }
       if(length(unique(train2[,i]))==2 & length(which(is.na(train2[,i])==TRUE))>0)
        {
        cont<-cont+1
        index[cont]<-i

         }
        }

      if(length(index)>0)
    {
      train2<-train2[,-index]
	  test2<-test2[,-index]
       PROD2<-PROD2[,-index]
      }
   
   cont100=0
index10<-c()
for(i in 1:ncol(train2))
{
if(length(unique(train2[,i]))==1)
{
cont100<-cont100+1
index[cont100]<-i
}

if(length(unique(test2[,i]))==1)
{
cont100<-cont100+1
index[cont100]<-i
}

if(length(unique(PROD2[,i]))==1)
{
cont100<-cont100+1
index[cont100]<-i
}


}



if(length(index)>0)
{
index<-index[which(!(names(train2)[index] %in% c(input$vars2,input$vars3)))]
train2<-train2[,-index]
test2<-test2[,-index]
PROD2<-PROD2[,-index]
}
train2[,input$vars2]<-ifelse(train2[,input$vars2]==input$vars4,1,0)
test2[,input$vars2]<-ifelse(test2[,input$vars2]==input$vars4,1,0)
 
# 
f1<-bivariada(data_model=train2,data_teste=test2,dados1=NA,chave1=input$vars3,alvo1=input$vars2,manual=FALSE,analise="TRUE",mau=1,bom=0,id="ZCGINSTALL",projeto=nome_projeto,PROD=PROD2,ADE=input$vars5,type=input$vars6)
      
}
 }  
 
 
  if(is.null(nrow(train))!=TRUE & is.null(nrow(test))==TRUE & is.null(nrow(PROD))!=TRUE)
   {
    
	if(input$checkbox)
	{
	 if(length(which(input$typeInput0 %in% "ALL"))>0)
	 {
	 qtd_loop<-length(unique(part_train))
	 nv<-unique(part_train)
	 }else{
	 qtd_loop<-length(unique(part_train[which(as.character(part_train) %in% input$typeInput0)]))
	 nv<-unique(part_train[which(as.character(part_train) %in% input$typeInput0)])
	 }
	
	}else{
	qtd_loop<-1
	
	}
	
	 for(k in 1:qtd_loop)
	 {
	 train2<-train
	 PROD2<-PROD
	  if(qtd_loop>=1 & input$checkbox==TRUE)
	  {
	  indextrain<-which(part_train==nv[k])
	  indexPROD<-which(part_prod==nv[k])
	  nome_projeto<-paste(input$caption,"_",nv[k],sep="")
	  }
	   if(qtd_loop==1 & input$checkbox!=TRUE)
	  {
	  indextrain<-1:nrow(train2)
	  indexPROD<-1:nrow(PROD2)
	  nome_projeto<-input$caption
	  }
   
     if(is.null(input$vars1)!=TRUE)
	 {
	 train2[input$vars1]<-lapply(train2[input$vars1], as.factor)
	 
	 }
	 if(is.null(input$vars)!=TRUE)
	 {
	 train2[input$vars]<-lapply(train2[input$vars], as.numeric)
	 
	 }
	 
	 
	  if(is.null(input$vars1)!=TRUE)
	 {
	 PROD2[input$vars1]<-lapply(PROD2[input$vars1], as.factor)
	 
	 }
	 if(is.null(input$vars)!=TRUE)
	 {
	 PROD2[input$vars]<-lapply(PROD2[input$vars], as.numeric)
	 
	 }
	 

   train1<-train2[indextrain,] 
   PROD2<-PROD2[indexPROD,]
    index<-c()
     cont<-0
      for(i in 1:ncol(train2))
       {
   if(length(unique(train2[,i]))==1)
     {
       cont<-cont+1
           index[cont]<-i

        }
       if(length(unique(train2[,i]))==2 & length(which(is.na(train2[,i])==TRUE))>0)
        {
        cont<-cont+1
        index[cont]<-i

         }
        }

      if(length(index)>0)
    {
      train2<-train2[,-index]
	 
       PROD2<-PROD2[,-index]
      }
   
   cont100=0
index10<-c()
for(i in 1:ncol(train2))
{
if(length(unique(train2[,i]))==1)
{
cont100<-cont100+1
index[cont100]<-i
}


if(length(unique(PROD2[,i]))==1)
{
cont100<-cont100+1
index[cont100]<-i
}


}



if(length(index)>0)
{
index<-index[which(!(names(train)[index] %in% c(input$vars2,input$vars3)))]
train2<-train2[,-index]

PROD2<-PROD2[,-index]
}
 train1<-train2
 train1[,input$vars2]<-ifelse(train1[,input$vars2]==input$vars4,1,0)
 ind_b<-which(train1[,input$vars2]==1)
 ind_m<-which(train1[,input$vars2]==0)
 pb<-length(ind_b)/nrow(train1)
 pm<-1-pb
 ind_bs<-sample(ind_b,nrow(train1)*pb*input$obs,replace=F)
 ind_ms<-sample(ind_m,nrow(train1)*pm*input$obs,replace=F)
 
 test<-train1[-c(ind_bs,ind_ms),]
 train1<-train1[c(ind_bs,ind_ms),]
 
 f1<-bivariada(data_model=train1,data_teste=test,dados1=NA,chave1=input$vars3,alvo1=input$vars2,manual=FALSE,analise="TRUE",mau=1,bom=0,id="ZCGINSTALL",projeto=nome_projeto,PROD=PROD2,ADE=input$vars5,type=input$vars6)
  rm(train1)  
 }
   }
   
    if(is.null(nrow(train))!=TRUE & is.null(nrow(test))==TRUE & is.null(nrow(PROD))==TRUE)
   {
   
   if(input$checkbox)
	{
	 if(length(which(input$typeInput0 %in% "ALL"))>0)
	 {
	 qtd_loop<-length(unique(part_train))
	 nv<-unique(part_train)
	 }else{
	 qtd_loop<-length(unique(part_train[which(as.character(part_train) %in% input$typeInput0)]))
	 nv<-unique(part_train[which(as.character(part_train) %in% input$typeInput0)])
	 }
	
	}else{
	qtd_loop<-1
	
	}
	
	 for(k in 1:qtd_loop)
	 {
	 train2<-train
	  if(qtd_loop>=1 & input$checkbox==TRUE)
	  {
	  indextrain<-which(part_train==nv[k])
	  nome_projeto<-paste(input$caption,"_",nv[k],sep="")
	  }
	  if(qtd_loop==1 & input$checkbox!=TRUE)
	  {
	  indextrain<-1:nrow(train2)
	  nome_projeto<-input$caption
	  }
   
   if(is.null(input$vars1)!=TRUE)
	 {
	 train2[input$vars1]<-lapply(train2[input$vars1], as.factor)
	 
	 }
	 if(is.null(input$vars)!=TRUE)
	 {
	 train2[input$vars]<-lapply(train2[input$vars], as.numeric)
	 
	 }
   
 train2<-train2[indextrain,]  
   index<-c()
     cont<-0
      for(i in 1:ncol(train2))
       {
   if(length(unique(train2[,i]))==1)
     {
       cont<-cont+1
           index[cont]<-i

        }
       if(length(unique(train2[,i]))==2 & length(which(is.na(train2[,i])==TRUE))>0)
        {
        cont<-cont+1
        index[cont]<-i

         }
        }

      if(length(index)>0)
    {
      train2<-train2[,-index]
	 
    
      }
   
   cont100=0
index10<-c()
for(i in 1:ncol(train2))
{
if(length(unique(train2[,i]))==1)
{
cont100<-cont100+1
index[cont100]<-i
}





}



if(length(index)>0)
{
index<-index[which(!(names(train2)[index] %in% c(input$vars2,input$vars3)))]
train2<-train2[,-index]

}
 train1<-train2
 train1[,input$vars2]<-ifelse(train1[,input$vars2]==input$vars4,1,0)
 ind_b<-which(train1[,input$vars2]==1)
 ind_m<-which(train1[,input$vars2]==0)
 pb<-length(ind_b)/nrow(train1)
 pm<-1-pb
 ind_bs<-sample(ind_b,nrow(train1)*pb*input$obs,replace=F)
 ind_ms<-sample(ind_m,nrow(train1)*pm*input$obs,replace=F)
 
 test<-train1[-c(ind_bs,ind_ms),]
 train1<-train1[c(ind_bs,ind_ms),]
 
 f1<-bivariada(data_model=train1,data_teste=test,dados1=NA,chave1=input$vars3,alvo1=input$vars2,manual=FALSE,analise="TRUE",mau=1,bom=0,id="ZCGINSTALL",projeto=nome_projeto,PROD=NA,ADE=FALSE,type=input$vars6)
    rm(train1)
   
}   
   
   }
   
 head(train[sample(1:nrow(train),200,replace=F),])
 
  })
  
  
 output$contents4 <- renderTable({
 
 if(is.null(randomVals())!=TRUE)
 {
return(data.frame(id="FINISH MODEL!"))
}
})

 

output$plot <- renderPlot({

if(is.null(nrow(randomVals()))!=TRUE)
{
  if(input$checkbox)
	{
 load(paste("C:/Resultados_bivariadas_",input$caption,"_",input$typeInput1,"/Plot_KS.RData",sep=""))
 }else{

load(paste("C:/Resultados_bivariadas_",input$caption,"/Plot_KS.RData",sep=""))
}
ks_plot
}
})


output$EFIC <- renderPlot({

if(is.null(nrow(randomVals()))!=TRUE)
{
if(input$checkbox)
	{
load(paste("C:/Resultados_bivariadas_",input$caption,"_",input$typeInput1,"/Plot_erro.RData",sep=""))	
	}else{
load(paste("C:/Resultados_bivariadas_",input$caption,"/Plot_erro.RData",sep=""))
}
plot_err
}
})


output$ADER <- renderPlot({

if(is.null(nrow(randomVals()))!=TRUE)
{
if(input$checkbox)
	{
load(paste("C:/Resultados_bivariadas_",input$caption,"_",input$typeInput1,"/Plot_ader.RData",sep=""))	
	}else{
load(paste("C:/Resultados_bivariadas_",input$caption,"/Plot_ader.RData",sep=""))
}
myplot
}
})

output$downloadData <- downloadHandler(
     
    filename = 'resultados.zip',
    content = function(fname) {
      
      
	   if(input$checkbox)
	{
	 cont=0
	 for(k in unique(dfr()[,input$typeInput]))
	 {
	 cont<-cont+1
	 tmpdir <- paste("C:/Resultados_bivariadas_",input$caption,"_",k,sep="")
      fs <- dir(tmpdir, full.names = TRUE)
	  if(cont==1)
	  {
	  fs1<-fs
	  }else{
	  fs1<-c(fs1,fs)
	  }
	 
	 }
	zip(zipfile=fname, files=fs1)
	}else{
      tmpdir <- paste("C:/Resultados_bivariadas_",input$caption,sep="")
      fs <- dir(tmpdir, full.names = TRUE)
 

      zip(zipfile=fname, files=fs)
	  }
    },
    contentType = "application/zip"
	
  )
  
  
  output$downloadDataR <- downloadHandler(
     
    filename = 'resultados_rules.zip',
    content = function(fname) {
      
      
	   if(input$checkbox1)
	{
	 cont=0
	 for(k in unique(dfr()[,input$typeInput]))
	 {
	 cont<-cont+1
	 tmpdir <- paste("C:/Resultados_regras_",input$caption,"_",k,sep="")
      fs <- dir(tmpdir, full.names = TRUE)
	  if(cont==1)
	  {
	  fs1<-fs
	  }else{
	  fs1<-c(fs1,fs)
	  }
	 
	 }
	zip(zipfile=fname, files=fs1)
	}else{
      tmpdir <- paste("C:/Resultados_regras_",input$caption,sep="")
      fs <- dir(tmpdir, full.names = TRUE)
 

      zip(zipfile=fname, files=fs)
	  }
    },
    contentType = "application/zip"
	
  )
  
  
  
  
  
  
   output$downloadData1 <- downloadHandler(
    filename <- function(){
      paste("Features.RData")
    },

    content = function(file) {
	
	rf2<-list()
	rf2[[1]]<-input$vars
	rf2[[2]]<-input$vars1
	rf2[[3]]<-input$vars2
	rf2[[4]]<-input$vars3
	names(rf2)<-c("Numeric","Categoric","Target","Key")
      save(rf2, file = file)
    }
  )
  

  values <- reactiveValues(
    upload_state = NULL
  )

   observeEvent(input$filevar, {
    values$upload_state <- 'uploaded'
  })
 
  
 observeEvent(input$go2, {

 values$upload_state <- 'reset'
  
  
  })


      listvar<-reactive({
	  
	   if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
	  load(input$filevar$datapath)
	  return(rf2) 
    } else if (values$upload_state == 'reset') {
      return(NULL)
    }
	  
	
  
  })
  
  output$conditionalInput <- renderUI({
    if(input$checkbox){
      selectInput("typeInput", "Select Feature aplication for multiply models.",
                  choices = names(dfr()),
                  selected = NULL)
				  
	# selectInput("typeInput0",label ="Select the Models!",multiple = TRUE,choices=c("ALL",as.character(unique(dfr()[,input$typeInput] ))),selected = "ALL")
    }
  })
  
  output$conditionalInput0 <- renderUI({
    if(input$checkbox){   				  
	 selectInput("typeInput0",label ="Select the Models!",multiple = TRUE,choices=c("ALL",as.character(unique(dfr()[,input$typeInput] ))),selected = "ALL")
    }
  })
  
   output$conditionalInput1 <- renderUI({
    if(input$checkbox){
	  if(length(which(input$typeInput0 %in% "ALL"))>0)
	  {
      selectInput("typeInput1", "Select of models.",
                  choices = unique(dfr()[,input$typeInput]),
                  selected = NULL)
				  }else{
		selectInput("typeInput1", "Select of models.",
                  choices = input$typeInput0,
                  selected = NULL)		  
				  
				  }
    }
  })
  
   output$conditionalInput2 <- renderUI({
    if(input$checkbox1){
      selectInput("typeInput2", "Select Feature aplication for multiply rules.",
                  choices = names(dfr()),
                  selected = NULL)
				  
	# selectInput("typeInput0",label ="Select the Models!",multiple = TRUE,choices=c("ALL",as.character(unique(dfr()[,input$typeInput] ))),selected = "ALL")
    }
  })
  
  output$conditionalInput02 <- renderUI({
    if(input$checkbox1){   				  
	 selectInput("typeInput02",label ="Select the Rules!",multiple = TRUE,choices=c("ALL",as.character(unique(dfr()[,input$typeInput2] ))),selected = "ALL")
    }
  })
  
 
 
 randomVals1 <-  eventReactive(input$go1, {
  if(is.null(input$filetr)!=TRUE)
   {
   train<-dfr()
     if(length(which(names(train) %in% input$vars2))>0 & length(which(names(train) %in% input$vars3))>0 & length(which(names(train) %in% input$vars))>0 & length(which(names(train) %in% input$vars1))>0)
	{
	if(input$checkbox1)
	{
	part_train<-unlist(train[,input$typeInput2])
	}
   train<-train[,which(names(train) %in% c(input$vars3,input$vars,input$vars1,input$vars2))]
   }
   }else{
   train<-NA
   }
     if(is.null(input$filets)!=TRUE)
   {
   test<-dfts()
     if(length(which(names(test) %in% input$vars2))>0 & length(which(names(test) %in% input$vars3))>0 & length(which(names(test) %in% input$vars))>0 & length(which(names(test) %in% input$vars1))>0)
	{
	if(input$checkbox1)
	{
	part_test<-unlist(test[,input$typeInput2])
	}
    test<-test[,which(names(test) %in% c(input$vars3,input$vars,input$vars1,input$vars2))]
	 }
   }else{
   test<-NA
   }
   
   
 if(is.null(nrow(train))!=TRUE & is.null(nrow(test))!=TRUE )
   {
    if(input$checkbox1)
	{
	 if(length(which(input$typeInput02 %in% "ALL"))>0)
	 {
	 qtd_loop<-length(unique(part_train))
	 nv<-unique(part_train)
	 }else{
	 qtd_loop<-length(unique(part_train[which(as.character(part_train) %in% input$typeInput02)]))
	 nv<-unique(part_train[which(as.character(part_train) %in% input$typeInput02)])
	 }
	
	}else{
	qtd_loop<-1
	
	}
	
	 for(k in 1:qtd_loop)
	 {
	 train2<-train
	 test2<-test
	  if(qtd_loop>=1 & input$checkbox1==TRUE)
	  {
	  indextrain<-which(part_train==nv[k])
	  indextest<-which(part_test==nv[k])
	  nome_projeto<-paste(input$caption1,"_",nv[k],sep="")
	  }
	  if(qtd_loop==1 & input$checkbox1!=TRUE)
	  {
	  indextrain<-1:nrow(train2)
	  indextest<-1:nrow(test2)
	  nome_projeto<-input$caption1
	  }
	  
      if(is.null(input$vars1)!=TRUE)
	 {
	 train2[input$vars1]<-lapply(train2[input$vars1], as.factor)
	 
	 }
	 if(is.null(input$vars)!=TRUE)
	 {
	 train2[input$vars]<-lapply(train2[input$vars], as.numeric)
	 
	 }
	 
	    if(is.null(input$vars1)!=TRUE)
	 {
	 test2[input$vars1]<-lapply(test2[input$vars1], as.factor)
	 
	 }
	 if(is.null(input$vars)!=TRUE)
	 {
	 test2[input$vars]<-lapply(test2[input$vars], as.numeric)
	 
	 }
	  train2<-train2[indextrain,]
      test2<-test2[indextest,]
	  
	   index<-c()
     cont<-0
      for(i in 1:ncol(train2))
       {
   if(length(unique(train2[,i]))==1)
     {
       cont<-cont+1
           index[cont]<-i

        }
       if(length(unique(train2[,i]))==2 & length(which(is.na(train2[,i])==TRUE))>0)
        {
        cont<-cont+1
        index[cont]<-i

         }
        }

      if(length(index)>0)
    {
      train2<-train2[,-index]
	  test2<-test2[,-index]
   
      }
   
   cont100=0
index10<-c()
for(i in 1:ncol(train2))
{
if(length(unique(train2[,i]))==1)
{
cont100<-cont100+1
index[cont100]<-i
}

if(length(unique(test2[,i]))==1)
{
cont100<-cont100+1
index[cont100]<-i
}




}



if(length(index)>0)
{
index<-index[which(!(names(train2)[index] %in% c(input$vars2,input$vars3)))]
train2<-train2[,-index]
test2<-test2[,-index]

}
	  
	  
	  
	  
	   train2<-train2[,-which(names(train2) %in% c(input$vars3,input$typeInput2))]
	    test2<-test2[,-which(names(test2) %in% c(input$vars3,input$typeInput2))]
		
		train2<-rbind(train2,test2)
		
		contcc<-0
indexcc<-c()
for(i in 1:ncol(train2))
{
if(sum(is.na(train2[,i])==TRUE)>=0.7*nrow(train2))
{
contcc<-contcc+1
indexcc[contcc]<-i
}

}

if(length(indexcc)>0)
{
train2<-train2[,-indexcc]

}
	 
	rules_generate(dados_train=train2,alvo=input$vars2,target_class=input$vars10,min_freq_leaf=input$Min,max_split_tree=input$Max,lift1=input$lift,type="SQL SAP HANA",projeto=nome_projeto) 
    
   
   }
   }
   
     if(is.null(nrow(train))!=TRUE & is.null(nrow(test))==TRUE )
   {
   
   if(input$checkbox1)
	{
	 if(length(which(input$typeInput02 %in% "ALL"))>0)
	 {
	 qtd_loop<-length(unique(part_train))
	 nv<-unique(part_train)
	 }else{
	 qtd_loop<-length(unique(part_train[which(as.character(part_train) %in% input$typeInput02)]))
	 nv<-unique(part_train[which(as.character(part_train) %in% input$typeInput02)])
	 }
	
	}else{
	qtd_loop<-1
	
	}
	
	 for(k in 1:qtd_loop)
	 {
	 train2<-train
	  if(qtd_loop>=1 & input$checkbox1==TRUE)
	  {
	  indextrain<-which(part_train==nv[k])
	  nome_projeto<-paste(input$caption1,"_",nv[k],sep="")
	  }
	  if(qtd_loop==1 & input$checkbox1!=TRUE)
	  {
	  indextrain<-1:nrow(train2)
	  nome_projeto<-input$caption1
	  }
   
   if(is.null(input$vars1)!=TRUE)
	 {
	 train2[input$vars1]<-lapply(train2[input$vars1], as.factor)
	 
	 }
	 if(is.null(input$vars)!=TRUE)
	 {
	 train2[input$vars]<-lapply(train2[input$vars], as.numeric)
	 
	 }
   train2<-train2[indextrain,]
 train2<-train2[,-which(names(train2) %in% c(input$vars3,input$typeInput2))]
 
  index<-c()
     cont<-0
      for(i in 1:ncol(train2))
       {
   if(length(unique(train2[,i]))==1)
     {
       cont<-cont+1
           index[cont]<-i

        }
       if(length(unique(train2[,i]))==2 & length(which(is.na(train2[,i])==TRUE))>0)
        {
        cont<-cont+1
        index[cont]<-i

         }
        }

      if(length(index)>0)
    {
      train2<-train2[,-index]

      }
   
   cont100=0
index10<-c()
for(i in 1:ncol(train2))
{
if(length(unique(train2[,i]))==1)
{
cont100<-cont100+1
index[cont100]<-i
}




}



if(length(index)>0)
{
index<-index[which(!(names(train2)[index] %in% c(input$vars2,input$vars3)))]
train2<-train2[,-index]

}

contcc<-0
indexcc<-c()
for(i in 1:ncol(train2))
{
if(sum(is.na(train2[,i])==TRUE)>=0.7*nrow(train2))
{
contcc<-contcc+1
indexcc[contcc]<-i
}

}

if(length(indexcc)>0)
{
train2<-train2[,-indexcc]

}
 
rules_generate(dados_train=train2,alvo=input$vars2,target_class=input$vars10,min_freq_leaf=input$Min,max_split_tree=input$Max,lift1=input$lift,type="SQL SAP HANA",projeto=nome_projeto)   

   
}   
   
   }


      
  head(train[sample(1:nrow(train),200,replace=F),])
 
 
 })

  output$contents7 <- renderTable({
 
 if(is.null(randomVals1())!=TRUE)
 {
return(data.frame(id="FINISH RULE!"))
}
})
  
}

shinyApp(ui, server)

#unlink(paste("C:/Resultados_bivariadas_",input$caption,sep=""),recursive =TRUE)
