require(shiny)
folder_address = 'C:/Users/valter.e.junior/Desktop/AUTO_MODEL/dash_automated_cla_multi2.r'

x <- system("ipconfig", intern=TRUE)
z <- x[grep("IPv4", x)]
ip <- gsub(".*? ([[:digit:]])", "\\1", z)
print(paste0("the Shiny Web application runs on: http://", ip[1], ":1235/"))

runApp(folder_address, launch.browser=FALSE, port = 1235, host = ip[1])