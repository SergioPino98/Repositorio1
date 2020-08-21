# General-purpose data wrangling
library(tidyverse)  

# Parsing of HTML/XML files  
library(rvest)    

# String manipulation
library(stringr)   

# Verbose regular expressions
library(rebus)     

# Eases DateTime manipulation
library(lubridate)

# Lectura de la p?gina de la p?gina inicial
url1 <- "https://www.yapo.cl/biobio/arrendar?ca=9_s&l=0&w=1&cmn="
url2 <- "https://www.yapo.cl/biobio/arrendar?ca=9_s&cg=1240&o="
webpage <- read_html(url2)
# Iniciaci?n de variables
id=c()
fecha=c()
hora=c()
desc=c()
precio=c()
precioUF=c()
precioBaja=c()
detalles=c()
aviso=c()
comuna=c()
region=c()
ii <- 0
# B?squeda de la ?ltima p?gina.
resultados <- webpage %>% html_nodes(".resultcontainer")
a <- resultados %>% html_node("a")%>% html_attr("href")
nFin <- as.numeric(substr(a, nchar(a)-1,nchar(a)))
# Procesamiento de todas las p?ginas de la b?squeda.
for(j in 1:nFin) {
  # Lectura de la p?gina j
  if(j==1) {
    webpage <- read_html(url1)
  } else {
    webpage <- read_html(gsub(" ","",paste(url2,j)))
  }
  resultados <- webpage %>% html_nodes("tr")
  # Revisi?n de la p?gina
  print(c(j,length(resultados)))
  for(i in 1:length(resultados)) {
    a <- as.numeric(resultados[i] %>% html_attr("id"))
    if(!is.na(a)) {
      ii <- ii+1
      id[ii] <- as.numeric(a)
      f <- xml_contents(resultados[i] %>% html_nodes(".date")) %>% html_text(trim=TRUE)
      if(f=="Hoy") {f <- format(Sys.Date(), "%d %b %Y")} else {
        if(f=="Ayer") {f <- format(Sys.Date()-1, "%d %b %Y")} else {
          if(substr(f, nchar(f)-2,nchar(f))=="Sep") {
            f <- gsub(" t","t",paste(f,"t. 2019"))} else {
              f <- gsub(" \\.","\\.",paste(f,". 2019"))}
        }
      }
      h <- xml_contents(resultados[i] %>% html_nodes(".hour")) %>% html_text(trim=TRUE)
      ff <- paste(f,gsub(" \\:","\\:",paste(h,":00")))
      fecha[ii] <- as.POSIXct(ff, format = "%d %b %Y %H:%M:%S")
      
      nFin <- as.numeric(substr(a, nchar(a)-1,nchar(a)))
      desc[ii] <- xml_contents(resultados[i] %>% html_nodes(".title")) %>% html_text(trim=TRUE)
      p <- xml_contents(resultados[i] %>% html_nodes(".price")) %>% html_text(trim=TRUE)
      precio[ii] <- as.numeric(gsub("\\.","",gsub("\\$ ","",p)))
      precioUF[ii] <- xml_contents(resultados[i] %>% html_nodes(".convertedPrice")) %>% html_text(trim=TRUE)
      #"Ha bajado de precio"
      pp <- (resultados[i] %>% html_nodes("img")) %>% html_attr("title")
      if(length(pp)==1) {precioBaja[ii] <- "No"} else {
        precioBaja[ii] <- "Si"}
      a <- resultados[i] %>% html_nodes(".icons__element")%>% html_nodes("span")%>% html_text(trim=TRUE)
      if(length(a)>0) {
        detalles[ii] <- toString(a) }
      else {
        detalles[ii] <- "--"
      }
      a <- resultados[i] %>% html_nodes(".company_ad")%>% html_text(trim=TRUE)
      if(length(a)>0) {
        aviso[ii] <- toString(a) }
      else {
        aviso[ii] <- "--"
      }
      comuna[ii] <- xml_contents(resultados[i] %>% html_nodes(".commune")) %>% html_text(trim=TRUE)
      region[ii] <- xml_contents(resultados[i] %>% html_nodes(".region")) %>% html_text(trim=TRUE)
      #    print(c(id, hora, desc, precio, precioUF, detalles, comuna, region))
    }
  }
}
# Generaci?n del dataset base
datos <- data.frame(id, fecha, desc, precio, precioUF, precioBaja, detalles,aviso, comuna, region)
datos$dia <- as.Date(as.POSIXct(datos$fecha,origin="1970-01-01"))
# An?lisis
# -- cantidad de avisos por comuna
aggregate(precio ~ comuna, data = datos, length)
# -- cantidad de avisos por d?a
aggregate(precio ~ dia, data = datos, length)
# Generaci?n del archivo csv.
write.table(datos,"yapo.csv", sep=";", row.names=FALSE)


f <- xml_contents(resultados[7] %>% html_nodes(".date")) %>% html_text(trim=TRUE)
if(f=="Hoy") {f <- format(Sys.Date(), "%d %b %Y")} else {
  if(f=="Ayer") {f <- format(Sys.Date()-1, "%d %b %Y")} else {
    if(substr(f, nchar(f)-2,nchar(f))=="Sep") {
      f <- gsub(" t","t",paste(f,"t. 2019"))} else {
        f <- gsub(" \\.","\\.",paste(f,". 2019"))}
  }
}
h <- xml_contents(resultados[7] %>% html_nodes(".hour")) %>% html_text(trim=TRUE)
ff <- paste(f,gsub(" \\:","\\:",paste(h,":00")))
tm2 <- as.POSIXct(ff, format = "%d %b %Y %H:%M:%S")
