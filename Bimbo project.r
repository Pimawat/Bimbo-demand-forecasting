---
  title: "Term Project"
output: html_notebook
---
  
  ##Load Data

getwd()
setwd('C:/Users/JIB/Desktop/Com meth')

data <- read.csv('trainSmp2.csv',header = T)
clientSmp <- read.csv('clientSmp.csv',header = T)
productSmp <- read.csv('productSmp.csv',header = T)
town_state <- read.csv('town_state.csv',header = T)

dim(data)


##Packages

require(moments)
require(dplyr)
require(data.table)
require(lattice)
require(maps)
#if (!require(devtools)) {
#    install.packages("devtools")
#}
#devtools::install_github('diegovalle/mxmaps')
require(mxmaps)
require(stringr)
require(ggplot2)
require(forecast)


##Exporing and processing data

data.DT <- as.data.table(data)
glimpse(data)

#check na & duplicated
any(is.na.data.frame(data))
#any(duplicated(data))

#amount in each Semana
hist(data$Semana,col = 'gray',labels = T)
table(data$Semana)

#amount in each Canal
hist(data$Canal_ID,col = 'gray')
table(data$Canal_ID)

#explore damand
summary(data[,7:11])
hist(data$Dev_uni_proxima,col = 'gray',xlim = c(0,2000))
data.DT[Demanda_uni_equil>=200,]
data.DT[Dev_uni_proxima>=2000,]
data.DT[Cliente_ID==1050905,]
boxplot(data$Demanda_uni_equil)$out
data.DT[Dev_uni_proxima>Venta_uni_hoy]

#explore client
data.dt.cus1 <- data.DT %>% group_by(Cliente_ID) %>% count(Cliente_ID) %>% arrange(-Cliente_ID)
data.dt.cus2 <- data.DT %>% group_by(Cliente_ID) %>% summarise(avg_unit = mean(Demanda_uni_equil),min_unit = min(Demanda_uni_equil),max_unit = max(Demanda_uni_equil)) %>% arrange(-Cliente_ID)
data.dt.cus3 <- data.dt.cus1
data.dt.cus3$avg_unit <- data.dt.cus2$avg_unit ; data.dt.cus3$min_unit <- data.dt.cus2$min_unit ; data.dt.cus3$max_unit <- data.dt.cus2$max_unit
data.dt.cus3 <- as.data.table(data.dt.cus3)
data.dt.cus3[order(-data.dt.cus3$avg_unit,-data.dt.cus3$n),]

#explore product
productSmp.DT <- as.data.table(productSmp)
data.DT.product <- data.DT[productSmp.DT,on=.(Producto_ID==Producto_ID)]
data.DT.product <- na.omit(data.DT.product)
data.DT.product.com <- data.DT.product
#---Extract short name from first in the product name
data.DT.product.com$short_name <- str_split_fixed(data.DT.product.com$NombreProducto,' ',n = 2)[,1]
#---Extract company name from the end of product name
data.DT.product.com <- mutate(data.DT.product.com,com = str_extract(NombreProducto,"[:upper:]+[:upper:]"))

data.DT.product.com.sum <- data.DT.product.com %>% group_by(short_name) %>% summarise(counts = n(),sum_demand = sum(Demanda_uni_equil),avg_unit = mean(Demanda_uni_equil),min_unit = min(Demanda_uni_equil),max_unit = max(Demanda_uni_equil))
data.DT.product.com.sum2 <- data.DT.product.com %>% group_by(com) %>% summarise(counts = n(),sum_demand = sum(Demanda_uni_equil),avg_unit = mean(Demanda_uni_equil),min_unit = min(Demanda_uni_equil),max_unit = max(Demanda_uni_equil))

barplot(table(data.DT.product.com$short_name))
barplot(table(data.DT.product.com$com))

data.DT.product.com2 <- data.DT.product.com
data.DT.product.com2 <- as.data.table(data.DT.product.com2)
data.DT.product.com2[which(com != 'BIM'),]$com <- "Others" 

barplot(table(data.DT.product.com2$com))

data.DT.product.com.sum3 <- data.DT.product.com2 %>% group_by(com) %>% summarise(counts = n(),sum_demand = sum(Demanda_uni_equil),avg_unit = mean(Demanda_uni_equil),min_unit = min(Demanda_uni_equil),max_unit = max(Demanda_uni_equil))


#explore state
data.DT.product.com2 <- as.data.table(data.DT.product.com2)
data.DT.product.com <- as.data.table(data.DT.product.com)
data.CT <- data.DT.product.com2[town_state,on=.(Agencia_ID==Agencia_ID)]
data.CT <- na.omit(data.CT)
table(data.CT$State)
barplot(table(data.CT$State))

data.CT <- as.data.table(data.CT)
data.CT.sum <- data.CT %>% group_by(State) %>% summarise(sum_demand = sum(Demanda_uni_equil),avg_unit = mean(Demanda_uni_equil),min_unit = min(Demanda_uni_equil),max_unit = max(Demanda_uni_equil))

df<-df_mxstate
df$value <- rep(0,nrow(df))
df<-as.data.table(df)
df2 <- df %>% arrange(state_name)
data.CT.sum2 <- data.CT.sum %>% arrange(State)
data.CT2 <-  data.CT

df3<- as.data.table(df2)
df4<- as.data.table(df2)
df5<- as.data.table(df2)
df6<- as.data.table(df2)
df7<- as.data.table(df2)
df8<- as.data.table(df2)
df9<- as.data.table(df2)

data.CT.3 <- data.CT2 %>% filter(Semana==3&com=='BIM') %>% group_by(State) %>% summarise(sum_demand = sum(Demanda_uni_equil)) %>% arrange(State)
data.CT.3 <- as.data.table(data.CT.3)
#---Correct name of state
df3$value[1:5] <- data.CT.3$sum_demand[1:5]
df3$value[7]<- data.CT.3$sum_demand[8]
df3$value[9:10] <- data.CT.3$sum_demand[6:7]
df3$value[11:18] <- data.CT.3$sum_demand[9:16]
df3$value[20:21] <- data.CT.3$sum_demand[17:18]
df3$value[22] <- data.CT.3$sum_demand[19]+data.CT.3$sum_demand[20]
df3$value[23:32] <- data.CT.3$sum_demand[21:30]
df3$value[6] <- NA
df3$value[8] <- NA
df3$value[19] <- NA

data.CT.3 <- data.CT2 %>% filter(Semana==4&com=='BIM') %>% group_by(State) %>% summarise(sum_demand = sum(Demanda_uni_equil)) %>% arrange(State)
df4$value[1:5] <- data.CT.3$sum_demand[1:5]
df4$value[7]<- data.CT.3$sum_demand[8]
df4$value[9:10] <- data.CT.3$sum_demand[6:7]
df4$value[11:18] <- data.CT.3$sum_demand[9:16]
df4$value[20:21] <- data.CT.3$sum_demand[17:18]
df4$value[22] <- data.CT.3$sum_demand[19]+data.CT.3$sum_demand[20]
df4$value[23:32] <- data.CT.3$sum_demand[21:30]
df4$value[6] <- NA
df4$value[8] <- NA
df4$value[19] <- NA

data.CT.3 <- data.CT2 %>% filter(Semana==5&com=='BIM') %>% group_by(State) %>% summarise(sum_demand = sum(Demanda_uni_equil)) %>% arrange(State)
df5$value[1:5] <- data.CT.3$sum_demand[1:5]
df5$value[7]<- data.CT.3$sum_demand[8]
df5$value[9:10] <- data.CT.3$sum_demand[6:7]
df5$value[11:18] <- data.CT.3$sum_demand[9:16]
df5$value[20:21] <- data.CT.3$sum_demand[17:18]
df5$value[22] <- data.CT.3$sum_demand[19]+data.CT.3$sum_demand[20]
df5$value[23:32] <- data.CT.3$sum_demand[21:30]
df5$value[6] <- NA
df5$value[8] <- NA
df5$value[19] <- NA

data.CT.3 <- data.CT2 %>% filter(Semana==6&com=='BIM') %>% group_by(State) %>% summarise(sum_demand = sum(Demanda_uni_equil)) %>% arrange(State)
df6$value[1:5] <- data.CT.3$sum_demand[1:5]
df6$value[7]<- data.CT.3$sum_demand[8]
df6$value[9:10] <- data.CT.3$sum_demand[6:7]
df6$value[11:18] <- data.CT.3$sum_demand[9:16]
df6$value[20:21] <- data.CT.3$sum_demand[17:18]
df6$value[22] <- data.CT.3$sum_demand[19]+data.CT.3$sum_demand[20]
df6$value[23:32] <- data.CT.3$sum_demand[21:30]
df6$value[6] <- NA
df6$value[8] <- NA
df6$value[19] <- NA

data.CT.3 <- data.CT2 %>% filter(Semana==7&com=='BIM') %>% group_by(State) %>% summarise(sum_demand = sum(Demanda_uni_equil)) %>% arrange(State)
df7$value[1:5] <- data.CT.3$sum_demand[1:5]
df7$value[7]<- data.CT.3$sum_demand[8]
df7$value[9:10] <- data.CT.3$sum_demand[6:7]
df7$value[11:18] <- data.CT.3$sum_demand[9:16]
df7$value[20:21] <- data.CT.3$sum_demand[17:18]
df7$value[22] <- data.CT.3$sum_demand[19]+data.CT.3$sum_demand[20]
df7$value[23:32] <- data.CT.3$sum_demand[21:30]
df7$value[6] <- NA
df7$value[8] <- NA
df7$value[19] <- NA

data.CT.3 <- data.CT2 %>% filter(Semana==8&com=='BIM') %>% group_by(State) %>% summarise(sum_demand = sum(Demanda_uni_equil)) %>% arrange(State)
df8$value[1:5] <- data.CT.3$sum_demand[1:5]
df8$value[7]<- data.CT.3$sum_demand[8]
df8$value[9:10] <- data.CT.3$sum_demand[6:7]
df8$value[11:18] <- data.CT.3$sum_demand[9:16]
df8$value[20:21] <- data.CT.3$sum_demand[17:18]
df8$value[22] <- data.CT.3$sum_demand[19]+data.CT.3$sum_demand[20]
df8$value[23:32] <- data.CT.3$sum_demand[21:30]
df8$value[6] <- NA
df8$value[8] <- NA
df8$value[19] <- NA

data.CT.3 <- data.CT2 %>% filter(Semana==9&com=='BIM') %>% group_by(State) %>% summarise(sum_demand = sum(Demanda_uni_equil)) %>% arrange(State)
df9$value[1:5] <- data.CT.3$sum_demand[1:5]
df9$value[7]<- data.CT.3$sum_demand[8]
df9$value[9:10] <- data.CT.3$sum_demand[6:7]
df9$value[11:18] <- data.CT.3$sum_demand[9:16]
df9$value[20:21] <- data.CT.3$sum_demand[17:18]
df9$value[22] <- data.CT.3$sum_demand[19]+data.CT.3$sum_demand[20]
df9$value[23:32] <- data.CT.3$sum_demand[21:30]
df9$value[6] <- NA
df9$value[8] <- NA
df9$value[19] <- NA

pdf('BIM4.pdf')
mxstate_choropleth(df3,title = "Semana 3",num_colors = 8)
mxstate_choropleth(df4,title = "Semana 4",num_colors = 8)
mxstate_choropleth(df5,title = "Semana 5",num_colors = 8)
mxstate_choropleth(df6,title = "Semana 6",num_colors = 8)
mxstate_choropleth(df7,title = "Semana 7",num_colors = 8)
mxstate_choropleth(df8,title = "Semana 8",num_colors = 8)
mxstate_choropleth(df9,title = "Semana 9",num_colors = 8)




df3<- df2
df4<- df2
df5<- df2
df6<- df2
df7<- df2
df8<- df2
df9<- df2

data.CT.3 <- data.CT2 %>% filter(Semana==3&com=='Others') %>% group_by(State) %>% summarise(sum_demand = sum(Demanda_uni_equil)) %>% arrange(State)
df3$value[1:6] <- data.CT.3$sum_demand[1:6]
df3$value[7]<- data.CT.3$sum_demand[10]
df3$value[8:10] <- data.CT.3$sum_demand[7:9]
df3$value[11:21] <- data.CT.3$sum_demand[11:21]
df3$value[22] <- data.CT.3$sum_demand[22]+data.CT.3$sum_demand[23]
df3$value[23:32] <- data.CT.3$sum_demand[24:33]

data.CT.3 <- data.CT2 %>% filter(Semana==4&com=='Others') %>% group_by(State) %>% summarise(sum_demand = sum(Demanda_uni_equil)) %>% arrange(State)
df4$value[1:6] <- data.CT.3$sum_demand[1:6]
df4$value[7]<- data.CT.3$sum_demand[10]
df4$value[8:10] <- data.CT.3$sum_demand[7:9]
df4$value[11:21] <- data.CT.3$sum_demand[11:21]
df4$value[22] <- data.CT.3$sum_demand[22]+data.CT.3$sum_demand[23]
df4$value[23:32] <- data.CT.3$sum_demand[24:33]

data.CT.3 <- data.CT2 %>% filter(Semana==5&com=='Others') %>% group_by(State) %>% summarise(sum_demand = sum(Demanda_uni_equil)) %>% arrange(State)
df5$value[1:6] <- data.CT.3$sum_demand[1:6]
df5$value[7]<- data.CT.3$sum_demand[10]
df5$value[8:10] <- data.CT.3$sum_demand[7:9]
df5$value[11:21] <- data.CT.3$sum_demand[11:21]
df5$value[22] <- data.CT.3$sum_demand[22]+data.CT.3$sum_demand[23]
df5$value[23:32] <- data.CT.3$sum_demand[24:33]

data.CT.3 <- data.CT2 %>% filter(Semana==6&com=='Others') %>% group_by(State) %>% summarise(sum_demand = sum(Demanda_uni_equil)) %>% arrange(State)
df6$value[1:6] <- data.CT.3$sum_demand[1:6]
df6$value[7]<- data.CT.3$sum_demand[10]
df6$value[8:10] <- data.CT.3$sum_demand[7:9]
df6$value[11:21] <- data.CT.3$sum_demand[11:21]
df6$value[22] <- data.CT.3$sum_demand[22]+data.CT.3$sum_demand[23]
df6$value[23:32] <- data.CT.3$sum_demand[24:33]

data.CT.3 <- data.CT2 %>% filter(Semana==7&com=='Others') %>% group_by(State) %>% summarise(sum_demand = sum(Demanda_uni_equil)) %>% arrange(State)
df7$value[1:6] <- data.CT.3$sum_demand[1:6]
df7$value[7]<- data.CT.3$sum_demand[10]
df7$value[8:10] <- data.CT.3$sum_demand[7:9]
df7$value[11:21] <- data.CT.3$sum_demand[11:21]
df7$value[22] <- data.CT.3$sum_demand[22]+data.CT.3$sum_demand[23]
df7$value[23:32] <- data.CT.3$sum_demand[24:33]

data.CT.3 <- data.CT2 %>% filter(Semana==8&com=='Others') %>% group_by(State) %>% summarise(sum_demand = sum(Demanda_uni_equil)) %>% arrange(State)
df8$value[1:6] <- data.CT.3$sum_demand[1:6]
df8$value[7]<- data.CT.3$sum_demand[10]
df8$value[8:10] <- data.CT.3$sum_demand[7:9]
df8$value[11:21] <- data.CT.3$sum_demand[11:21]
df8$value[22] <- data.CT.3$sum_demand[22]+data.CT.3$sum_demand[23]
df8$value[23:32] <- data.CT.3$sum_demand[24:33]

data.CT.3 <- data.CT2 %>% filter(Semana==9&com=='Others') %>% group_by(State) %>% summarise(sum_demand = sum(Demanda_uni_equil)) %>% arrange(State)
df9$value[1:6] <- data.CT.3$sum_demand[1:6]
df9$value[7]<- data.CT.3$sum_demand[10]
df9$value[8:10] <- data.CT.3$sum_demand[7:9]
df9$value[11:21] <- data.CT.3$sum_demand[11:21]
df9$value[22] <- data.CT.3$sum_demand[22]+data.CT.3$sum_demand[23]
df9$value[23:32] <- data.CT.3$sum_demand[24:33]

pdf('Others3.pdf')
mxstate_choropleth(df3,title = "Semana 3",num_colors = 9)
mxstate_choropleth(df4,title = "Semana 4",num_colors = 9)
mxstate_choropleth(df5,title = "Semana 5",num_colors = 9)
mxstate_choropleth(df6,title = "Semana 6",num_colors = 9)
mxstate_choropleth(df7,title = "Semana 7",num_colors = 9)
mxstate_choropleth(df8,title = "Semana 8",num_colors = 9)
mxstate_choropleth(df9,title = "Semana 9",num_colors = 9)




tail(data.DT.product.com)
data.DT.product.com.test <- mutate(data.DT.product.com,name = str_extract(NombreProducto,"[^[:digit:]]*"))
data.DT.product.com.test2 <- mutate(data.DT.product.com.test,weight = str_extract(NombreProducto,"\\d+[kgKG]"))

data.DT.product.com.test2 <- as.data.table(data.DT.product.com.test2)
data.DT.product.com.test2[Producto_ID==43364,]
data.DT.product.com.test2[Producto_ID==43364,]$weight <- "1360g"
data.DT.product.com.test2[Producto_ID==43364,]$name <- "12Granos Multigra TwinPack"


unique(data.DT.product.com.test2$weight)
unique(data.DT.product.com.test2[which(is.na(data.DT.product.com.test2$weight)),]$NombreProducto)

data.DT.product.com.test2[is.na(data.DT.product.com.test2$weight),]$weight <- "None"

data.DT.product.com.test3 <- mutate(data.DT.product.com.test2,piece = str_extract(NombreProducto,"\\d+[p]"))

unique(data.DT.product.com.test3$piece)
unique(data.DT.product.com.test3[which(is.na(data.DT.product.com.test3$piece)),]$NombreProducto)

data.DT.product.com.test3 <- as.data.table(data.DT.product.com.test3)
data.DT.product.com.test3[piece=="200p",]

data.DT.product.com.test3[piece=="0p",]$piece <- "None"
data.DT.product.com.test3[Producto_ID==46962,]$piece <- "6p"

unique(data.DT.product.com.test3[str_detect(data.DT.product.com.test3$NombreProducto,"pct"),]$NombreProducto)

data.DT.product.com.test3[str_detect(data.DT.product.com.test3$NombreProducto,"pct"),]$piece <- "None"
data.DT.product.com.test3[is.na(data.DT.product.com.test3$piece),]$piece <- "None"

data.DT.product.com.test3[Producto_ID==46962,]$piece <- "6p"
data.DT.product.com.test3[Producto_ID==34983,]$piece <- "10p"

data.DT.product.com.test4 <- data.DT.product.com.test3
table(str_replace(data.DT.product.com.test4$piece,"p",""))
data.DT.product.com.test4$piece <- str_replace(data.DT.product.com.test4$piece,"p","")

unique(data.DT.product.com.test4$weight)


data.DT.product.cus <- data.DT.product.com.test4
CusSmp.DT <- as.data.table(clientSmp)
data.DT.product.cus.test <- data.DT.product.cus[CusSmp.DT,on=.(Cliente_ID==Cliente_ID)]
data.DT.product.cus.test2 <- na.omit(data.DT.product.cus.test)
any(duplicated(data.DT.product.cus.test2))

unique(data.DT.product.cus.test2$NombreCliente)

#cafe
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"CAFE"),]$NombreCliente)

#resturant
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"REST"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"CERVECE"),]$NombreCliente)

#hot dog
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"DOG"),]$NombreCliente)

#taco
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"TACO"),]$NombreCliente)

#bakery
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"PANADE"),]$NombreCliente)

#????????????????????????????????????
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"ABAR"),]$NombreCliente)

#department store
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"ALMAC"),]$NombreCliente)

#hotel
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"HOTEL"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"POSAD"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"MOTEL"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"RESORT"),]$NombreCliente)

#hospital
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"HOSP"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"CLINI"),]$NombreCliente)

#Phamacia
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"FARM"),]$NombreCliente)

#market
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"MERCA"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"MARKE"),]$NombreCliente)

#park
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"PARQ"),]$NombreCliente)

#cinema
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"CINE"),]$NombreCliente)

#school
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"COLEG"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"INSTI"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"UNIV"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"ESCUELA"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"PREPAR"),]$NombreCliente)

#bimbo
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"BIMB"),]$NombreCliente)

#post
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"PUESTO"),]$NombreCliente)

#office
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"OFICI"),]$NombreCliente)

#bank
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"BANCO"),]$NombreCliente)

#church
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"IGLE"),]$NombreCliente)

#airport
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"AERO"),]$NombreCliente)

#gasolinia
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"GASO"),]$NombreCliente)

#club
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"CLUB"),]$NombreCliente)

#gov store
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"CONASUPO"),]$NombreCliente)

#fresh market
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"VERDU"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"FRUT"),]$NombreCliente)

#eatery
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"CAFE"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"CREMERIA"),]$NombreCliente) #??
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"DULCERIA"),]$NombreCliente) #??
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"REST"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"BURGER"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"BURGU"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"TACO"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"TORTA"),]$NombreCliente) #??
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"TAQUER"),]$NombreCliente) #??
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"HOT DOG"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"COMEDOR"),]$NombreCliente) #??
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"ERIA"),]$NombreCliente) #??

#supermarket
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"SUPER"),]$NombreCliente)

#mart/market
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"COMERCIAL"),]$NombreCliente) #??
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"BODEGA"),]$NombreCliente) #??
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"DEPOSITO"),]$NombreCliente) #??
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"ABARROTES"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"MERCADO"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"CAMBIO"),]$NombreCliente) #??
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"MARKET"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"MART"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"MINI"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"CAFE"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"PLAZA"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"MISC"),]$NombreCliente) #??
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"EXP"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"SNACK"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"PAPELERIA"),]$NombreCliente) #??
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"CARNICERIA"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"LOCAL"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"COMODIN"),]$NombreCliente)
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"PROVIDENCIA"),]$NombreCliente)


#oxxo store
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"OXXO"),]$NombreCliente)

#7-11
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$NombreCliente,"ELEVEN"),]$NombreCliente)

#--------------------------------------------------------------------------------------------

#product
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Nito"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Nito"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Pan"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Pan"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Tost"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Tost"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Duo"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Duo"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Tub"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Tub"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Delici"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Delici"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Gallet"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Gallet"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Gansit"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Gansit"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Barrit"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Barrit"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Tortill"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Tortill"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Suavicrem"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Suavicrem"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Frut"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Frut"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Bran"),]$name) #F
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Bran"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Lat"),]$name) #can
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Lat"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Mantec"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Mantec"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Boll"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Boll"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Princip"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Princip"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Triki"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Triki"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Mini"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Mini"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Tira"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Tira"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Medi"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Medi"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Sandwi"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Sandwi"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Muff"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Muff"),]$name) #??

#taste
unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Vainill"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Vainill"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Choc"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Choc"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Coco"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Coco"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Fres"),]$name) #strawberry
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Fres"),]$name) 

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Nuez"),]$name) #Nut
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Nuez"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Multigran"),]$name) #multigrain
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Multigran"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Pin"),]$name) #pineapple
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Pin"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Integral"),]$name) #wholemeal
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Integral"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Jamon"),]$name) #Ham
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Jamon"),]$name) #??

unique(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Blanc"),]$name) #??
length(data.DT.product.cus.test2[str_detect(data.DT.product.cus.test2$name,"Blanc"),]$name) #??

#---------------------------------------------------------------------------------------------------

data.cus.seg <- mutate(data.DT.product.cus.test2,segment = "individual")
data.cus.seg <- as.data.table(data.cus.seg)

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"\\d|EL |LA |LOS |LAS | Y |DE |DEL |SAN |SANTA |AG |MI |MA |II")),]$segment <- "small store"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"OXXO")),]$segment <- "oxxo store"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"COMERCIAL|BODEGA|DEPOSITO|ABARROTES|MERCADO|CAMBIO|MARKET|MART|MINI|CARNICERIA|PLAZA|MISC|EXP|SNACK|PAPELERIA|LOCAL|COMODIN|PROVIDENCIA")),]$segment <- "market/mart"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"SUPER")),]$segment <- "supermarket"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"VERDU|FRUT")),]$segment <- "fresh market"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"CREMERIA|DULCERIA|REST|BURGER|BURGU|TACO|TORTA|TAQUER|HOT DOG|COMEDOR|ERIA")),]$segment <- "eatery"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"PUESTO")),]$segment <- "post"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"PANADE")),]$segment <- "bakery"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"CONASUPO")),]$segment <- "gov store"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"CLUB")),]$segment <- "club"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"CONASUPO")),]$segment <- "gov store"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"AERO")),]$segment <- "airport"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"BANCO")),]$segment <- "bank"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"OFICI")),]$segment <- "office"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"IGLE")),]$segment <- "church"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"PARQ")),]$segment <- "park"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"CINE")),]$segment <- "cinema"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"ALMAC")),]$segment <- "department store"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"ELEVEN")),]$segment <- "7-11"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"BIMB")),]$segment <- "bimbo store"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"CAFE")),]$segment <- "cafe"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"HOTEL|POSAD|MOTEL|INN")),]$segment <- "hotel"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"HOSP|CLINI")),]$segment <- "hospital"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"FARMA")),]$segment <- "phamarcy"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"COLEG|INSTI|UNIV|ESCUELA|PREPAR")),]$segment <- "institute"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"GASO")),]$segment <- "gas station"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"NO IDEN")),]$segment <- "no identical"

data.cus.seg[which(str_detect(data.cus.seg$NombreCliente,"MARTIN|MARTINEZ|MARTHA")),]$segment <- "individual"

table(data.cus.seg$segment)
barplot(table(data.cus.seg$segment))

#-------------------------------------------------------------------------------------------------

data.cus.seg.group <- mutate(data.cus.seg,product_group = "Others")
data.cus.seg.group <- mutate(data.cus.seg.group,taste = "Others")

data.cus.seg.group <- as.data.table(data.cus.seg.group)

data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Nito")),]$product_group <- "nito"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Pan")),]$product_group <- "pan"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Tost")),]$product_group <- "toasted"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Duo")),]$product_group <- "duo"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Tub")),]$product_group <- "tubo"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Delici")),]$product_group <- "deliciosas"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Gallet")),]$product_group <- "galleta"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Gansit")),]$product_group <- "gansito"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Barrit")),]$product_group <- "barritas"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Tortill")),]$product_group <- "tortillas"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Suavicrem")),]$product_group <- "suavicrem"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Frut|Bran")),]$product_group <- "bran frut"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Lat")),]$product_group <- "lata"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Mantec")),]$product_group <- "mantecadas"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Boll")),]$product_group <- "bollos"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Princip")),]$product_group <- "principe"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Triki")),]$product_group <- "triki"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Mini")),]$product_group <- "mini"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Tira")),]$product_group <- "tira"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Medi")),]$product_group <- "medias"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Sandwi")),]$product_group <- "sandwich"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Muff")),]$product_group <- "muffin"


data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Blanc")),]$taste <- "white"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Vainill")),]$taste <- "vanilla"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Choc")),]$taste <- "Chocolate"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Coco")),]$taste <- "coco"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Fres")),]$taste <- "strawberry"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Nuez")),]$taste <- "nut"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Multigran")),]$taste <- "multigrain"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Pin")),]$taste <- "pineapple"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Integral")),]$taste <- "integral"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Jamon")),]$taste <- "ham"
data.cus.seg.group[which(str_detect(data.cus.seg.group$name,"Mango")),]$taste <- "mango"





data.cus.seg2 <- data.cus.seg

data.cus.seg.sum <- data.cus.seg %>% group_by(segment) %>% summarise(avg_demand = mean(Demanda_uni_equil),sd_demand = sd(Demanda_uni_equil),min_demand = min(Demanda_uni_equil),max_demand = max(Demanda_uni_equil),count=n(),sum_demand=sum(Demanda_uni_equil)) %>% arrange(-avg_demand)

data.cus.seg.sum



ggplot(data.cus.seg, aes(x=segment,y=Demanda_uni_equil,color = segment)) + geom_boxplot()
ggplot(data.cus.seg.sum, aes(x=reorder(segment,-avg_demand),y=avg_demand)) + geom_col(aes(fill=segment))
ggplot(data.cus.seg, aes(x=segment,y=Demanda_uni_equil,color = segment)) + geom_boxplot() + ylim(0,60)

nrow(data.cus.seg[which(data.cus.seg$Demanda_uni_equil >= 50),])

unique(data.cus.seg[which(data.cus.seg$weight=="None"),]$name)


#clean
data.cus.seg.cln <- data.cus.seg

data.cus.seg.cln$Semana <- as.character(data.cus.seg.cln$Semana)
data.cus.seg.cln$Canal_ID <- as.character(data.cus.seg.cln$Canal_ID)

data.cus.seg.cln2 <- data.cus.seg.cln

data.cus.seg.cln2 <- mutate(data.cus.seg.cln2,w = 0)

data.cus.seg.cln2[which(str_detect(data.cus.seg.cln2$weight,"g|G")),]$w <- mutate(data.cus.seg.cln2[which(str_detect(data.cus.seg.cln2$weight,"g|G")),],
       w =  as.numeric(str_replace(weight,"g|G","")))$w

data.cus.seg.cln2[which(str_detect(data.cus.seg.cln2$weight,"k|K")),]$w <- mutate(data.cus.seg.cln2[which(str_detect(data.cus.seg.cln2$weight,"k|K")),],
                                                                                 w =  1000*as.numeric(str_replace(weight,"k|K","")))$w
data.cus.seg.cln3 <- data.cus.seg.cln2

data.cus.seg.cln3 <- mutate(data.cus.seg.cln3,p = 0)
data.cus.seg.cln3[which(data.cus.seg.cln3$piece != "None"),]$p <- 
  mutate(data.cus.seg.cln3[which(data.cus.seg.cln3$piece!="None"),] , p =  as.numeric(piece))$p

View(data.cus.seg.cln3)
data.feature <- data.cus.seg.cln3[,c(1,3,7:11,13,14,19.20,21)]

data.feature$Semana <- as.numeric(data.feature$Semana)
unique(data.feature$short_name)

barplot(table(data.feature$short_name))
unique(data.cus.seg.cln3$name)



#factor effect
anova(lm(Demanda_uni_equil~.,data = data.cus.seg))


data.test <- as.data.table(data.cus.seg.cln3)
data.all <- data.test[town_state,on=.(Agencia_ID==Agencia_ID)]
data.all2 <- na.omit(data.all)
unique(data.all2$Town)

memory.size(max=NA)
memory.limit(size = 20000)

data.all2

anova(lm(Dev_uni_proxima~.,data = test.feature6))
anova(lm(Dev_uni_proxima~.,data = test.feature7))

cor(test.feature5)












