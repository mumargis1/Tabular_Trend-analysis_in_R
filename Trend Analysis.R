library(dplyr)
library(tidyr)
library(trend)
library(ggplot2)
library(modifiedmk)
require(reshape2)
require(dply)
require(ply)


#
####### Rivers trend ######
# data <- Total_Annual_Flow_Eastern_Western_rivers[c(-13,-14),]   
# data1 <- data %>% gather(Year, monthly, 2:61)
# #data1$annual <- Total_Annual_Flow_Eastern_Western_rivers[13,]
# plot(data1)
# colnames(data1)[1] <- c("Row_label")
# str(data1)
# ggplot(data1, aes(Row_label,(Year)))+geom_line()
# 
# x1 <- data$1962
# mkttest(x1)
# 
# data_t <- as.data.frame(t(data))
# colnames(data_t) <- data_t[1,] 
# data_t <- data_t[-1,]

# 
 # X1 <- Indus_annual_flow[,2]
 # acf(X17, lag.max = 1)
 # #a$acf
 # bcpw(X1)
a <- c()

for (i in seq(2,18,by=1)){
   a[i] <-    assign(paste0("Indus_X",i), Indus_annual_flow[,i]) 
#   print(paste0("X",i))

   }

acf(a[[18]], lag.max = 1)$acf

bcpw(a[[14]]) # when series is auto-corelated
 
mkttest(a[[18]]) # when series is not auto-corelated
#_____________ jhelum ________#

a <- c()

for (i in seq(2,18,by=1)){
   a[i] <-    assign(paste0("jhelum_X",i), jhelum_annual_flow[,i]) 
   #   print(paste0("X",i))
   
}

acf(a[[18]], lag.max = 1)$acf

bcpw(a[[18]]) # Auto_correlated series

mkttest(a[[17]]) # NoN Auto_correlated series

#_____________ chanab ________#

a <- c()

for (i in seq(2,18,by=1)){
   a[i] <-    assign(paste0("chanab_X",i), chanab_annual_flow[,i]) 
   #   print(paste0("X",i))
   
}

acf(a[[18]], lag.max = 1)$acf

bcpw(a[[8]]) # Auto_correlated series

mkttest(a[[18]]) # NoN Auto_correlated series

#_____________ Ravi ________#

a <- c()

for (i in seq(2,18,by=1)){
   a[i] <-    assign(paste0("Ravi_X",i), ravi_annual_flow[,i]) 
   #   print(paste0("X",i))
   
}

acf(a[[18]], lag.max = 1)$acf
mkttest(a[[15]]) # NoN Auto_correlated series
bcpw(a[[18]]) # Auto_correlated series


#_____________ Satluj ________#

a <- c()

for (i in seq(2,18,by=1)){
   a[i] <-    assign(paste0("Satluj_X",i), satjul_annual_flow[,i]) 
   #   print(paste0("X",i))
   
}

acf(a[[4]], lag.max = 1)$acf

mkttest(a[[4]]) # NoN Auto_correlated series
bcpw(a[[14]]) # Auto_correlated series


###### PMD all station #######
# winter 	Dec-Feb 
# Auntumn	Sep-Nov
# Summer 	June-Aug
# Spring	March-May


PMD_ALL_STATIONS$Annual <- rowMeans(PMD_ALL_STATIONS[,3:14])
PMD_ALL_STATIONS$Winter <- rowMeans(PMD_ALL_STATIONS[,c(3,4,14)])
PMD_ALL_STATIONS$Auntumn <- rowMeans(PMD_ALL_STATIONS[,c(13,12,11)])
PMD_ALL_STATIONS$Summer <- rowMeans(PMD_ALL_STATIONS[,c(8,9,10)])
PMD_ALL_STATIONS$Spring <- rowMeans(PMD_ALL_STATIONS[,c(5,6,7)])


stations <- unique(PMD_ALL_STATIONS$Station)

#name <- as.data.frame(stations)


for (i in seq(35)){
   assign(stations[i], PMD_ALL_STATIONS %>% filter(Station==stations[i]))
}

a <- c()
acef <- c()
Trend <-array( , dim = c(1,6,19,35), 
               dimnames = list( "1",c("Z_Value","Sens_slope","S","Var(S)","P_value","Tau" ), 
                                c("missing","missing","Jan","Feb","March","April","May","June","July",
                                "Aug","Sep","Oct","Nov","Dec","Anuual","Winter","Autumn","Summer",
                                "Spring"), 
                                c("Astore","Bahawalpur","Balakot","Bhawalnagar","Bunji","Cheerat",
                                 "Chitral","Dalbadin","DI_Khan","Drosh","Faisalabad","Ghari_Dupatta",
                                 "Gilgit","Gupis","hyderabad","Islamabad","Jacobabad","Jhelum","Kakul",
                                 "Karachi","Khanpur","Kohat","Kotli","lahore","Mianwali",
                                 "Multan","Murree","Muzafarabad","Parachinar","Peshawar","Quetta",
                                 "Sargodha","Sialkot","Skardu","Zhob")))
Trend_corr <-array( , dim = c(1,7,19,35), 
               dimnames = list( "1",c("Z_Value"," Prewhitened Sen's Slope","Sens_slope","P_value",
                                      "S","Var(S)","Tau" ), 
                                c("missing","missing","Jan","Feb","March","April","May","June","July",
                                  "Aug","Sep","Oct","Nov","Dec","Anuual","Winter","Autumn","Summer",
                                  "Spring"), 
                                c("Astore","Bahawalpur","Balakot","Bhawalnagar","Bunji","Cheerat",
                                  "Chitral","Dalbadin","DI_Khan","Drosh","Faisalabad","Ghari_Dupatta",
                                  "Gilgit","Gupis","hyderabad","Islamabad","Jacobabad","Jhelum","Kakul",
                                  "Karachi","Khanpur","Kohat","Kotli","lahore","Mianwali",
                                  "Multan","Murree","Muzafarabad","Parachinar","Peshawar","Quetta",
                                  "Sargodha","Sialkot","Skardu","Zhob")))
ACF <- array( , dim = c(1,1,19,35),dimnames = list("1","Lag_1_auto.value", 
                                             c("missing","missing","Jan","Feb","March","April","May","June","July",
                                             "Aug","Sep","Oct","Nov","Dec","Anuual","Winter","Autumn","Summer",
                                             "Spring"),
                                             c("Astore","Bahawalpur","Balakot","Bhawalnagar","Bunji","Cheerat",
                                               "Chitral","Dalbadin","DI_Khan","Drosh","Faisalabad","Ghari_Dupatta",
                                               "Gilgit","Gupis","hyderabad","Islamabad","Jacobabad","Jhelum","Kakul",
                                               "Karachi","Khanpur","Kohat","Kotli","lahore","Mianwali",
                                               "Multan","Murree","Muzafarabad","Parachinar","Peshawar","Quetta",
                                               "Sargodha","Sialkot","Skardu","Zhob")))

# for (i in seq(3,19, by=1)){
#          a[i] <-    assign(paste0("Astore_X",i), Astore[,i])
#          acef[i] <- acf(a[[i]], lag.max = 1)$acf[2,1,]
#       
#             if( acef[i] <= 0.265){
#                print(mkttest(a[[i]]))
#                Trend[,,i,1] <- (mkttest(a[[i]]))
#                ACF[,,i,1] <- acef[i]
#                }else {
#                   print(bcpw(a[[i]])) # Auto_correlated series
#                  Trend_corr[,,i,15] <- bcpw(a[[i]])
#                  ACF[,,i,1] <- acef[i]
#             }
#    
#    }



a=c()
acef=c()

for (station in stations){
   
   for (i in seq(3,19, by=1)){
      #print(assign(paste0(station,"_X",i), get(station)[,i]))
      a[i] <-    assign(paste0(station,"_X",i), get(station)[,i])
      acef[i] <- acf(a[[i]], lag.max = 1)$acf[2,1,]
       
       if( acef[i] <= 0.265){
          #print(mkttest(a[[i]]))
          Trend[,,i,station] <- (mkttest(a[[i]]))
          ACF[,,i,station] <- acef[i]
       }else {
          #print(bcpw(a[[i]])) # Auto_correlated series
          Trend_corr[,,i,station] <- bcpw(a[[i]])
          ACF[,,i,station] <- acef[i]
       }
       
   }
    #a=c()
    #acef <- c()
   print(station)
}
Trend[,,,3]
ACF[,,,3]
Trend_corr[,,,3]



ACF_result <- melt(ACF)
ACF_result <- ACF_result %>% filter(Var3 != "missing") #%>% select(c(-Var1, -Var2))

Trend_result <-melt(Trend) 
Trend_result <- Trend_result %>%
                  filter( Var3 != "missing") %>%
                  select(-Var1) %>%
                  pivot_wider(names_from = Var2, values_from = value) #%>% 

Trend_result$lag_1_auto <-  (ACF_result$value)

Trend_corr_result <- melt(Trend_corr)
Trend_corr_result <- Trend_corr_result %>% 
                     filter(Var3 != "missing") %>% 
                     select(-Var1) %>% 
                     pivot_wider(names_from = Var2, values_from = value)  

Trend_final <- join(Trend_result, Trend_corr_result, by=c("Var3",'Var4'), type="inner")

write.csv(Trend_final, file="D:/JOB_WORK/Dr_Salman/PMD_all_stations/Trend.csv")

