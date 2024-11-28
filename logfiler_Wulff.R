                                                      ###################install sektionen##################################                                                                                   
                                                      #       install.packages(c("rvest", "httr", "stringr", "tidyverse",  #                                                                                 
                                                      #                          "countrycode", "leaflet", "sf", "dplyr")) #                                                                                 
                                                      #       install.packages("devtools")                                 #                                                 
                                                      #       devtools::install_github("ropensci/rnaturalearth")           #                                                                       
                                                      #       devtools::install_github("ropensci/rnaturalearthdata")       #                                                                           
                                                      #                                                                    #                    
                                                      #       devtools::install_github("hrbrmstr/ipapi")                   #                                                             
                                                      ######################################################################


                 
######################### opgave beskrivelse ##############################################
#Opgave 3 – Analyse af logfiler Opgave                                                    #
#3.1 – Rapport fra en webserver                                                           #
#I skal åbne logfilerne og lave en rapport over aktiviteten på webserveren.               #Status:
#Rapporten skal indeholde en optælling af aktive ip-adresser pr døgn.                     #Status: Done
#Plot med de mest aktive. whois-info på den mest aktive.                                  #Status: Done
#Gruppering på 404, herunder en beskrivelse af ”mistænksomme” requests.                   #Status:
#Det værste der kan ske, er at en mistænksom request returnerer 200.                      #
#Overvej hvordan man kan fange dem.  (vi antager at vi fanger reqs inden de gør skade)    #Status: https://en.wikipedia.org/wiki/Fail2ban; kun europeanske ip'er, kun danske ip'er, Hvis det går galt må man gøre som i artiklen (ø-drift) og bare frakoble sig internettet, man kan næsten ikke hacke unden internettet
########################################################################################### + være mere stram når det kommer til user-agents eller på en eller anden måde sige at hvis ikke referencen er i en direkte sti med det du vil GET == DENIED
library(ipapi)
library(rvest)
library(httr)
library(stringr)
library(tidyverse)
library(countrycode)
library(sf)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(ggplot2)
                                                      
                                                      
####### must load ####
All_ip_location <- read.csv("Documents/DAL-Projects/ola4/ALLEIPLOKATIONER.csv")                                                      
hackercommands <- read.csv("Documents/DAL-Projects/ola4/get_rekt.csv")
logfiler <- read.csv("Wulf'sLogfiler.csv")
res_df <- read.csv("Documents/DAL-Projects/ola4/full country list.csv")
AllIpLocations <- read.csv("Documents/DAL-Projects/ola4/ALLEIPLOKATIONER.csv")
####################### logs fra mappe a ###########################################
##logs fra mappen A
#log <-  read.table("Downloads/a/logfiler - 13 nov/access.log")
#log1 <-  read.table("Downloads/a/logfiler - 13 nov/access1.log")
#log2 <-  read.table("Downloads/a/logfiler - 13 nov/access2.log")
#log3 <-  read.table("Downloads/a/logfiler - 13 nov/access3.log")
#log4 <-  read.table("Downloads/a/logfiler - 13 nov/access4.log")
#log5 <-  read.table("Downloads/a/logfiler - 13 nov/access5.log")
#log6 <-  read.table("Downloads/a/logfiler - 13 nov/access6.log")
#log7 <-  read.table("Downloads/a/logfiler - 13 nov/access7.log")
#log8 <-  read.table("Downloads/a/logfiler - 13 nov/access8.log")
#log9 <-  read.table("Downloads/a/logfiler - 13 nov/access9.log")
#log10 <-  read.table("Downloads/a/logfiler - 13 nov/access10.log")
#log11 <-  read.table("Downloads/a/logfiler - 13 nov/access11.log")
#log12 <-  read.table("Downloads/a/logfiler - 13 nov/access12.log")
#log13 <-  read.table("Downloads/a/logfiler - 13 nov/access13.log")
#log14 <-  read.table("Downloads/a/logfiler - 13 nov/access14.log")
#
#ip_log4 <- unique(log4$V1)
#ip_count <- table(log4$V1)
#
#get_request_log4 <- table(log4$V6)
#print(get_request_log4)
#
#
#all_log <- as.data.frame(rbind(log,
#                 log1,
#                 log2,
#                 log3,
#                 log4,
#                 log5,
#                 log6,
#                 log7,
#                 log8,
#                 log9,
#                 log10,
#                 log11,
#                 log12,
#                 log13,
#                 log14))
#
## Tildel passende kolonnenavne til dataframen
#colnames(all_log) <- c(
#  "ip_adresse",
#  "bruger_identifikation",
#  "autentificeret_bruger",
#  "tidsstempel",
#  "tidszone",
#  "anmodning",
#  "statuskode",
#  "svar_størrelse",
#  "henvisning",
#  "bruger_agent"
#)



#####logs fra mappen logs######

loglog <-  read.table("Downloads/log2/access.log")
loglog1 <-  read.table("Downloads/log2/access1.log")
loglog2 <-  read.table("Downloads/log2/access2.log")
loglog3 <-  read.table("Downloads/log2/access3.log")
loglog4 <-  read.table("Downloads/log2/access4.log")
loglog5 <-  read.table("Downloads/log2/access5.log")
loglog6 <-  read.table("Downloads/log2/access6.log")
loglog7 <-  read.table("Downloads/log2/access7.log")
loglog8 <-  read.table("Downloads/log2/access8.log")
loglog9 <-  read.table("Downloads/log2/access9.log")
loglog10 <-  read.table("Downloads/log2/access10.log")
loglog11 <-  read.table("Downloads/log2/access11.log")
loglog12 <-  read.table("Downloads/log2/access12.log")
loglog13 <-  read.table("Downloads/log2/access13.log")
loglog14 <-  read.table("Downloads/log2/access14.log")


#get_request_log4 <- table(log4$V6)
#print(get_request_log4)


logfiler <- as.data.frame(rbind(
                 loglog,
                 loglog1,
                 loglog2,
                 loglog3,
                 loglog4,
                 loglog5,
                 loglog6,
                 loglog7,
                 loglog8,
                 loglog9,
                 loglog10,
                 loglog11,
                 loglog12,
                 loglog13,
                 loglog14))

# Tildel passende kolonnenavne til dataframen
colnames(logfiler) <- c(
  "ip_adresse",
  "bruger_identifikation",
  "autentificeret_bruger",
  "tidsstempel",
  "tidszone",
  "anmodning",
  "statuskode",
  "svar_størrelse",
  "henvisning",
  "bruger_agent"
)


####ALLE LOGFILER####

#write.csv(logfiler, "Wulf'sLogfiler.csv")
logfiler <- read.csv("Wulf'sLogfiler.csv")
#logfiler <- rbind(all_log,all_loglog)

#vi tæller freq på ip'ernes besøg 
iplist <- as.data.frame(sort(table(logfiler$ip_adresse), decreasing = T))



unique(logfiler$tidsstempel) #i alt er der 15 forskellige dage



####################################################################################################
#####FILTER PÅ 404######
FIREnulFIRE <- filter(logfiler, statuskode == "404")
#vi fokuserer på "GET" requests
FirenulFireGET <- filter(FIREnulFIRE, grepl("^GET", anmodning))
#Nu vil vi gerne gransformere vores data til noget mere læsligt så vi skal gsub "GET " og " HTTP/1.1"
FirenulFireGET$anmodning <- gsub("^GET | HTTP/1.1$", "", FirenulFireGET$anmodning)
FIREnulFIRE <- FIREnulFIRE[,c(1,4,6,7,10)]

#######Vi fokuserer på POST requests######
FirenulFirePOST <- filter(FIREnulFIRE, grepl("^POST", anmodning))
#Nu vil vi gerne gransformere vores data til noget mere læsligt så vi skal gsub "GET " og " HTTP/1.1"
FirenulFirePOST$anmodning <- gsub("^GET | HTTP/1.1$", "", FirenulFirePOST$anmodning)
#da "POST" nok er enten til at prøve at bruteforce eller uploade payloads via for eksempel sql injections.



####SUSPISIOUS ACtIVITY####
suslog <- filter(logfiler, grepl("\\.git|\\.env|\\.php|admin|\\.asp|\\.aspx|\\.cfm|\\.cgi|\\.jhtml|\\.jsp|fwrite|fopen|auth|authenticator|log|\\.log|login|log-in|\\.dll", anmodning))
suslog <- suslog[,c(1,4,6,7,10)]
topsus <- as.data.frame(sort(table(suslog$ip_adresse), decreasing = TRUE))
topsus10 <- topsus[1:10,]
topsus10 <- merge(topsus10,AllIpLocations, by.x = "Var1", by.y = "query")
topsus10 <- topsus10[,-c(3,4)]
colnames(topsus10) <- c("IP","Freq","country","countryCode","region","regionName","city","zip","lat","lon","timezone","isp","org","as")
topsus10 <- topsus10[order(topsus10$Freq, decreasing = TRUE), ]
#### useragents ###
susagents <- filter(logfiler, !grepl("^Mozilla/", bruger_agent))
unique(susagents$bruger_agent) #55
unique(susagents$ip_adresse) #321
unique(logfiler$bruger_agent) #548
###### WULF? and friends??#####
WAF <- filter(logfiler, grepl("^GET /images/",anmodning))
#############################################Hvor mange providers##########
provider <- as.data.frame(sort(table(allipfreq$as), decreasing = TRUE))
colnames(provider) <- c("Provider", "Frekvens")
#############################################besøg pr dag#########
#vi gsubbber via gsub & regex - vi tager \\=[to tal, A-Z for store og små bogstaver (3) og til sidst 4 tal mellem 0-9 - "The \\1 refers to the first capture group, which is ([0-9]{2}/[A-Za-z]{3}/[0-9]{4}). This ensures only the captured date part is retained." - Chat gpt
logfiler$tidsstempel <- gsub("\\[([0-9]{2}/[A-Za-z]{3}/[0-9]{4}):.*", "\\1", logfiler$tidsstempel) 
logfiler$tidsstempel <- as.Date(logfiler$tidsstempel, format = "%d/%b/%Y")
# Tæl antallet af gange hver IP besøger pr. dag
DagligIpFreq <- as.data.frame(table(logfiler$ip_adresse, logfiler$tidsstempel))
colnames(DagligIpFreq) <- c("IP_adresse", "Dato", "Frekvens")

# vi subsætter den brugbare data (frekvens på over 0) og smider alle de ubrugbare data  ud (alle med frekvens på 0)
DagligIpFreq <- subset(DagligIpFreq, Frekvens > 0)

# Sortér efter dato og frekvens
DagligIpFreq <- DagligIpFreq[order(DagligIpFreq$Dato, -DagligIpFreq$Frekvens), ]

# Vis resultatet
print(DagligIpFreq)

# CSV
#write.csv(DagligIpFreq, "daglig_ip_freq.csv", row.names = FALSE)

################################### Tæl unikke datoer for hver IP##################################################################
# Tæl unikke datoer for hver IP
IP_over_fleredage <- aggregate(Dato ~ IP_adresse, data = DagligIpFreq, FUN = function(x) length(unique(x)))
IP_over_fleredage <- IP_over_fleredage[order(IP_over_fleredage$Dato, decreasing = TRUE), ]
# Filtrér kun IP'er, der optræder på mere end én dato
IP_over_fleredage <- subset(IP_over_fleredage, Dato > 1)

colnames(IP_over_fleredage) <- c("IP_adresse", "Antal_Dage")

print(IP_over_fleredage)
#########################optælling af hvor mange ip'er og reqs pr dag####################################
#optælling af hvor mange ip'er og reqs pr dag

# Kontrollér data for at sikre, at datoformatet er korrekt
str(DagligIpFreq)
table(DagligIpFreq$Dato)  # Tjek datoernes fordeling

# Brug split() til at opdele data pr. dag
split_data <- split(DagligIpFreq, DagligIpFreq$Dato)

# Beregn antal unikke IP'er pr. dag
Antal_IPer <- sapply(split_data, function(day_data) {
  length(unique(day_data$IP_adresse))  # Unikke IP'er for den dag
})

# Beregn samlede requests pr. dag
Total_Requests <- sapply(split_data, function(day_data) {
  sum(day_data$Frekvens)  # Summerer requests for den dag
})

# Kombiner resultaterne i en ny dataframe
DagligOpsummering <- data.frame(
  Dato = as.Date(names(Antal_IPer)),
  Antal_IPer = as.numeric(Antal_IPer),
  Total_Requests = as.numeric(Total_Requests)
)

# Kontrollér resultatet
print(DagligOpsummering)

                                                        #################################################                                                  
                                                        #                                               #                                            
                                                        #      ##################   ######!######       #                                          
                                                        #   ##################      ######!#######      #                                            
                                                        #          #====#            ######!#######     #                                            
                                                        #          #====#              ####!  ####      #                                            
                                                        #          #====#              ####!  ###       #                                          
                                                        #          #====#              ####! ###        #                                          
                                                        #          #====#              ####!###         #                                        
                                                        #          #====#              ####!#           #                                      
                                                        #    ##################        ####!            #                                      
                                                        #  ##################          ####!            #                                      
                                                        #                                               #  
                                                        #################################################                                                  



##################### API #############################################
#test af api -- Test Success!
tid <- 60/45+0.01
library(ipapi)
iplisttop10 <- as.vector(iplist[, 1])
API_ip_results <- list()
for (i in seq_along(iplisttop10)) {   
  #en cat for at følge med
  cat("Tjekker IP", i, "/", length(iplisttop10), ":", iplisttop10[i], "\n")
  result <- tryCatch({
    geolocate(iplisttop10[i], .progress = TRUE)
  }, error = function(e) {
    cat("fejl med IP:", iplisttop10[i], "-", conditionMessage(e), "\n")
    return(NULL) 
  })
  API_ip_results[[i]] <- result
  #lille pause som passer på vi ikke bryder 45 ip scan pr minut fra vores api-rate-limit
  Sys.sleep(tid)
}
All_ip_location <- do.call(rbind, lapply(API_ip_results, function(x) {
  if (!is.null(x)) as.data.frame(x) else NULL
}))



cat("Faaaaaaaar jeeeeg fæææææææærrrdiiiiiiigg", length(API_ip_results), "IP addresser.\n")

#write.csv(All_ip_location, "Documents/DAL-Projects/ola4/ALLEIPLOKATIONER.csv")
All_ip_location <- read.csv("Documents/DAL-Projects/ola4/ALLEIPLOKATIONER.csv")




####################################################################################################


####super ip scrape-loop begyndelse####
library(rvest)
library(httr)
library(stringr)
library(tidyverse)
library(countrycode)

# countrycode package magi!
countries <- countrycode::codelist$country.name.en            #countrycode::codelist$.  ################################################# kig her for webscraping fejl.
countries <- na.omit(countries)  # Her fjerner vi 0 Na'er
tilføjelser_til_countries <- c("The Netherlands" , "Hong Kong")

# kombiner lists
countries <- c(countries, tilføjelser_til_countries)


dbip <- "https://db-ip.com/"
res <- list()


for (i in 1:100) {
 
  ip <- iplist$Var1[i]
  print(paste("finder location for:", ip))

  url <- paste0(dbip, ip)

  Sys.sleep(5)
  
  response <- GET(url)
  
  # status tjek
  if (status_code(response) == 200) {
    cat("Data hentet for IP:", ip, "\n")

    page <- read_html(content(response, "text", encoding = "UTF-8"))
 
    all_info <- page %>% html_elements("td") %>% html_text(trim = TRUE)

    
    #find land med at checke om elementerne passer præcist på landenavne
    country <- all_info[all_info %in% countries]
    
    if (length(country) > 0) {
      country <- country[1]  #tager første PRÆCISE fit
    } else {
      country <- "ingen information"
    }
    
   # FJERN INFO for straight to the point  kun country liste. #info = all_info, 
    res[[ip]] <- list(ip = ip, land = country, info=all_info)
  } else {
    cat("kunne ikke finde data på: ", ip, "\n")
    res[[ip]] <- list(ip = ip, info = "Lykkedes ikke...")
  }
}


res_df <- bind_rows(lapply(res, as.data.frame))
#write_csv(res_df, "top_100_ip_info")



#nyt scrape med opdaterede countrycodes så de alle har mindst en
#write_csv(res_df, "Documents/DAL-Projects/ola4/full country list.csv")
res_df <- read.csv("Documents/DAL-Projects/ola4/full country list.csv")
AllIpLocations <- read.csv("Documents/DAL-Projects/ola4/ALLEIPLOKATIONER.csv")

################## ALL REQUESTS LOCATION #####

#først laver vi en ny df med freqs af alle vores ip'er
#vi tæller freq på ip'ernes besøg 
allipfreq <- merge(iplist, All_ip_location, by.x ="Var1", by.y = "query" )

AlleIpRequests <- allipfreq %>%
  group_by(countryCode) %>%
  summarise(totalFreq = sum(Freq, na.rm = TRUE)) %>%
  arrange(desc(totalFreq))  # Sortér efter totalFreq, faldende

# Hent verdenskortdata
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge frekvensdata med verdenskortet
world_data <- merge(world, AlleIpRequests, by.x = "iso_a2_eh", by.y = "countryCode", all.x = TRUE)

# Definer farvepalette
pal <- colorNumeric(palette = "YlOrRd", domain = world_data$totalFreq, na.color = "gray")

# Skab Leaflet-kort
leaflet(data = world_data) %>%
  addTiles() %>%
  addPolygons(
    color = "black",
    weight = 1,
    fillColor = ~pal(totalFreq),
    fillOpacity = 0.7,
    popup = ~paste(name, "<br>", "samlede requests:", totalFreq)
  ) %>%
  addLegend(
    pal = pal,
    values = world_data$totalFreq,
    title = "Total Requests",
    position = "bottomright",
    labFormat = labelFormat(suffix = " req"),
    na.label = "Ikke besøgt"
  )



################## ALLE BESØGENDE ##################

library(sf)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)

# allipunique skal kun have landekoder og antal unikke ip'er fra samme land
allipunique <- as.data.frame(table(All_ip_location$countryCode))

# Omdøb kolonner for klarhed (valgfrit)
colnames(allipunique) <- c("countryCode", "uniqueVisitors")
#faldende rækkefølge
allipunique <- allipunique[order(-allipunique$uniqueVisitors), ]

# verdens kortdata med landegrænser
world <- ne_countries(scale = "medium", returnclass = "sf")

# Slå data fra allipunique sammen med verdens kortdata
world_data <- merge(world, allipunique, by.x = "iso_a2_eh", by.y = "countryCode", all.x = TRUE)

# Erstat NA-værdier i uniqueVisitors med 0
world_data$uniqueVisitors[is.na(world_data$uniqueVisitors)] <- 0

# farveskala baseret på unikke visits, grå for 0 besøg
color_pal <- function(visitors) {
  ifelse(visitors == 0, "gray", colorNumeric(palette = "YlOrRd", domain = world_data$uniqueVisitors)(visitors))
}

# Opret Leaflet-kort med heatmap
leaflet(data = world_data) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~color_pal(uniqueVisitors),  # Farver baseret på uniqueVisitors, grå for 0
    fillOpacity = 0.7,
    color = "black",
    weight = 1,
    popup = ~paste(name, "<br>", "Unikke besøgende:", uniqueVisitors)  # Popup med landnavn og unikke besøgende
  ) %>%
  addLegend(
    pal = colorNumeric(palette = "YlOrRd", domain = world_data$uniqueVisitors),
    values = ~uniqueVisitors,
    title = "Unikke besøgende",
    position = "bottomright"
  )



################### GGPLOTS ###################
library(ggplot2)
######################Unikke besøgende på land #####
#lav top 10 i stigende 
top10_allipunique <- head(allipunique[order(-allipunique$uniqueVisitors), ], 10)

# Plot top 10 lande med forskellige farver for hvert land
ggplot(top10_allipunique, aes(x = reorder(countryCode, -uniqueVisitors), y = uniqueVisitors, fill = countryCode)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Beach boys havde ret: everybody is surfing in the USA",
    x = "Landekoder",
    y = "Unikke besøgende"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 
###################SAMLEDE REQUESTS på land#####

top10_alleipRequests <- head(AlleIpRequests[order(-AlleIpRequests$totalFreq), ], 10)

# Plot top 10 lande med forskellige farver for hvert land
ggplot(top10_alleipRequests, aes(x = reorder(countryCode, -totalFreq), y = totalFreq, fill = countryCode)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Vesten siger at russerne pløjer gennem internettet, men vores data siger noget andet",
    x = "Landekoder",
    y = "Requests"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 

######### unique ip pr dag#####
#start med at fjerne as.data på vores date - de bliver as.factor
DagligOpsummering$Dato <- as.factor(DagligOpsummering$Dato)

ggplot(DagligOpsummering, aes(x = Dato, y = Antal_IPer, fill = Dato)) +
  geom_bar(stat = "identity") +
  labs(
    title = "start november tager guldet hjem, med unikke besøgende",
    x = "Datoer",
    y = "Unikke besøgende"
  ) +
  theme_minimal() +
  theme( axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none") 

######### total requests pr dag######

ggplot(DagligOpsummering, aes(x = Dato, y = Total_Requests, fill = Dato)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Hackerne skal have penge inden juletid",
    x = "Datoer",
    y = "Antal requests"
  ) +
  theme_minimal() +
  theme( axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none") 

#### plot providers top 10 ####
provider10 <- provider[1:10,]
ggplot(provider10, aes(x = Provider, y = Frekvens, fill = Provider)) +
  geom_bar(stat = "identity") +
  labs(
    title = "DigitalOcean,LLC er mere end doubelt så populært end google, som provider",
    x = "Providers",
    y = "Antal brugere som har besøgt talmedos.com"
  ) +
  theme_minimal() +
  theme( axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none") 

#### mest request ip ####
iptop10 <- iplist[1:10,]
ggplot(iptop10, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Overblik over internet-tratik fra top 10 request-frekvenser",
    x = "Ip addresser",
    y = "Samlede requests i logfilerne"
  ) +
  theme_minimal() +
  theme( axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none") 

######## flest dage i træk ####
ipoverfleredage10 <- IP_over_fleredage[1:10,]
ggplot(ipoverfleredage10, aes(x = IP_adresse, y = Antal_Dage, fill = IP_adresse)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Overblik over tilbagevendende IP'er over flere dage.",
    x = "Ip addresser",
    y = "Antal dage de har besøgt siden"
  ) +
  theme_minimal() +
  theme( axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none") 



###################################### Sådan gør de#####
#strings af onde requests
hackercommands <- read.csv("Documents/DAL-Projects/ola4/get_rekt.csv")
hackercommands$bla <- NA
hackercommands <- hackercommands[[1]]
hackercommands <- gsub("^GET /| HTTP/1.1$", "", hackercommands)


#hackercommands <- as.data.frame(hackercommands)


url <- "https://koldinghundemad.dk/"

for (i in 1:length(hackercommands)) {
  offer_url <- paste0(url,hackercommands[i])
  res <- GET(offer_url)
  if (status_code(res) == 200 ){
    cat("det ligner der er en dør som burde være lukket på: ",hackercommands[i]," !","\n")
  }else cat(hackercommands[i]," virkede ikke :(","\n")
  Sys.sleep(1)
}




### FLAGS####

109.74.204.123
