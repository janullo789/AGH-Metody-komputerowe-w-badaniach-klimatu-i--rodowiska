#wczytanie pakietow
library(httr)
library(jsonlite)
library(utils)
library(sp)
library(sf)
library("spatstat")


#wczytanie klucza API z pliku
kluczAPI <- readLines("keyAPI.txt")

#pobranie danych o czujnikach w odleg�o�ci 15km od ratusza 
r <- GET("https://airapi.airly.eu/v2/installations/nearest?lat=50.0617022&lng=19.9373569&maxDistanceKM=15&maxResults=-1",
         add_headers(apikey = kluczAPI, Accept = "application/json")
)

#przej�cie do listy
jsonRespText<-content(r,as="text")
test15<-fromJSON(jsonRespText)
#View(test15)

#tworzymy ramk� data15 - z danymi o lokalizacji, wysoko�ci i id czjnik�w
longitude<-test15$location$longitude
latitude<-test15$location$latitude
data15<-data.frame(longitude,latitude)
data15$elevation<-test15$elev
data15$id<-test15$id

head(data15)

#tworzymy obiekt przestrzenny
data_spat<-data.frame(lon=data15$longitude,lat=data15$latitude,elev=data15$elev,id=data15$id)
coordinates(data_spat) <- ~lon+lat #okre�lamy, kt�re elementy to koordynaty (potrzebne do ppp)
proj4string(data_spat) <- CRS("+proj=longlat +datum=WGS84") #okre�lamy, jaki mamy uk�ad
data_spat # mamy ju� obiekt w uk�adzie sferycznym, kt�ry mo�na automatycznie 

#konwersja do UTM (bo tworzymy ppp, a to jego uk�ad)
data_UTM <- spTransform(data_spat, CRS("+proj=utm +zone=34 +datum=WGS84"))

dzielnice<-st_read("dzielnice_Krakowa/dzielnice_Krakowa.shp") #uk�ad odniesienia(CRS) to ETRS89 (Poland CS92)
# konwertujemy do WGS84
dzielniceWGS84<-st_transform(dzielnice,crs = 4326) # "4326" to kod dla WGS84
# zostawiamy tylko kontur miasta 
krakowWGS84<-st_union(dzielniceWGS84)
#przekszta�camy na UTM
krakowUTM<-st_transform(krakowWGS84,CRS("+proj=utm +zone=34 +datum=WGS84"))

data15_ppp_id<-ppp(x=data_UTM$lon,y=data_UTM$lat,marks=data.frame(elev=data_UTM$elev,id=data_UTM$id),window=as.owin(krakowUTM))
data15_ppp_id$marks$id #mamy od razu tylko te id kt�re s� w Krakowie!

#najpierw musimy utworzy�:
##1) dwa obiekty zawieraj�ce:
###liczb� czujnik�w
n_id<-length(data15_ppp_id$marks$id)
n_id
###id czujnik�w
id<-data15_ppp_id$marks$id
id
##2) pust� list� do odczyt�w z czujnik�w (installations) AIRLY 
list_instDzien1Rano <- vector(mode = "list", length = n_id) #funkcja do stworzenia struktury danych
list_instDzien1Poludnie <- vector(mode = "list", length = n_id)
list_instDzien1Wieczor <- vector(mode = "list", length = n_id)

list_instDzien2Rano <- vector(mode = "list", length = n_id)
list_instDzien2Poludnie <- vector(mode = "list", length = n_id)
list_instDzien2Wieczor <- vector(mode = "list", length = n_id)

list_instDzien3Rano <- vector(mode = "list", length = n_id)
list_instDzien3Poludnie <- vector(mode = "list", length = n_id)
list_instDzien3Wieczor <- vector(mode = "list", length = n_id)

for (i in seq(1,n_id)) {
  
  print(i) #to tylko pomocniczo, �eby wiedzie�, kt�ry obr�t p�tli
  #tworzymy ci�g znak�w okre�lajacy adres, pod k�trym znajduj� si� pomiary z czujnika
  str<-paste("https://airapi.airly.eu/v2/measurements/installation?installationId=",id[i],sep="")
  #pobieramy dane z adresu
  r <- GET(url=str,add_headers(apikey = kluczAPI, Accept = "application/json"))
  #przechodzimy z formatu r na json i z json na tekst
  jsonRespText<-content(r,as="text")
  inst<-fromJSON(jsonRespText)
  
  list_instDzien1Rano<-inst #tutaj zmieniamy zmienn� do zapisu
  
}
#koniec p�tli

#zapis pe�nej listy do pliku (na wszelki wypadek, bo mamy tylko 100 zapyta� dziennie do AIRLY
save(list_instDzien1Rano,file="saves/list_instDzien1Rano.Rdata") #tutaj tez zmieniamy zmienn� do zapisu