#wczytanie pakietow
library(httr)
library(jsonlite)
library(utils)
library(sp)
library(sf)
library(spatstat)
library(maptools)
library(automap)
library(rgdal)
library(raster)
library(rgeos)
library(gstat)
library(tmaptools)




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
data15$elevation<-test15$elev #wysoko�� nie bedzie potrzebna, ale niech b�dzie dla przyk�adu
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

data15_ppp<-ppp(x=data_UTM$lon,y=data_UTM$lat,window=as.owin(krakowUTM))
plot(data15_ppp)

data15_ppp_e<-ppp(x=data_UTM$lon,y=data_UTM$lat,marks=data_UTM$elev,window=as.owin(krakowUTM))
plot(data15_ppp_e)

#najpierw musimy utworzy�:
##1) dwa obiekty zawieraj�ce:
###liczb� czujnik�w
n_id<-length(data15_ppp_id$marks$id)
n_id
###id czujnik�w
id<-data15_ppp_id$marks$id
id
##2) pust� list� do odczyt�w z czujnik�w (installations) AIRLY 
list_instDzien1Rano <- vector(mode = "list", length = n_id) #18.12.2022 9:02 7km/h zach
list_instDzien1Poludnie <- vector(mode = "list", length = n_id) #18.12.2022 17:04 7km/h wsch
list_instDzien1Wieczor <- vector(mode = "list", length = n_id) #18.12.2022 20.08 13 km/h 	p�n. wsch

list_instDzien2Rano <- vector(mode = "list", length = n_id) # 19.12.2022 8:20 5km/h wsch
list_instDzien2Poludnie <- vector(mode = "list", length = n_id) #19.12.2022 16:40  6km/h p�n, wsch.
list_instDzien2Wieczor <- vector(mode = "list", length = n_id) #19.12.2022 20:45 4 km/g p�n. wsch

list_instDzien3Rano <- vector(mode = "list", length = n_id) #20.12.2022 9:20 7km/h pd. zach
list_instDzien3Poludnie <- vector(mode = "list", length = n_id) #20.12.2022 16:20 2km/h zach
list_instDzien3Wieczor <- vector(mode = "list", length = n_id) #20.12.2022 20:45 9 km/h zach

for (i in seq(1,n_id)) {
  
  print(i) #to tylko pomocniczo, �eby wiedzie�, kt�ry obr�t p�tli
  #tworzymy ci�g znak�w okre�lajacy adres, pod k�trym znajduj� si� pomiary z czujnika
  str<-paste("https://airapi.airly.eu/v2/measurements/installation?installationId=",id[i],sep="")
  #pobieramy dane z adresu
  r <- GET(url=str,add_headers(apikey = kluczAPI, Accept = "application/json"))
  #przechodzimy z formatu r na json i z json na tekst
  jsonRespText<-content(r,as="text")
  inst<-fromJSON(jsonRespText)
  
  list_instDzien3Wieczor[[i]]<-inst #tutaj zmieniamy zmienn� do zapisu
  
}
#koniec p�tli

#zapis pe�nej listy do pliku (na wszelki wypadek, bo mamy tylko 100 zapyta� dziennie do AIRLY
#save(list_instDzien3Wieczor,file="saves/list_instDzien3Wieczor.Rdata") #tutaj tez zmieniamy zmienn� do zapisu

load(file="saves/list_instDzien3Poludnie.Rdata")

list_inst2<-list_instDzien3Poludnie #tutaj tez zmieniamy zmienn�

#teraz wybieramy potrzebne dane
##tworzymy pusty wektor dla danych "current"
current<-rep(NA,n_id)

##p�tla do "wyci�gni�cia" warto�ci "current"
for (i in seq(1,n_id)) {
  
  print(i)
  
  logic<-list_inst2[[i]]$current$values$name=="PM25" #zmienna logiczna do wyszukania p�l o nazwie "PM25"
  
  if (sum(logic)==1) #testujemy, czy istnieje jedno i tylko jedno takie pole (zdarzaj� si� b��dne odczyty - tych nie chcemy zapisa�)
    current[i]<-list_inst2[[i]]$current$values[logic,2] 
}

current

data15_spdf<-as.SpatialPointsDataFrame.ppp(data15_ppp_id)
coordinates(data15_spdf)
# dodajemy kolumn� current
data15_spdf$current<-current
dev.off() #bo mo�e wariowa� RStudio
plot(data15_spdf)

miss <- is.na(data15_spdf$current)

pm25_auto <- autoKrige(current ~ 1, input_data = data15_spdf[!miss,])
plot(pm25_auto$krige_output[1],main="PM 2.5")
points(data15_ppp_id[!miss,],pch="*",col="White")
plot(Window(data15_ppp_e),add=TRUE)

plot(pm25_auto)

#zmie�my model i por�wnajmy wyniki, popatrzmy na wariogram
pm25_auto <- autoKrige(current ~ 1, input_data = data15_spdf[!miss,], model="Gau")
plot(pm25_auto$krige_output[1],main="PM 2.5")
points(data15_ppp_id[!miss,],pch="*",col="White")
plot(pm25_auto)

pm25_auto <- autoKrige(current ~ 1, input_data = data15_spdf[!miss,],
                       model="Gau")
show.vgms()
show.vgms(models=c('Nug', 'Sph', 'Gau', 'Pow', 'Exp'), range=1.4,
          max=2.5)

##�adna mapa

#Musimy mied kontur Krakowa w odpowiednim formacie:
bound<-st_as_sf(krakowUTM)
plot(bound)

#Pobieramy wsp�rz�dne punkt�w konturu w formie macierzy:
coord<-as.data.frame(st_coordinates(krakowUTM))

#Najpierw utworzymy siatk� - prostok�t okalaj�cy kontur Krakowa:
#1. Okre�lamy wsp�rz�dne naro�y
left_down<-c( min(coord$X), min(coord$Y))
right_up<-c( max(coord$X), max(coord$Y))
#2. Ustalamy rozmiar oczka siatki (100x100 metr�w)
size<-c(100,100)
#3. Obliczamy liczb� oczek siatki przypadj�cych na d�ugo�d i szeroko�d prostok�ta:
points<- (right_up-left_down)/size
num_points<-ceiling(points) #zaokr�glenie w g�r�
#4. Wreszcie tworzymy siatk�
grid <- GridTopology(left_down, size,num_points)
#5. �i konwertujemy j� do odpowiedniego formatu, w odpowiednim uk�adzie (tu: WGS84)
gridpoints <- SpatialPoints(grid, proj4string = CRS("+proj=utm +zone=34
+datum=WGS84"))
plot(gridpoints) #czekamy cierpliwie
#Teraz przycinamy utworzon� siatk� konturem Krakowa funkcj� crop_shape z pakietu tmaptools
g<-st_as_sf(gridpoints)#konwersja do formatu na kt�rym dzia�a crop_shape
cg<-crop_shape(g,bound,polygon = TRUE)
spgrid <- SpatialPixels(as_Spatial(cg)) #konwersja z powrotem do st i
#nast�pnie do SpatialPixels
plot(spgrid)

#Rysujemy mape z wykorzystaniem krigingu:
##uwaga: �current� zamiast �marks�!
elev_auto <- autoKrige(current ~ 1, input_data =
                             data15_spdf[!miss,],new_data=spgrid)
plot(elev_auto$krige_output[1],main="PM 2.5")
points(data15_ppp_id[!miss,],pch="*",col="White")

plot(elev_auto)









###### temperatura (skopiowane co jest wyzej z podmienionymi danymi)
for (i in seq(1,n_id)) {
  
  print(i)
  
  logic<-list_inst2[[i]]$current$values$name=="TEMPERATURE" #zmienna logiczna do wyszukania p�l o nazwie "PM25"
  
  if (sum(logic)==1) #testujemy, czy istnieje jedno i tylko jedno takie pole (zdarzaj� si� b��dne odczyty - tych nie chcemy zapisa�)
    current[i]<-list_inst2[[i]]$current$values[logic,2] 
}

current

data15_spdf<-as.SpatialPointsDataFrame.ppp(data15_ppp_id)
coordinates(data15_spdf)
# dodajemy kolumn� current
data15_spdf$current<-current
dev.off() #bo mo�e wariowa� RStudio
plot(data15_spdf)

miss <- is.na(data15_spdf$current)

pm25_auto <- autoKrige(current ~ 1, input_data = data15_spdf[!miss,])
plot(pm25_auto$krige_output[1],main="TEMP")
points(data15_ppp_id[!miss,],pch="*",col="White")
plot(Window(data15_ppp_e),add=TRUE)

plot(pm25_auto)

#zmie�my model i por�wnajmy wyniki, popatrzmy na wariogram
pm25_auto <- autoKrige(current ~ 1, input_data = data15_spdf[!miss,], model="Gau")
plot(pm25_auto$krige_output[1],main="TEMP")
points(data15_ppp_id[!miss,],pch="*",col="White")
plot(pm25_auto)

pm25_auto <- autoKrige(current ~ 1, input_data = data15_spdf[!miss,],
                       model="Gau")
show.vgms()
show.vgms(models=c('Nug', 'Sph', 'Gau', 'Pow', 'Exp'), range=1.4,
          max=2.5)

##�adna mapa

#Musimy mied kontur Krakowa w odpowiednim formacie:
bound<-st_as_sf(krakowUTM)
plot(bound)

#Pobieramy wsp�rz�dne punkt�w konturu w formie macierzy:
coord<-as.data.frame(st_coordinates(krakowUTM))

#Najpierw utworzymy siatk� - prostok�t okalaj�cy kontur Krakowa:
#1. Okre�lamy wsp�rz�dne naro�y
left_down<-c( min(coord$X), min(coord$Y))
right_up<-c( max(coord$X), max(coord$Y))
#2. Ustalamy rozmiar oczka siatki (100x100 metr�w)
size<-c(100,100)
#3. Obliczamy liczb� oczek siatki przypadj�cych na d�ugo�d i szeroko�d prostok�ta:
points<- (right_up-left_down)/size
num_points<-ceiling(points) #zaokr�glenie w g�r�
#4. Wreszcie tworzymy siatk�
grid <- GridTopology(left_down, size,num_points)
#5. �i konwertujemy j� do odpowiedniego formatu, w odpowiednim uk�adzie (tu: WGS84)
gridpoints <- SpatialPoints(grid, proj4string = CRS("+proj=utm +zone=34
+datum=WGS84"))
plot(gridpoints) #czekamy cierpliwie
#Teraz przycinamy utworzon� siatk� konturem Krakowa funkcj� crop_shape z pakietu tmaptools
g<-st_as_sf(gridpoints)#konwersja do formatu na kt�rym dzia�a crop_shape
cg<-crop_shape(g,bound,polygon = TRUE)
spgrid <- SpatialPixels(as_Spatial(cg)) #konwersja z powrotem do st i
#nast�pnie do SpatialPixels
plot(spgrid)

#Rysujemy mape z wykorzystaniem krigingu:
##uwaga: �current� zamiast �marks�!
elev_auto <- autoKrige(current ~ 1, input_data =
                         data15_spdf[!miss,],new_data=spgrid)
plot(elev_auto$krige_output[1],main="TEMP")
points(data15_ppp_id[!miss,],pch="*",col="White")

plot(elev_auto)


##awaryjne api
#dhDFuJimjc0l6eOg9r1XFPVcYimG8Ynx
#a7R8xdsR7QhnnDc3nojkndhw3EWAEKaw
#Nvw5QZwC9WlItbR7qYOJW2PMwLHVrfdF
