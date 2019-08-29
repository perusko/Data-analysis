# INSTALACIJA PAKETA
#install.packages("devtools")
#install.packages("maptools")
#install.packages("rgdal")
#install.packages("RColorBrewer")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("Hmisc")
#install.packages("sp")
#install.packages("plotrix")
#install.packages("vioplot")
#install.packages("scatterplot3d")
#install.packages("mapview")
#install.packages("raster")
#install.packages("foreign")
#install.packages("tmap")
#install.packages("foreign")
#install.packages("tmaptools")
#install.packages("mapproj")

sessionInfo()
#R version 3.6.1 (2019-07-05)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 17763)

#Matrix products: default

#locale:
#[1] LC_COLLATE=Croatian_Croatia.1250  LC_CTYPE=Croatian_Croatia.1250    LC_MONETARY=Croatian_Croatia.1250
#[4] LC_NUMERIC=C                      LC_TIME=Croatian_Croatia.1250    
#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods   bas

if (!require("rspatial")) devtools::install_github('rspatial/rspatial')

#UČITAVANJE SHP PODATAKA
myFile<-choose.files()# učitavanje SHP datoteke opcine-gradovi
mySHP<- readOGR(myFile)# čitanje objekata unutar SHP file-a

## OGR data source with driver: ESRI Shapefile
## Source: "C:\Users\iperusko\Documents\IIP\Opcine_gradovi.shp", layer: "Opcine_gradovi"
## with 556 features
## It has 8 fields

class(mySHP)
mode(mySHP)
length(mySHP)
mySHP2<-fortify(mySHP)
class(mysHP2)
head(mySHP2)
dim(mySHP2)
names(mySHP2)

#VIZUALIZACIJA SHP UČITANIH PODTAKA
library(ggplot2)
ggplot(data = mySHP2, aes(x=long, y=lat, group=group))+geom_path()

#UČITAVANJE DBF PODATAKA
library(foreign)
myFile2<-choose.files()
myDBF <-read.dbf(myFile2)
class(myDBF)
dim(myDBF)
head(myDBF)
#OBJECTID OG_MB     OG_NAZIV OG_STATUS ZUP_RB                     ZUP_NAZIV Shape_Leng Shape_Area
#1        1 00019 ANDRIJASEVCI         O     16 Vukovarsko-srijemska zupanija   29596.05   39775425
#2        2 00027    ANTUNOVAC         O     14    Osjecko-baranjska zupanija   41539.50   57375727
#3        3 00035 BABINA GREDA         O     16 Vukovarsko-srijemska zupanija   48043.66   76035500
#4        4 00043        BAKAR         G      8   Primorsko-goranska zupanija   67224.81  125341617
#5        5 00051         BALE         O     18             Istarska zupanija   41091.99   82064208
#6        6 00060       BARBAN         O     18             Istarska zupanija   58935.11   90174848
myFile3<-choose.files()
myDBF2 <-read.dbf(myFile3)
class(myDBF2)
dim(myDBF2)
head(myDBF2)
#OG_MB BrojStan Kucanstva StObjekti Dolasci Nocenja BrojDana
#1 00019     4116      1313      1482       0       0      0.0
#2 00027     3704      1197      1281       0       0      0.0
#3 00035     3585      1129      1324       0       0      0.0
#4 00043     8254      3003      3647    3128   19234      6.1
#5 00051     1129       415       493   52047  313590      6.0
#6 00060     2715       961      1257   11967  100853      8.4

#sPAJANJE DBF PODATAKA
DBF<-merge(myDBF,myDBF2,
                      by="OG_MB")
class(DBF)
dim(DBF)
head(DBF)

#sPAJANJE SHP i DBF PODATAKA i VIZUALIZACIJA ISTIH UZ PAKET MAPVIEW
m100 <- merge(mySHP, DBF, by='OG_MB')
library(mapview)
mapview(m100)
m300 <- merge(mySHP, OG300, by='OG_MB')
mapview(m300)
m300 <- merge(mySHP101, OG300, by='OG_MB')
mapview(m300, zcol = "ZUP_NAZIV")
mapview(m300, zcol = "BrojDana", at = seq(0, 10, 2), legend = TRUE)
mapview(m300, zcol = "Kuc/Obj", at = seq(0.25, 1.05, 0.2), legend = TRUE)
mapview(m300, zcol = "St/Obj", at = seq(0.75, 3.75, 0.6), legend = TRUE)

library(RColorBrewer)


mapviewOptions(vector.palette = colorRampPalette(brewer.pal(3, "Dark2")),
               na.color = "white")
mapview(m300, zcol = "Nocenja", at = seq(50000, 4100000),legend = TRUE)
mapview(m300, zcol = "Nocenja", breaks = c(0, 5, 20, 100,300) * 10000,legend = TRUE)

#VIZUALIZACIJA  UZ PAKET TMAP 

library(tmap)
tm_shape(m300)+tm_fill()+tm_borders()

tm_shape(m300) + tm_polygons(col = "ZUP_NAZIV", style = "cat")
tm_shape(m300) + tm_polygons(col = "BrojDana", style = "equal")
tm_shape(m300) + tm_polygons(col = "St/Kuc", style = "pretty")
tm_shape(m300) + tm_polygons(col = "BrojStan", style = "quantile")
tm_shape(m300) + tm_polygons(col = "BrojStan", style = "jenks")

#interaktivna karta

tm_shape(m300) + tm_text(text = "OG_NAZIV")+tm_polygons(col = "BrojDana", style = "equal")
tmap_mode("view")


#Podaci za Istru
Istra = subset(m300, ZUP_NAZIV=="Istarska zupanija")
tm_shape(Istra)+tm_fill()+tm_borders()
tm_shape(Istra) + tm_text(text = "OG_NAZIV")+tm_polygons(col = "BrojStan", style = "quantile")
tm_shape(Istra) + tm_text(text = "OG_NAZIV")+tm_polygons(col = "Nocenja", style = "quantile")
tm_shape(Istra) + tm_text(text = "OG_NAZIV")+tm_polygons(col = "BrojDana", style = "quantile")

#izračun prostornog centroida za istarske općine
library(rgeos)
trueCentroids = gCentroid(Istra,byid =TRUE)
mapView(Istra)
points(coordinates(Istra),pch=1)
points(trueCentroids,pch=2)
mapView(Istra)+mapView(trueCentroids,col.regions = "red")

#izračun prostornog centroida za istarsku županiju
trueCentroids = gCentroid(Istra,byid =FALSE)
mapView(Istra)
points(coordinates(Istra),pch=1)
points(trueCentroids,pch=2)
mapView(Istra)+mapView(trueCentroids,col.regions = "red")



