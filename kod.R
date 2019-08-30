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

library(rgdal)


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
m300 <- merge(mySHP, OG300, by='OG_MB')
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


#IZRADA PIVOT TABILCE KAKO BI DOBILI ZBIRNE PODATKE PO ZUPANIJAMA

library(dplyr)
library(tidyr)
pivotZupanije<-OG %>%
  select(ZUP_NAZIV,Nocenja) %>%
  group_by(ZUP_NAZIV) %>%
  summarise(TotalNocenja=sum(Nocenja))

#PODACI PO ŽUPANIJA, MIN, MAX NOĆENJA
summary(pivotZupanije)

         #TotalNocenja     
         #Min.   :   12370  
         #1st Qu.:   94842  
         #Median :  193668  
         #Mean   : 4259482  
         #3rd Qu.: 5511680  
         #Max.   :26184842 

#SORTIRANJE PODATAKA
pivotZupanije[order(pivotZupanije$TotalNocenja),]
pivotZupanije[rev(order(pivotZupanije$TotalNocenja)),]

#ZUP_NAZIV TotalNocenja
#5                Istarska zupanija     26184842
#16   Splitsko-dalmatinska zupanija     17506367
#13     Primorsko-goranska zupanija     15282520
#20               Zadarska zupanija      9587238
#3  Dubrovacko-neretvanska zupanija      8047090
#14       Sibensko-kninska zupanija      5511680
#9           Licko-senjska zupanija      2749230
#4                      Grad Zagreb      2511817
#6              Karlovacka zupanija       607712
#8      Krapinsko-zagorska zupanija       351921
#11      Osjecko-baranjska zupanija       193668
#21             Zagrebacka zupanija       191340
#10            Medjimurska zupanija       180633
#17            Varazdinska zupanija       134355
#19   Vukovarsko-srijemska zupanija       128545
#15     Sisacko-moslavacka zupanija        94842
#1  Bjelovarsko-bilogorska zupanija        69874
#2        Brodsko-posavska zupanija        45303
#12      Pozesko-slavonska zupanija        35274
#7  Koprivnicko-krizevacka zupanija        22491
#18  Viroviticko-podravska zupanija        12370



#PRIKAZ  NOĆENJA PO ŽUPANIJAMA
library(plotrix)
pie3D(pivotZupanije[[2]],labels=pivotZupanije[[1]],
      explode=0.1,main="Udio noćenja po županijama")

#vELIK BROJ ŽUPANIJA, PRIKAZUJE SE NAZIV I POSTAtAK SAMO ZA ISTRU
postotak <- round(pivotZupanije$TotalNocenja/sum(pivotZupanije$TotalNocenja),2)
postotak[postotak<0.2]<-0
postotak <- paste0(postotak*100, "%")
postotak[grep("0%", postotak)] <- ""


library(plotly)
plot_ly(pivotZupanije, 
        labels = ~ZUP_NAZIV,  
        values = ~TotalNocenja,  
        type = "pie",
        text = postotak,
        textposition = "inside",
        textinfo = "text"
)

#HISTOGRAMI NOĆENJA PO ŽUPANIJAMA
library(dplyr)
ggbarplot(pivotZupanije, x = "ZUP_NAZIV", y = "TotalNocenja",
          fill = "lightgray", 
          xlab = "ŽUPANIJA", ylab = "BROJ NOĆENJA",
          sort.val = "desc", 
          x.text.angle = 45  
)

ggbarplot(pivotZupanije, x = "ZUP_NAZIV", y = "TotalNocenja",
          fill = "red", 
          xlab = "ŽUPANIJA", ylab = "BROJ NOĆENJA",
          label = "TotalNocenja",
          sort.val = "desc", 
          top = 8,          
          x.text.angle = 45  
)
#HISTOGRAMI NOĆENJA PO OPĆINAMA
ggbarplot(OG, x = "OG_NAZIV", y = "Nocenja",
          fill = "red", 
          xlab = "OPĆINA", ylab = "NOĆENJA",
          label = "Nocenja",
          sort.val = "desc", 
          top = 15,          
          x.text.angle = 45  
)

#PRIKAZ  BROJA DOLAZAKA TURSISTA  PO ŽUPANIJAMA
#PIVOT
library(dplyr)
library(tidyr)
pivotDolasci<-OG %>%
  select(ZUP_NAZIV,Dolasci) %>%
  group_by(ZUP_NAZIV) %>%
  summarise(TotalDolasci=sum(Dolasci))

#vELIK BROJ ŽUPANIJA, PRIKAZUJE SE NAZIV I POSTAtAK SAMO ZA ISTRU
postotak1 <- round(pivotDolasci$TotalDolasci/sum(pivotDolasci$TotalDolasci),2)
postotak1[postotak1<0.05]<-0
postotak1 <- paste0(postotak1*100, "%")
postotak1[grep("0%", postotak1)] <- ""


library(plotly)
plot_ly(pivotDolasci, 
        labels = ~ZUP_NAZIV,  
        values = ~TotalDolasci,  
        type = "pie",
        text = postotak1,
        textposition = "inside",
        textinfo = "text"
)

#HISTOGRAMI DOLAZAKA PO ŽUPANIJAMA
library(dplyr)
ggbarplot(pivotDolasci, x = "ZUP_NAZIV", y = "TotalDolasci",
          fill = "lightgray", 
          xlab = "ŽUPANIJA", ylab = "BROJ DOLAZAKA",
          sort.val = "desc", 
          x.text.angle = 45  
)

ggbarplot(pivotDolasci, x = "ZUP_NAZIV", y = "TotalDolasci",
          fill = "red", 
          xlab = "ŽUPANIJA", ylab = "BROJ DOLAZAKA",
          label = "TotalDolasci",
          sort.val = "desc", 
          top = 8,          
          x.text.angle = 45  
)

#HISTOGRAM DOLAZAKA PO OPĆINAMA
ggbarplot(OG, x = "OG_NAZIV", y = "Dolasci",
          fill = "red", 
          xlab = "OPĆINA", ylab = "DOLASCI",
          label = "Dolasci",
          sort.val = "desc", 
          top = 15,          
          x.text.angle = 90  
)

#sPAJANJE TABLICA KAKO BI DOBILI PROSJEČN BROJ DANA BORAVKA PO ŽUPANIJAMA

BrojDana <- merge(pivotZupanije, pivotDolasci, by = "ZUP_NAZIV", all = TRUE)

BrojDana<-BrojDana %>% 
  mutate(BrojDana = TotalNocenja/TotalDolasci,options(digits=1))

#ZUP_NAZIV TotalNocenja TotalDolasci BrojDana
#1  Bjelovarsko-bilogorska zupanija        69874        20068  3.5
#2        Brodsko-posavska zupanija        45303        26863  1.7
#3  Dubrovacko-neretvanska zupanija      8047090      2013577  4.0
#4                      Grad Zagreb      2511817      1400201  1.8
#5                Istarska zupanija     26184842      4350195  6.1
#6              Karlovacka zupanija       607712       353028  1.7
#7  Koprivnicko-krizevacka zupanija        22491        11892  1.9
#8      Krapinsko-zagorska zupanija       351921       154770  2.3
#9           Licko-senjska zupanija      2749230       789330  3.5
#10            Medjimurska zupanija       180633        73495  2.5
#11      Osjecko-baranjska zupanija       193668        98514  2.0
#12      Pozesko-slavonska zupanija        35274        15806  2.2
#13     Primorsko-goranska zupanija     15282520      2909581  5.3
#14       Sibensko-kninska zupanija      5511680       965089  5.7
#15     Sisacko-moslavacka zupanija        94842        36807  2.6
#16   Splitsko-dalmatinska zupanija     17506367      3467926  5.0
#17            Varazdinska zupanija       134355        52364  2.6
#18  Viroviticko-podravska zupanija        12370         4753  2.6
#19   Vukovarsko-srijemska zupanija       128545        78735  1.6
#20               Zadarska zupanija      9587238      1664144  5.8
#21             Zagrebacka zupanija       191340       116159  1.6


ggbarplot(BrojDana, x = "ZUP_NAZIV", y = "BrojDana",
          fill = "red", 
          xlab = "ŽUPANIJA", ylab = "BROJ DANA",
          label = TRUE,lab.pos = c("in"),lab.nb.digits = 1,lab.col = "white",
          sort.val = "desc", 
          x.text.angle = 90  
)

ggbarplot(BrojDana, x = "ZUP_NAZIV", y = "BrojDana",
          fill = "red", 
          xlab = "ŽUPANIJA", ylab = "BROJ DANA",
          label = TRUE,lab.pos = c("in"),lab.nb.digits = 2,lab.col = "white",
          sort.val = "desc", 
          top = 8,          
          x.text.angle = 45  
)

#HISTOGRAM BROJA DANA PO OPĆINAMA
ggbarplot(OG, x = "OG_NAZIV", y = "BrojDana",
          fill = "red", 
          xlab = "OPĆINA", ylab = "BROJ DANA",
          label = TRUE,lab.pos = c("in"),lab.nb.digits = 2,lab.col = "white",
          sort.val = "desc", 
          top = 15,          
          x.text.angle = 90,  
)
#HISTOGRAM BROJ NOĆENJA PO STANOVNIKU
ggbarplot(OG, x = "OG_NAZIV", y = "Nocenja_Stanovnika",
          fill = "red", 
          xlab = "OPĆINA", ylab = "NOĆENJA PO STANOVNIKU",
          label = TRUE,lab.pos = c("in"),lab.nb.digits = 0,lab.col = "white",
          sort.val = "desc", 
          top = 15,          
          x.text.angle = 90,  
)
#HISTOGRAM BROJ DOLAZAKA PO STANOVNIKU
ggbarplot(OG, x = "OG_NAZIV", y = "Dolazaka_Stanovnika",
          fill = "red", 
          xlab = "OPĆINA", ylab = "DOLASKA PO STANOVNIKU",
          label = TRUE,lab.pos = c("in"),lab.nb.digits = 0,lab.col = "white",
          sort.val = "desc", 
          top = 15,          
          x.text.angle = 90,  
)

#LINIJE IZMEĐU SUSJEDNIH POLIGONA
library(raster)
library(spdep)
Istra_shp = subset(mySHP, ZUP_NAZIV=="Istarska zupanija")

wr <- poly2nb(Istra_shp, row.names=Istra_shp$OG_NAZIV, queen=FALSE)
wr
##Neighbour list object:
##Number of regions: 41 
##Number of nonzero links: 186 
##Percentage nonzero weights: 11 
##Average number of links: 5
wm <- nb2mat(wr, style='B', zero.policy = TRUE)
dim(wm)
## [1] 41 41
i <- rowSums(wm)
i
##BALE             BARBAN            BRTONIGLA           BUJE              BUZET           CEROVLJE 
## 4                  7                 5                  3                 6               6 
##GRACISCE         GROZNJAN          KANFANAR            KRSAN             LABIN           LANISCE 
## 5                  6                 6                  5                 3               2 
##LIZNJAN          LUPOGLAV          MARCANA             MEDULIN           MOTOVUN         NOVIGRAD 
## 3                  4                 5                  2                 6               3 
##OPRTALJ          PAZIN             PICAN               POREC             PULA            RASA 
## 3                  8                 5                  7                 5               3 
##ROVINJ           VRSAR             ZMINJ               SVETI LOVREC      SVETA NEDELJA   SVETI PETAR U SUMI 
## 2                  3                 7                  4                  5              3 
##SVETVINCENAT     TINJAN            UMAG                VISNJAN           VIZINADA        VODNJAN 
## 6                  8                 2                  5                  5              5 
##KAROJBA          KASTELIR-LABINCI  FAZANA              TAR-VABRIGA       FUNTANA 
## 5                  7                 2                  3                  2

round(100 * table(i) / length(i), 1)
par(mai=c(0,0,0,0))
plot(Istra_shp, col='gray', border='white')
xy <- coordinates(Istra_shp)
plot(wr, xy, col='red', lwd=2, add=TRUE)

#vORONOI DIAGRAM
library(dismo)
library(ggvoronoi)
IstraVoronoi <- voronoi(Istra_shp)
plot(IstraVoronoi)
