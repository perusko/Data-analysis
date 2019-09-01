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
library(rgdal)
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
tm_shape(m300) + tm_polygons(col = "Nocenja", style = "cat")

#interaktivna karta

tm_shape(m300) + tm_text(text = "OG_NAZIV")+tm_polygons(col = "BrojDana", style = "equal")
tmap_mode("view")




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
library(ggpubr)
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
          top=14,          
          x.text.angle = 45  
)



library(lattice)
#BOXPLOT NOĆENJA
OG$OG_MB<- as.factor(OG$OG_MB)
OG$OG_NAZIV<- as.factor(OG$OG_NAZIV)
OG$OG_STATUS<- as.factor(OG$OG_STATUS)
OG$ZUP_NAZIV<- as.factor(OG$ZUP_NAZIV)
boxplot(OG$Nocenja)
bwplot(Nocenja ~ ZUP_NAZIV, data=rbind(
  OG[OG$ZUP_NAZIV=="Istarska zupanija",],
  OG[OG$ZUP_NAZIV=="Splitsko-dalmatinska zupanija",],
  OG[OG$ZUP_NAZIV=="Primorsko-goranska zupanija",])
)



#PRIKAZ  BROJA DOLAZAKA TURSISTA  PO ŽUPANIJAMA
#PIVOT

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
          top = 7,          
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

#BOXPLOT BROJ DANA PO ŽUPANIJAMA
bwplot(BrojDana ~ ZUP_NAZIV, data=rbind(
  OG[OG$ZUP_NAZIV=="Istarska zupanija",],
  OG[OG$ZUP_NAZIV=="Splitsko-dalmatinska zupanija",],
  OG[OG$ZUP_NAZIV=="Primorsko-goranska zupanija",],
  )
 
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




#Podaci za Istru
Istra = subset(m300, ZUP_NAZIV=="Istarska zupanija")
tm_shape(Istra)+tm_fill()+tm_borders()
tmap_mode("view")
tm_shape(Istra) + tm_text(text = "OG_NAZIV")+tm_polygons(col = "BrojStan", style = "quantile")
tm_shape(Istra) + tm_text(text = "OG_NAZIV")+tm_polygons(col = "Nocenja", style = "quantile")
tm_shape(Istra) + tm_text(text = "OG_NAZIV",)+tm_polygons(col = "BrojDana", style = "equal")



str(OGIstra)
dim(OGIstra)
summary(OGIstra)
## OBJECTID      OG_MB             OG_NAZIV          OG_STATUS             ZUP_RB    ZUP_NAZIV           Shape_Leng    
## Min.   :  5   Length:41          Length:41          Length:41          Min.   :18   Length:41          Min.   : 16168  
## 1st Qu.:196   Class :character   Class :character   Class :character   1st Qu.:18   Class :character   1st Qu.: 33725  
## Median :289   Mode  :character   Mode  :character   Mode  :character   Median :18   Mode  :character   Median : 45453  
## Mean   :290                                                            Mean   :18                      Mean   : 50577  
## 3rd Qu.:404                                                            3rd Qu.:18                      3rd Qu.: 65484  
## Max.   :554                                                            Max.   :18                      Max.   :123836  
## Shape_Area          BrojStan       Kucanstva       StObjekti         St/Kuc      St/Obj       Kuc/Obj   
## Min.   :7.86e+06   Min.   :  327   Min.   :  130   Min.   :  338   Min.   :2   Min.   :0.8   Min.   :0.3  
## 1st Qu.:3.53e+07   1st Qu.: 1408   1st Qu.:  415   1st Qu.:  560   1st Qu.:3   1st Qu.:1.8   1st Qu.:0.7  
## Median :6.41e+07   Median : 2183   Median :  782   Median : 1160   Median :3   Median :2.1   Median :0.7  
## Mean   :6.86e+07   Mean   : 5039   Mean   : 1914   Mean   : 2506   Mean   :3   Mean   :2.1   Mean   :0.7  
## 3rd Qu.:9.02e+07   3rd Qu.: 4323   3rd Qu.: 1701   3rd Qu.: 2402   3rd Qu.:3   3rd Qu.:2.3   3rd Qu.:0.8  
## Max.   :1.67e+08   Max.   :57191   Max.   :22705   Max.   :27686   Max.   :3   Max.   :3.0   Max.   :1.0  
## Dolasci          Nocenja           BrojDana Nocenja_Stanovnika Dolazaka_Stanovnika
## Min.   :     0   Min.   :      0   Min.   :0   Min.   :   0       Min.   :  0        
## 1st Qu.:  7212   1st Qu.:  54247   1st Qu.:6   1st Qu.:  22       1st Qu.:  3        
## Median : 14756   Median :  99814   Median :7   Median :  42       Median :  7        
## Mean   :106102   Mean   : 638655   Mean   :7   Mean   : 168       Mean   : 25        
## 3rd Qu.:144316   3rd Qu.:1033690   3rd Qu.:8   3rd Qu.: 181       3rd Qu.: 33        
## Max.   :693348   Max.   :3905090   Max.   :9   Max.   :1832       Max.   :248  
OGIstra$OG_MB<- as.factor(OGIstra$OG_MB)
OGIstra$OG_NAZIV<- as.factor(OGIstra$OG_NAZIV)
OGIstra$OG_STATUS<- as.factor(OGIstra$OG_STATUS)
OGIstra$ZUP_NAZIV<- as.factor(OGIstra$ZUP_NAZIV)
plot(OGIstra)

#ZAMILJIVI ODNOSI=>VELIK BROJ NOĆENJA I STAN,5 DO 6 DANA,MALI BROJ, VIŠE OD 6 DANA
ggplot(OGIstra, aes( x = BrojDana, y = Nocenja)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(OGIstra, aes( x = BrojDana, y = BrojStan)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

boxplot(OGIstra$BrojDana)

#HISTOGRAM BROJA DANA PO OPĆINAMA U ISTRI
library(ggpubr)
ggbarplot(OGIstra, x = "OG_NAZIV", y = "BrojDana",
          fill = "red", 
          xlab = "OPĆINA", ylab = "BROJ DANA",
          label = TRUE,lab.pos = c("in"),lab.nb.digits = 2,lab.col = "white",
          sort.val = "desc",
          x.text.angle = 90,  
)

ggbarplot(OGIstra, x = "OG_NAZIV", y = "Nocenja_Stanovnika",
          fill = "red", 
          xlab = "OPĆINA", ylab = "NOĆENJA PO STANOVNIKU",
          label = TRUE,lab.pos = c("in"),lab.nb.digits = 2,lab.col = "white",
          sort.val = "desc",
          top = 10,
          x.text.angle = 90,  
)



ggbarplot(OGIstra, x = "OG_NAZIV", y = "Dolazaka_Stanovnika",
          fill = "red", 
          xlab = "OPĆINA", ylab = "DOLAZAKA PO STANOVIKU",
          label = TRUE,lab.pos = c("in"),lab.nb.digits = 2,lab.col = "white",
          sort.val = "desc",
          top = 10,
          x.text.angle = 90,  
)

library(vioplot)
vioplot(OGIstra$St_Kuc, OGIstra$St_Obj, 
        ylim=c(0,5),
        col = "dodgerblue", rectCol="dodgerblue3", colMed="red",
        names=c("Stanovnika po kućanstvu", "Stanovnika po objektu"))

#KORELACIJA

corIstra<-OGIstra[,c("BrojStan","StObjekti","Dolasci","Nocenja","BrojDana")]
cor(corIstra)
##            BrojStan  StObjekti    Dolasci    Nocenja   BrojDana
## BrojStan   1.0000000  0.9969248  0.5749051  0.5004559 -0.2202042
## StObjekti  0.9969248  1.0000000  0.5832200  0.5102351 -0.2304577
## Dolasci    0.5749051  0.5832200  1.0000000  0.9896285 -0.2118487
## Nocenja    0.5004559  0.5102351  0.9896285  1.0000000 -0.1670388
## BrojDana  -0.2202042 -0.2304577 -0.2118487 -0.1670388  1.0000000

cor.test(corIstra$BrojStan, corIstra$Nocenja)
#Pearson's product-moment correlation

#data:  corIstra$BrojStan and corIstra$Nocenja
# t = 3.6099, df = 39, p-value = 0.0008616
##alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.2278931 0.7002864
#sample estimates:
#      cor 
#0.5004559 

#KOLEROGRAMI
library(ggplot2)
library(reshape2)
qplot(x=Var1, y=Var2, data=melt(cor(corIstra, use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1))

library(gclus)
dta <- corIstra      
dta.r <- abs(cor(dta))       
dta.col <- dmat.color(dta.r)  
dta.o <- order.single(dta.r)  
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Varijable poredane i obojane prema veličini korelaciji")


library(corrgram)
corrgram(corIstra, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Korelogram varijabli ")


#REGRESIJA
model <- lm(OGIstra$BrojStan~ OGIstra$Nocenja +
                OGIstra$BrojDana)
summary(model)
## Call:
##   lm(formula = OGIstra$BrojStan ~ OGIstra$Nocenja + OGIstra$BrojDana)

## Residuals:
#3   Min     1Q Median     3Q    Max 
## -8328  -2903   -208    891  44873 

## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)   
## (Intercept)       6.616e+03  4.808e+03   1.376  0.17687   
##OGIstra$Nocenja   4.457e-03  1.314e-03   3.392  0.00163 **
##  OGIstra$BrojDana -6.633e+02  6.637e+02  -0.999  0.32391   
##---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## Residual standard error: 8066 on 38 degrees of freedom
## Multiple R-squared:  0.2697,	Adjusted R-squared:  0.2312 
## F-statistic: 7.015 on 2 and 38 DF,  p-value: 0.002553

fit <- lm(Nocenja ~ BrojDana + I(BrojDana^2), data=OGIstra)
summary(fit)
par(mfrow=c(2,2))
plot(fit)

library(mapview)

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

wr2 <- poly2nb(Istra_shp, queen = FALSE)
plot(wr2, coordinates(Istra_shp), add=TRUE, col='red')

#CENTROIDI OPĆINA KOJI SU UDALJENI MANJE OD 5 KM I 10 KM
nb <- dnearneigh(coordinates(Istra_shp),0,5000)
nb_lw <- nb2listw(nb, style = 'B')
plot(Istra_shp, border = 'lightgrey')
plot(nb, coordinates(Istra_shp), add=TRUE, col = 'red')

nb <- dnearneigh(coordinates(Istra_shp),0,8000)
nb_lw <- nb2listw(nb, style = 'B')
plot(Istra_shp, border = 'lightgrey')
plot(nb, coordinates(Istra_shp), add=TRUE, col = 'red')

nb <- dnearneigh(coordinates(Istra_shp),0,10000)
nb_lw <- nb2listw(nb, style = 'B')
plot(Istra_shp, border = 'lightgrey')
plot(nb, coordinates(Istra_shp), add=TRUE, col = 'red')
## vidimo da je Marčana jedina koja nema središte susjednog poligona bliže od 10 km




#vORONOI DIAGRAM
library(dismo)
library(ggvoronoi)
IstraVoronoi <- voronoi(Istra_shp)
plot(IstraVoronoi)


#UBACIVANJE KOORDINATA ZA 6 GRAVITACIJSKIH CENTARA U ISTRI
library(rgdal)
library(deldir)
library(dplyr)
library(ggplot2)
gradovi <- c("Pula", "Rovinj", "Porec", "Umag", "Pazin", "Labin")
lat <- c(44.86,45.08,45.23,45.43,45.24,45.10)
lon <- c(13.85,13.64,13.59,13.52,13.94,14.12)
GravitacijskiCentri <- data.frame(gradovi, lon, lat)

#PRIKAZ VORONOI DIAGRAMA NA MAPI S 6 GRAVITACISKIH CENTARA
coordinates(GravitacijskiCentri) <- c("lon", "lat")
proj4string(GravitacijskiCentri) <- CRS("+proj=longlat")

Istra2 <- Istra_shp
proj4string(Istra2) <- proj4string(GravitacijskiCentri)
Istra2 <- spTransform(Istra2, proj4string(GravitacijskiCentri))


PolygonesVoronoi <- function(layer) {
  require(deldir)
  crds = layer@coords
  z = deldir(crds[,1], crds[,2])
  w = tile.list(z)
  polys = vector(mode='list', length=length(w))
  require(sp)
  for (i in seq(along=polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1,])
    polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP = SpatialPolygons(polys)
  voronoi = SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1], 
                                                         y=crds[,2], row.names=sapply(slot(SP, 'polygons'), 
                                                                                      function(x) slot(x, 'ID'))))
}
ResultsVoronoi <- PolygonesVoronoi(GravitacijskiCentri)


proj4string(ResultsVoronoi) <- proj4string(GravitacijskiCentri)
ResultsVoronoi <- spTransform(ResultsVoronoi, proj4string(GravitacijskiCentri))

ResultsEnclosed <- gIntersection(ResultsVoronoi, Istra2, byid = TRUE)

plot(ResultsEnclosed)
points(x = GravitacijskiCentri$lon, y = GravitacijskiCentri$lat, pch = 20, col = "red", cex = 2)
lines(ResultsVoronoi)



#prikaz udaljenosti od granica u HRVATSKOJ
library(rnaturalearth)
library(sf)
library(raster)
library(tidyverse)
library(RColorBrewer)


world <- ne_countries(scale = 50)

istra <- ne_countries(scale = 10, country = "Istra", returnclass = "sf")
plot(croatia)
croatia <- st_transform(croatia, 3055)
grid <- st_make_grid(croatia, cellsize = 5000, what = "centers")
grid <- st_intersection(grid, croatia)   

plot(grid)

croatia <- st_cast(croatia, "MULTILINESTRING")

#izračun udaljenosti od granica
dist <- st_distance(croatia, grid)
df <- data.frame(dist = as.vector(dist)/1000,
                 st_coordinates(grid))

col_dist <- brewer.pal(11, "RdGy")


ggplot(df, aes(X, Y, fill = dist))+
  geom_tile()+ 
  scale_fill_gradientn(colours = rev(col_dist))
  labs(fill = "Distance (km)")+ 
  theme_void()+ 
  theme(legend.position = "bottom")
  
  ext <- extent(as(grid, "Spatial"))
  
 
  ext
 # class      : Extent 
 # xmin       : 3185406 
 # xmax       : 3745406 
 # ymin       : 5484178 
# ymax       : 5919178 
  
  r <- raster(resolution = 5000, ext = ext, crs = "+proj=utm +zone=27 +ellps=intl
              +towgs84=-73,47,-83,0,0,0,0 +units=m +no_defs")
  

  dist_sf <- st_as_sf(df, coords = c("X", "Y")) %>%
    st_set_crs(3055)

  dist_raster <- rasterize(dist_sf, r, "dist", fun = mean)
  
 
  dist_raster
  
  plot(dist_raster)

  Šibenik = subset(m300, ZUP_NAZIV=="Sibensko-kninska zupanija")
  tm_shape(Šibenik)+tm_fill()+tm_borders()
  tmap_mode("view")
  tm_shape(Šibenik) + tm_text(text = "OG_NAZIV")+tm_polygons(col = "BrojStan", style = "quantile")
  tm_shape(Šibenik) + tm_text(text = "OG_NAZIV")+tm_polygons(col = "Nocenja", style = "quantile")
  tm_shape(Šibenik) + tm_text(text = "OG_NAZIV",)+tm_polygons(col = "BrojDana", style = "quantile")
  tm_shape(Šibenik) + tm_text(text = "OG_NAZIV")+tm_polygons(col = "Nocenja", style = "pretty")
 
  Zadar = subset(m300,ZUP_NAZIV=="Zadarska zupanija")
  tm_shape(Zadar) + tm_text(text = "OG_NAZIV")+tm_polygons(col = "BrojStan", style = "quantile")
  tm_shape(Zadar) + tm_text(text = "OG_NAZIV")+tm_polygons(col = "Nocenja", style = "quantile")
  tm_shape(Zadar) + tm_text(text = "OG_NAZIV",)+tm_polygons(col = "BrojDana", style = "pretty")
  tm_shape(Zadar) + tm_text(text = "OG_NAZIV")+tm_polygons(col = "Nocenja", style = "pretty")
  
 Split = subset(m300,ZUP_NAZIV=="Splitsko-dalmatinska zupanija")
 tm_shape(Split) + tm_text(text = "OG_NAZIV")+tm_polygons(col = "BrojStan", style = "quantile")
 tm_shape(Split) + tm_text(text = "OG_NAZIV")+tm_polygons(col = "Nocenja", style = "quantile")
 tm_shape(Split) + tm_text(text = "OG_NAZIV",)+tm_polygons(col = "BrojDana", style = "pretty")
 tm_shape(Split) + tm_text(text = "OG_NAZIV")+tm_polygons(col = "Nocenja", style = "pretty")
 
 
 
 library(data.table)
 library(formattable)
 customGreen0 = "#DeF7E9"
 customGreen = "#71CA97"
 customRed = "#ff7f7f"
 i1<-Sibensko_kninska
 formattable(i1)
 
 
 
 
  