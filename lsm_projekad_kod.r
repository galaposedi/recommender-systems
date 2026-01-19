library(HistData)
library(dplyr)
library(lme4)
library(ggplot2)

baza = GaltonFamilies
attach(GaltonFamilies)

summary(GaltonFamilies)


#vidimo da je p-vrednost mala i za svaki prediktor posebno i za F-stat
#tako da odbacujemo nultu hipoteyu da su prediktori beznacajni

zp = midparentHeight;
zo = childHeight

#fenotipska kovarijacija roditelj-potomak
cov_p_o = cov(zp,zo)
cov_p_o

#aditivna geneticka disperzija
sigma2_A = 2*cov_p_o
sigma2_A

#Fenotipska disperzija dece

sigma2_z = var(zo)
sigma2_z

#heritabilnost - sigma2_A/simga2_z

h2 = sigma2_A/sigma2_z
h2
#h2 je oko 0.32 posto, sto znaci da je oko 32% varijacije visinu u populaciji objasnjavaju aditivni geneticki faktori
#oko 32% se prenosi sa roditelja na potomak

#procena heritabilnosti preko lm modela
model <- lm(childHeight ~ midparentHeight, data = GaltonFamilies)
summary(model)

h2_est <- coef(model)[2]
h2_est
#sto je priblizno umnozak od vrednosti iz definicije, on nam predstavlja vrednost nagiba regresione prave

se_h2 <- summary(model)$coefficients[2,2]
se_h2
#standarda greska koeficijenta 

#simuliramo selekciju tako sto cemo uzeti 20% uzorka

# populacioni prosek mid-parent
mu <- mean(midparentHeight)

# biramo 20% roditelja sa izrazenom visinom
#zelimo da vidimo kakav ce odgovor biti, tj kakve ce visine biti ta deca
threshold = quantile(midparentHeight, 0.8)
selected = midparentHeight[midparentHeight > threshold]
# prosek izabranih roditelja
mu_star = mean(selected)

# selekcioni diferencijal
S = mu_star - mu
S
#sto je S veci, vece sanse za odgovor na selekciju
# očekivani odgovor
R = h2_est * S
R
mu_potomak = mu + R
#R = 1.62, sto znaci da ce prosecna visina deteta sa visokim roditeljima
#biti veca za 1.63 od proseka cele populacije

#grafik populacion proseka, i ocekivanog prosecnog potomstva
plot(zp, zo, pch=16, col="lightblue", 
     xlab="Mid-parent height", ylab="Child height",
     main="Odgovor na selekciju (Breeder's Equation)")
abline(h=mean(zo), col="blue", lwd=2)             # populacioni prosek
points(mean(selected), mu_offspring, col="green", pch=19, cex=1.5)  # očekivani potomci
points(mean(selected), mean(selected), col="red", pch=19, cex=1.5)  # selektovani roditelji
legend("topleft", legend=c("Populacioni prosek", "Selektovani roditelji", "Ocekivani potomci"),
       col=c("blue", "red", "green"), pch=19)

#analiza grafika


#breeding values

prosek_potomaka = mean(GaltonFamilies$childHeight)

breeding_value = h2_est*(childHeight - prosek_potomaka)
#uzimamo ocenjenu vrednost jer u stvarnosti ne znamo tacne podakte za disperziju
breeding_value[1:10]
#preko ove vrednosti mozemo videti koliko ce svaka jedinka doprineti(oduzeti) visini svom potomku
baza$breeding_value = breeding_value
head(baza)

#multivarijantna analiza G I P matrice; viseoosbinska
indeksi = which(gender == "male")
visina_sina = childHeight[indeksi]
visina_cerke = childHeight[-indeksi]
familije = unique(family)
visina_sina_par =  sapply(familije, function(f) mean(visina_sina[family[indeksi] == f]))
visina_cerke_par = sapply(familije, function(f) mean(visina_cerke[family[indeksi] == f]))

# fenotipska kovarijaciona matrica P
P = cov(cbind(visina_sina_par, visina_cerke_par), use = "pairwise.complete.obs")
P
#aditivna genetska kovarijacija G heratibilnost 0.63 sa pocetka
h2 = 0.63
G = h2 * P
G

#viseosobinski odgovor R
R = G %*% solve(P) * S
R

#redovi - osobinu koja s menja kod potomaka
#kolone - osobina koja je bila selekciona
#iz rez priblizno |1.604  0|
#                 |~0    1.604|
#nema znacajne korelacije izmedju selekcije sinova i promene kod cerke i obrnuto
#ocekivano povecanje visine potomaka zbog visine roditelja ista skoro
#to smo dobili i ranije

#animal model - lin mesoviti
library(lme4)
animal_model = lmer(childHeight ~ 1 + (1|family), data = GaltonFamilies, REML = TRUE)
summary(animal_model)
#disperzija family = 2.235
#disperzija residual (E) je 10.548
h2_animal = 2.235/(10.548+2.235) 
h2_animal
#oko 17% disperzije visine se moze pripisati genetskim razlikama u ovoj populaciji
#razlikuje se od teorijskog jer se procena deli i na porodicni random efekat




