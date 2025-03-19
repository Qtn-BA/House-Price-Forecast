install.packages("wooldridge")
install.packages("psych")
install.packages("readxl")
install.packages("lmtest")
install.packages("dplyr")
install.packages("car")
install.packages("AER")
install.packages("sandwich")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("whitestrap")
install.packages("pscl")
library(whitestrap)
library(wooldridge)
library(psych)
library(wooldridge)
library(lmtest)
library(readxl)
library(dplyr)
library(sandwich)
library(AER)
library(car)
library(ggplot2)
library(corrplot)
library(pscl)



setwd("/Users/qtn-BA")
prix<-read_excel("Downloads/Projet Econométrie/bd prix de logements.xlsx")
View(prix)



## Afficher les doublons et les valeurs manquantes


prix<-prix%>% filter(!if_any(everything(), is.na))
doublons_prix<-prix[duplicated(prix), ]
print(doublons_prix)




## Distribution des observations et repérages des points aberrants : Boites à moustaches



prix$Superficie_propriété<-as.numeric(prix$Superficie_propriété)
prix$Nombre_Chambres<-as.numeric(prix$Nombre_Chambres)
prix$Nombre_Salles_bain<-as.numeric(prix$Nombre_Salles_bain)
prix$Nombre_Étages<-as.numeric(prix$Nombre_Étages)
prix$Possede_Jardin<-as.numeric(prix$Possede_Jardin)
prix$Possede_Piscine<-as.numeric(prix$Possede_Piscine)
prix$Garage_Taille<-as.numeric(prix$`Garage_Taille  m`)
prix$Score_Qualité_quartier<-as.numeric(prix$Score_Qualité_quartier)
prix$Distance_Centre_ville<-as.numeric(prix$Distance_Centre_ville)
prix$Prix<-as.numeric(prix$Prix)




par(mfrow=c(4,3),cex=0.1)

{
  boxplot(prix$Superficie_propriété,main="Superficie de la propriété en mètres carrés",col="blue" )
  boxplot(prix$Nombre_Chambres,main="Nombre de chambres dans la propriété",col="blue")
  boxplot(prix$Nombre_Salles_bain,main="Nombre de salles de bains dans la propriété",col="blue")
  boxplot(prix$Nombre_Étages,main="Nombre d'étages dans la propriété",col="blue")
  boxplot(prix$Possede_Jardin,main="Indique si la propriété a un jardin",col="blue")
  boxplot(prix$Possede_Piscine,main="Indique si la propriété a un pool",col="blue")
  boxplot(prix$Garage_Taille,main=" Taille du garage en mètres carrés",col="blue")
  boxplot(prix$Score_Qualité_quartier,main="Score de la qualité du quartier ",col="blue")
  boxplot(prix$Distance_Centre_ville,main="Distance entre la propriété et le centre-ville en kilomètres",col="blue")
}




# Estimation du modele par la methode des moindres carrées ordinnaires multiple (MCO) 

prix_reg<-lm(Prix~Superficie_propriété+Nombre_Chambres+Nombre_Salles_bain+Nombre_Étages+Possede_Jardin+Possede_Piscine+Garage_Taille+Score_Qualité_quartier+Distance_Centre_ville, data=prix)
summary(prix_reg)




## Extraire les p-values des coefficients

 
p_values <- summary(prix_reg)$coefficients[, 4]
print(p_values)




## Vérifier les variables significatives au seuil de 5%


significatif_5pct <- p_values < 0.05
print(significatif_5pct)


# Calcule de la variance 

 
anova<-aov(prix_reg)
summary(anova)
aov(prix_reg)




## Calcule et affichage des residus

 
prix$residus<-residuals(prix_reg)
summary(prix$residus)
par(mfrow=c(1,1))
plot(prix_reg$residuals,main = "Residus du modele",col="blue")
abline(h=0,col="red")
hist(prix$residus,col = "green")



qqnorm(residuals(prix_reg),col="blue")
qqline(residuals(prix_reg),col="red")



# H2 Verifier si les variables sont corrélé

 
data_bis = prix[, c("Superficie_propriété", "Nombre_Chambres", "Nombre_Salles_bain","Nombre_Étages",
                    "Possede_Jardin", "Possede_Piscine", "Garage_Taille", "Score_Qualité_quartier", 
                    "Distance_Centre_ville"  )]
corr.matrix <- cor(data_bis)
corr.matrix
corrplot(corr.matrix, method = 'square', diag = F, addCoef.col ='black', number.cex = 0.5)




# Calcule VIF : Variance Inflation Factor

 
vif(lm(Prix~Superficie_propriété+Nombre_Chambres+Nombre_Salles_bain+Nombre_Étages+Possede_Jardin+Possede_Piscine+Garage_Taille+Score_Qualité_quartier+Distance_Centre_ville, data=prix))


# Test d'endogénéité en vérifiant la corrélation entre les résidus et les variables explicatives

 
residuals <- residuals(prix_reg)

endo <- vector("numeric", length = ncol(prix))
for (i in 1:ncol(prix)) {
  cor_residuals <- cor(residuals, prix[[i]])  
  endo[i] <- cor_residuals
}

print(endo)



### test de Breush-Pagan

 
bptest(prix_reg)


### test de white

white_test(prix_reg)


## test d'auto-correlation


dw_test <- durbinWatsonTest(prix_reg)
print(dw_test)



# Prédiction des prix à partir du modèle


 
prediction<- predict(prix_reg, newdata = prix)
print(prediction)


# Poursuite de l'analyse avec le modèle Logit
##Nous pouvons déterminer si le prix d'une maison est considérer comme élevéé

##On calcul la médiane pour déterminer si un prix est élevée ou non
 
mediane_prix <- median(prix$Prix, na.rm = TRUE)
print(mediane_prix)

Donc le prix est considéré élevé si > 574 724

##Il faut ensuite créer la variable binaire "Prix_élevé" de 1 si > 574724, sinon 0
 
df$Prix_élevé <- ifelse(df$Prix > 574724, 1, 0)
str(df$Prix_élevé)


##Nous pouvons désormais estimer notre modèle logit
 
model_logit <- glm(Prix_élevé ~ Superficie_propriété + Nombre_Chambres +
                     Nombre_Salles_bain + Nombre_Étages + Possede_Jardin +
                     Possede_Piscine + Garage_Taille + Score_Qualité_quartier+ Distance_Centre_ville, 
                   data = prix, 
                   family = binomial)
summary(model_logit)

Néanmoins nous ne pouvons pas interpréter les coefficients, seulement leur signe


##Test R² 
 
PR2 = pR2(model_logit)
print(PR2)


McFadden R² : 0.5026 → Nous pouvons considérer que le modèle améliore significativement les prédictions par rapport à un modèle sans aucune information


##Il faut créer de nouvelles colonnes "proba_prédite", "classe_prédite ", qui respectivement donne la probabilité de si la maison a un prix elevé ou non, et lui assigne 1 si proba > 0,5, sinon 0


##Prédire les probabilités que la maison soit chère
 
prix$proba_prédite <- predict(model_logit, type = "response")


##Convertir les probabilités en classes binaires. Les proba deviennent donc 1 ou 0
 
prix$classe_prédite <- ifelse(prix$proba_prédite > 0.5, 1, 0)


## Matrice de confusion permet de montrer combien de prediction sont justes ou pas
 
Matrice_confusion = table(Actual = prix$Prix_élevé, Predicted = prix$classe_prédite)
print(Matrice_confusion)


##Calcul de la probabilité de trouver si une maison est réellement chère ou non
 
VP <- 202  # Vrais Positifs
VN <- 213  # Vrais Négatifs
FP <- 37   # Faux Positifs
FN <- 48   # Faux Négatifs

total <- VP + VN + FP + FN

accuracy <- (VP + VN) / total

print(paste("Précision globale (Accuracy) :", round(accuracy * 100, 2), "%"))


## Illustration montrant la relation entre la superficie et la probabilité que le prix soit élevé
 
Cor_Prix_Surface = ggplot(prix, aes(x = Superficie_propriété, y = proba_prédite, color = as.factor(Prix_élevé))) +
  geom_point() +
  labs(title = "Probabilité prédite de Prix élevé en fonction de la Superficie",
       x = "Superficie de la propriété",
       y = "Probabilité prédite de Prix élevé") +
  theme_minimal()
print (Cor_Prix_Surface)



#Prédictions pour de nouveaux cas

## Créer un nouveau dataframe avec les caractéristiques la maison
 
nouvelle_maison <- data.frame(
  Superficie = 150,
  Nb_Chambres = 2,
  Nb_SDB = 1,
  Nb_Étages = 1,
  Jardin = 1,
  Piscine = 1,
  Garage_Taille = 48,
  Score_Qualité_quartier = 8,
  Distance_Centre_ville = 6
)


##Prédit la probabilité ave le modèle logit
 
proba_logit <- predict(model_logit, newdata = nouvelle_maison, type = "response")
print(proba_logit)


##Affichage en binaire
 
# Convertir en classe binaire
classe_predite <- ifelse(proba_logit > 0.5, "Prix élevé", "Prix faible")
print(classe_predite)

Une maison avec ces caractéristique aurait donc un prix faible, c'est à dire < 574 724€, avec un taux de réussite à 83%. J'aimerais que tu m'enleve tout les  
 ,
