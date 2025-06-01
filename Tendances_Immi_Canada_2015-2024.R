# importer les donnees de l'immigration du Canada
immigration<-read.csv("canada_immigrant_2024_q2(1).csv", sep=",", header = TRUE)
# appercu des donnees 
str(immigration)
# les dimensions 
dim(immigration)
# les variables (colonnes)
colnames(immigration)[1]
#appercu des donnees 
head(immigration[,2], 20) # les 20 premieres lignes
tail(immigration[,2], 20) # les 20 dernieres lignes

#changement du nom de la premiere colonne
colnames(immigration)[1]<-"Categories d'Immigration"
# nombre de colonnes
ncol(immigration)

# les valeurs de la premiere colonne
unique(immigration[,1])
#nom de la premiere colonne
colnames(immigration)[1]
# remplacement des valeurs non definies '--' par '0'
immigration[immigration=='--']=0
head(immigration[,2],20) # les 20 premieres colonnes

# remplacer les valeurs non definies '--' par de '0'
immigration<-data.frame(lapply(immigration, function(x) gsub('--','0',x)))
str(immigration)

#alternative a gsub
immigration[immigration=='--']='0'


# remplacer la virgule ',' dans les valeurs numeriques 
immigration<-data.frame(lapply(immigration, function(x) gsub(',','',x)))
tail(immigration[,2], 20)

# convertir les valeur en type numerique 
immigration<-data.frame(immigration[,1],lapply(immigration[,c(2:length(immigration))], as.numeric))
#reconstitution du dataframe

str(immigration)
immigration
tail(immigration)
immigration[,1]

# fonction qui ressort les donnees d'une province.
# la fonction de nom 'province' prend en parametre le nom de la province ou une
# partie du nom. 

province<-function(prov){
    index<-grep(prov,immigration[,1],ignore.case = TRUE)
    return(immigration[index,])
}

# fonction pour avoir les donnees par  trimestres
# Q est le trimestre (Q1, Q2, Q3, Q4) et  x est le dataframe qu'on filtre
trimestre<-function(Q,x){
  index<-grep(Q,colnames(x),ignore.case = TRUE)
  return(x[,index])
}

# index pour conserver les colonnes des totaux
  
  index_total<-grep('total',colnames(immigration),ignore.case = TRUE)
  names(immigration[,index_total])
  
# donnees de l'immigration du nunavut
immigration_Nunavut<-province('nunavut')
str(immigration_Nunavut)

# Donnees de l'immigration de l'Ontario 
immigration_Ontario<-province('ontario')
str(immigration_Ontario)

#immigration de tout le pays , le total 
immigration_Canada<-immigration[nrow(immigration),]
str(immigration_Canada)

# immigartion Canada total, on a conserver uniquement les totaux 
immigration_Canada_total<-immigration_Canada[,index_total]
names(immigration_Canada_total)

# conserver les index de colonnes trimestre
index_trimestre<-grep('Q',names(immigration_Canada_total), ignore.case = TRUE)
#immigration totale par trimestre
immigration_trimestre<-immigration_Canada_total[,index_trimestre]
names(immigration_trimestre)

# immigration pour chaque annees
immigration_annees<-immigration_Canada_total[,-index_trimestre]
str(immigration_annees)

#immigration Canada par trimestre
#Q1
canada_Q1<-trimestre('Q1',immigration_Canada_total)
#Q2
canada_Q2<-trimestre('Q2',immigration_Canada_total)
#Q3
canada_Q3<-trimestre('Q3',immigration_Canada_total)
#Q4
canada_Q4<-trimestre('Q4',immigration_Canada_total)


# les graphiques
# graphiques des donnees par trimestre 
# graphique avec 'xaxt='n': desactiver l'axe des x pour le personnaliser, 
# las=2, ecriture sur l'axe des x en verticale
plot(0,0, main="Nombre d'immigrant par trimestre", 
     xlab=" ", ylab="Nombre d'immigrants",
     xlim=c(1,ncol(immigration_trimestre)), 
     ylim=c(min(immigration_trimestre[1,]),
            max(immigration_trimestre[1,])), type='n', xaxt='n' )

#type='n' pour ne rien afficher, type='b' ligne et point, type='h' lignse horizontales
# l'axe des x:
Q<-c(rep(c('Q1','Q2','Q3','Q4'),9),'Q1','Q2')
annees_x<-c(rep('2015',4),rep('2016',4),rep('2017',4),
            rep('2018',4),rep('2019',4),rep('2020',4),
            rep('2021',4),rep('2022',4),rep('2023',4),
            rep('2024',2))
x_labels<-paste(annees_x,Q, sep='.')
x_labels

axis(side=1, at=c(1:ncol(immigration_trimestre)),labels =x_labels, las=2  )

points(x=1:ncol(immigration_trimestre),
       y=immigration_trimestre[1,], type = 'b', lwd=2 ) 



# graphique des valeurs totales selon les annees
plot(0,0, type='n', main="Nombre d'immigrant par an",
     xlab='', ylab="Nombre d'immigrant",xlim=c(1,ncol(immigration_annees)),
     ylim=c(min(immigration_annees[1,]), max(immigration_annees[1,])), xaxt='n')

axis(side=1, at=c(1:ncol(immigration_annees)), labels=2015:2024, las=2)

points(x=1:ncol(immigration_annees), y=immigration_annees[1,], type='b',
       lwd=2)
# Histogramme de la repartion selon annees
sum_annee<-unlist(immigration_annees[1,],use.names = FALSE)
# Changement d'echelle (1/1000)
sum_annee<-sum_annee/1000
#on nomme les valeurs
names(sum_annee)<-2015:2024
sum_annee
barplot(sum_annee, main="Repartition par annees",
        col='pink',xlab='Annees',
        ylab='Nombre en millier (x1000)',
        ylim=c(0, max(sum_annee)+100))
sum_annee

# elimination des donnees aberantes
annees_abr<-grep('2020',names(immigration_annees), ignore.case = TRUE)
annees_abr<-c(annees_abr, grep('2024', names(immigration_annees), ignore.case = TRUE))
immigration_annees_sans_abr<-immigration_annees[,-annees_abr]
str(immigration_annees_sans_abr)

# reconstitution d'une table de donnees pour le test ANOVA
# data.frame avec les groupes par trimestre pour l'analyse l'Anova
# variable categorique 'Trimestre'
Trimestre<-c(rep('Q1',length(canada_Q1)),rep('Q2',length(canada_Q2)),
             rep('Q3',length(canada_Q3)),rep('Q4',length(canada_Q4)))
# variables nombre d'immigrants
Nombre<-(unlist(c(canada_Q1[1,],canada_Q2[1,],canada_Q3[1,],canada_Q4[1,]), use.names = FALSE))
str(Nombre)

# data frame canada_Trimestre qui regroupe les valeurs trimestre
canada_trimestre<-data.frame(Nombre,Trimestre)
canada_trimestre

# somme totale par trimestre 
sum_trimestre<-tapply(canada_trimestre$Nombre, 
                      INDEX = canada_trimestre$Trimestre, FUN =sum)
# changement d'echelle pour la representation sur l'histogramme (1/1000)
sum_trimestre<-sum_trimestre/1000

barplot(sum_trimestre,xlab='Trimestres' , ylab = 'Nombres en millier (x1000)',
        main='Repartition par trimestre', col = 'red', xpd = TRUE,
        ylim=c(0, max(sum_trimestre) +200))
sum_trimestre

# transformation en variable categorique
canada_trimestre$Trimestre<-as.factor(canada_trimestre$Trimestre)
str(canada_trimestre)

# Calcule de l'erreur type : SSE
# SE_Q1
Q1<-canada_trimestre[canada_trimestre$Trimestre=='Q1',]
SSE_Q1<-sum((Q1$Nombre-mean(Q1$Nombre))^2)
# SE_Q2
Q2<-canada_trimestre[canada_trimestre$Trimestre=='Q2',]
SSE_Q2<-sum((Q2$Nombre-mean(Q2$Nombre))^2)
# SE_Q3
Q3<-canada_trimestre[canada_trimestre$Trimestre=='Q3',]
SSE_Q3<-sum((Q3$Nombre-mean(Q3$Nombre))^2)
# SE_Q4
Q4<-canada_trimestre[canada_trimestre$Trimestre=='Q4',]
SSE_Q4<-sum((Q4$Nombre-mean(Q4$Nombre))^2)
# SSE pour l'ensemble des trimestres
SSE<- SSE_Q1+SSE_Q2+SSE_Q3+SSE_Q4
# degre de liberte : df=k(n-1)
# k: le nombre de groupe=4
# n le nombre de le nomdre d'observation par groupe
df<-34
# la moyenne des groupes
moy_Q<-tapply(canada_trimestre$Nombre,INDEX=canada_trimestre$Trimestre, FUN = mean )

# l'erreur type SE:
SE<-sqrt(SSE/df)
SE
# repartition des voleurs moyennes selon les trimestres
barplot(moy_Q, main='Repartion des moyennes par trimestre',
        xlab='Trimestres', ylab='Valeurs moyennes',
        ylim=c(0,max(moy_Q)), col='blue')
# construction de l'intervalle de confiance
#limites inferieures de l'intervalle de confiance
lim.inf<-moy_Q - SE
#limites superieurs de l'intervalle de confiance
lim.sup<-moy_Q + SE 
# graphique des intervalles 
plot(x=0, y=0, type ='n',ylim=c(min(lim.inf), max(lim.sup)),
     main='Intervalles de confiances', xlab='Trimestres',
     xlim=c(0,5), ylab='Valeurs moyennes', xaxt='n' )
#definition de l'axe des abscisses
axis(side=1, at=1:4, labels = names(moy_Q))
# les moyennes
points(x=1:4, y=moy_Q, col='blue')
# intervalle
arrows(x0 =1:4,y0 = lim.inf, x1 = 1:4, y1 = lim.sup,
       col='red', code=3, angle=90 )
# test ANOVA
aov<-aov(Nombre~Trimestre,data=canada_trimestre)
summary(aov)
# graphe des residus en fonction des valeurs predites
# verification de l'egalite de la variance
plot(residuals(aov), fitted(aov), main='Residus en fonction des valeurs predites',
     xlab = 'Residus', ylab = 'Valeurs predites')
# diagramme des quartiles
boxplot(Nombre~Trimestre, data=canada_trimestre, main='Diagramme des Quartiles')
# verification de la normalite des residus
# graphe des residus
qqnorm(residuals(aov), main="Quantiles-Quantiles", 
       xlab='Quantiles Theoriques', ylab='Quantiles observe')
qqline(residuals(aov))

#test de regression 
str(immigration_annees)
# transformation des donnees
Annees<-c(2015:2024)
canada_annees<-data.frame(Annees, Nombre=unlist(immigration_annees[1,],use.names = FALSE) )
str(canada_annees)

########### A la place de unlist()
valeur<-c()
for(i in c(1:ncol(immigration_annees))){
  a<-immigration_annees[1,i]
  valeur<-c(valeur,a)
  }
valeur
##########
#combinaison avec les donnes demographiques 
canada_demo_annees<-data.frame(canada_annees,
                               demographie=demographie_annees)
head(canada_demo_annees)

# modele de regression simple
Regression<-lm(Nombre~Annees,data=canada_demo_annees)
summary(Regression)

# le nuage de points

# Tracer la Regression Nombre en fonction des annees
plot(canada_annees$Nombre~canada_annees$Annees,col='red',cex=1,
     xlab='Annees', ylab='Nombre', main="Modele de regression",
     xlim=c(2016, 2024), 
     ylim=c(min(canada_annees$Nombre)-20000,max(canada_annees$Nombre)+20000))


    
# les coefficient de la droite de regression
Regression
# Tracer de la droite de regression
abline(Regression, lwd=1.5)
# tracer des ecarts entre les valeur predites et les valeurs observees
segments(x0 = canada_annees$Annees,
         y0= canada_annees$Nombre,
         x1= canada_annees$Annees,
         y1= fitted(Regression),
         col='blue', lwd=1.5)  

# Regression en fonction de la demographie
Regression<-lm(Nombre~demographie,data=canada_demo_annees)
summary(Regression)

# le nuage de points

# Tracer la Regression Nombre en fonction des annees
plot(canada_demo_annees$Nombre~canada_demo_annees$demographie,col='red',cex=1,
     xlab='Annees', ylab='Nombre', main="Modele de regression",
     xlim=c(min(canada_demo_annees$demographie),max(canada_demo_annees$demographie)), 
     ylim=c(min(canada_annees$Nombre),max(canada_annees$Nombre)))



# les coefficient de la droite de regression
Regression
# Tracer de la droite de regression
abline(Regression, lwd=1.5)
# tracer des ecarts entre les valeur predites et les valeurs observees
segments(x0 = canada_demo_annees$demographie,
         y0= canada_demo_annees$Nombre,
         x1= canada_demo_annees$demographie,
         y1= fitted(Regression),
         col='blue', lwd=1.5)  


# verification des supposition
# verification de la normalite des residus
qqnorm(residuals(Regression), main='Quartiles-Quartiles',
       xlab = 'Quartile Theorique',
       ylab = 'Quartile Observe')
qqline(residuals((Regression)))

#verification de l'homogeneite de la variance
plot(fitted(Regression),residuals(Regression), main='Residus en fonction des valeurs predites',
     xlab='Residus',
     ylab='Valeurs Predites')

# modele de regression multiple
Regression<-lm(Nombre~Annees+demographie,data=canada_demo_annees)
summary(Regression)

# Traitement des donnees de demographie entre 2015 et 2024
demographie<-read.csv2('Demographie_Canada.csv', sep=';', header = TRUE)
head(demographie)
demographie$Géographie
names(demographie)
str(demographie)

# remplacer les espaces ' ' dans les valeurs numeriques 
demographie<-data.frame(lapply(demographie, function(x) gsub(' ','',x)))
# convertir les valeur en type numerique 
demographie<-data.frame(demographie[,1],lapply(demographie[,c(2:length(demographie))], as.numeric))

# creer un dataframe qui regroupe les donnees par annees
somme_annee<-function(data){
annees<-2014:2024
  # Initialiser un data.frame vide avec les mêmes lignes que data
  demographie_annees <- data.frame(matrix(nrow = nrow(data), ncol = 0))

    for ( annee in annees) {
      # on determine les trimestres pour chaque annees
      trim<-grep(annee,names(demographie),ignore.case = TRUE)
      # on additionne pour avoir la somme pour l'annee
      if (length(trim) == 4) {
        # Calculer la somme des 4 trimestres
        demographie_annees[[as.character(annee)]] <- rowSums(data[, trim], na.rm = TRUE)
      }
      # la somme de toutes les provinces   
    demographie_somme<-colSums(demographie_annees)
    
  }

  return(demographie_somme)
}
# on apllique la fonction au dataframe demographie 
demographie_somme<-somme_annee(demographie)
head(demographie_somme)

str(demographie_annees)

# donnee de la population active
# source : https://www.kaggle.com/datasets/pienik/unemployment-in-canada-by-province-1976-present
population_active<-read.csv('Unemployment_Canada_1976_present.csv',sep=',',header = TRUE)
str(population_active)
head(population_active)

population_active$REF_DATE<-as.factor(population_active$REF_DATE)
population_active$Employment<-as.numeric(population_active$Employment)

# colonne annees
years <- substr(as.character(population_active$REF_DATE), 1, 4)
years

# ajout de la colonne annees
population_active$Annes<-years

# coversion en type facteur
population_active$Annes<-as.factor(population_active$Annes)
# population active moyenne par annees  
employment<-tapply(population_active$Employment,INDEX=population_active$Annes,FUN =mean)
# population active de 2015 a 2024
employment_canada<-employment[names(employment)>='2015' & names(employment)<='2024']
employment_canada

# Regression multiple
# on coupe les donnees jusqu'a 2023
canada_demo_annees_2023<-canada_demo_annees[1:
                                            nrow(canada_demo_annees)-1,]
canada_demo_annees_emp<-data.frame(canada_demo_annees_2023,employabilite=employment_canada)

# Regression multiple 
Regression_Emp<-lm(Nombre~Annees+demographie+employabilite,data=canada_demo_annees_emp)
summary(Regression_Emp)
