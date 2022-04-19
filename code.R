##################################################################
#   MODELISATION DE LA SURFACE HABITABLE DU BATI FRANCAIS        #
##################################################################

#-------------------(1) Importation et Exploration des données

# Importation des données

bati<- read.csv("D:/DSMS1/Semestre 2/Projet Tutoré/data_bati_UBS.csv",header = TRUE)

# Statistique descriptive 

summary(bati)

bati$dept<- as.factor(bati$dept)
bati$mat_murs<- as.factor(bati$mat_murs)
bati$mat_toits<- as.factor(bati$mat_toits)
bati$nb_bati_dur<- as.factor(bati$nb_bati_dur)
bati$nb_bati_leger<- as.factor(bati$nb_bati_leger)
bati<-bati[!bati$annee_const==0,]       # supprime les observation 
bati$annee_const<- as.factor(bati$annee_const)
plot(table(bati$NbPieces))# varible discrete
# Transformation des valeurs de mat_murs et mat_toits pour fusionner les sous-catégorie en une seule catégorie principale

library(plyr)
bati$mat_murs <- mapvalues(bati$mat_murs, from =c(10,11,12,13	,14,	15,	16,	19	,20,	21,	22,	23,	24	,25,	26,	29,	30,	31,	32	,33	,34	,35,	36	,39,	40,	41,	42,	43,	44,	45,	46,	49,	50,	51,	52,	53,	54,	55	,56	,59	,60,	61,	62,	63,	64,	65,	66,	69,	90,	91	,92,	93,	94,	95,	96,99), to = c(1,	1,	1,	1,	1,	1,	1,	1,	2,	2,	2,	2,	2,	2,	2,	2,	3,	3,	3,	3,	3,	3,	3,	3,	4,	4,	4,	4,	4,	4,	4,	4,	5,	5,	5,	5,	5,	5,	5,	5,	6,	6,	6,	6,	6,	6,	6,	6,	9,	9,	9,	9,	9,	9,	9,9)) 

bati$mat_toits <- mapvalues(bati$mat_toits, from =c(10,	11,	12,	13,	14,	19,	20,	21,	22,	23,	24,	29,	30,	31,	32,	33,	34,	39,	40,	41,	42,	43,	44,	49,	90,	91,	92,	93,	94,	99), to = c(1,	1,	1,	1,	1,	1,	2,	2,	2,	2,	2,	2,	3,	3,	3,	3,	3,	3,	4,	4,	4,	4,	4,	4,	9,	9,	9,	9,	9,	9))

bati<-bati[!bati$surface_sol>=1000,]
bati<-bati[!bati$surface_bati_dur>=10000,]
bati<-bati[!bati$surface_bati_leger>=600,]
bati<-bati[!bati$surface_parcelle>=10000,]
bati<-bati[!bati$nb_angles>=40,]

# Test de normalité

hist(bati$hauteur,xlab = "Metre",ylim = c(0,0.3),  main = "Hauteur",probability = TRUE)
lines(density(bati$hauteur))# permet de tracer la courbe de la densité



#------------------------ (2)Division des données

######## Division de la base de donnée en tranche de : Formation; Test

## Mélange aléatoire des données
seed<- 50
set.seed(seed)
rows<- sample(nrow(bati ))
bati_mel<- bati[rows,] 

######## Division en données de Formation=70000 Test=30000

split<- round(nrow(bati_mel)*0.70)

# Données de formation
train<- bati_mel[1:split,]
# Données de test 

test<- bati_mel[(split+1):nrow(bati_mel),]
rattle()
table(bati$nb_bati_leger,bati$NbPieces)
table(bati$nb_bati_leger,bati$nb_bati_dur)
table(bati$nb_bati_leger,bati$nb_bati_dur,bati$NbPieces)
plot(bati$nb_angles, bati$superficie)
plot(bati$annee_const,bati$NbPieces,xlim=c(1400,2022))
plot(bati$annee_const,bati$nb_angles,xlim=c(1400,2022))
plot(bati$superficie,bati$nb_bati_leger)
plot(bati$nb_bati_leger,bati$surface_parcelle)

dhgehgzegbfytgfbvzefveyvfzyb
gvytfytgvb
ygynfgvyutrfv 


uyvdtr 

iuhnuybf kjknk,gy
hghtgdrtrh
kjhghtrcg
jnhgtrddcg
kjh,gntbrdcrg
khjnytrrd
