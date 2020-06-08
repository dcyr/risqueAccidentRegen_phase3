###############################
outputCompiledFire.csv
###############################
simID: Un identifiant de traitements (les 24 scénarios simulés jusqu'à présent)
fireScenario: Baseline ou RCP 8.5
mgmtScenario: un long nom (favoriser les facteurs décomposés disponibles dans 'scenTable.xlxs' pour les analyses.
year: le pas de temps (année initiale 0: 2015)
replicate: réplicat n sur 100
areaBurned_ha: Superficie brûlée totale (peut inclure des zones non productives)
ID: La zone de feu. Ici il n'y en a qu'une.
areaZoneTotal_ha: Les superficies de la zone de feu, inclut des zones éligibles aux feux, mais non productives

###############################
outputCompiledHarvest.csv
###############################
simID: Un identifiant de traitements (les 24 scénarios simulés jusqu'à présent)  
fireScenario: Baseline ou RCP 8.5
replicate: réplicat n sur 100
year: le pas de temps (année initiale: 2015)
clearcutting, varReten, salv, plantPostFire, plantPostSalv (TRUE/FALSE, self-explanatory; ici il manque le facteur "accès au territoire pour plantation". Se référer à 'scenTable.txt')
areaHarvestedTotal_ha: superficie récoltée en coupe totale ou rétention variable (Exclut la coupe de récup.)
volHarvestedTotal_cubMeter: volume total récolté issu de la coupe totale ou coupe de rétention
areaSalvagedTotal_ha:  superficie récoltée en coupe de récupération
volSalvagedTotal_cubMeter:  volume récolté issu de la coupe de récupération
areaRetenTotal_ha: superficie récoltée en coupe de rétention
volRetenTotal_cubMeter: volume laissé sur le site (valeur négative)
areaPlantedPostSalv_ha: 'self explanatory'
areaPlantPostFire_ha:  Les superficies mal régénérées plantées post-feu ailleurs qu'en post-récup
id: le # de la zone (une seule zone ici)
totalEligibleArea_ha: la superficie productible éligible à la récolte
harvTargetArea_ha: la cible (ici, c'est généralement 0.62% de totalEligibleArea_ha, arrondi au 25ha le plus près, mais ça pourrait varier.
clearcutting, varReten, salv, plantPostFire, plantPostSalv: les facteurs associés aux scénarios
harvAreaTotal_ha: Les superficies totale récoltées. Peuvent dépasser la cible puisque les zones récupérées ont un facteur d'équivalence de 1.33 pour compenser la perte de volume p/r à la coupe normale.
harvVolTotal_cubMeter: le volume total récolté; volHarvestedTotal_cubMeter + volSalvagedTotal_cubMeter
propTarget: harvAreaTotal_ha / harvTargetArea_ha
salvProp: volSalvagedTotal / harvVolTotal_cubMeter

##### HarvestSummary.csv #### et #### HarvestShortfallSummary.csv ####sont des versions sommaires de outputCompiledHarvest.csv.
HarvestSummary.csv contient des valeurs moyennnes, et certains percentiles,
tandis que HarvestShortfallSummary.csv contient les probabilités cumulées qu'une certaine proportion de réplicats
ait connu une rupture d'approvisionnement.

###############################
OutputCompiledVolAt120Cls.csv
###############################
Celui-ci est assez évident: contient les superficies (area_ha) par chaque combinaison de volAt120Cls et coverType.

###############################
outputCompiledVolAt120Mean.csv 
###############################
Idem pour les valeurs de productivité potentielle moyenne à l'échelle du paysage.
