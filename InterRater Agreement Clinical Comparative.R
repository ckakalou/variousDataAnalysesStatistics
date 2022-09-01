library(irr)
library(dplyr)
library(magrittr)
library(plotly)


medication_discontinuation <- read.csv("Raw/Papageorgiou's Comparative Study Results_AnalysisInterRater.csv", header = TRUE, sep = ";")

Percentage_Agreement <- agree(medication_discontinuation, tolerance=0)

Cohens_Kappa_Unweighted <- kappa2(medication_discontinuation)

raterStats <- read.csv("Raw/AnalysisInterRaterComplete.csv", header = TRUE, sep = ";")

uniqueDEC <- unique(raterStats[,c('drug', 'adverse.reaction')])
raterDefinite <- raterStats[ raterStats$Liverpool.Algorithm=="Definite",]
raterPossible <- raterStats[ raterStats$Liverpool.Algorithm=="Possible",]
raterNone <- raterStats[ raterStats$Liverpool.Algorithm=="-",]

raterDefiniteMatrix <- raterDefinite %>% select('Rater1','Rater2')
raterPossibleMatrix <- raterPossible %>% select('Rater1','Rater2')
raterNoneMatrix <- raterNone %>% select('Rater1','Rater2')


Percentage_AgreementDefinite <- agree(raterDefiniteMatrix, tolerance=0)
Percentage_AgreementPossible <- agree(raterPossibleMatrix, tolerance=0)
Percentage_AgreementNone <- agree(raterNoneMatrix, tolerance=0)

Cohens_Kappa_Unweighted_Definite <- kappa2(raterDefiniteMatrix)
Cohens_Kappa_Unweighted_Possible <- kappa2(raterPossibleMatrix)
Cohens_Kappa_Unweighted_None <- kappa2(raterNoneMatrix)

Cohens_Kappa_List <- c(Cohens_Kappa_Unweighted$value, Cohens_Kappa_Unweighted_Definite$value, Cohens_Kappa_Unweighted_Possible$value, Cohens_Kappa_Unweighted_None$value)
AgreementList <- c(Percentage_Agreement$value, Percentage_AgreementDefinite$value, Percentage_AgreementPossible$value, Percentage_AgreementNone$value)

RaterStats_Dataframe <- data.frame(unlist(Cohens_Kappa_List), unlist(AgreementList))
Agreement_Dataframe <- data.frame(AgreementList)

RaterStats_Dataframe$Agreement <- Agreement_Dataframe

names(RaterStats_Dataframe) = c("CohensKappa","Agreement")
row.names(RaterStats_Dataframe) <- c("Total", "Defite", "Possible", "N/A")
RaterStats_Dataframe[is.na(RaterStats_Dataframe)] <- 1
RaterStats_Dataframe$Agreement <- RaterStats_Dataframe$Agreement / 100
library(plotly)

fig <- plot_ly(data = RaterStats_Dataframe, x = ~Agreement, y = ~)

fig

