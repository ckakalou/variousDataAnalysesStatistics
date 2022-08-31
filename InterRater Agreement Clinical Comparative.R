library(irr)

medication_discontinuation <- read.csv("Raw/Papageorgiou's Comparative Study Results_AnalysisInterRater.csv", header = TRUE, sep = ";")

Percentage_Agreement <- agree(medication_discontinuation, tolerance=0)

Cohens_Kappa_Unweighted <- kappa2(medication_discontinuation)

