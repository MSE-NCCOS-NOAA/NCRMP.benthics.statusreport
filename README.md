# NCRMP.benthics.statusreport
Calculates and compiles the 2020 NCRMP Status Report benthic indicator scores

# Installation 
```
install.packages("devtools")
devtools::install_github("shgroves/NCRMP.benthics.statusreport")
```
# Export scores
```
library(NCRMP.benthics.statusreport)
tmp <- Compile_NCRMP_status_report_scores()

for(k in 1:length(tmp))assign(names(tmp)[k], tmp[[k]])

write.csv(NCRMP_StatusReport_2020_benthic_scores, "Your file path/NCRMP_StatusReport_2020_benthic_scores.csv")
```
