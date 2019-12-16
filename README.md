# Introduction
Calculates and compiles the 2020 Atlantic NCRMP Status Report benthic indicator scores. 

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


# Legal Disclaimer
This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce (DOC). All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. The DOC seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the United States Government.
