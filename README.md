# Introduction
Calculates and compiles the 2020 Atlantic NCRMP Status Report benthic indicator scores. The reports and associated methodology document can be found at, https://www.coris.noaa.gov/monitoring/status_report/.

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

# License 

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. §105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.


# [Section 508](https://oceanservice.noaa.gov/accessibility-statement.html)

The National Ocean Service is committed to making its website accessible to the widest possible audience, including people with disabilities, in accordance with Section 508 of the Rehabilitation Act (29 U.S.C. 794d).

Section 508 is a federal law that requires agencies to provide individuals with disabilities equal access to electronic information and data comparable to those who do not have disabilities, unless an undue burden would be imposed on the agency.

The Section 508 standards are the technical requirements and criteria that are used to measure conformance within this law. More information on Section 508 and the technical standards can be found at Section508.gov.
