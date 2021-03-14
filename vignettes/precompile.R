#Vignettes that are computer intensive have been precompiled:

library(knitr)
knit("v4_simple_non_normal.Rmd.orig", "v4_simple_non_normal.Rmd")
knit("v5_advanced_non_normal.Rmd.orig", "v5_advanced_non_normal.Rmd")
knit("v6_expert_non_normal.Rmd.orig", "v6_expert_non_normal.Rmd")

library(devtools)
build_vignettes()