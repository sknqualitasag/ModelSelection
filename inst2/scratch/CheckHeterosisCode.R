### #
### # Check Heterosis Codierung vor Optimierung
### # 2020-03-06 (skn)
### # -------------------------------------------------

### # Libraries
library(dplyr)
library(ggplot2)

### # Read Output codeEffHetRec
s_het <- "../../work/Sample_ForModellSelektion_Produktionssystem_Het/fleisch.out"
tbl_het <- readr::read_delim(file = s_het, delim = " ")
tbl_het <- tbl_het %>% select(c("ind","cccn","ccco","ccan","ccas","ccao","cwcn","cwco","cwan","cwas","cwao","cfcn","cfco","cfan","cfas","cfao","Effekt_HeterosisSumHet","Effekt_RekombinationSumRec"))
rm(s_het)

### # Overview number per productionsystem
tbl_het %>% filter(cccn != 0) %>% count()
tbl_het %>% filter(ccco != 0) %>% count()
tbl_het %>% filter(ccan != 0) %>% count()
tbl_het %>% filter(ccas != 0) %>% count()
tbl_het %>% filter(ccao != 0) %>% count()

### # Read Output calcHetRec
s_calcHet <- "../../work/Sample_ForModellSelektion_Produktionssystem_Het/hetRecRed.csv"
tbl_calcHet <- readr::read_delim(file = s_calcHet, delim = ";")
rm(s_calcHet)

### # Merge both file
tbl_heterosis <- tbl_calcHet %>% inner_join(tbl_het, by = c("TierID_TVD" = "ind"))
rm(tbl_het);rm(tbl_calcHet)

### # Plot Heterosis again Y
ggplot(tbl_heterosis, aes(x=ccao, y=SumHet)) +
  geom_point()

ggplot(tbl_heterosis, aes(x=cfao, y=SumHet)) +
  geom_point()

ggplot(tbl_heterosis, aes(x=cwao, y=SumHet)) +
  geom_point()
