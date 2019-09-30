library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
setwd(here::here())

hdr = readr::read_rds("wide_headers_with_folds.rds")
hdr = hdr %>%
  select(file, ID, PatientID, SeriesInstanceUID,
         x, y, z, scan_id, index, 
         ss_file, ss_robust_file, maskfile, outfile, 
         alt_outfile)


df = readr::read_rds("stage_1_data.rds")
df = df %>%
  select(ID,
         group, 
         any,
         epidural,
         intraparenchymal,
         intraventricular,
         subarachnoid,
         subdural,
         dcm)

stopifnot(
  length(setdiff(hdr$ID, df$ID)) == 0,
  length(setdiff(df$ID, hdr$ID)) == 0
)

df = full_join(df, hdr)

train = df %>% 
  filter(group == "train")

pct_per_scan = train %>% 
  group_by(scan_id) %>% 
  summarise_at(
    vars(any,
         epidural,
         intraparenchymal,
         intraventricular,
         subarachnoid,
         subdural), mean)
l = pct_per_scan %>% 
  tidyr::gather(measure, value = "value",
                any,
                epidural,
                intraparenchymal,
                intraventricular,
                subarachnoid,
                subdural)


any_per_scan = train %>%
  mutate_at(
    vars(any,
         epidural,
         intraparenchymal,
         intraventricular,
         subarachnoid,
         subdural), as.logical) %>% 
  group_by(scan_id) %>% 
  summarise_at(
    vars(any,
         epidural,
         intraparenchymal,
         intraventricular,
         subarachnoid,
         subdural), any)

any_per_scan %>% 
  group_by(epidural,
           intraparenchymal,
           intraventricular,
           subarachnoid,
           subdural) %>% 
  tally() %>% 
  arrange(desc(n))
  

