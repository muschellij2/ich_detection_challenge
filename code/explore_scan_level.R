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

true_ids = train %>% 
  filter(any > 0) %>% 
  select(scan_id) %>% 
  distinct() %>% 
  pull()
true = train %>% 
  filter(scan_id %in% true_ids)
true = true %>% 
  mutate(n = 1) %>% 
  group_by(scan_id, ss_file, outfile, maskfile) %>%
  summarise_at(
    vars(any,
         epidural,
         intraparenchymal,
         intraventricular,
         subarachnoid,
         subdural,
         n), sum) %>% 
  ungroup() %>% 
  distinct() %>%
  mutate(pct_any = any / n) %>% 
  arrange(desc(pct_any), n, scan_id)

get_stats = function(file, maskfile) {
  img = RNifti::readNifti(file)
  mask = RNifti::readNifti(maskfile)
  ind = which(mask > 0, arr.ind = TRUE)
  vals = img[ind]
  ind = as.data.frame(ind)
  ind$vals = vals
  stats = ind %>% 
    summarise(
      mn = mean(vals), 
      sd = sd(vals), 
      md = median(vals),
      mn_over_30 = mean(vals > 30 & vals < 80),
      mn_over_40 = mean(vals > 40 & vals < 80),
      mn_over_50 = mean(vals > 50 & vals < 80)
    )
  slice_stats = ind %>% 
    group_by(dim3) %>% 
    summarise(
      n = n(),
      mn = mean(vals), 
      sd = sd(vals), 
      md = median(vals),
      mn_over_30 = mean(vals > 30 & vals < 80),
      mn_over_40 = mean(vals > 40 & vals < 80),
      mn_over_50 = mean(vals > 50 & vals < 80)
    )
  return(list(stats = stats, slice_stats))
}

inds = c(1:10, (nrow(true) - 9):nrow(true))
res = mapply(get_stats, true$outfile[inds], true$maskfile[inds])



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
  

