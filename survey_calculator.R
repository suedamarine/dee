library(rcompanion)
library(tidyverse)

## import file - in future create new data.frame from CSV, using vectors to import count & mass into individual columns

temp_cockle <- read.csv("data/temp_cockle.csv")





sampled_md <- sum(temp_cockle$Sampled == "Y" & temp_cockle$Block =="Mostyn Deep", na.rm = TRUE)
sampled_sm <- sum(temp_cockle$Sampled == "Y" & temp_cockle$Block =="Salisbury Middle", na.rm = TRUE)

# import and tidy count data
cockle_counts <- temp_cockle %>%
  select(Stn, Block, Grid, Sampled, Y0Count, Y1Count, Y2Count, Y3Count) %>% 
  mutate(total_count = Y0Count + Y1Count + Y2Count + Y3Count) %>% 
  gather(year_class, count, 'Y0Count', 'Y1Count', 'Y2Count', 'Y3Count', 'total_count')

# summary table count (selected blocks)
summary_counts_blocks <- cockle_counts %>% 
  filter(Block %in% c ("Mostyn Deep", "Salisbury Middle")) %>%
  mutate(count_sum = count*10*150^2) %>%
  group_by(year_class, Block) %>%
  summarize(count_totals = sum(count_sum, na.rm = TRUE))

# import and tidy mass data
cockle_mass <- temp_cockle %>%
  select(Stn, Block, Grid, Sampled, Y0Weight, Y1Weight, Y2Weight, Y3Weight, twenty) %>%
  mutate(total_weight = Y0Weight + Y1Weight + Y2Weight+ Y3Weight) %>%
  gather(year_class, mass, 'Y0Weight', 'Y1Weight', 'Y2Weight', 'Y3Weight', 'total_weight', 'twenty')

# summary table mass (selected blocks)
summary_mass_blocks <- cockle_mass %>% 
  filter(Block %in% c ("Mostyn Deep", "Salisbury Middle")) %>%
  mutate(mass_sum = (mass/100)*150^2) %>%
  group_by(year_class, Block) %>%
  summarize(mass_totals = sum(mass_sum, na.rm = TRUE))

# groupwise means for counts, filtered to remove NAs

g_150_c <- cockle_counts %>% 
  filter(year_class %in% c("Y0Count", "Y1Count", "Y2Count", "Y3Count", "total_count")) %>%
  filter(!is.na(count))

# perform the groupwisemean selecting 10000 replicates and Bca
count_conf_md <- groupwiseMean(count ~ year_class + Block, data = g_150_c, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

# calculate count confidence intervals per block
count_intervals_md <- count_conf_md %>%  
  mutate(count_lower =  Bca.lower * 10 * 150^2 * n) %>%
  mutate(count_upper = Bca.upper * 10 * 150^2 * n) %>%
  mutate(total_mean = Mean * 10 * 150^2*n)

# write file to csv
write.csv(count_intervals_md, "tabs/count_intervals_md.csv")


# groupwise means for mass, filtered to remove NAs

g_150_m <- cockle_mass %>% 
  filter(year_class %in% c("Y0Weight", "Y1Weight", "Y2Weight", "Y3Weight", "total_weight", "twenty")) %>%
  filter(!is.na(mass))

# perform the groupwisemean selecting 10000 replicates and Bca
mass_conf_md <- groupwiseMean(mass ~ year_class + Block, data = g_150_m, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

# calculate count confidence intervals per block
mass_intervals_md <- mass_conf_md %>%  
  mutate(mass_lower =  Bca.lower * 0.01 * 150^2 * n) %>%
  mutate(mass_upper = Bca.upper * 0.01 * 150^2 * n) %>%
  mutate(total_mean = Mean * 0.01 * 150^2*n)

# write file to csv
write.csv(mass_intervals_md, "tabs/mass_intervals_md.csv")

