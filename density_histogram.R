# load libraries
library(tidyverse)

# import data
temp_sizes <- read.csv("data/temp_sizes.csv")

# produce new vectors for individual sizes
temp_length <- rep(temp_sizes$length, times = temp_sizes$quantity) 
temp_class <- rep(temp_sizes$year_class, times = temp_sizes$quantity)
temp_block <- rep(temp_sizes$Block, times = temp_sizes$quantity)

# new data frame for plots
temp_sizes_df <- data.frame(temp_length, temp_class, temp_block)

# calculate medians for each year class and block
mu <- temp_sizes_df %>%
  group_by(temp_class, temp_block) %>%
  summarize(grp_med = median(temp_length))

head(mu)

# plot 
p <- ggplot(temp_sizes_df, aes(x=temp_length, color=temp_class, fill=temp_class, position="dodge")) +
  geom_histogram(aes(y=..density..), binwidth = 1.0, alpha=0.4, position="identity") +
  geom_density(alpha=0.2, bw = 0.75) + geom_vline(data = mu, aes(xintercept=grp_med, colour = temp_class), linetype = "dashed") +
  scale_color_manual(values=c("#a6611a","#dfc27d","#80cdc1"), guide = FALSE)  +
  scale_fill_manual(name = "Class",labels = c("Year0", "Year1", "Year2+"), values=c("#a6611a","#dfc27d","#80cdc1")) +
  theme(legend.position="top") + labs(title="Autumn 2020 Cockle Size Distribution", x="Size mm", y = "Frequency") +
  theme_classic() +
  facet_wrap(~temp_block, ncol = 1)


# Open a pdf file
pdf("plots/freq_dist.pdf") 

# 2. Create a plot
p

# Close the pdf file
dev.off()


