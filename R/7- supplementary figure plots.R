library(tidyverse)

# read datafile containing pubmed data
pubmed_data <- read.csv("PubMed_Timeline_Results_by_Year.csv")
# add column denoting which era each year falls under
pubmed_data$era <- ifelse(pubmed_data$Year<2013, "pre", "post")
# mutate df to include rate of increase for each year
pubmed_data <- pubmed_data %>%
  mutate(diff_year = Year - lag(Year),  # Difference in time (just in case there are gaps)
         diff_growth = Count - lag(Count), # Difference in growth between years
         rate_percent = (diff_growth / diff_year)/Count * 100) # growth rate in percent
# slice off 1977 (no rate) and 2021 (incomplete year)
pubmed_data <- slice(pubmed_data, 2:39)

# calculate mean differences in growth for the pre- and post-CRISPR eras
mean_diff_growth_pre_PM <- sum(pubmed_data$diff_growth[1:31])/length(pubmed_data$diff_growth[1:31])
mean_diff_growth_post_PM <- sum(pubmed_data$diff_growth[32:38])/length(pubmed_data$diff_growth[32:38])

# plot papers published by year and % increase in papers on previous year on same x axis
ggplot(pubmed_data, aes(Year, Count)) +
  geom_area(alpha = 0.6, fill = "#91bdda") +
  geom_bar(stat = "identity", data = pubmed_data, aes(Year, diff_growth/0.4), fill = "#cc474d") + #divide by 0.4 to counteract transformation below
  theme_minimal() +
  xlab("Year") +
  scale_y_continuous(name = "Total papers published", sec.axis = sec_axis( trans=~.*0.4, name="% increase in papers on previous year")) + #add second axis which transforms data by times 0.4
  theme(axis.title.y.right = element_text(colour = "#cc474d", face = "bold", vjust = 2)) +
  theme(axis.title.y.left = element_text(colour = "#1c4764", face = "bold")) +
  theme(axis.text.y.right = element_text(colour = "#cc474d")) +
  theme(axis.text.y.left = element_text(colour = "#1c4764")) +
  geom_segment(aes(x = 2012, y = -3,
                   xend = 2012, yend = 0),
               arrow = arrow(length = unit(0.5, "cm"),ends = "last"),
               size = 2,
               lineend = "butt",
               linejoin = "mitre") +
  geom_segment(aes(x = 2012, y = -500,
                   xend = 2012, yend = 0),
               size = 2)
# load cowplot package for compound figure creation
library(cowplot)
# create plot of papers published per year
count_plot <- ggplot(pubmed_data, aes(Year, Count)) +
  geom_area(alpha = 0.6, fill = "#91bdda", col = "#91bdda", size = 2) +
  theme_minimal() +
  ylab("Total papers published") +
  xlab("") +
  geom_segment(aes(x = 2012, y = 300,
                   xend = 2012, yend = 290),
               arrow = arrow(length = unit(0.5, "cm"),ends = "last"),
               size = 1.5,
               lineend = "butt",
               linejoin = "mitre") +
  geom_segment(aes(x = 2012, y = 290,
                   xend = 2012, yend = 900),
               size = 1.5) +
  theme(text = element_text(size = 20))
# create plot of % increase...
growth_plot <- ggplot(pubmed_data, aes(Year, diff_growth)) +
  geom_bar(stat = "identity", fill = "#cc474d") +
  theme_minimal() +
  ylab("Increase in papers on\n previous year") +
  xlab("") +
  geom_segment(aes(x = 2012, y = -1,
                   xend = 2012, yend = 0),
               arrow = arrow(length = unit(0.5, "cm"),ends = "last"),
               size = 1.5,
               lineend = "butt",
               linejoin = "mitre") +
  geom_segment(aes(x = 2012, y = -200,
                   xend = 2012, yend = 0),
               size = 1.5) +
  theme(text = element_text(size = 20))
# facet last two plots one above the other
plot_grid(count_plot, growth_plot, ncol = 1, labels = c("A", "B"))

# CRISPR mentions ---------------------------------------------------------
# import data from pubmed about CRISPR mentions
pubmed_crispr_data <- read.csv("PubMed_CRISPR.csv")
# arrange rows in descending order
pubmed_crispr_data <- arrange(pubmed_crispr_data, desc(row_number()))
# calculate log of CRISPR mentions per year
pubmed_crispr_data$log_count <- log(pubmed_crispr_data$Count)

# plot year against log count of CRISPR mentions
ggplot(pubmed_crispr_data[10:19,], aes(Year, log_count)) +
  geom_point() +
  geom_smooth(aes(Year, log_count), method = "loess", se = F, colour = "#cc474d") +
  theme_minimal() +
  ylab("Log of total papers published\n") +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020))

# CRISPR vs ZFN vs TALENs -------------------------------------------------
# save pubmed crispr data to new variable
crispr <- pubmed_crispr_data %>%
  arrange(desc(row_number()))
# import data from pubmed about ZFN mentions
zfn <- read.csv("pubmed zfn.csv", skip = 1) %>%
  arrange(desc(row_number()))
# import data from pubmed about TALENs mentions
talens <- read.csv("pubmed talens.csv", skip = 1) %>%
  arrange(desc(row_number()))

# make a new df from crispr df
CZT <- crispr
# join the ZFN data by matching Year columns
CZT <- full_join(CZT, zfn, by = c("year" = "Year"))
# join the TALENs data by matching year columns
CZT <- full_join(CZT, talens, by = c("year" = "Year"))
# remame columns
CZT <- rename(CZT, CRISPR = count, ZFN = Count.x, TALENs = Count.y)
# mutate to be in tidy format
CZT <- pivot_longer(CZT, !year, names_to = "Technique", values_to = "Count")
# replace NA values with 0s
CZT <-  CZT %>%
  mutate_if(is.numeric , replace_na, replace = 0) %>%
  arrange(year)

# plot CZT df excluding 2021 values
ggplot(CZT[1:156,], aes(year, Count, col = Technique, fill = Technique)) +
  geom_line(size = 2) +
  theme_minimal() +
  xlim(2005, 2020) +
  xlab("") +
  ylab("Papers published\n") +
  theme(text = element_text(size = 26)) +
  scale_colour_manual(values = c("#cc474d", "#91bdda", "#414535"))

# Wordcloud of 'discussion' topic (for presentation) -----------------------------------------
# load wordcloud2 package for wordcloud generation
library(wordcloud2)
model_tidy_beta_2020 %>%
  group_by(topic) %>%
  top_n(30, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  left_join(topic_labels_2020, by = "topic") %>%
  filter(label == "discussion") %>%
  select(term, beta) %>%
  rename(word = term, freq = beta) %>%
  wordcloud2(shuffle = F, size = 0.6, rotate = 0, color = "random-dark")