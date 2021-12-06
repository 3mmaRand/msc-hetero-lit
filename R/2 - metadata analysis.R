library(tidyverse) # for general data manipulation and graphing

setwd("~/My Files/University/Work/3rd year/BSc Research project/dissertation")
# import metadata from part 1
metadata <- read.table("metadata.csv", header = T)

# Rate of publication -------------------------------------------------------
# calculate number of papers published per year
year_counts <- count(metadata, vars = PY) %>%
  rename(year = vars, count = n)

#calculate the percentage increase in number of papers
rate_increase <- year_counts %>%
  mutate(diff_year = year - lag(year),  # Difference in time (just in case there are gaps)
  diff_growth = count - lag(count), # Difference in growth between years
  rate_percent = (diff_growth / diff_year)/count * 100) # growth rate in percent
# exclude first row (1988 - no rate data) and last row (2021 - incomplete data)
rate_increase <- slice(rate_increase, 2:33)

# calculate the mean difference in growth for the pre- and post-CRISPR periods
mean_diff_growth_pre <- sum(rate_increase$diff_growth[1:24])/length(rate_increase$diff_growth[1:24])
mean_diff_growth_post <- sum(rate_increase$diff_growth[25:32])/length(rate_increase$diff_growth[25:32])

# write data to file
write.table(rate_increase, "rate_increase.csv")

# load cowplot for compound figure creation
library(cowplot)
# draw plot of count over time with arrow pointing out the year 2012
count_plot <- ggplot(rate_increase, aes(year, count)) +
                geom_area(alpha = 0.6, fill = "#91bdda", col = "#91bdda", size = 2) +
                theme_minimal() +
                xlab("") +
                ylab("Total papers published") +
  theme(text = element_text(size = 20)) +
  geom_segment(aes(x = 2012, y = 200,
                   xend = 2012, yend = 185),
               arrow = arrow(length = unit(0.5, "cm"),ends = "last"),
               size = 1.5,
               lineend = "butt",
               linejoin = "mitre") +
  geom_segment(aes(x = 2012, y = 185,
                   xend = 2012, yend = 350),
               size = 1.5) +
  theme(text = element_text(size = 20))
# draw plot of difference in growth over time with arrow for 2012
growth_plot <- ggplot(rate_increase, aes(year, diff_growth)) +
                geom_bar(stat = "identity", fill = "#cc474d") +
                theme_minimal() +
                xlab("") +
                ylab("Increase in papers on\n previous year") +
  theme(text = element_text(size = 20)) +
  geom_segment(aes(x = 2012, y = -1,
                   xend = 2012, yend = 0),
               arrow = arrow(length = unit(0.5, "cm"),ends = "last"),
               size = 1.5,
               lineend = "butt",
               linejoin = "mitre") +
  geom_segment(aes(x = 2012, y = -40,
                   xend = 2012, yend = 0),
               size = 1.5) +
  theme(text = element_text(size = 20))
# output plots one on top of the other using cowplot
plot_grid(count_plot, growth_plot, ncol = 1)

# Categories --------------------------------------------------------------

# separate out categories where multiple are listed in one column
category_slice <- separate_rows(metadata, WC, sep = ";") %>%
  gdata::trim()

# find out how many papers in each category
category_counts <- category_slice %>%
  count(vars = WC, .drop = F) %>%
  rename(category = vars, count = n)
# select only the 15 most commonly recorded categories
category_counts_top <- category_counts %>%
  slice_max(order_by = count, n = 15) %>%
  arrange(desc(count))

# convert the 'WC' (category) field to a character vector
category_slice$WC <- as.character(category_slice$WC)
# calculate the number of papers published per category per year
category_years_counts <- category_slice %>%
  group_by(WC, PY, .drop = F) %>%
  count(PY) %>%
  ungroup() %>%
  complete(WC, PY,
           fill = list(N = 0, freq = 0)) %>%
  mutate(n = replace_na(n, 0)) %>%
  rename(year = PY, count = n)
# set NA values to 0
category_years_counts[is.na(category_years_counts)] <- 0

# name the second column of year_counts
names(year_counts)[2] <- "total"
# merge to give a table with the count per category per year and the total number over all categories per year
category_years_counts <- merge(category_years_counts, year_counts, by = "year")
# calculate the % of papers which were from each category per year
category_years_counts$percent <- (category_years_counts$count/category_years_counts$total)*100

#write to file
write.table(category_years_counts, "category_years_counts.csv")

# pull out top 15 categories overall as a vector
top15 <- category_counts_top$category
# see how many papers were published in each of the top 15 categories per year
category_counts_year_top <- subset(category_years_counts, WC %in% top15)

# Non-interactive version of shiny graph - total papers published in each category, by year
ggplot(subset(subset(category_years_counts, year == 2011), WC %in% top15)) +
  geom_col(aes(WC, percent, fill = WC)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#non-interactive version of shiny graph - total papers published each year, by category
ggplot(subset(category_years_counts, WC == "Immunology")) +
  geom_col(aes(year, count, fill = WC)) +
  scale_y_continuous(limits = c(0, 250)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Category ranks -------------------------------------------------------------------------

# subset category_slice to get only papers published before 2013
category_slice_pre <- subset(category_slice, PY < 2013)
# calculate how many papers published per category per year for this subset and arrange in descending order
category_counts_pre <- category_slice_pre %>%
  count(vars = WC, .drop = F) %>%
  rename(category = vars, count_pre = n) %>%
  arrange(desc(count_pre))
# rank each category by number of papers
category_counts_pre$rank_pre <- rank(-category_counts_pre$count_pre, ties.method = "max")

# as previous block but for papers published after 2012
category_slice_post <- subset(category_slice, PY >2012)
category_counts_post <- category_slice_post %>%
  count(vars = WC, .drop = F) %>%
  rename(category = vars, count_post = n) %>%
  arrange(desc(count_post))
category_counts_post$rank_post <- rank(-category_counts_post$count_post, ties.method = "max")
category_counts_post$rank_post[15] <- 15

# merge the two dataframes of pre- and post-2012 counts/ranks
compare_category_counts <- merge(category_counts_pre, category_counts_post, by = "category")
# calculate how the rank has changed from pre-2012 to post
compare_category_counts$change_rank <- compare_category_counts$rank_pre - compare_category_counts$rank_post
# arrange in ascending order of rank post-2012
compare_category_counts <- arrange(compare_category_counts, compare_category_counts$rank_post)

# select only the rank-related columns and top 15 rows
compare_category_counts_top <- select(compare_category_counts, category, rank_pre, rank_post, change_rank) %>%
  slice(1:15)
# if rank has increased, add column with ▲, if it has decreased, use ▼
compare_category_counts_top$symbol <- ifelse(compare_category_counts_top$change_rank > 0, "▲", "▼")
# rename column headings
compare_category_counts_top <- compare_category_counts_top %>% 
  rename(Category = category, "Rank pre-CRISPR" = rank_pre, "Rank post-CRISPR" = rank_post, "Change" = change_rank, " " = symbol)

# load gt package for table creation
library(gt)
# pass the table of category ranks and changes to the gt() function to produce neat table
compare_category_table <- compare_category_counts_top %>% 
  gt()
compare_category_table
# load webshot package to save image of gt table
library(webshot)
gtsave(compare_category_table, "compare_category_table.png")