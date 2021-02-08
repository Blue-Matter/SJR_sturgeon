

library(dplyr)
library(readxl)

dir <- getwd()
data_path <- file.path(dir, "data")

#### Most fish are double tagged with PIT and a floy tag
#### Track records of PIT tags, then any extra fish with a floy-only tag
tag_pit <- readxl::read_excel(file.path(data_path, "sturgeon 2009_2020 tagging.xlsx"), range = "A1:AD6021") %>%
  dplyr::filter(!is.na(PIT)) %>% mutate(ID = PIT, tag_type = "PIT")

tag_floy <- readxl::read_excel(file.path(data_path, "sturgeon 2009_2020 tagging.xlsx"), range = "A1:AD6021") %>%
  #dplyr::filter(!is.na(Number) & !is.na(Floy)) %>%
  dplyr::filter(is.na(PIT) & (!is.na(Number) | !is.na(Floy))) %>%
  dplyr::mutate(ID = ifelse(Series == "B" | Series == "b" | grepl("UNB", Series) | Series == "unb", 
                            ifelse(nchar(Number) == 3, paste0("B", 0, Number), paste0("B", Number)), 
                            ifelse(Series == "A" | grepl("cadia", Series) | grepl("CADIA", Series),
                                   ifelse(nchar(Number) == 3, paste0("A", 0, Number), paste0("A", Number)), 
                                   ifelse(grepl("L", Series) | grepl("l", Series),
                                          ifelse(nchar(Number) == 3, paste0("L", 0, Number), paste0("L", Number)),
                                          ifelse(is.na(Series), Number, paste0(Series, Number))))),
                tag_type = "Floy")

tag_df <- rbind(tag_pit, tag_floy)
tag_list <- split(tag_df, tag_df$ID) 

#### Create summary 
tag_summary <- lapply(tag_list, function(x) {
  ID <- x$ID %>% unique()
  Ytag <- x[1, ]$YYYY
  rel_status <- x[1, ]$Status
  if(nrow(x) > 1) {
    Yrecap <- paste0(x[-1, ]$YYYY, collapse = ", ")
  } else {
    Yrecap <- NA_character_
  }
  data.frame(ID = ID, Ytag = Ytag, Yrecap = Yrecap, n_recap = nrow(x) - 1, rel_status = rel_status,
             tag_type = x$tag_type %>% unique())
}) %>% do.call(rbind, .)

tag_summary[order(tag_summary$Ytag), ] %>% 
  write.csv(file.path("processed_data", "unique_tags.csv"))

# Release-recap events
tag_events_recap <- lapply(tag_list, function(xx) {
  ID_out <- xx$ID %>% unique()
  tag_out <- xx$tag_type %>% unique()
  
  x <- xx %>% mutate(ymd = paste0(YYYY, "-", MM, "-", DD) %>% lubridate::ymd())
  x <- x[order(x$ymd), ]
  
  if(nrow(x) > 1) {
    rel_df <- x[-nrow(x), ]
    recap_df <- x[2:nrow(x), ]
    
    rel_status <- rel_df$Status
    recap_status <- recap_df$Status
    
    rel <- rel_df$ymd
    recap <- recap_df$ymd
    
    data.frame(rel = rel, recap = recap, Days_at_liberty = recap - rel, 
               Yrel = lubridate::year(rel), Yrecap = lubridate::year(recap),
               rel_status = rel_status, recap_status = recap_status, 
               rel_Series = rel_df$Series, rel_Number = rel_df$Number,
               recap_Series = recap_df$Series, recap_Number = recap_df$Number,
               rel_PIT = rel_df$PIT, recap_PIT = recap_df$PIT,
               rel_sex = rel_df$Sex, recap_sex = recap_df$Sex,
               rel_TL = rel_df$`TL(cm)` %>% as.numeric(), recap_TL = recap_df$`TL(cm)` %>% as.numeric()) %>% 
      mutate(ID = as.character(ID_out), tag_type = tag_out)
  } else {
    data.frame()
  }
  
}) %>% do.call(rbind, .) %>% 
  dplyr::filter(as.numeric(rel_TL) >= 150,
                !rel_status %in% c("H", "RM", "RMPY", "P", "p", "DEAD"))

tag_events_recap[order(tag_events_recap$Yrel), ] %>% 
  write.csv(file.path("processed_data", "tag_events_recap.csv"))

# Tag release events
tag_events_rel <- lapply(tag_list, function(xx) {
  ID_out <- xx$ID %>% unique()
  tag_out <- xx$tag_type %>% unique()
  
  x <- xx %>% mutate(ymd = paste0(YYYY, "-", MM, "-", DD) %>% lubridate::ymd())
  x <- x[order(x$ymd), ]
  
  final_tag_event_dead <- x$Status[nrow(x)] %in% c("dead", "DEAD", "h", "H", "p", "P", "RM", "RMPY")
  
  if(nrow(x) > 1) {
    rel_df <- x[-nrow(x), ]
    recap_df <- x[2:nrow(x), ]
    
    keep <- c(recap_df$YYYY - rel_df$YYYY > 0, !final_tag_event_dead)
  } else {
    keep <- !final_tag_event_dead
  }
  
  if(sum(keep)) {
    xxx <- x[keep, ]
    data.frame(rel = xxx$ymd, Yrel = lubridate::year(xxx$ymd),
               rel_status = xxx$Status, rel_Series = xxx$Series, rel_Number = xxx$Number,
               rel_PIT = xxx$PIT, rel_sex = xxx$Sex,
               rel_TL = xxx$`TL(cm)`) %>% 
      mutate(ID = as.character(ID_out), tag_type = tag_out)
  } else {
    data.frame()
  }
}) %>% do.call(rbind, .) %>% dplyr::filter(!is.na(as.numeric(rel_TL)), as.numeric(rel_TL) >= 150)

tag_events_rel[order(tag_events_rel$Yrel), ] %>% 
  write.csv(file.path("processed_data", "tag_events_rel.csv"))

# Quality check
table(tag_events_rel$rel_status)
dplyr::filter(tag_events_rel, rel_status %in% c("DEAD", "H", "p", "P", "RM", "RMPY")) %>%
  select(rel_status, tag_type)


# Number of releases by year
tag_rel_years <- group_by(tag_events_rel, Yrel) %>% summarise(n = n())
barplot(n ~ Yrel, tag_rel_years)

# Summarize tag events vs. year of recapture
tag_events_yr <- group_by(tag_events_recap, Yrel, Yrecap, tag_type) %>% summarise(n_recap = n()) %>%
  dplyr::left_join(tag_rel_years, by = "Yrel") %>% mutate(Yrel2 = paste0(Yrel, " (", n, " releases", ")"))
ggplot(tag_events_yr, aes(as.factor(Yrecap), n_recap)) + 
  #geom_bar(stat = 'identity', data = tag_rel_years, aes(as.factor(Yrel), n)) + 
  #geom_text(data = tag_rel_years, aes(label = n)) + 
  geom_bar(stat = 'identity', aes(fill = tag_type)) + 
  facet_wrap(~Yrel2, scales = "free") + 
  labs(x = "Year of recapture", y = "Number of recaptures") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_fill_discrete(name = "Tag type")
ggsave("figures/data/tag_rel_vs_recapture.png", height = 6, width = 8)

ggplot(tag_events_yr %>% dplyr::filter(Yrecap - Yrel > 0), aes(as.factor(Yrecap), n_recap)) + 
  geom_bar(stat = 'identity', aes(fill = tag_type)) + 
  facet_wrap(~Yrel2, scales = "free") + 
  labs(x = "Year of recapture", y = "Number of recaptures") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  scale_fill_discrete(name = "Tag type")
ggsave("figures/data/tag_recapture_liberty_oneyear.png", height = 6, width = 8)

# Years at liberty for all tag events
tag_events_yr_all <- tag_events_yr %>% mutate(years_at_liberty = Yrecap - Yrel) %>% 
  group_by(years_at_liberty) %>% summarise(n_recap = sum(n_recap))
ggplot(tag_events_yr_all, aes(years_at_liberty, n_recap)) + geom_bar(stat = "identity") + 
  labs(x = "Years at liberty", y = "Number of recaptures") + theme_bw()
ggsave("figures/data/tag_recapture_summary.png", height = 3, width = 4)

ggplot(tag_events_yr_all %>% dplyr::filter(years_at_liberty > 0), aes(years_at_liberty, n_recap)) + geom_bar(stat = "identity") + 
  labs(x = "Years at liberty", y = "Number of recaptures") + theme_bw()
ggsave("figures/data/tag_recapture_summary_oneyearliberty.png", height = 3, width = 4)

# Growth vs. time at liberty
#plot(tag_events$Days_at_liberty/365, tag_events$rel_TL, ylim = c(150, 300), typ = 'n')
#arrows(x0 = 0, x1 = tag_events$Days_at_liberty/365, y0 = tag_events$rel_TL, 
#       y1 = tag_events$recap_TL, length = 0)
#points(tag_events$Days_at_liberty/365, tag_events$rel_TL)
#points(tag_events$Days_at_liberty/365, tag_events$recap_TL, col = 'red')
#
#plot(tag_events$rel_TL, tag_events$recap_TL)
#arrows(x0 = 0, x1 = tag_events$Days_at_liberty/365, y0 = tag_events$rel_TL, 
#       y1 = tag_events$recap_TL, length = 0)
#points(tag_events$Days_at_liberty/365, tag_events$rel_TL)
#points(tag_events$Days_at_liberty/365, tag_events$recap_TL, col = 'red')

# Size at release vs. time at liberty
png("figures/data/tag_size_release.png", width = 4, height = 3, units = "in", res = 400)
par(mar = c(5, 4, 1, 1))
hist(tag_events_rel$rel_TL %>% as.numeric(), seq(0, 260, 5), xlab = "Size at release (cm)", main = "")
dev.off()

rgl::plot3d(tag_recap$rel_TL, tag_recap$Days_at_liberty, tag_recap$recap_TL)

ggplot(tag_recap, aes(rel_TL, recap_TL, 
                      colour = Days_at_liberty %>% as.numeric %>% '/'(365) %>% round() %>% factor())) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) + 
  geom_point() + coord_cartesian(xlim = c(150, 250), ylim = c(150, 250)) + theme_bw() +
  labs(x = "Length at release (cm)", y = "Length at recapture (cm)") +
  scale_colour_viridis_d(name = "Time at\nliberty (years)")
ggsave("figures/data/tag_size_recap.png", width = 5, height = 3)


# For SS - models only included tags at liberty >= 1 year, but this doesn't matter as SS can't handle incomplete
# mixing assumptions
make_tag_rel_ss <- function() {
  ymin <- 2009
  
  data.frame(Yrel = tag_rel_years$Yrel - ymin + 1, Area = 1, Year = tag_rel_years$Yrel, Season = 1, tfill = 999,
             Sex = 0, Age = 24, nrel = tag_rel_years$n)
}
make_tag_rel_ss() %>% write.csv(file.path("processed_data", "tag_release.csv"))

make_tag_recap_ss <- function() {
  ymin <- 2009
  out <- group_by(tag_events_recap, Yrel, Yrecap) %>% summarise(n_recap = n())
  data.frame(Yrel = out$Yrel - ymin + 1, Year = out$Yrecap, Season = 1, Fleet = 1, nrecap = out$n_recap)
}
make_tag_recap_ss() %>% write.csv(file.path("processed_data", "tag_recapture.csv"))

# For Brownie model
make_tag_Brownie <- function() {
  N_tag <- tag_rel_years$n
  N_recap <- group_by(tag_events_yr, Yrel, Yrecap) %>% summarise(n = sum(n_recap)) %>% 
    reshape2::acast(list("Yrel", "Yrecap"), fill = 0, value.var = "n")
  list(N_tag = N_tag, N_recap = N_recap)
}
make_tag_Brownie() %>% saveRDS("processed_data/tag_table.rds")

