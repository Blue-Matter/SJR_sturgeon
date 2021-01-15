

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

# Number of releases by year
tag_years <- lapply(tag_list, function(x) if(nrow(x) == 1) x$YYYY else x$YYYY[-length(x$YYYY)]) %>%
  do.call(c, .)
hist(tag_years)

# n_recap per tag
png("figures/data/recaptures_per_tag.png", height = 4, width = 5, units = "in", res = 400)
hist(tag_summary$n_recap, breaks = 0:7, main = NA, xlab = "Number of recaptures per tag")
dev.off()

# Release-recap events
tag_events <- lapply(tag_list, function(xx) {
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
  dplyr::filter(!is.na(Days_at_liberty), !rel_status %in% c("H", "RM", "RMPY", "P", "p", "DEAD"))

tag_events[order(tag_events$Yrel), ] %>% 
  write.csv(file.path("processed_data", "tag_events.csv"))

# Summarize tag events vs. day
tag_events_dal <- group_by(tag_events, Yrel, Days_at_liberty, tag_type) %>% summarise(n_recap = n())
ggplot(tag_events_dal, aes(Days_at_liberty/365, n_recap, fill = tag_type)) + geom_bar(stat = 'identity') + 
  facet_wrap(~Yrel, scales = "free") + 
  labs(x = "Time at liberty (years)", y = "Number of recaptures") +
  gfplot::theme_pbs()

# Summarize tag events vs. year of recapture
tag_events_yr <- group_by(tag_events, Yrel, Yrecap, tag_type) %>% summarise(n_recap = n())
ggplot(tag_events_yr, aes(as.factor(Yrecap), n_recap, fill = tag_type)) + 
  geom_bar(stat = 'identity') + facet_wrap(~Yrel, scales = "free") + 
  labs(x = "Year of recapture", y = "Number of recaptures") +
  gfplot::theme_pbs() + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
ggsave("figures/data/tag_recapture.png", height = 6, width = 8)

# Summarize tag events vs. year of recapture, years at liberty greater than zero
tag_events %>% group_by(Yrel, Yrecap, tag_type) %>% summarise(n_recap = n()) %>%
  dplyr::filter(Yrecap - Yrel > 0) %>%
  ggplot(aes(as.factor(Yrecap), n_recap, fill = tag_type)) + 
  geom_bar(stat = 'identity') + facet_wrap(~Yrel, scales = "free") + 
  labs(x = "Year of recapture", y = "Number of recaptures") +
  gfplot::theme_pbs() + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
ggsave("figures/data/tag_recapture2.png", height = 6, width = 8)

# Years at liberty for all tag events
tag_events_yr_all <- tag_events_yr %>% mutate(years_at_liberty = Yrecap - Yrel) %>% 
  group_by(years_at_liberty) %>% summarise(n_recap = sum(n_recap))
ggplot(tag_events_yr_all, aes(years_at_liberty, n_recap)) + geom_bar(stat = "identity") + 
  labs(x = "Years at liberty", y = "Number of recaptures") + gfplot::theme_pbs()
ggsave("figures/data/tag_recapture_summary.png", height = 3, width = 4)

ggplot(tag_events_yr_all[-1, ], aes(years_at_liberty, n_recap)) + geom_bar(stat = "identity") + 
  labs(x = "Years at liberty", y = "Number of recaptures") + gfplot::theme_pbs()
ggsave("figures/data/tag_recapture_summary.png2", height = 3, width = 4)

# Growth vs. time at liberty
plot(tag_events$Days_at_liberty/365, tag_events$rel_TL, ylim = c(150, 300), typ = 'n')
arrows(x0 = 0, x1 = tag_events$Days_at_liberty/365, y0 = tag_events$rel_TL, 
       y1 = tag_events$recap_TL, length = 0)
points(tag_events$Days_at_liberty/365, tag_events$rel_TL)
points(tag_events$Days_at_liberty/365, tag_events$recap_TL, col = 'red')

plot(tag_events$rel_TL, tag_events$recap_TL)
arrows(x0 = 0, x1 = tag_events$Days_at_liberty/365, y0 = tag_events$rel_TL, 
       y1 = tag_events$recap_TL, length = 0)
points(tag_events$Days_at_liberty/365, tag_events$rel_TL)
points(tag_events$Days_at_liberty/365, tag_events$recap_TL, col = 'red')


# Tag releases - summary by sex and year
tag_rel <- lapply(tag_list, function(x) {
  if(x[nrow(x), ]$Status %in% c("dead", "DEAD", "h", "H", "p", "P", "RM", "RMPY")) {
    x <- x[-nrow(x), ]
  }
  select(x, Yrel = YYYY, Status = Status, Sex = Sex, tag_type = tag_type)
}) %>% do.call(rbind, .) %>% group_by(Yrel, Sex) %>% summarise(n = n())

tag_recap <- group_by(tag_events, Yrel, Yrecap) %>% summarise(n_recap = n())


# For SS
make_tag_rel_ss <- function() {
  ymin <- 2009
  out <- group_by(tag_rel, Yrel) %>% summarise(n = sum(n))
  
  data.frame(Yrel = out$Yrel - ymin + 1, Area = 1, Year = out$Yrel, Season = 1, tfill = 999,
             Sex = 0, Age = 24, nrel = out$n)
}
make_tag_rel_ss() %>% write.csv(file.path("processed_data", "tag_release.csv"))



make_tag_recap_ss <- function() {
  ymin <- 2009
  out <- group_by(tag_events, Yrel, Yrecap) %>% summarise(n_recap = n())
  data.frame(Yrel = out$Yrel - ymin + 1, Year = out$Yrecap, Season = 1, Fleet = 1, nrecap = out$n_recap)
}
make_tag_recap_ss() %>% write.csv(file.path("processed_data", "tag_recapture.csv"))
