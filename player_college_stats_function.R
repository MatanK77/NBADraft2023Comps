library(rvest)
library(xml2)


# This is the basic function I used to scrape data on college players from College Basketball Reference.
# It takes a single argument (name_id) which can be found in the URL of a player page on CBB Ref. It
# generally follows the format of first-last-#. This function does have a few bugs with college transfers
# and players who did not play a conference game, which require more specific scraping, but this function
# should work for >90% of college players on CBB Ref.

player_college_stats_function <- function(name_id) {


url <- paste0('https://www.sports-reference.com/cbb/players/', name_id, '.html')


web_page<- read_html(url)
#Only save and work with the body
body<-html_node(web_page,"body")
write_xml(body, "temp.xml")

#Find and remove comments
lines<-readLines("temp.xml")
lines<-lines[-grep("<!--", lines)]
lines<-lines[-grep("-->", lines)]
writeLines(lines, "temp2.xml")

#Read the file back in and process normally
body <-read_html("temp2.xml")
table <- html_nodes(body, "table") %>% 
  html_table()

player_advanced <- table[[9]]

player_per100 <- table[[7]]

player_pergame <- table[[1]]


if (nrow(player_pergame) > 2) {
  player_pergame <- tail(player_pergame, n = 2)
}


player_pergame_wider <- player_pergame %>%
  select(Season, G, GS, MP, SOS) %>%
  pivot_wider(names_from = Season, values_from = c(G, GS, MP, SOS))

colnames(player_pergame_wider) <-(c("G_last", "G_career", "GS_last", "GS_career", "MP_last", "MP_career", "SOS_last", 
                                      "SOS_career")) 

player_pergame_wider  <- player_pergame_wider %>%
  mutate(Player = name_id, .before = G_last)

if (nrow(player_per100) > 2) {
  player_per100 <- tail(player_per100, n = 2)
}

player_per100_wider <- player_per100 %>%
  select(Season, FG, FGA,`FG%`, `2P`, `2PA`, `2P%`, `3P`, `3PA`,`3P%`, FT, FTA, `FT%`, TRB, AST, STL, BLK, TOV, PF, PTS) %>%
  pivot_wider(names_from = Season, values_from = c( FG, FGA, `FG%`, `2P`, `2PA`, `2P%`, `3P`, `3PA`,
                                                    `3P%`, FT, FTA, `FT%`, TRB, AST, STL, BLK, TOV, PF, PTS)) 

colnames(player_per100_wider) <-(c("FG100_last", "FG100_career", "FGA100_last", "FGA100_career", "FG%_last", "FG%_career", "2P100_last", "2P100_career",
                                     "2PA100_last", "2PA100_career", "2P%_last", "2P%_career", "3P100_last", "3P100_career", "3PA100_last", "3PA100_career",
                                     "3P%_last", "3P%_career", "FT_last", "FT_career", "FTA_last", "FTA_career", "FT%_last", "FT%_career",
                                     "TRB100_last", "TRB100_career", "AST100_last", "AST100_career", "STL100_last", "STL100_career", "BLK100_last", "BLK100_career",
                                     "TOV100_last", "TOV100_career", "PF100_last", "PF100_career", "PTS100_last", "PTS100_career")) 

if (nrow(player_advanced) > 2) {
  player_advanced <- tail(player_advanced, n = 2)
}

player_advanced_wider <- player_advanced %>%
  select(Season, `TS%`, `eFG%`, `3PAr`, `FTr`, `ORB%`, `DRB%`,
         `TRB%`, `AST%`, `STL%`, `BLK%`, `TOV%`, `USG%`) %>%
  pivot_wider(names_from = Season, values_from = c( `TS%`, `eFG%`, `3PAr`, `FTr`, `ORB%`, `DRB%`,
                                                    `TRB%`, `AST%`, `STL%`, `BLK%`, `TOV%`, `USG%`)) 

colnames(player_advanced_wider) <-(c("TS%_last", "TS%_career", "eFG%_last", "eFG%_career", "3PAr_last", "3PAr_career", "FTr_last", "FTr_career",
                                       "ORB%_last", "ORB%_career", "DRB%_last", "DRB%_career", "TRB%_last", "TRB%_career", "AST%_last", "AST%_career",
                                       "STL%_last", "STL%_career", "BLK%_last", "BLK%_career", "TOV%_last", "TOV%_career", "USG%_last", "USG%_career"))

partial <- merge(player_pergame_wider, player_per100_wider)

player_stats <- merge(partial, player_advanced_wider)

return(player_stats)

}
