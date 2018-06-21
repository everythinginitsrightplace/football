library("httr")          # to html parsing (version 1.2.0)
library("htmltab")       # easy tool to get html tables (version 0.7.0)
library("data.table")    # to data manipulation (version 1.9.6)
library("XML")           # to work with xml nodes (version 3.98-1.4)
devtools::install_github("mattflor/chorddiag")
library("chorddiag")     # provide d3 chordiagramm (version 0.0.1)
library(stringr)
library(rvest)
library(httr)
library(xml2)
library(XML)
library(dplyr)

fifa.2018.squads.url <- "https://en.wikipedia.org/wiki/2018_FIFA_World_Cup_squads"
wiki.page <- GET(fifa.2018.squads.url)
temp.file <- content(wiki.page, "text")
temp.html <- htmlTreeParse(temp.file, useInternalNodes = TRUE)
leagues <- xpathSApply(temp.html, "//table[@class='sortable wikitable plainrowheaders']//span[@class='flagicon']/a", xmlAttrs)
leagues <- leagues["title",]
unlist(str_extract_all(leagues, "Northern Ireland"))

national.teams <- xpathSApply(temp.html, "//h3//span[@class='mw-headline']", xmlValue)[1:32]
national.teams <- c("Египет", "Россия", "Саудовская Аравия", "Уругвай", "Иран", "Марокко", "Португалия", "Испания", "Австралия", "Дания", "Франция", "Перу", "Аргентина",
                    "Хорватия", "Исландия", "Нигерия", "Бразилия", "Коста-Рика", "Сербия", "Швейцария", "Германия", "Мексика",
                    "", "", "", "", "", "", "", "", "", "", "")
players <- lapply(1:32, function(i)
  data.table(htmltab(temp.html, which = i), squad = national.teams[i]))

players <- rbindlist(players)
players[,league_country:=leagues]

# Swansea City and Cardiff City teams play in English football leagues  
players[Club %in% c("Swansea City", "Cardiff City"), league_country := "England"]
setkey(players, squad, league_country)

squad.league.db <- players[, list(value = length(Player)), by = key(players)]
cntries <- sort(national.teams)
setnames(squad.league.db, 1:2, c("to_cntry", "from_cntry"))
squad.league.db <- squad.league.db[from_cntry %in% cntries]
setkey(squad.league.db, from_cntry, to_cntry)


# initialize adjacency matrix
adjmat <- data.table(expand.grid(from_cntry = cntries,
                                 to_cntry = cntries))
setkey(adjmat, from_cntry, to_cntry)
adjmat <- squad.league.db[adjmat]
adjmat[is.na(value), value:=0L]
adjmat <- t(matrix(adjmat[,value], nrow=length(cntries)))

colnames(adjmat) <- rownames(adjmat) <- cntries
ord <- order(-rowSums(adjmat))
adjmat <- adjmat[ord,ord]

# set number of colors needed
colorCount <- length(cntries)
# makes function to create palette
getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
diag.colors <- getPalette(colorCount)
names(diag.colors) <- colnames(adjmat)

chorddiag(adjmat, showTicks =FALSE, margin = 80,
          groupnameFontsize = 12,
          groupnamePadding = 5,
          groupThickness = .05,
          groupColors = unname(diag.colors),
          width = 820, height = 820
)

adjmat2 <- adjmat
diag(adjmat2) <- 0
ord2 <- order(-rowSums(adjmat2))
adjmat2 <- adjmat2[ord2, ord2]
chorddiag(adjmat2, showTicks =FALSE, margin = 80,
          groupnameFontsize = 12,
          groupnamePadding = 5,
          groupThickness = .05,
          groupColors = unname(diag.colors[colnames(adjmat2)]),
          width = 820, height = 820    
)

adjmat.restricted <- adjmat2[1:7, 1:7]
diag(adjmat.restricted) <- 0

chorddiag(adjmat.restricted,
          showTicks =FALSE, margin = 80,
          groupnameFontsize = 12,
          groupnamePadding = 5,
          groupThickness = .05,
          groupColors = unname(diag.colors[colnames(adjmat.restricted)]),
          width = 820, height = 820    
)

write.csv(players, "players2018.csv")
