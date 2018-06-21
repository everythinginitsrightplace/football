library("dplyr")
library("magrittr")
library("ggplot2")
library(phytools)

library("dplyr")
library("magrittr")
normalgoals <- 2.25 # After 20 matches

## This data frams contains information about the teams.
## You are free to add information here that you can use when determining winners
team_data <- tibble(
  number = 1:32,
  name = c("Египет","Россия","Саудовская Аравия","Уругвай",
           "Иран","Марокко","Португалия","Испания",
           "Австралия","Дания","Франция","Перу",
           "Аргентина","Хорватия","Исландия","Нигерия",
           "Бразилия","Коста-Рика","Швейцария","Сербия",
           "Германия","Южная Корея","Мексика","Швеция",
           "Бельгия","Англия","Панама","Тунис",
           "Колумбия","Япония","Польша","Сенегал"),
  group = rep(LETTERS[1:8], each=4),
  rating = c(251, 34, 1001, 29, # From http://www.bookmakersranking.com/ 
             501, 501, 26, 8,
             251, 101, 6.5, 126,
             9, 34, 151, 201,
             6, 251, 67, 151,
             5.5, 501, 81, 81,
             13, 17, 1001, 501,
             34, 251, 41, 126),
  elo = c(1610, 1733, 1553, 1913, # From https://www.eloratings.net/, June the 20
          1801, 1697, 1987, 2051,
          1730, 1891, 1999, 1880,
          1969, 1877, 1781, 1657,
          2123, 1717, 1909, 1804,
          2030, 1691, 1897, 1818,
          1956, 1957, 1642, 1648,
          1880, 1732, 1794, 1787)
)

# Giv hjemmebanefordel
team_data$elo[team_data$name=="Россия"] <- round( team_data$elo[team_data$name=="Россия"]*1.05, 0)

group_match_data <- read.csv(text=
                               "team1,team2,date,goals1,goals2
                             Россия,Саудовская Аравия,14/06/2018,,
                             Египет,Уругвай,15/06/2018,,
                             Марокко,Иран,15/06/2018,,
                             Португалия,Испания,15/06/2018,,
                             Франция,Австралия,16/06/2018,,
                             Аргентина,Исландия,16/06/2018,,
                             Перу,Дания,16/06/2018,,
                             Хорватия,Нигерия,16/06/2018,,
                             Коста-Рика,Сербия,17/06/2018,,
                             Германия,Мексика,17/06/2018,,
                             Бразилия,Швейцария,17/06/2018,,
                             Швеция,Южная Корея,18/06/2018,,
                             Бельгия,Панама,18/06/2018,,
                             Тунис,Англия,18/06/2018,,
                             Колумбия,Япония,19/06/2018,,
                             Польша,Сенегал,19/06/2018,,
                             Россия,Египет,19/06/2018,,
                             Португалия,Марокко,20/06/2018,,
                             Уругвай,Саудовская Аравия,20/06/2018,,
                             Иран,Испания,20/06/2018,,
                             Дания,Австралия,21/06/2018,,
                             Франция,Перу,21/06/2018,,
                             Аргентина,Хорватия,21/06/2018,,
                             Бразилия,Коста-Рика,22/06/2018,,
                             Нигерия,Исландия,22/06/2018,,
                             Сербия,Швейцария,22/06/2018,,
                             Бельгия,Тунис,23/06/2018,,
                             Южная Корея,Мексика,23/06/2018,,
                             Германия,Швеция,23/06/2018,,
                             Англий,Панама,24/06/2018,,
                             Япония,Сенегал,24/06/2018,,
                             Польша,Колумбия,24/06/2018,,
                             Саудовская Аравия,Египет,25/06/2018,,
                             Уругвай,Россия,25/06/2018,,
                             Иран,Португалия,25/06/2018,,
                             Испания,Марокко,25/06/2018,,
                             Австралия,Перу,26/06/2018,,
                             Дания,Франция,26/06/2018,,
                             Нигерия,Аргентина,26/06/2018,,
                             Исландия,Хорватия,26/06/2018,,
                             Мексика,Швеция,27/06/2018,,
                             Южная Корея,Германия,27/06/2018,,
                             Сербия,Бразилия,27/06/2018,,
                             Швейцария,Коста-Рика,27/06/2018,,
                             Англия,Бельгия,28/06/2018,,
                             Сенегал,Колумбия,28/06/2018,,
                             Панама,Тунис,28/06/2018,,
                             Япония,Польша,28/06/2018,,
                             ",header=TRUE)
tt <- team_data
tt <- tt %>% rename(odds=rating, ELO=elo, Gruppe=group, Navn=name)
knitr::kable(cbind(tt[1:16,-1], ` `=rep("  ", 16), tt[17:32,-1]))

library("reshape2")
elo2prob <- function(rating1, rating2) {
  1/(1 + 10^((rating2-rating1)/400))
}






pw <- outer(team_data$elo, team_data$elo, elo2prob)
diag(pw) <- NA
griddata <- melt(pw) %>% 
  mutate(v1=factor(team_data$name[Var1], levels=sort(team_data$name)), 
         v2=factor(team_data$name[Var2], levels=rev(sort(team_data$name))))
#reverse level order of state

library("RColorBrewer")
griddata %>% mutate(value=round(value, 2)) %>%
  ggplot(aes(v1, v2, fill = 1-value,
             text = paste('Вероятность, что', v2,
                          '<br>одержит победу над', v1, ': ', (1-value)*100, '%')
  )) + geom_tile(colour="white",size=0.25) + 
  labs(x="", y="") + coord_fixed() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_distiller(palette = "RdBu", direction=-1) +
  scale_y_discrete(expand=c(0,0)) + 
  theme(legend.position="bottom") +
  labs(fill='Шансы\nна победу\nв матче') + 
  coord_fixed() -> p    #remove extra space

library("plotly")

# p

ggplotly(p, tooltip="text") # %>% layer(paper_bgcolor='transparent')





dskellam <- function(x, mu1, mu2) {
  return(exp(-(mu1+mu2))*(mu1/mu2)^(x/2)*besselI(2*sqrt(mu1*mu2),nu=x)
  )
}


eta <- 2.25
beta1 <- seq(0.05, eta-0.05, 0.05)



skellam <- rep(0, length(beta1))
counter <- 1
for (i in beta1) {
  
  # Udregn ssh for at hold 1 vinder
  skellam[counter] <- sum(dskellam(1:12, i, eta-i)) / ( sum(dskellam(seq(-10,10,1), i, eta-i)) - dskellam(0, i, eta-i) )
  counter <- counter+1
}

skellam <- data.frame(beta=beta1, prob=skellam)


FindParameter <- function(prob) {
  sapply(prob, function(i) {
    if (i<.009) {
      return (.1)
    }
    if (i>.995) {
      return (eta-.05)
    }
    return(min(skellam$beta[skellam$prob>i]))
  }
  )
}




## This function fills out the missing matches in the order from top to bottom
## It returns a list of two data frames - one is the of the form

# Input: 
# Returns: 

odds2probs <- function(odds, rescale=TRUE) {
  if (any(odds<0))
    stop("Odds must be non-negative")
  probs <- 1/(odds)  # Note: decimal odds here!
  if (rescale)
    probs <- probs/sum(probs)
  probs
}

play_game <- function(team_data, team1, team2, musthavewinner=FALSE, k=58) {
  # Sanity checks
  if (length(team1) != length(team2))
    stop("Lengths of team should be the same")
  
  if (any(team1==team2))
    stop("A team cannot play against itself")
  
  # print(team1)
  
  ## Simplest version. 
  ## All teams are equal
  result <- cbind(rpois(length(team1), lambda=2.25/2), 
                  rpois(length(team1), lambda=2.25/2))
  
  ## Skellam distribution
  p1 <- .91/team_data$rating[team1]
  p2 <- .91/team_data$rating[team2]
  
  p1 <- odds2probs(team_data$rating)[team1]
  p2 <- odds2probs(team_data$rating)[team2]
  
  prob <- p1 / (p1 + p2)
  lambdaA <- FindParameter(prob)
  Agoals <- rpois(length(prob), lambdaA)
  Bgoals <- rpois(length(prob), normalgoals-lambdaA)
  result <- cbind(Agoals, Bgoals)
  
  ## ELO version (no update here). Using sapply here instead of
  ## vectorization in case the elo ranking should be updated after each match
  # result <- t(sapply(seq_len(length(team1)), function(i) {
  #                          AWinProb <- 1/(1 + 10^((team_data$elo[team2[i]] - team_data$elo[team1[i]])/400))
  #                         myres <- rbinom(1, size=1, prob=AWinProb)
  #                            fakegoals <- c(1,0)  
   #                           if (myres==0)
    #                            fakegoals <- c(0,1)
      #                         fakegoals
      #                       }))
  
    #result <- cbind(rpois(length(team1), lambda=ifelse(team1==1, 2, 1)), 
    #                rpois(length(team1), lambda=ifelse(team2==1, 2, 1)))
  
  # If we MUST have a winner then one simple trick is to add a random goal 
  # to one of the two teams that have the same score. Penalty goals seem rather 
  # random anyway
  if (musthavewinner) {
    result[result[,1]==result[,2],1] + 2*rbinom(sum(result[,1]==result[,2]), size=1, prob=.5) - 1
    
  }
  result
  
}



#
#
#  Uses the external team_data

find_group_winners <- function(team_data, group_match_data, FUN=play_game) {
  
  ## Create a copy of the the matches that we can fill out
  group_match_results <- group_match_data
  
  ## Simulate each match that hasn't already been played  
  pick <- (!complete.cases(group_match_results[c("goals1", "goals2")]))
  group_results <- play_game(team_data, 
                             team_data$number[match(group_match_data$team1, team_data$name)],
                             team_data$number[match(group_match_data$team2, team_data$name)],
                             musthavewinner = FALSE)
  
  ## Now add the results (the goals) to the match resuls
  group_match_results[, c("goals1", "goals2")] <- group_results
  ## Compute points earned per team for each match
  group_match_results$pointsForA <- with(group_match_results, 3*(goals1>goals2)+1*(goals1==goals2))
  group_match_results$pointsForB <- with(group_match_results, 3*(goals1<goals2)+1*(goals1==goals2))
  
  
  team_data$points <- 
    sapply(team_data$name, function(i) { sum(group_match_results[c("pointsForA", "pointsForB")][i == group_match_data[c("team1","team2")]]) })
  team_data$goalsFore <- sapply(team_data$name, function(i) { sum(group_match_results[c("goals1", "goals2")][i == group_match_data[c("team1","team2")]]) })
  
  team_data$goalsAgainst <- sapply(team_data$name, function(i) { sum(group_match_results[c("goals2", "goals2")][i == group_match_data[c("team1","team2")]]) })
  
  team_data$goalsDifference <- team_data$goalsFore-team_data$goalsAgainst
  
  
  team_data %>% 
    group_by(group) %>% 
    arrange(desc(points), desc(goalsDifference), desc(goalsFore)) %>% 
    mutate(groupRank = row_number()) %>% 
    ungroup() %>%
    arrange(group, groupRank)
}


find_knockout_winners <- function(team_data, match_data, FUN=play_game) {
  ## Get the results
  results <- play_game(team_data, match_data[,1], match_data[,2], musthavewinner=TRUE)
  ## Find the teams that won
  winners <- match_data[cbind(seq(nrow(results)), ifelse(results[,1]>results[,2], 1, 2))]
  winners
}



simulate_tournament <- function(n=10, FUN=playgame,
                                teams=team_data, 
                                group_matches=group_match_data) {
  
  
  sapply(1:n, function(matchnumber) {
    
    ## Step 1: Find the results from the group matcges
    group_results <- find_group_winners(team_data=teams, group_match_data)
    
    ## Step 2: Design matches for the first part of the knockout match
    eigth_matches <- cbind(group_results$number[seq(1, 32, by=4)], group_results$number[c(6, 2, 14, 10, 22, 18, 30, 26)])
    ## and find the results
    eigth_winners <- find_knockout_winners(team_data, eigth_matches)
    
    ## Step 3: Design matches for the quarter finals and run them
    quarter_matches <- cbind(eigth_winners[c(1, 2, 5, 6)], eigth_winners[c(3, 4, 7, 8)])
    quarter_winners <- find_knockout_winners(team_data, quarter_matches)
    
    ## Step 4: Semi finals ... yada yada yada
    semi_matches <- cbind(quarter_winners[c(1,2)], quarter_winners[c(3,4)])
    semi_winners <- find_knockout_winners(team_data, semi_matches)
    
    ## Steps 5 and 6 Find number 1-4
    bronze_match <- matrix(quarter_winners[!quarter_winners %in% semi_winners], ncol=2)
    bronze_winner <- find_knockout_winners(team_data, bronze_match)
    
    final_match <- matrix(semi_winners, ncol=2)
    final_result <- find_knockout_winners(team_data, final_match)
    
    ## Return a vector with the teams in ranked order. 
    ## Note only the first 4 are individuals - the rest are really groups
    final_ranking <- c(final_result, # Number 1
                       final_match[!(final_match %in% final_result)], #2
                       bronze_winner, # Number 3
                       bronze_match[!(bronze_match %in% bronze_winner)], #4
                       quarter_matches[!(quarter_matches %in% quarter_winners)], # 5-8
                       eigth_matches[!(eigth_matches %in% eigth_winners)], # 9-16
                       seq(32)[!(seq(32) %in% eigth_matches)]
    )
    
    final_ranking 
  })
}
# Endelig kode giver en vektor af sandsynligheder




set.seed(140616)
result <- simulate_tournament(100000)

# Final players
finalteams <- sort(table(apply(result[1:2,], 2, function(x) {paste(sort(team_data$name[x]), collapse=", ")})),
                   decreasing = TRUE)

finalteams <- finalteams/ncol(result)*100


winner <- table(result[1,])
names(winner) <- team_data$name[match(names(winner), team_data$number)]

# winner/sum(winner)

DF2 <- as.data.frame(winner/sum(winner)*100)
library("ggplot2")
library(ggthemes)
library("cowplot")
p2 <- DF2 %>% #filter(Freq>1) %>% 
  ggplot(aes(x=reorder(Var1, -Freq), y=Freq)) + geom_bar(stat="identity") + 
  xlab("Страна") + ylab("Вероятность победы") + coord_flip() +
  theme_economist_white()

 p2


p3 <- DF2 %>% #filter(Freq>1) %>% 
  ggplot(aes(x=reorder(Var1, -Freq), y=Freq)) + geom_point(size=3) + 
  geom_segment(aes(x=reorder(Var1, -Freq), xend=reorder(Var1, -Freq), y=0, yend=Freq)) + 
  xlab("Страна") + ylab("Вероятность победы на ЧМ-2018") + coord_flip() +
  theme_economist()

ggplotly(p3)



