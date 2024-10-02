# My first attempt at a #tidytuesday!
# 2024-10-01 - Chess
# script by Jeff Pannell
# data from lichess data set, https://github.com/rfordatascience/tidytuesday/tree/master/data/2024/2024-10-01
# Images are the maestro chess set pieces from lichess 
library(tidyverse)
library(ggimage)
library(tidytuesdayR)
library(here)

# Option 1: tidytuesdayR package 

# Option 2: Read directly from GitHub

chess <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-01/chess.csv')


chess_limited <- chess |> 
  select(winner, opening_name) |> 
  group_by(opening_name, winner) |> 
  summarise(victory_count = n()) |> 
  ungroup()

avg_elo <- chess |> 
  select(opening_name, black_rating, white_rating) |> 
  group_by(opening_name) |> 
  summarise(avg_elo = mean((black_rating + white_rating) / 2))

chess_ratios <- chess_limited |>
  pivot_wider(names_from = winner, values_from = victory_count) |> 
  replace_na(list(black = 0, white = 0, draw = 0)) |> 
  mutate(total_matches = black + draw + white,
         black_w_ratio = black/(black + draw + white),
         draw_ratio = draw/(black + draw + white),
         white_w_ratio = white/(black + draw + white))|>
  arrange(desc(total_matches)) |> 
  ungroup() |> 
  slice_head(n=8) |> 
  select(opening_name, 'black' = black_w_ratio, 'draw' = draw_ratio, 'white' = white_w_ratio, total_matches) |> 
  pivot_longer(2:4, values_to = "ratio", names_to = "winner" ) |> 
  left_join(avg_elo, by = "opening_name") |> 
  mutate(image = ifelse(winner == "white", here("images/wP.svg"), # Originally loaded pawns to give a chess board appearance
                        ifelse(winner == "black", here("images/bP.svg"), NA)))

# Decided to replace the pawns with the major/minor piece setup
# I know it's fluff ... but hard to let go fluff. It looks nice.
chess_ratios[1,6] <- here("images/bR.svg")
chess_ratios[3,6] <- here("images/wR.svg")
chess_ratios[4,6] <- here("images/bB.svg")
chess_ratios[6,6] <- here("images/wB.svg")
chess_ratios[7,6] <- here("images/bN.svg")
chess_ratios[9,6] <- here("images/wN.svg")
chess_ratios[22,6] <- here("images/bK.svg")
chess_ratios[24,6] <- here("images/wK.svg")
chess_ratios[10,6] <- here("images/bQ.svg")
chess_ratios[12,6] <- here("images/wQ.svg")
chess_ratios[19,6] <- here("images/bB.svg")
chess_ratios[21,6] <- here("images/wB.svg")
chess_ratios[13,6] <- here("images/bN.svg")
chess_ratios[15,6] <- here("images/wN.svg")
chess_ratios[16,6] <- here("images/bR.svg")
chess_ratios[18,6] <- here("images/wR.svg")

# ggplot output, there's a warning as I don't have an image input for the draw conditions, only white and black pieces
ggplot(chess_ratios, aes(x = reorder(opening_name, -ratio * (winner == "white")), y = ratio, fill = winner)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_image(aes(image = image), size = 0.1, position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values=c("#555d50", "#c4c3d0", "#faf0e6"))  +
  labs(title = "Win Ratios of 8 Most Played Chess Openings",
       subtitle = "Size of bar indicatess win ratio per color, grey indicates draw outcomes",
       caption = "Source: Lichess.org via Kaggle/Mitchell J.\nGraphic: Jeff Pannell @pezkin\n Piece Images: Lichess Maestro Chess Set",
       alt = paste("A bar chart showing win ratios of the 8 most played chess openings",
                   "ordered from most winning ratio for white to most winning ratio for black")) +
  guides(fill="none") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#000036"),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#000036"),
    plot.caption = element_text(size = 8, hjust = 1, vjust = 0, color = "#000036"),
    axis.text.x = element_text(size = 8, color = "#000036", vjust = 0.75, hjust = 0.5),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(color = "#000036"),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "#e3dac9"),
    plot.margin = margin(b = 20, r = 20, t = 20, l = 20)
  )

# This will save your most recent plot
ggsave(
  filename = "ChessWinRatios.png",
  device = "png",
  width = 7,
  height = 7,
)
