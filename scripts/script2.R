library(tidyverse)

classroom_df <- read_csv('data/classroom.csv')

M_classroom <- lmer(
  mathscore ~ ses + (ses||classid) + (ses|schoolid),
  data = classroom_df)

M_classroom_vi1 <- lmer(
  mathscore ~ ses + (1|schoolid/classid2),
  data = classroom_df)

M_classroom_vi2 <- lmer(
  mathscore ~ ses + (1|schoolid) + (1|classid2:schoolid),
  data = classroom_df)

M_classroom_vi3 <- lmer(
  mathscore ~ ses + (1|schoolid) + (1|classid),
  data = classroom_df)
