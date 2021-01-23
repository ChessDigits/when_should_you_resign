"
2021.01.23
Chess Digits
When should you resign?

fn
"

#### imports ####


#### load data ####
load_data <- function()
{
  fpath <- "d:/Chess/databases/lichess_May2019/out/10k_blitz_rapid_classical_bullet.csv"
  df <- read.csv(fpath)
  return(df)
}