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
  df <- read.csv(fpath, stringsAsFactors = T)
  return(df)
}


#### data prep ####
# replace mates with extreme evaluations
replace_mates_with_extreme_evaluations <- function(df)
{
  eval_cols <- grep(pattern = "Eval_ply_", x=colnames(df), value=TRUE)
  for (c in eval_cols)
  {
    # get row numbers at which eval is mate
    ix_mate <- grep(pattern="#", x=df[,c], fixed = T)
    if (length(ix_mate)==0) next
    
    # remove mate sign and make var numeric
    new_col <- gsub(pattern = "#", replacement="", x = df[,c], fixed=T)
    new_col <- as.numeric(as.character(new_col))
    
    # replace mate eval with extreme val
    for (ix in ix_mate)
    {
      new_col[ix] <- ifelse(new_col[ix] < 0, -999, 999)
    }
    
    # replace in df
    df[,c] <- new_col
  }
  
  
  # out
  print("Replaced mate evaluations with +999 or -999")
  return(df)
}


#### get worst eval for each player ####