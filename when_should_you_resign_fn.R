"
2021.01.23
Chess Digits
When should you resign?

fn
"

#### imports ####
library(ggplot2)


#### variables ####
WHITE_MATE_EVAL <- 200
BLACK_MATE_EVAL <- WHITE_MATE_EVAL*-1



#### load data ####
load_data <- function(k_games=c(10,50,100,200))
{
  fpath <- paste0("d:/Chess/databases/lichess_May2019/out/", k_games, "k_blitz_rapid_classical_bullet.csv")
  df <- read.csv(fpath, stringsAsFactors = T)
  return(df)
}


#### data prep ####
# make time category ordered
make_time_category_ordered <- function(df)
{
  df$Category <- factor(x = df$Category, levels = c("Bullet", "Blitz", "Rapid", "Classical"), ordered = TRUE)
  print("Made time category df$Category an ordered factor")
  return(df)
}

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
      new_col[ix] <- ifelse(new_col[ix] < 0, BLACK_MATE_EVAL, WHITE_MATE_EVAL)
    }
    
    # replace in df
    df[,c] <- new_col
  }
  
  
  # out
  print(paste("Replaced mate evaluations with", WHITE_MATE_EVAL, "or", BLACK_MATE_EVAL))
  return(df)
}


#### worst evals ####
# add worst eval for each player
add_worst_eval_for_each_player <- function(df)
{
  eval_cols <- grep(pattern = "Eval_ply_", x=colnames(df), value=TRUE)
  df$worst_white_eval <- apply(df[eval_cols], 1, min, na.rm=TRUE)
  df$worst_black_eval <- apply(df[eval_cols], 1, max, na.rm=TRUE)
  
  # out
  print("Added worst_white_eval and worst_black_eval to df")
  return(df)
}

# get worst eval by buckets
add_worst_eval_bucket <- function(df, breaks_preset=c(1,2,3))
{
  if(breaks_preset==1) breaks <- c(BLACK_MATE_EVAL,0,1,2,3,4,5,6,7,8,9,10,15,20,50,WHITE_MATE_EVAL)
  if(breaks_preset==2) breaks <- c(BLACK_MATE_EVAL,0,1,2,3,4,5,10,50,WHITE_MATE_EVAL)
  if(breaks_preset==3) breaks <- c(BLACK_MATE_EVAL,seq(0,50,1),WHITE_MATE_EVAL)
  df$worst_black_eval_bucket <- cut(df$worst_black_eval,
                                    breaks=breaks, right=TRUE, ordered=TRUE
  )
  df$worst_white_eval_bucket <- cut(df$worst_white_eval*-1,
                                    breaks=breaks, right=TRUE, ordered=TRUE
  )

  # out
  print("Added ordered factors worst_white_eval_bucket and worst_black_eval_bucket to df")
  return(df)
}


