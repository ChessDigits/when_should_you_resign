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
add_worst_eval_for_each_player <- function(df, eval_after_opponent_move_only=TRUE)
{
  eval_cols_white <- eval_cols_black <- grep(pattern = "Eval_ply_", x=colnames(df), value=TRUE)
  if (eval_after_opponent_move_only) 
  {
    eval_cols_white <- eval_cols_white[seq(2,200,2)] # eval after Black's move
    eval_cols_black <- eval_cols_black[seq(1,200,2)] # eval after White's move
  }
  df$worst_white_eval <- apply(df[eval_cols_white], 1, min, na.rm=TRUE)
  df$worst_black_eval <- apply(df[eval_cols_black], 1, max, na.rm=TRUE)
  
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
  if(breaks_preset==4) breaks <- c(BLACK_MATE_EVAL,0,1,2,3,4,5,6,7,8,9,10,50,WHITE_MATE_EVAL)
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


#### ratings ####
# rating buckets
add_rating_buckets <- function(df)
{
  df$BlackElo_bucket <- factor(floor(df$BlackElo/100), ordered=TRUE)
  df$WhiteElo_bucket <- factor(floor(df$WhiteElo/100), ordered=TRUE)
  print("Added variables WhiteElo_bucket and BlackElo_bucket")
  return(df)
}

# restrict by rating
restrict_by_rating <- function(df, player=c("White", "Black"), min_rating=900, max_rating=2400)
{
  n_pre <- nrow(df)
  player <- match.arg(player)
  df <- df[df[paste0(player, "Elo")] < max_rating & df[paste0(player, "Elo")] >= min_rating,]
  n_post <- nrow(df)
  
  # out
  print(paste0("Removed games where had Elo >= ", max_rating, " (n pre = ", n_pre, ", n post = ", n_post, ")"))
  return(df)
}

# use rating diff aussi! already provided
# oups ca cest le changement après coté


#### plots ####
get_plot_worst_white_eval_by <- function(df, by=NULL, exclude_categories=NULL)
{
  if(!is.null(exclude_categories)) df <- df[!df$Category %in% exclude_categories,]
  groups <- list(Worst_Eval=df$worst_white_eval_bucket)
  if(!is.null(by)) groups[[by]] <- df[,by]
  t <- aggregate(
    df$Result,
    groups,
    function(x) (.t <- table(x))/sum(.t)
  )
  t$White_Wins <- t$x[,"1-0"]
  if(is.null(by)) ggplot(t, aes_string(x="Worst_Eval", y="White_Wins", group=1)) + geom_line(size=2) + ylim(0,1) # if no time control
  else ggplot(t, aes_string(x="Worst_Eval", y="White_Wins", group=by, color=by)) + geom_line(aes_string(linetype=NULL), size=2) + ylim(0,1)

}
