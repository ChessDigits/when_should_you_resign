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
load_data <- function(k_games=c(10,50,100,200,500))
{
  fpath <- paste0("d:/Chess/databases/lichess_May2019/out/", k_games, "k_blitz_rapid_classical_bullet.csv")
  df <- read.csv(fpath, stringsAsFactors = T)
  return(df)
}


#### data prep ####
# remove abnormal termination
remove_abnormal_termination <- function(df)
{
  n_pre <- nrow(df)
  remove <- c("Abandoned", "Rules infraction")
  df <- df[!df$Termination %in% remove,]
  df$Termination <- factor(df$Termination) # to ensure non-zero levels
  df$Result <- factor(df$Result) # to ensure non-zero levels
  n_post <- nrow(df)
  
  # out
  print("Removed abnormal terminations:")
  print(remove)
  print(paste("Total games removed:", n_pre - n_post))
  return(df)
}

# remove results
remove_results <- function(df, results)
{
  n_pre <- nrow(df)
  df <- df[!df$Result %in% results,]
  df$Result <- factor(df$Result) # to ensure non-zero levels
  n_post <- nrow(df)
  # out
  print("Removed results:")
  print(results)
  print(paste("Total games removed:", n_pre - n_post))
  return(df)
}

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
    if (length(ix_mate)==0) # no mate
    {
      df[,c] <- as.numeric(as.character(df[,c]))
      next 
    }
    
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


# helper fn
get_breaks <- function(breaks_preset=c(1,2,3,4))
{
  if(breaks_preset==1) breaks <- c(BLACK_MATE_EVAL,0,1,2,3,4,5,6,7,8,9,10,15,20,50,WHITE_MATE_EVAL)
  if(breaks_preset==2) breaks <- c(BLACK_MATE_EVAL,0,1,2,3,4,5,10,50,WHITE_MATE_EVAL)
  if(breaks_preset==3) breaks <- c(BLACK_MATE_EVAL,seq(0,50,1),WHITE_MATE_EVAL)
  if(breaks_preset==4) breaks <- c(BLACK_MATE_EVAL,0,1,2,3,4,5,6,7,8,9,10,50,WHITE_MATE_EVAL)
  return(breaks)
}

# get worst eval by buckets
add_worst_eval_bucket <- function(df, breaks_preset=c(1,2,3,4))
{
  breaks <- get_breaks(breaks_preset)
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

# 
add_disadvantage_reached_in_game <- function(df)
{
  "
  need to have run add_worst_eval_for_each_player(df) before
  "
  breaks <- get_breaks(1)
  for (b in breaks)
  {
    df[,paste0("black_disadvantage_reached_", b)] <- df$worst_black_eval >= b
    df[,paste0("white_disadvantage_reached_", b)] <- df$worst_white_eval*-1 >= b
  }
  
  # out
  print("Added variables white_disadvantage_reached_... and white_disadvantaged_reached_...")
  return(df)
}


#### ratings ####
# rating buckets
add_rating_buckets <- function(df)
{
  for (player in c("White", "Black"))
  {
    # ordered factor
    df[,paste0(player, "Elo_bucket")] <- factor(floor(df[,paste0(player, "Elo")]/100), ordered=TRUE)
    
    # levels (e.g. 1400 instead of 14)
    levels(df[,paste0(player, "Elo_bucket")]) <- as.numeric(levels(df[,paste0(player, "Elo_bucket")]))*100
  }

  # out
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

restrict_by_rating_differential <- function(df, max_diff)
{
  n_pre <- nrow(df)
  diff <- abs(df$WhiteElo - df$BlackElo)
  df <- df[diff <= max_diff,]
  n_post <- nrow(df)
  
  # out
  print(paste0("Removed games where players differed by Elo > ", max_diff, " (n pre = ", n_pre, ", n post = ", n_post, ")"))
  return(df)
}


#### plots ####
get_plot_worst_white_eval_by <- function(df, by=NULL, exclude_time_forfeits=FALSE, exclude_categories=NULL)
{
  if(exclude_time_forfeits) df <- df[df$Termination != "Time forfeit",]
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
  else ggplot(t, aes_string(x="Worst_Eval", y="White_Wins", group=by, color=by)) + geom_line(aes_string(linetype=if(by %in% c("WhiteElo_bucket", "BlackElo_bucket")) NULL else by), size=2) + ylim(0,1)

}

# # 
# get_plot_worst_white_eval_by_rating_for_each_time_category <- function(df, by=NULL, exclude_time_forfeits=FALSE)
# {
#   if(exclude_time_forfeits) df <- df[df$Termination != "Time forfeit",]
#   for (cat in c("Bullet", "Blitz", "Rapid", "Classical"))
#   {
#     .df <- df[df$Category == cat,]
#     groups <- list(Worst_Eval=.df$worst_white_eval_bucket)
#     groups[[by]] <- .df[,by]
#     t <- aggregate(
#       .df$Result,
#       groups,
#       function(x) (.t <- table(x))/sum(.t)
#     )
#     t$White_Wins <- t$x[,"1-0"]
#     p <- ggplot(t, aes_string(x="Worst_Eval", y="White_Wins", group=by, color=by)) + geom_line(aes_string(linetype=if(by %in% c("WhiteElo_bucket", "BlackElo_bucket")) NULL else by), size=2) + ylim(0,1)
#     print(p)
#   }
#   
# }

get_plot_disadvantage_reached_by <- function(df, results="1-0", by=NULL, by_label=NULL, exclude_time_forfeits=FALSE, exclude_categories=NULL)
{
  # requested data filtering
  if(exclude_time_forfeits) df <- df[df$Termination != "Time forfeit",]
  if(!is.null(exclude_categories)) df <- df[!df$Category %in% exclude_categories,]
  
  # groups
  if(is.null(by)) { by <- "All_Games"; df[,by] <- "All Games" }
  if(is.null(by_label)) by_label <- by
  
  # create df to plot from disadvantage variables
  # this is where we get the numbers to plot
  dis_cols <- grep(pattern = "white_dis", x = colnames(df), fixed = T, value = T)
  #dis_props <- apply(df[dis_cols], 2, function(x) sum(x)/length(x)) # props of games that reached the dis
  prop_winning <- list()
  for (col in dis_cols)
  {
    .df <- df[df[,col]==TRUE,]
    groups <- list(.df[,by]); names(groups) <- by
    prop_winning[[col]] <- aggregate(list(Percent_Winning=.df$Result), groups, function(x) sum(x %in% results)/length(x))
  }
  
  
  # into df for plotting
  dis_label <- gsub("white_disadvantage_reached_", replacement = "", dis_cols)
  dis_label_map <- list(
    `-200`="Game Starts",
    `0`="Less Than a Pawn",
    `1`="1 Pawn",
    `2`="2 Pawns",
    `3`="A Piece",
    `4`="4 Pawns",
    `5`="A Rook",
    `6`="6 Pawns",
    `7`="7 Pawns",
    `8`="8 Pawns",
    `9`="A Queen",
    `10`="More Than 10 Pawns",
    `15`="More Than 15 Pawns",
    `20`="More Than 20 Pawns",
    `50`="More Than 50 Pawns",
    `200`="Mate Possible"
  )
  dis_label <- unlist(dis_label_map)[match(dis_label, names(dis_label_map))]
  dis_label <- stringr::str_wrap(dis_label, width = 10)
  for (i in 1:length(prop_winning)) prop_winning[[i]]$Disadvantage_Reached <- dis_label[i]
  prop_winning <- do.call(rbind, prop_winning)
  dis_df <- data.frame(`Disadvantage_Reached`=factor(prop_winning$Disadvantage_Reached, levels=dis_label, ordered=TRUE))
  dis_df$`Percent_Winning` <- prop_winning$Percent_Winning*100
  if(!is.null(by)) dis_df[,by] <- prop_winning[,by]
  
  # plot
  ymax <- 55
  labs <- list(
    x="Disadvantage Reached During Game",
    y="Odds of Winning (%)"
  )
  yticks <- seq(0,ymax,5)
  if(by=="All_Games") ggplot(dis_df, aes_string(x="Disadvantage_Reached", y="Percent_Winning", group=1)) + 
    geom_line(size=2) + #ylim(0,ymax) +
    labs(x=labs$x, y=labs$y) +
    scale_y_continuous(breaks=yticks, limits=c(0,ymax))
  
  else ggplot(dis_df, aes_string(x="Disadvantage_Reached", y="Percent_Winning", group=by, color=by)) + geom_line(aes_string(linetype=if(by %in% c("WhiteElo_bucket", "BlackElo_bucket")) NULL else by), size=2) + 
    #ylim(0,ymax) +
    labs(color=by_label, x=labs$x, y=labs$y) +
    scale_y_continuous(breaks=yticks, limits=c(0,ymax))
  
}

