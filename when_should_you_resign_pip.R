"
2021.01.23
Chess Digits
When should you resign?

Pipeline for article:
https://web.chessdigits.com/articles/when-should-you-resign

"
setcwd()
df <- load_data(k_games=200, use_local_file=FALSE)
df <- remove_abnormal_termination(df)
df <- remove_results(df, results="1/2-1/2")
df <- restrict_by_rating_differential(df, max_diff=100)
df <- restrict_by_rating(df, player = "White", min_rating=800, max_rating=2300)
df <- restrict_by_rating(df, player = "Black", min_rating=800, max_rating=2300)
df <- add_rating_buckets(df)
df <- make_time_category_ordered(df)
df <- add_increment_indicator(df)
df <- add_increment_by_time_category_indicator(df)
df <- replace_mates_with_extreme_evaluations(df)
df <- add_worst_eval_for_each_player(df, eval_after_opponent_move_only=TRUE)
breaks_preset <- 1
df <- add_worst_eval_bucket(df, breaks_preset = breaks_preset)


#### disadvantage reached in game ####
#### THIS ONE FOR ARTICLE ####
#### this asks the question what are my chances of winning ONCE I REACH a disadv of __
exclude_time_forfeits <- FALSE
df <- add_disadvantage_reached_in_game(df, breaks_preset)
get_plot_disadvantage_reached_by(df, exclude_categories = list(NULL, "Bullet")[[1]], exclude_time_forfeits=exclude_time_forfeits)
get_plot_disadvantage_reached_by(df, exclude_categories = list(NULL, "Bullet")[[1]], exclude_time_forfeits=TRUE)
get_plot_disadvantage_reached_by(df, by="Category", by_label="Time Control", exclude_categories = list(NULL, "Bullet")[[1]], exclude_time_forfeits=exclude_time_forfeits)
get_plot_disadvantage_reached_by(df, by="Category", exclude_categories = list(NULL, "Bullet")[[1]], exclude_time_forfeits=TRUE)
#get_plot_disadvantage_reached_by(df, by="WhiteElo_bucket", exclude_categories = list(NULL, "Bullet")[[1]], exclude_time_forfeits=exclude_time_forfeits)
get_plot_disadvantage_reached_by(df, by="BlackElo_bucket", by_label="Opponent\nRating", exclude_categories = list(NULL, "Bullet")[[1]], exclude_time_forfeits=exclude_time_forfeits)
get_plot_disadvantage_reached_by(df, by="BlackElo_bucket", exclude_categories = list(NULL, "Bullet")[[1]], exclude_time_forfeits=TRUE)

#
#get_plot_disadvantage_reached_by(df, results=c("1-0", "1/2-1/2"), by="BlackElo_bucket", exclude_categories = list(NULL, "Bullet")[[1]], exclude_time_forfeits=exclude_time_forfeits)


# added following comment on lichess
# inc vs no inc
# https://lichess.org/forum/general-chess-discussion/when-should-you-resign--chance-of-winning-on-lichess-when-stockfish-says-youre-losing#8
get_plot_disadvantage_reached_by(df, by="Increment", by_label=NULL, exclude_categories = list(NULL, "Bullet")[[1]], exclude_time_forfeits=exclude_time_forfeits)
get_plot_disadvantage_reached_by(df, by="Category_Inc", by_label="Time Control\nand Increment", linetype=F, exclude_categories = list(NULL, "Bullet")[[1]], exclude_time_forfeits=exclude_time_forfeits)
