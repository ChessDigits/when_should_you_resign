"
2021.01.23
Chess Digits
When should you resign?

pip
"
setcwd()
df <- load_data(k_games=200)
df <- remove_abnormal_termination(df)
df <- remove_results(df, results="1/2-1/2")
df <- restrict_by_rating_differential(df, max_diff=100)
df <- restrict_by_rating(df, player = "White", min_rating=800, max_rating=2300)
df <- restrict_by_rating(df, player = "Black", min_rating=800, max_rating=2300)
df <- add_rating_buckets(df)
df <- make_time_category_ordered(df)
df <- replace_mates_with_extreme_evaluations(df)
df <- add_worst_eval_for_each_player(df, eval_after_opponent_move_only=TRUE)
breaks_preset <- 1
df <- add_worst_eval_bucket(df, breaks_preset = breaks_preset)

# hist(df$worst_black_eval)
# hist(df$worst_white_eval)
# with(subset(df, worst_black_eval > 5), table(Result))
# with(subset(df, worst_black_eval == WHITE_MATE_EVAL), table(Result))
# t <- with(subset(df, worst_black_eval >= WHITE_MATE_EVAL), xtabs(~Category+Result))
# plot(t)
# 
# .df <- subset(df, worst_white_eval <= -1)
# ggplot(.df, aes(x=Category, fill=Result))+geom_bar(position = "fill")
# 


#### plots worst eval during game ####
#### NOT FOR ARTICLE
#### this asks the question what are my chances of winning if the max disadvantage in my game is __ 
#### (instead of ONCE I REACH a disadv of __)
exclude_time_forfeits <- FALSE
get_plot_worst_white_eval_by(df, by=NULL, exclude_time_forfeits=exclude_time_forfeits)
get_plot_worst_white_eval_by(df, by="Category", exclude_time_forfeits=exclude_time_forfeits)
get_plot_worst_white_eval_by(df, by="WhiteElo_bucket", exclude_categories=list(NULL, "Bullet")[[2]], exclude_time_forfeits=exclude_time_forfeits)
get_plot_worst_white_eval_by(df, by="BlackElo_bucket", exclude_categories=list(NULL, "Bullet")[[2]], exclude_time_forfeits=exclude_time_forfeits)

# for the onion plot, can choose white or black rating bucket; exclude or include bullet; exclude or include time forfeits
# (can also not exclude black players based on rating)

# rating separate by time category
#get_plot_worst_white_eval_by_rating_for_each_time_category(df, by="WhiteElo_bucket", exclude_time_forfeits = exclude_time_forfeits)


# scatterplot worst evals
ggplot(df[sample(x = 1:nrow(df), size = 20000, replace = FALSE),], 
       aes(x=worst_white_eval, y=worst_black_eval)) + geom_point()

# confusion matrix
cm <- yardstick::conf_mat(data=df, truth=worst_white_eval_bucket, estimate=worst_black_eval_bucket)
autoplot(cm, type="heatmap")





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
get_plot_disadvantage_reached_by(df, results=c("1-0", "1/2-1/2"), by="BlackElo_bucket", exclude_categories = list(NULL, "Bullet")[[1]], exclude_time_forfeits=exclude_time_forfeits)
