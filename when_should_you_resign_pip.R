"
2021.01.23
Chess Digits
When should you resign?

pip
"

df <- load_data(k_games=10)
df <- remove_abnormal_termination(df)
df <- restrict_by_rating(df, player = "White", min_rating=900, max_rating=2400)
df <- restrict_by_rating(df, player = "Black", min_rating=900, max_rating=2400)
df <- add_rating_buckets(df)
df <- make_time_category_ordered(df)
df <- replace_mates_with_extreme_evaluations(df)
df <- add_worst_eval_for_each_player(df, eval_after_opponent_move_only=TRUE)
df <- add_worst_eval_bucket(df, breaks_preset = 1)

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


#### plots ####
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
exclude_time_forfeits <- FALSE
df <- add_disadvantage_reached_in_game(df)
get_plot_disadvantage_reached_by(df, exclude_categories = list(NULL, "Bullet")[[1]], exclude_time_forfeits=exclude_time_forfeits)
get_plot_disadvantage_reached_by(df, by="Category", exclude_categories = list(NULL, "Bullet")[[1]], exclude_time_forfeits=exclude_time_forfeits)
get_plot_disadvantage_reached_by(df, by="BlackElo_bucket", exclude_categories = list(NULL, "Bullet")[[1]], exclude_time_forfeits=exclude_time_forfeits)
