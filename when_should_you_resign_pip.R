"
2021.01.23
Chess Digits
When should you resign?

pip
"

df <- load_data(200)
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






# line graph % winning
# without time control
t <- aggregate(
  df$Result,
  list(Worst_Eval=df$worst_white_eval_bucket),#, Time_Control=df$Category),
  function(x) (.t <- table(x))/sum(.t)
)
t$White_Wins <- t$x[,"1-0"]
ggplot(t, aes(x=Worst_Eval, y=White_Wins, group=1)) + geom_line(size=2) + ylim(0,1) # if no time control


# with time control
t <- aggregate(
  df$Result,
  list(Worst_Eval=df$worst_white_eval_bucket, Time_Control=df$Category),
  function(x) (.t <- table(x))/sum(.t)
)
t$White_Wins <- t$x[,"1-0"]
ggplot(t, aes(x=Worst_Eval, y=White_Wins, group=Time_Control, color=Time_Control)) + geom_line(aes(linetype=Time_Control), size=2) + ylim(0,1)


# # black without time control
# t <- aggregate(
#   df$Result,
#   list(Worst_Eval=df$worst_black_eval_bucket),#, Time_Control=df$Category),
#   function(x) (.t <- table(x))/sum(.t)
# )
# t$Black_Wins <- t$x[,"0-1"]
# ggplot(t, aes(x=Worst_Eval, y=Black_Wins, group=1)) + geom_line() + ylim(0,1) # if no time control
