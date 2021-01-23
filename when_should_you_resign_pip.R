"
2021.01.23
Chess Digits
When should you resign?

pip
"

df <- load_data()
df <- replace_mates_with_extreme_evaluations(df)
df <- add_worst_eval_for_each_player(df)



hist(df$worst_black_eval)
hist(df$worst_white_eval)
with(subset(df, worst_black_eval > 5), table(Result))
with(subset(df, worst_black_eval == WHITE_MATE_EVAL), table(Result))
t <- with(subset(df, worst_black_eval >= WHITE_MATE_EVAL), xtabs(~Category+Result))
plot(t)
