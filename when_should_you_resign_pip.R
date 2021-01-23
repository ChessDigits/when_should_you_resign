"
2021.01.23
Chess Digits
When should you resign?

pip
"

df <- load_data()
df <- replace_mates_with_extreme_evaluations(df)
df <- add_worst_eval_for_each_player(df)
