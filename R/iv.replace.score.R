# Replace original value of variables with interval score
#
# @param df  original data
# @param iv_score  iv from returned value of iv.interval.score()
# @param base_score base_score from returned value of iv.interval.score()
# @export df iv with interval score, base score and total score
# @example
# value_list <- iv.interval.score(P0, PDO, theta0, iv, model.fit) 
# iv.replace.score(df, value_list)

iv.replace.score <- function(df, iv_score, base_score) {
  library(plyr)
  library(sqldf)

  iv_df <- rbind.fill(iv_score)
  for (n in iv_score) {
    variable_name <- n[1, 1]
    variable_name_woe_score <- paste(variable_name, "_woe_score", sep="")
    
    if(!("sql" %in% colnames(n)))
    {
      if(any(is.na(df[, variable_name])))
      {
        df[, variable_name][is.na(df[, variable_name])] <- "NA"
      }    
      sqlstr <- paste("select df.*, iv_df.score as ", variable_name_woe_score, " from df join iv_df on (df.", variable_name, " = iv_df.class and iv_df.variable = '", variable_name, "')", sep="")
      df <- sqldf(sqlstr, drv="SQLite")
    } else
      { 
        if(any(is.na(df[,variable_name])))
        {
          n_first_line <- n[1,]
          n <- n[-1,]
          splitstr <- unlist(strsplit(n$sql, split="then"))[c(TRUE, FALSE)]
          combinestr <- paste(splitstr, "then ", n$score, sep="", collapse=" ")
          sqlstr <- paste("select df.*, case ", combinestr, " else ", n_first_line$score, " end as ", variable_name_woe_score, " from df", sep="")
          df <-sqldf(sqlstr,drv="SQLite")
        } else
          {
            splitstr <- unlist(strsplit(n$sql, split="then"))[c(TRUE, FALSE)]
            combinestr <- paste(splitstr, "then ", n$score, sep="", collapse=" ")
            sqlstr <- paste("select df.*, case ", combinestr, " else 0 end as ", variable_name_woe_score, " from df", sep="")
            df <- sqldf(sqlstr, drv="SQLite")
          }
      }
  }
  
  # calculate total score of samples
  df$base_woe_score <- base_score
  df$total_woe_score <- rowSums(select(df, ends_with("woe_score")))

  return(df)
}

