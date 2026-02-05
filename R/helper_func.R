growth_rate = function(x) {
  if (length(x) == 0) {
    return(numeric(0)) 
  }
  
  output = x
  output[1] = NA
  for (i in 2:length(x)) {
    both_not_na = !(any(is.na(c(x[i - 1], x[i]))))
    if (x[i - 1] == x[i]) {
      output[i] = 0
    } else if (x[i - 1] != 0 && both_not_na) {
      output[i] = round((x[i] - x[i - 1]) / x[i - 1], 3) * 100
    } else {
      output[i] = NA
    }
  }
  return (output)
}

plot_rank = function(data, top, dark_mode, subcategory, year) {
  subset = if (top) {
    data |> slice_head(n = 5)
  } else {
    data |> slice_tail(n = 5)
  }
  
  subset = subset |> 
    mutate(
      tooltip = paste0(
        "State: ", State, "<br>",
        "Avg Margin: ", Average_Margin, "%<br>",
        "Growth Rate: ", ifelse(is.na(Growth_Rate), "N/A", paste0(Growth_Rate, "%")), "<br>",
        "Total Units: ", Total_Units
      )
    )
  
  y_max <- max(subset$Average_Margin, na.rm = TRUE)
  y_min <- min(subset$Average_Margin, na.rm = TRUE)
  
  y_pad <- 0.5 * (y_max - y_min)
  
  plot = subset |> 
    ggplot(aes(x = reorder(State, -Average_Margin),
               y = Average_Margin,
               text = tooltip)) + 
    geom_col(
      fill = if (dark_mode) "#4596CC" else "#CC4545",
      color = if (dark_mode) "#FFF" else "#111"
    ) +
    labs(
      title = if (top) {
        paste("Top 5 Most Profitable States for", subcategory, "in", year, sep = " ")
        } else {(paste("Top 5 Least Profitable States for", subcategory, "in", year, sep = " "))
        },
      x = "State",
      y = "Average Profitability Margin (%)"
    )
  
  plot |> 
    ggplotly(tooltip = "text") |> 
    layout(yaxis = list(range = c(y_min - y_pad, y_max + y_pad)))
}
