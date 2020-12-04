create_summary_keys = function(df, shade_val){
  min_val = df %>%
    ungroup() %>%
    select(.data[[shade_val]]) %>%
    arrange(.data[[shade_val]]) %>%
    slice(1) %>%
    unlist() %>%
    unname()
  
  max_val = df %>%
    ungroup() %>%
    select(.data[[shade_val]]) %>%
    arrange(-.data[[shade_val]]) %>%
    slice(1) %>%
    unlist() %>%
    unname()
  
  min_max = c(min_val, max_val)
  return(min_max)
}
  
#





filter_df = function(df, brand_val, shade_val){

  
  if(brand_val == "all brands"){
    new_df = df %>%
      group_by(.data[[shade_val]]) %>%
      mutate(y = seq(1:n())) %>%
      arrange(.data[[shade_val]], y)
    
    return(new_df)
    
  } else {
    
    new_df = df %>%
      filter(brand == !!brand_val) %>%
      group_by(.data[[shade_val]]) %>%
      mutate(y = seq(1:n())) %>%
      arrange(.data[[shade_val]], y)
    
    return(new_df)
  }
}


generate_plot = function(df, palette, variable, label){
  plot = df %>%
    ggplot() + 
    geom_point(aes_string(x = variable,
                          y = "y",
                          color  = "hex"),
               shape = 15,
               size = 4) +
    geom_segment(aes(x = 0,
                     xend = 100,
                     y = 0,
                     yend = 0),
                 lineend = "round",
                 linejoin = "mitre",
                 arrow = arrow(length = unit(0.1,"cm"))) +
    labs(x = label) +
    scale_color_manual(values = {{palette}}) +
    scale_x_continuous(limits = c(0, 100)) +
    scale_y_continuous(limits = c(0, 55)) +
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(family = "Roboto Condensed"),
          axis.ticks = element_blank(),
          plot.background = element_rect(fill = "#f9f9f9"),
          panel.background = element_rect(fill = "#f9f9f9"),
          panel.grid = element_blank()) 
  
  return(plot)
}