# funciones
limpieza <- function(df) {
  df %>% 
    gather(id, value, -date) %>% 
    group_by(date) %>%
    filter(id != "Bolivia") %>%
    ungroup() %>% 
    group_split(id) %>% 
    map(., ~arrange(., date) %>% 
          mutate(cumsum = cumsum(.$value))) %>% 
    bind_rows() %>% 
    select(-value) %>% 
    rename(value = cumsum) %>% 
    filter(value > 0) %>% 
    group_split(date) %>% 
    map(., ~mutate(., n = n())) %>% 
    bind_rows() %>% 
    filter(n > 8) %>% 
    ungroup() %>% 
    group_by(date) %>% 
    filter(value > 0 & (n() - row_number(value)) <= 8) -> casos
  
  return(casos)
}

graf_estaticas <- function(.data) {
  .data %>% 
    group_split(date) %>% 
    map(., ~arrange(., desc(value)) %>% 
          slice(1)) %>% 
    bind_rows() %>% 
    count(id) %>% 
    mutate(
      prop = prop.table(n),
      prop = round(prop, 3),
      dias = sum(n)
    ) %>% 
    arrange(prop) %>% 
    mutate(
      num = 1:nrow(.), 
      es = case_when(
        id == "Santa Cruz" ~ "Santa Cruz",
        T ~ "Otros"
      )
    )
}


