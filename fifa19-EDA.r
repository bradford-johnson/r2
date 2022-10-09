---
title: "FIFA19 EDA"
author: "Bradford Johnson"
format: html
    code-fold: true
    highlight-style: github
editor: visual
---

```{r packages}
#| message: false
#| warning: false
library(DBI)
library(RPostgres)
library(tidyverse)
library(janitor)
library(formattable)
```

```{r SQL-Database}
#| echo: false
con <- dbConnect(RPostgres::Postgres(),dbname = 'fifa19', 
                 host = '142.93.121.174', 
                 port = 5432, 
                 user = 'dabc_student',
                 password = '7*.8G9QH21')
```

```{r query}
res <- dbSendQuery(con, "

  SELECT *
  FROM fifa19
                   ")

 fifa_df <- dbFetch(res)
dbClearResult(res)
dbDisconnect(con)
```

```{r clean-up-df}
fifa_df <- fifa_df %>%
  clean_names() %>%
  select(id,name,nationality,overall,potential,club,wage,preferred_foot,position)
```

```{r clean-wage-col}
fifa_df$wage <- gsub("€","",as.character(fifa_df$wage))
fifa_df$wage <- gsub("K","000",as.character(fifa_df$wage))
fifa_df$wage <- as.numeric(fifa_df$wage)
```

```{r total-table}
fifa_df %>%
  drop_na() %>%
  summarise(total_players = formatC(length(id),digits = NULL, big.mark = ","), total_wages = prefix(accounting(sum(wage), format = "d"),prefix = "$", sep = ""))
```

```{r}
fifa_tb <- fifa_df %>%
  drop_na() %>%
  group_by(position) %>%
  summarise(max_wage = max(wage), avg_wages = round(mean(wage),digits = 0), median_wages = median(wage), count_players = length(id), total_wages = sum(wage)) %>%
  arrange(desc(count_players))
```

```{r count-plot}
colors_df <-c("#7E9CC7","#90BF8C","#90BF8C","#7E9CC7")
labcolors_df <-c("#455770","#253725","#253725","#455770")
fifa_tb_1 <- fifa_tb %>%
  filter(median_wages > 6000) %>%
  mutate(median_wages = accounting(median_wages))
fifa_tb_1 %>%
  ggplot(aes(x = position, y = median_wages, label = median_wages)) +
  geom_col(fill = colors_df) + 
  scale_fill_manual(values = colors_df) +
  geom_label(nudge_y = 500, colour = labcolors_df) +
  scale_colour_manual(values = labcolors_df) +
  theme_classic() +
  labs(title = "Positions with median wage > $5,000",
       x = "",
       y = "")
```
