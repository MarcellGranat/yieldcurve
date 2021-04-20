message('start!')

library(tidyverse)
library(patchwork)
library(knitr)
library(broom)
library(geofacet)
library(tidytext)
library(tm)
library(wordcloud)
library(lubridate)
library(parallel)

load("C:/rprojects/CoronaSentiment/topics_bydat2.RData")

cl <- makeCluster(7)
clusterExport(cl, list("dat_topics"), envir = environment())
clusterEvalQ(cl, library(tidyverse))
dat_topics$top_topic <- parApply(cl = cl, select(dat_topics, starts_with('topic')), 1, function(x) {
  which.max(x)
})
stopCluster(cl)

message('end parallel')

topic_df <- tibble(
  topic = c("Topik 1", "Topik 2", "Topik 3", "Topik 4", "Topik 5", "Topik 6", "Topik 7", "Topik 8", "Topik 9", "Topik 10", "Topik 11", "Topik 12"),
  n = dat_topics %>% 
    select(country, starts_with('topic')) %>% 
    group_by(country) %>% 
    summarise_all(.funs = function(x) mean(x, na.rm = T)) %>% 
    ungroup() %>% 
    select(-country) %>% 
    apply(2, mean),
  date = dat_topics %>% 
    select(country, date, starts_with('topic')) %>% 
    mutate(date = ym(str_sub(as.character(date), end = -4))) %>% 
    group_by(country, date) %>% 
    summarise_all(.funs = function(x) mean(x, na.rm = T)) %>% 
    ungroup() %>% 
    select(-country) %>% 
    group_by(date) %>% 
    summarise_all(.funs = function(x) mean(x, na.rm = T)) %>% 
    {
      apply(.[2:13], 2, function(x) {
        as.character(.$date)[which.max(x)]
      })
    } %>% 
    ymd(),
  sentiment = dat_topics %>% 
    select(country, sentiment, n_sentiment, top_topic) %>% 
    na.omit() %>% 
    group_by(country, top_topic) %>% 
    summarise(sentiment = weighted.mean(x = sentiment,
                                        w = n_sentiment,
    )) %>% 
    ungroup() %>% 
    group_by(top_topic) %>% 
    summarise(sentiment = mean(sentiment)) %>% 
    arrange(desc(topic)) %>% 
    .$sentiment
) 
  
  ggplot(topic_df) +
  geom_point(aes(x = date, y = sentiment*n, fill = sentiment, size = n, label = topic)) + 
  scale_size(range = c(20, 40), breaks = c(.1), 
             labels = function(x) scales::percent(x, accuracy = 1, decimal.mark = ',')) +
  scale_fill_gradient(low = 'grey90', high = 'cyan4', guide = guide_colorsteps()) +
  geom_text(mapping = aes(x = date, y = sentiment*n, label = topic),
            show.legend = F, size = 4) + 
  labs(x = 'Topik legjellemzőbb dátuma', y = expression(Szentiment %*% relatív~gyakoriság),
       fill = 'Átlagos szentiment', size = 'Relatív gyakoriság (méret)') +
    theme_minimal()

ggsave(filename = 'topic.png')
save(list = c('topic_df'), file = 'topic_df.RData')

message('done!')

tcltk::tkmessageBox(title = "Job done!",
                    message = paste('Job done at', Sys.time()), icon = "info", type = "ok")
