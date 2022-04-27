---
title: "Netflix Data Analysis"
author: "Tarek Atassi"
date: '2022-04-26'
output: 
  html_document: 
    toc: yes
    fig_caption: yes
    keep_md: yes
---

# Netflix Movies Data

## Loading the libraries
```{r echo=TRUE}
install.packages("pacman")
library(pacman)
p_load(tidyverse,lubridate,showtext)
showtext_auto()
font_add_google("Bebas Neue", "Bebas Neue")
```

## Loading the dataset
```{r}
netflix <- read_csv('NetflixOriginals.csv')
```

### looking at the data in brief

```{r}
head(netflix)
```

### types of datatype in each column
```{r}
as_tibble(sapply(netflix, class))
```

### Does the data contain any N/A values?
```{r}
any(is.na(netflix))
```

### Converting the "Premiere" from character to datetime
```{r}
netflix <- netflix %>% mutate(Released = mdy(Premiere))
```

### Getting each year, month, and date separated, also adding the corresponding day of the week for each release.
```{r}
netflix <- netflix %>%
        mutate(Year = year(Released)) %>%
        mutate(Month = month(Released, label=TRUE)) %>%
        mutate(Date = day(Released)) %>%
        mutate(Day = wday(Released, label=TRUE, abbr=FALSE))
```

### Whem the movies were released?

#### Number of Movies released each year
```{r}
n <- netflix %>% group_by(Year) %>% summarise(total=n())
n_graph <- ggplot(data=n)+
        geom_col(mapping=aes(
          x=Year,
          y=total,
          fill=ifelse(total==max(total),"red","grey"))
          )+
        labs(title="Netflix Movies released each year")+
        theme_minimal()+
        scale_fill_manual(values=c("#2d2d2d","#E50914"))+
        theme(
          legend.position="none",
          plot.title=element_text(family="Bebas Neue",size=35,color="#E50914"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank()
          )

n_graph
```

#### Number of Movies released each month
```{r}
n1 <- netflix %>% group_by(Month) %>% summarise(total=n())
n1_graph <- ggplot(data=n1)+
        geom_col(mapping = aes(
            x=Month,
            y=total,
            fill=ifelse(total==max(total),"red","grey")))+
        labs(title='Netflix Movies Released Each Month')+
        theme_minimal()+
        scale_fill_manual(values=c("#2d2d2d","#E50914"))+
        theme(
            legend.position='none',
            plot.title = element_text(family="Bebas Neue",size=30,color="#E50914"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size=20))

n1_graph
```

#### Number of movies released by date of the month
```{r}
n2 <- netflix %>% group_by(Date) %>% summarise(total=n())

n2_graph <- ggplot(data=n2)+
                  geom_col(mapping=aes(x=Date, y=total,
                  fill=ifelse(total==max(total),"red","grey")))+
                  labs(title="Netflix Movies released by date of each month")+
                  theme_minimal()+
                  scale_fill_manual(values = c("#2d2d2d","#E50914"))+
                  theme(
                    legend.position="none",
                    plot.title=element_text(
                      family="Bebas Neue",
                      size=30,
                      color="#E50914"
                      ),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    panel.grid.major.x=element_blank(),
                    panel.grid.minor = element_blank(),
                    text=element_text(size=20)
                    )

n2_graph
```

#### Number of movies releaes each day of the week
```{r}
n3 <- netflix %>% group_by(Day) %>% summarise(total=n())

n3_graph <- 
        ggplot(data=n3)+
        geom_col(mapping=aes(
            x=Day,
            y=total,
            fill=ifelse(total==max(total),"red","black")))+
        labs(title="Netflix Movies released by day of the week")+
        theme_minimal()+
        scale_fill_manual(values=c("#2d2d2d","#E50914"))+
        theme(
            legend.position="none",
            plot.title = element_text(
                family="Bebas Neue",
                size=30,
                color="#E50914"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major.x=element_blank(),
            panel.grid.minor=element_blank(),
            text=element_text(size=20)
            )
            
n3_graph
```

    

#### 5 Most popular Genres
```{r}
    n4 <- netflix %>% group_by(Genre) %>% summarise(Movies=n()) %>% arrange(desc(Movies)) %>% head(5)

    n4_graph <-
        ggplot(data=n4)+
        geom_col(mapping = aes(
            x=reorder(Genre, -Movies),
            y=Movies,
            fill=ifelse(Movies == max(Movies),"red","black")))+
        labs(title="Most Popular Genres")+
        theme_minimal()+
        scale_fill_manual(values = c("#2d2d2d","#E50914"))+
        theme(
            legend.position="none",
            plot.title = element_text(family="Bebas Neue",size=30,color="#E50914"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major.x=element_blank(),
            panel.grid.minor = element_blank(),
            text = element_text(size=20)
            )

    n4_graph
```

#### 5 Most Popular Languages
```{r}
n5 <- netflix %>% 
            group_by(Language) %>% 
            summarise(Movies=n()) %>% 
            arrange(desc(Movies)) %>% 
            head(5)

    n5_graph <- 
        ggplot(data=n5)+
        geom_col(mapping=aes(
        x=reorder(Language, -Movies),
        y=Movies,
        fill=ifelse(Movies == max(Movies),"red","black")))+
        labs(title="Most Popular Languages")+
        theme_minimal()+
        scale_fill_manual(values=c("#2d2d2d","#E50914"))+
        theme(
            legend.position="none",
            plot.title = element_text(
                family="Bebas Neue",
                size=30,
                color="#E50914"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major.x=element_blank(),
            panel.grid.minor = element_blank(),
            title=element_text(size=20)
            )

    n5_graph
```

#### IMDB Scores - How were most movies rated?
```{r}
    n6_graph <- ggplot(netflix)+
                    geom_dotplot(mapping=aes(x=`IMDB Score`),
                        binwidth=0.3,fill="#2d2d2d",color="#e9ecef")+
                    labs(title="IMDB Score Distribution")+
                    theme_minimal()+
                    theme(
                    legend.position="none",
                    plot.title=element_text(family="Bebas Neue",size=25,color="#E50914"),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    panel.grid.major.x=element_blank()
                    )

    n6_graph
```

#### Highest Rated Movies
```{r}
n7 <- netflix %>% arrange(desc(`IMDB Score`)) %>% head(5)

    n7_graph <- ggplot(data=n7)+
                    geom_col(mapping=aes(
                        x=reorder(`Title`,`IMDB Score`),
                        y=`IMDB Score`,
                        fill=ifelse(`IMDB Score`==max(`IMDB Score`),"red","black")))+
                    labs(title="Highest Rated Movies")+
                    theme_minimal()+
                    scale_fill_manual(values = c("#2d2d2d","#E50914"))+
                    coord_flip()+
                    theme(
                        legend.position="none",
                        plot.title = element_text(family="Bebas Neue",size=25,color="#E50914"),
                        axis.title.x=element_blank(),
                        axis.title.y=element_blank(),
                        panel.grid.major.x=element_blank()
                        )

    n7_graph
```

#### Lowest Rated Movies
```{r}
    n8 <- netflix %>% arrange(desc(-`IMDB Score`)) %>% head(5)

    n8_graph <- ggplot(data=n8)+
                    geom_col(mapping=aes(
                        x=reorder(`Title`, -`IMDB Score`),
                        y=`IMDB Score`,
                        fill=ifelse(`IMDB Score`==min(`IMDB Score`),"red","black")))+
                    labs(title="Lowest Rated Movies")+
                    theme_minimal()+
                    scale_fill_manual(values = c("#2d2d2d","#E50914"))+
                    coord_flip()+
                    theme(
                        legend.position="none",
                        plot.title = element_text(family="Bebas Neue",size=25,color="#E50914"),
                        axis.title.x=element_blank(),
                        axis.title.y=element_blank(),
                        panel.grid.major.x=element_blank()
                        )
            
    n8_graph
```

#### Runtime - How long are the movies?
```{r}
    n9_graph <- ggplot(data=netflix)+
                    geom_dotplot(mapping=aes(x=Runtime),binwidth=2.25,fill="#2d2d2d",color="#e9ecef")+
                    labs(title="Movie Runtime")+
                    theme_minimal()+
                    theme(
                        legend.position="none",
                        plot.title=element_text(family="Bebas Neue",size=25,color="#E50914"),
                        axis.title.x = element_blank(),
                        axis.title.y=element_blank(),
                        panel.grid.major.x=element_blank()
                        )

    n9_graph
```

#### Longest Movies
```{r}
    n10 <- netflix %>% arrange(desc(Runtime)) %>% head(5)

    n10_graph <- ggplot(data=n10)+
                    geom_col(mapping=aes(
                        x=reorder(`Title`,`Runtime`),
                        y=`Runtime`,
                        fill=ifelse(Runtime==max(`Runtime`),"red","black")))+
                    labs(title="Longest Movies")+
                    theme_minimal()+
                    scale_fill_manual(values=c("#2d2d2d","#E50914"))+
                    coord_flip()+
                    theme(
                        legend.position="none",
                        plot.title = element_text(family="Bebas Neue",size=25,color="#E50914"),
                        axis.title.x=element_blank(),
                        axis.title.y=element_blank(),
                        panel.grid.major.x=element_blank()
                        )

    n10_graph
```

#### Shortest Movies
```{r}
    n11 <- netflix %>% arrange(desc(-Runtime)) %>% head(5)

    n11_graph <- ggplot(data=n11)+
                    geom_col(mapping=aes(
                        x = reorder(`Title`,`Runtime`),
                        y = `Runtime`,
                        fill = ifelse(Runtime==min(`Runtime`),"red","black")))+
                    labs(title="Shortest Movies")+
                    theme_minimal()+
                    scale_fill_manual(values = c("#2d2d2d","#E50914"))+
                    coord_flip()+
                    theme(
                        legend.position="none",
                        plot.title = element_text(family="Bebas Neue",size=25,color="#E50914"),
                        axis.title.x=element_blank(),
                        axis.title.y=element_blank(),
                        panel.grid.major.x=element_blank(),
                        text=element_text(size=20)
                        )

    n11_graph
```

#### Runtime vs IMDB-Score
```{r}
    n12_graph <- ggplot(data=netflix,aes(x = `IMDB Score`, y = Runtime))+
                    geom_point()+
                    geom_smooth(method = "lm", color="#E50914")+
                    labs(title="Runtime vs IMDB Rating")+
                    theme_minimal()+
                    scale_fill_manual(values=c("#2d2d2d","#E50914"))+
                    theme(
                        legend.position = "none",
                        plot.title=element_text(family="Bebas Neue",size=25,color="#E50914"),
                        axis.title.x=element_blank(),
                        axis.title.y=element_blank(),
                        panel.grid.major.x=element_blank()
                        )

    n12_graph
```

## Basic Statistical Analysis

#### Linear Models
```{r}
    model <- lm(data=netflix, formula = Runtime ~ `IMDB Score`)

    summary(model)
```

#### Correlation Test
```{r}
res <- cor.test(netflix$Runtime, netflix$`IMDB Score`,method="pearson")

res
```

#### P-Value
```{r}
res$p.value
```

#### Correlation Coefficient
```{r}
res$estimate
```

# Thank You
