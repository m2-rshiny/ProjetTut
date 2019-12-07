# ProjetTut

Exécutez ce code pour voir ce que le prof attend de nous.

```{r}
install.packages("shinyML")
library(shinyML)
longley2 <- longley %>% mutate(Year = as.Date(as.character(Year),format = "%Y"))
shiny_h2o(data =longley2,y = "GNP",date_column = "Year",share_app = FALSE)
```
