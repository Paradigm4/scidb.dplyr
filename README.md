# A SciDB backend for dplyr

In development now...more soon.

## Install

```{r}
devtools::install_github("paradigm4/scidb.dplyr")
```

## Example

```{r}
library(scidb.dplyr)

db = scidbconnect()
x  = as.scidb(db, iris)    # a 'scidb' object
d  = tbl(x)                # a 'tbl_scidb' object (dplyr)

select(d, Petal_Length, Species) %>% filter(i < 100, Petal_Length < 1.4) %>% as.data.frame

d %>% group_by(Species) %>% summarise(mean=mean(Petal_Length)) %>% collect


# If you only want attributes...
d %>% group_by(Species) %>% summarise(mean=mean(Petal_Length)) %>% collect(only_attributes=TRUE)

d %>% group_by(Species) %>% summarise(mean=mean(Petal_Length)) %>% as.data.frame(only_attributes=TRUE)
```
