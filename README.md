# A SciDB backend for dplyr

In development now...more soon.

## Install

```{r}
devtools::install_github("paradigm4/scidb.dplyr")
```

## Examples

```{r}
library(scidb.dplyr)

db = scidbconnect()
x  = as.scidb(db, iris)    # a 'scidb' object
d  = tbl(x)                # a 'tbl_scidb' object (dplyr)

select(d, Petal_Length, Species) %>% filter(i < 100, Petal_Length < 1.4) %>% as.data.frame

d %>% group_by(Species) %>% summarise(mean=mean(Petal_Length)) %>% collect


# If you only want SciDB array attributes (excluding dimensions)...
d %>% group_by(Species) %>% summarise(mean=mean(Petal_Length)) %>% collect(only_attributes=TRUE)

d %>% group_by(Species) %>% summarise(mean=mean(Petal_Length)) %>% as.data.frame(only_attributes=TRUE)
```

## What's implemented so far...

```
as.data.frame
collect
compute
dim
filter_
full_join
group_by_
inner_join
left_join
mutate_
right_join
select_
summarise_
transmute_
```

See the TODO file for a list of verbs not yet implemented...
