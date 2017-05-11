# A SciDB backend for dplyr

The scidb.dplyr R package defines a SciDB interface to the dplyr way to
work with data backed by SciDB arrays (http://paradigm4.com). The package is still
experimental, please contribute!

The package style follows the dplyr data frame implementation more closely than
those of SQL-based external database back ends. We made this choice because
SciDB does not present a SQL API.


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

select(d, Petal_Length, Species) %>% filter(i < 100, Petal_Length < 1.4) %>% as.data.frame %>% head

##    i Petal_Length Species
## 1  3          1.3  setosa
## 2 14          1.1  setosa
## 3 15          1.2  setosa
## 4 17          1.3  setosa
## 5 23          1.0  setosa
## 6 36          1.2  setosa
```
Note the default includsion of SciDB array coordinate values above (the 'i'
variable in the output).  Use the optional `only_attributes=TRUE` option to the
`collect()` and `as.data.frame()` functions to only include data frame
variables (SciDB *attributes*) in the output.

```{r}
d %>% group_by(Species) %>% summarise(mean=mean(Petal_Length)) %>% collect

## # A tibble: 3 × 4
##   instance_id value_no    Species  mean
##         <dbl>    <dbl>      <chr> <dbl>
## 1           0        0     setosa 1.462
## 2           0        1  virginica 5.552
## 3           0        2 versicolor 4.260
```
Now without the SciDB array coordinate values...
```{r}
d %>% group_by(Species) %>% summarise(mean=mean(Petal_Length)) %>% collect(only_attributes=TRUE)

## # A tibble: 3 × 2
##      Species  mean
##        <chr> <dbl>
## 1     setosa 1.462
## 2  virginica 5.552
## 3 versicolor 4.260
```

```{r}
d %>% group_by(Species) %>% summarise(mean=mean(Petal_Length)) %>% as.data.frame(only_attributes=TRUE)

##      Species  mean
## 1     setosa 1.462
## 2  virginica 5.552
## 3 versicolor 4.260
```

## Implemented so far...

```
as.data.frame
collect
compute
dim
filter
full_join
group_by
inner_join
left_join
mutate
right_join
select
summarise
transmute
```

See the https://github.com/Paradigm4/scidb.dplyr/blob/master/TODO file for a
list of verbs not yet implemented (help/assistance is welcome)...
