
library(haven)
library(psych)
library(gt)

the_date <- as.character(Sys.Date())


# read in the data
class <- read_sas("class.sas7bdat")

# get the descriptive statistics
ht <- describeBy(class$Height, group=class$trt, mat=TRUE)
wt <- describeBy(class$Weight, group=class$trt, mat=TRUE)

# get the count and percentage
bign <- table(group=class$trt)
bign1 <- as.numeric(bign[1])
bign2 <- as.numeric(bign[2])


freq <- table(class$Sex, group=class$trt)
prop <- 100*prop.table(table(class$Sex, class$trt), 2)

# handle the decimals
ht$n <- format(ht$n, digits=2)
ht$mean <- format(ht$mean, digits=4)
ht$sd <- format(ht$sd, digits=4)
ht$median <- format(ht$median, digits=4)
ht$min <- format(ht$min, digits=4)
ht$max <- format(ht$max, digits=4)

wt$n <- format(wt$n, digits=2)
wt$mean <- format(wt$mean, digits=5)
wt$sd <- format(wt$sd, digits=4)
wt$median <- format(wt$median, digits=4)
wt$min <- format(wt$min, digits=4)
wt$max <- format(wt$max, digits=4)

# create a variable minmax, and do transpose
ht$minmax <- paste(ht$min, ',', ht$max) 
ht <- ht[c("n","mean","sd","median","minmax")]
ht2 <- t(ht)

wt$minmax <- paste(wt$min, ',', wt$max) 
wt <- wt[c("n","mean","sd","median","minmax")]
wt2 <- t(wt)

# combine count and percentage
X11 <- paste(freq[,1], '(', format(prop[,1], digit=3), ')')
X12 <- paste(freq[,2], '(', format(prop[,2], digit=3), ')')
sex <- cbind(X11, X12)

# create a new column called statistics and get the final data for reporting 
rownames(sex) <- c('Female','Male')
rownames(ht2)  <- c("n","mean","sd","median","min, max")

sex3 <- data.frame(statistics=rownames(sex), sex)
ht3 <- data.frame(statistics=rownames(ht2), ht2)
wt3 <- data.frame(statistics=rownames(ht2), wt2)

final <- rbind(ht3, wt3, sex3)


# use gt to do the reporting 
tab1 <- final %>% 
  gt() %>%
  tab_row_group(
    label = "Sex",
    rows = 11:12
  ) %>%
  
  tab_row_group(
    label = "Weight (kg)",
    rows = 6:10
  ) %>%
  
tab_row_group(
  label = "Height (in)",
  rows = 1:5
) %>%

tab_header(
  title = "Table 14.1 Demographics and Baseline Characteristics",
  subtitle = "Safety Population"
) %>%
tab_source_note(
  source_note = "Note: the source data is class."
) %>%

tab_source_note(
    source_note = paste('Program Source: demog.R            Executed: (Draft)',  the_date)
  ) %>%

cols_label(
  X11 = html(paste("Treatment A <br> (N=", bign1, ")")),
  X12 = html(paste("Treatment B <br> (N=", bign2, ")"))
) %>%

tab_options(
  table.border.top.color = "white",
  heading.border.bottom.color = "black",
  table.border.bottom.color = "white",
  table_body.border.bottom.color = "black",
  table_body.hlines.color = "white", 
  row_group.border.bottom.color = "white", 
  row_group.border.top.color = "white", 
  column_labels.border.top.color = "black",
  column_labels.border.bottom.color = "black",
) 

# output the HTML table

tab1 %>%
gtsave("demog.html", path = "C:\\demog_R" )



