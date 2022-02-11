pubh7462\_hw2\_mjdasilv
================
Maykon da Silva
2/7/2022

``` r
# Reading in the dataset with relative path
brfss.df <- read_csv("./data/brfss_smart_2010.csv")
```

# Problem 3. BRFSS SMART 2002-2010

## 3.1 Data Exploration and Cleaning

``` r
# Tidying up the data (Cleaning)
brfss.tidy <- brfss.df %>%
  clean_names() %>%
  filter(topic %in% "Overall Health") %>%
  rename(state = locationabbr, locations = locationdesc, prop_responses = data_value) %>%
  separate(locations, c("state_abb", "county"), sep = " - ") %>%
  select(year, state, county, response, sample_size, prop_responses) %>%
  mutate(year = as.factor(year))
```

## 3.2 Data Description

The BRFSS SMART 2002-2010 is a County Prevalence lad line only data set,
which can be used to identify emerging health problems, establish and
track health objectives, and develop and evaluate public health policies
and programs. The original data set contains 23 variables but for this
assignment only Year, State, County, Response, Sample Size and
Proportion of Responses will be utilized to assess the “Overall Health”
topic. The Overall Health topic contains five options of responses:
Excellent, Very Good, Good, Fair, and Poor and will be described as
“response” in the following paragraphs.

``` r
# Determining the number of rows and columns from the penguins data set
brfss.tidy_rows <- nrow(brfss.tidy)
brfss.tidy_columns <- ncol(brfss.tidy)
```

Therefore, the data set contains:

  - 6 variables: year (from 2002 to 2010), state (all 50 US states),
    county (counties with 500 or more respondents), response (5 options:
    Excellent, Very Good, Good, Fair and Poor), sample size
    (representing the number of respondents/observations), and
    proportion of responses (which represents the relationship between
    number of observations for each possible response and the total
    number of observations for a specific county in percentage).

  - 10625 observations/rows: each observation is a combination of all 6
    variables and their levels (as described above). As an example, row
    1 describes the the sample size and proportion of responses for
    Jefferson County in Alabama (AL) in 2010 for the Excellent response.
    In this case, 94 observations were accounted as Excellent response,
    which represents 18.9% of all observations for Jefferson County.

## 3.3 Do Data Science

### 3.3.1 In the year 2004, which states were observed at 6 locations?

``` r
# Determining the number of states observed at 6 locations in 2004 using distinct()
brfss.tidy %>%
  filter(year %in% 2004) %>%
  distinct(state, county) %>%
  group_by(state) %>%
  rename(State = state) %>%
  summarise(Observations = n()) %>%
  filter(Observations %in% 6) %>%
  gt() %>%
  tab_header("States observed at 6 locations in 2004")
```

<div id="skreddshao" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#skreddshao .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#skreddshao .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#skreddshao .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#skreddshao .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#skreddshao .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#skreddshao .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#skreddshao .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#skreddshao .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#skreddshao .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#skreddshao .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#skreddshao .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#skreddshao .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#skreddshao .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#skreddshao .gt_from_md > :first-child {
  margin-top: 0;
}

#skreddshao .gt_from_md > :last-child {
  margin-bottom: 0;
}

#skreddshao .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#skreddshao .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#skreddshao .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#skreddshao .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#skreddshao .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#skreddshao .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#skreddshao .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#skreddshao .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#skreddshao .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#skreddshao .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#skreddshao .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#skreddshao .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#skreddshao .gt_left {
  text-align: left;
}

#skreddshao .gt_center {
  text-align: center;
}

#skreddshao .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#skreddshao .gt_font_normal {
  font-weight: normal;
}

#skreddshao .gt_font_bold {
  font-weight: bold;
}

#skreddshao .gt_font_italic {
  font-style: italic;
}

#skreddshao .gt_super {
  font-size: 65%;
}

#skreddshao .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="2" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>States observed at 6 locations in 2004</th>
    </tr>
    
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">State</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">Observations</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">CO</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_left">CT</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_left">MD</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_left">NM</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_left">SC</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_left">TX</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_left">UT</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_left">VT</td>
<td class="gt_row gt_right">6</td></tr>
  </tbody>
  
  
</table>
</div>

As shown by the table above, eight states were observed at six locations
in 2004: Colorado (CO), Connecticut (CT), Maryland (MD), New Mexico
(NM), South Carolina (SC), Texas (TX), Utah (UT), and Vermont (VT).

### 3.3.2 Make a “spaghetti plot” that shows the number of observed locations in each state from 2002 to 2010. Which state has the highest mean number of locations over this period?

``` r
brfss.tidy %>%
  select(year, state, county) %>%
  distinct(year, state, county) %>%
  group_by(year, state) %>%
  summarise(n = n()) %>%
  mutate(state = fct_reorder(.f = state, .x = n, .fun = "mean", .desc = TRUE),
         year = year %>%
           as.character() %>%
           as.numeric()
  ) %>%
  ggplot(aes(x = year, y = n, colour = state)) +
  geom_line() +
  labs(
    x     = "Year",
    y     = "Number of Locations Observed",
    title = "Observed Locations (Counties) per State from 2002 - 2010"
  ) +
  scale_x_continuous(breaks = 2002:2010) +
  scale_colour_viridis_d("State (Desc. Average Observations)", option = "turbo") +
  theme(
    legend.text  = element_text(size = 6),
    legend.title = element_text(size = 8))
```

<img src="pubh7462_hw2_mjdasilv_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" style="display: block; margin: auto;" />

As shown by the “spaghetti plot” above, Pennsylvania is the state with
the highest number of locations observed from 2002 to 2010. In addition,
most of the states with the highest average of locations observed in
this period are located in the US East Cost. Furthermore, the majority
of the states with the lowest average of locations observed in this
period are among the less populated states in the US. This is probably
correlated to the size of the counties that make up each state. Since
only counties with at least 500 or more respondents were selected to
represent this data set, less populated states might have smaller (less
populated) counties, which reduces the amount of locations being
surveyed.

### 3.3.3 Make a table showing, for the years 2002, 2006, and 2010, the mean and standard deviation of sample size and proportion of Excellent, Good, and Poor responses across locations in MN.

``` r
brfss.tidy %>%
  filter(year     %in% c("2002", "2006", "2010"),
         state    %in% "MN",
         response %in% c("Excellent", "Good", "Poor")) %>%
  group_by(year, response) %>%
  summarise(
    across(
      contains(c("sample", "prop")),
        list(mean = mean, sd = sd), na.rm = TRUE,
        .names = "{.col}_{.fn}"
  )
) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  gt() %>%
  tab_header("Summary of Sample Size and Prop. Responses for MN in 2002, 2006, and 2010")
```

<div id="qrlbndeksj" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#qrlbndeksj .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#qrlbndeksj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qrlbndeksj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#qrlbndeksj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#qrlbndeksj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qrlbndeksj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qrlbndeksj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#qrlbndeksj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#qrlbndeksj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#qrlbndeksj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#qrlbndeksj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#qrlbndeksj .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#qrlbndeksj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#qrlbndeksj .gt_from_md > :first-child {
  margin-top: 0;
}

#qrlbndeksj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#qrlbndeksj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#qrlbndeksj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#qrlbndeksj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qrlbndeksj .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#qrlbndeksj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qrlbndeksj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#qrlbndeksj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#qrlbndeksj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qrlbndeksj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qrlbndeksj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#qrlbndeksj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qrlbndeksj .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#qrlbndeksj .gt_left {
  text-align: left;
}

#qrlbndeksj .gt_center {
  text-align: center;
}

#qrlbndeksj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#qrlbndeksj .gt_font_normal {
  font-weight: normal;
}

#qrlbndeksj .gt_font_bold {
  font-weight: bold;
}

#qrlbndeksj .gt_font_italic {
  font-style: italic;
}

#qrlbndeksj .gt_super {
  font-size: 65%;
}

#qrlbndeksj .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="5" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Summary of Sample Size and Prop. Responses for MN in 2002, 2006, and 2010</th>
    </tr>
    
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">response</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sample_size_mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sample_size_sd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">prop_responses_mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">prop_responses_sd</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">2002</td>
    </tr>
    <tr><td class="gt_row gt_left">Excellent</td>
<td class="gt_row gt_right">116.00</td>
<td class="gt_row gt_right">83.27</td>
<td class="gt_row gt_right">24.15</td>
<td class="gt_row gt_right">3.54</td></tr>
    <tr><td class="gt_row gt_left">Good</td>
<td class="gt_row gt_right">123.75</td>
<td class="gt_row gt_right">84.26</td>
<td class="gt_row gt_right">23.95</td>
<td class="gt_row gt_right">1.05</td></tr>
    <tr><td class="gt_row gt_left">Poor</td>
<td class="gt_row gt_right">13.75</td>
<td class="gt_row gt_right">9.57</td>
<td class="gt_row gt_right">2.40</td>
<td class="gt_row gt_right">1.17</td></tr>
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">2006</td>
    </tr>
    <tr><td class="gt_row gt_left">Excellent</td>
<td class="gt_row gt_right">122.33</td>
<td class="gt_row gt_right">72.62</td>
<td class="gt_row gt_right">23.83</td>
<td class="gt_row gt_right">2.99</td></tr>
    <tr><td class="gt_row gt_left">Good</td>
<td class="gt_row gt_right">137.33</td>
<td class="gt_row gt_right">85.82</td>
<td class="gt_row gt_right">26.37</td>
<td class="gt_row gt_right">0.45</td></tr>
    <tr><td class="gt_row gt_left">Poor</td>
<td class="gt_row gt_right">15.00</td>
<td class="gt_row gt_right">6.93</td>
<td class="gt_row gt_right">2.30</td>
<td class="gt_row gt_right">0.95</td></tr>
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">2010</td>
    </tr>
    <tr><td class="gt_row gt_left">Excellent</td>
<td class="gt_row gt_right">203.80</td>
<td class="gt_row gt_right">190.60</td>
<td class="gt_row gt_right">25.44</td>
<td class="gt_row gt_right">5.28</td></tr>
    <tr><td class="gt_row gt_left">Good</td>
<td class="gt_row gt_right">220.00</td>
<td class="gt_row gt_right">196.10</td>
<td class="gt_row gt_right">26.04</td>
<td class="gt_row gt_right">3.55</td></tr>
    <tr><td class="gt_row gt_left">Poor</td>
<td class="gt_row gt_right">27.40</td>
<td class="gt_row gt_right">27.32</td>
<td class="gt_row gt_right">2.36</td>
<td class="gt_row gt_right">0.77</td></tr>
  </tbody>
  
  
</table>
</div>

As shown by the table above, “in 2002, 2006, and 2010,”Good" was the
response of Overall Health with the highest average sample size,
followed by “Excellent” and “Poor”, respectively. This raises two main
questions to me:

  - 1)  Was a lower sample size mean verified for Poor Overall Health
        because people with poor overall health were not willing to be
        surveyed or because they could not do it?

  - 2)  Maybe it is the opposite. Maybe most people in MN are healthier
        and that is why the sample size mean for poor overall health was
        so low in the state.

In addition, a similar trend was verified for the average proportion of
responses. However, it is important to point out that the Standard
Deviation for Excellent and Good are substantially higher than the
Standard Deviation of Poor for the sample size mean, which means that
the data for excellent and good are more dispersed (or there is more
variation) in relation to the mean than for poor.

### 3.3.4 Create a ggplot that communicates the results/trends from the table above and stands on its own

``` r
brfss.tidy %>%
  filter(year     %in% c("2002", "2006", "2010"),
         state    %in% "MN",
         response %in% c("Excellent", "Good", "Poor")) %>%
  group_by(year, response) %>%
  summarise(
    across(
      contains(c("sample", "prop")),
        list(mean = mean, sd = sd), na.rm = TRUE,
        .names = "{.col}_{.fn}"
  )
) %>%
  pivot_longer(cols      = 3:6, 
               names_to  = "variable_mean.sd", 
               values_to = "value") %>%
  mutate(fct_relevel(response, c("Excellent", "Good", "Poor")),
         year = year %>%
           as.character() %>%
           as.numeric()) %>%
  mutate(variable_mean.sd = recode(variable_mean.sd, 
                                   sample_size_mean    = "Sample Size Mean", 
                                   sample_size_sd      = "Sample Size S.D.",
                                   prop_responses_mean = "Prop. Responses Mean",
                                   prop_responses_sd   = "Prop. Responses S.D.")) %>%
  ggplot(aes(x = year, y = value, colour = response)) +
  geom_line() +
  labs(x        = "Year", 
       y        = "Values of Summary Statistics (Mean and Standard Deviation)", 
       title    = "Mean and Standard Deviation of Sample Size and Proportion of Response",
       subtitle = "Data from MN in 2002, 2006 and 2010") +
  facet_wrap(~variable_mean.sd, scales = "free", ncol = 2, labeller = ) +
  scale_colour_viridis_d("Overall Health Response", option = "turbo") +
  scale_x_continuous(breaks = seq(from = 2002, to = 2010, by = 4)) +
  theme(
    legend.text   = element_text(size = 10),
    legend.title  = element_text(size = 10),
    plot.title    = element_text(hjust = 0.5, size = 10),
    plot.subtitle = element_text(hjust = 0.5, size = 10))
```

<img src="pubh7462_hw2_mjdasilv_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" style="display: block; margin: auto;" />
