library(openxlsx)
library(data.table)
library(gt)

Group_Names = c('1' = 'Northern Parallel',
                '2' = 'Central Canada',
                '3' = 'Rocky Mountains',
                '4' = 'Appalachians',
                '5' = 'Northern Pacific Coast',
                '6' = 'Southern Plains',
                '7' = 'Northern Plains',
                '8' = 'Central East',
                '9' = 'Northern Atlantic',
                '10' = 'Southwest',
                '11' = 'Southern Pacific Coast',
                '12' = 'Western Canada',
                '13' = 'Rocky Lowland',
                '14' = 'Pacific Northwest',
                '15' = 'Great Lakes',
                '16' = 'Southeast')

#Read table
Table = setDT(read.xlsx("C:\\Users\\cmeri\\OneDrive - Dartmouth College\\Research\\Runoff_Ratio\\Writing\\Earth's Future\\Initial_Submission\\2023EF003832_SI_TableS1.xlsx"))

#Modify Table
Table = Table[,cluster:=Group_Names[cluster]]

#Convert to Dataframe
Mod_df1 <- as.data.frame(Table[,c('cluster','R.Squared','intercept','Precip.p-value','OffPrecip.p-value','Temp.p-value','OffTemp.p-value')])

Mod_df2 <- as.data.frame(Table[,c('cluster','R.Squared','intercept','Precip.Coef.','OffPrecip.Coef.','Temp.Coef.','OffTemp.Coef.')])

Mod_df3 <- as.data.frame(Table[,c('cluster','R.Squared','intercept','Precip.VIF','OffPrecip.VIF','Temp.VIF','OffTemp.VIF')])

# Create a gt table from the data frame
gt_mod1 <- gt(data = Mod_df1)

# P-value
gt_mod1 <- gt_mod1 %>%
  tab_header(title = 'Table 1') %>%
  tab_style(style = cell_text(weight = "bold"),locations = cells_title()) %>%
  opt_horizontal_padding(scale=3) %>%
  cols_label(`R.Squared` = paste0('R','\U00B2'), cluster = 'Cluster', intercept='Intercept',
             `Precip.p-value` = 'Precip p-value', `OffPrecip.p-value`='OffPrecip p-value', `Temp.p-value` = 'Temp p-value', `OffTemp.p-value`='OffTemp p-value') %>%
  cols_align(align='center') %>%
  fmt_number(columns = c('R.Squared','intercept'),n_sigfig = 2) %>%
  fmt_scientific(columns = c('Precip.p-value','OffPrecip.p-value','Temp.p-value','OffTemp.p-value'),decimals = 2) %>%
  sub_missing(columns = everything(),rows = everything(),missing_text = " ")

# Print the table
print(gt_mod1)
as_latex(gt_mod1)

# Coef.
gt_mod2 <- gt(data = Mod_df2)
gt_mod2 <- gt_mod2 %>%
  tab_header(title = 'Table 1 (continued)') %>%
  tab_style(style = cell_text(weight = "bold"),locations = cells_title()) %>%
  opt_horizontal_padding(scale=3) %>%
  cols_label(`R.Squared` = paste0('R','\U00B2'), cluster = 'Cluster', intercept='Intercept',
             `Precip.Coef.`='Precip Coef', `OffPrecip.Coef.`='OffPrecip Coef', `Temp.Coef.`='Temp Coef', `OffTemp.Coef.`='OffTemp Coef') %>%
  cols_align(align='center') %>%
  fmt_number(columns = c('R.Squared','intercept','Precip.Coef.','OffPrecip.Coef.','Temp.Coef.','OffTemp.Coef.'),n_sigfig = 2) %>%
  sub_missing(columns = everything(),rows = everything(),missing_text = " ")

print(gt_mod2)
as_latex(gt_mod2)

# VIF
gt_mod3 <- gt(data = Mod_df3)
gt_mod3 <- gt_mod3 %>%
  tab_header(title = 'Table 1 (continued)') %>%
  tab_style(style = cell_text(weight = "bold"),locations = cells_title()) %>%
  opt_horizontal_padding(scale=3) %>%
  cols_label(`R.Squared` = paste0('R','\U00B2'), cluster = 'Cluster', intercept='Intercept',
             `Precip.VIF`='Precip VIF', `OffPrecip.VIF`='OffPrecip VIF', `Temp.VIF`='Temp VIF', `OffTemp.VIF`='OffTemp VIF') %>%
  cols_align(align='center') %>%
  fmt_number(columns = c('R.Squared','intercept','Precip.VIF','OffPrecip.VIF','Temp.VIF','OffTemp.VIF'),n_sigfig = 2) %>%
  sub_missing(columns = everything(),rows = everything(),missing_text = " ")

print(gt_mod3)
as_latex(gt_mod3)
