change log.md
### 08/10/2024
I have noticed that there was something wrong with the scoring of the SHAPS. For this reason, in the file `00_check_data.R` I created new scoring, not basded on the "Output_scales_39.csv" as before, but on the output of each single participant's file. I also plotted the histograms and ran some correlations with BDI and PANAS pos and results make sense
### 09/10/2024
Dimitra realized that there are duplicate IDs in the raw data. FOr example, ID 16 is repeated twice. For this reason, I am creating a new file, "000.prepr", to modify the IDs of those duplicate files. The duplicate files' are renamed as the old ID+1, after noticing that there are some prositions left free in the progressive numbering of IDs after duplicates (e.g., after the 16s there is not 17, but 18; after the 24s, there is the 27, etc.). The only exception is for number 4, which becomes 3. 
The duplicate IDs are:
16, 24, 39, 4, 47
I also renamed the "data_RL_screen" as "raw_data", and created the new "data_RL_screen" folder with the files with modified ID