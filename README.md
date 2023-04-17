# disseRtation

This repo is for the project ["Morton's Fork" - Dissonance dissertation project" (osf.io/zkdmu)](https://osf.io/zkdmu/)

## Scripts
There are two scripts within the "scripts" folder. If you want to start from the raw data, see "Cleanup.R;" if you want to reproduce what is reported in the manuscript, see "Analysis.R."

### **"Cleanup.R"** 
**"Cleanup.R"** processes the data from its raw format (i.e., exactly as exported from ANES, *anes_timeseries_2016.sav*, and *anes_timeseries_2020_spss_20220210.sav*) and exports two data sets:

**df_full.RDS** is the dataset that has all of the useful/relevant data, including e.g., both the computed variables *and* the vairables that were used for the computation. 

**df_analysis.RDS** is the dataset that is used for the primary analyses; it began with *df_full.RDS* then selected down to just the variables used in the manuscript. 

### **"Analysis.R"** 
**"Analysis.R"** reads in **df_analysis.RDS** and does all the modeling, tabling, plotting, etc. that is reported in the manuscript. 

<br>
<br>
<br>

## "Nana's famous crunch cookies"
### Cream together:
200g white sugar<br>
200g dark brown sugar<br>
1/2 cup butter (1 stick, or ~115 g.)<br>
1/2 cup shortening (preferably butter flavored)<br>

### Add in (and mix well):<br>
2 eggs (mix well after each) <br>
1 tsp vanilla extract <br>
250 g. self-rising flour (mix in 1/4 to 1/3 at a time, mixing after each)<br>

### Stir in:<br>
125 g. quick oats (~1.5 cups) <br>
90 g. shredded coconut (~1 cup; can be sweetened or unsweetend, depending on how Southern you are)<br>
125 g. chopped pecans (~1 cup) <br>
50 g. Rice Krispies cereal (~2 cups; stir gently after adding these) <br>

### Bake @ 350 for 9-10 mins <br>
Spoon the cookie mixture (1.5 to 2 tablespoons per cookie) onto a parchment lined cookie sheet. <br>
Bake until the edges *just* start to turn golden brown.<br>

**Under-cooking would likely be preferable to over-cooking.** <br>
This recipe makes approximately 3 batches of a dozen each. <br>
While the first cookie sheet is in the oven, you can begin scooping and prepping the next cookie sheet. 

