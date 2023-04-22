# disseRtation

This repo is for the project ["Morton's Fork" - Dissonance dissertation project" (osf.io/zkdmu)](https://osf.io/9frup/). <br><br>
To clone this repo (assuming you have set and connected up git/R), click "new project" in R, then click "version control," then "git," and enter the "repository URL" **https://github.com/adeubanks/disseRtation.git**. 


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
90 g. shredded coconut (~1 cup; can be sweetened or unsweetened, depending on how Southern you are)<br>
125 g. chopped pecans (~1 cup) <br>
50 g. Rice Krispies cereal (~2 cups; stir gently after adding these) <br>

### Bake @ 350 for 9-10 mins <br>
Spoon the cookie mixture (1.5 to 2 tablespoons per cookie) onto a parchment lined cookie sheet. <br>
Bake until the edges *just* start to turn golden brown.<br>

**Under-cooking would likely be preferable to over-cooking.** <br>
This recipe makes approximately 3 batches of a dozen each. <br>
While the first cookie sheet is in the oven, you can begin scooping and prepping the next cookie sheet. 
<br>

### Vegan alternative <br>
*Note.* For a vegan version of these cookies that is just as delicious, only two small changes are made. First, omit butter and use 1 cup of butter flavored shortening. Second omit the eggs. In place of eggs add 1.5-2 tablespoons of melted coconut oil and approximately 2 ounces of club soda.** Just before the final step of stirring in Rice Krispies, if the cookie dough seems too dry, another splash of club soda can be added and mixed in. <br> 
<br> 

###### ** It's entirely possible just water works every bit as well as club soda, but to date, they have only been attempted with club soda. 

