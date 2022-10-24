# Automate_shiny

`Automate_shiny` is an RShiny web apps (https://kan-e.shinyapps.io/Automate_shiny/) for automated data visualization from count matrix files.<br>
It has simplified functions for the creation of a basic graph. <br>
The condition number is automatically recognized from the count matrix file and then the statical analysis is performed. <br>
In the case of just 2 conditions (pairwise comparison), Welch's t-test is performed. In the case of more than 3 conditions (multiple comparisons), the Tukey HSD test and Dunnett's test are performed.<br>

# Local installation
## Method 1 (Docker is required)
- Download Docker
- Run the following commands once to get the docker image of RNAseqChef<br>
```
docker pull kanetoh1030/shiny-automate
```
You may now run Automate with just one command in the command line:
```
docker run --rm -p 3838:3838 kanetoh1030/shiny-automate:latest
```
Please access http://localhost:3838 in your browser.

## Method 2 (R environment setup is required)
To run this app locally on your machine, R environment setup is required.
- Download R and RStudio (In the case of macOS, additionally install XQuartz and Xcode)
- Run the following commands once
```
pkgs <- c("shiny","DT","gdata","rstatix","multcomp","tidyverse","ggpubr","shinyBS")

for(pkg in pkgs) if (!require(pkg, character.only = T)){
    install.packages(pkg, update = F)
}
```

You may now run Automate_shiny with just one command in R:
```
shiny::runGitHub("Automate_shiny", "Kan-E")
```

# Input file format
Input file format must be excel file format (.xlsx), tab-separated text file format (.txt), or CSV file format (.csv). <br>
A1 cell in the excel sheet must be __Row.names__. <br>
The replication number is represented by the underbar. Do not use it for anything else. <br>
<img width="890" alt="format example" src="https://user-images.githubusercontent.com/77435195/148402453-e87ce92e-fcf1-4d45-af72-e9d256366bfa.png">

# Output example
Error plot (TukeyHSD)
![example_TukeyHSD](https://user-images.githubusercontent.com/77435195/148402886-16a48e4c-8962-4066-95bc-a6d26e7fada9.png)
Statical analysis (TukeyHSD)
<img width="810" alt="result_TukeyHSD" src="https://user-images.githubusercontent.com/77435195/148403003-658bdf78-dcd9-4186-a392-8e6e5b2f7cc7.png">

# Reference
ggplot2 and ggpubr (for barplot, boxplot, and errorplot)
- H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.
- Alboukadel Kassambara (2020). ggpubr: 'ggplot2' Based Publication Ready Plots. R package version 0.4.0. https://CRAN.R-project.org/package=ggpubr

dplyr and tidyr (for data manipulation)
- Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2021). dplyr: A Grammar of Data Manipulation. R package version 1.0.7. https://CRAN.R-project.org/package=dplyr
- Hadley Wickham (2021). tidyr: Tidy Messy Data. R package version 1.1.3. https://CRAN.R-project.org/package=tidyr

rstatix and multcomp (for statical analysis)
- Alboukadel Kassambara (2021). rstatix: Pipe-Friendly Framework for Basic Statistical Tests. R package version 0.7.0. https://CRAN.R-project.org/package=rstatix
- Torsten Hothorn, Frank Bretz and Peter Westfall (2008). Simultaneous Inference in General Parametric Models. Biometrical Journal 50(3), 346--363.

shiny
- Winston Chang, Joe Cheng, JJ Allaire, Carson Sievert, Barret Schloerke, Yihui Xie, Jeff Allen, Jonathan McPherson, Alan
  Dipert and Barbara Borges (2021). shiny: Web Application Framework for R. R package version 1.7.1.
  https://CRAN.R-project.org/package=shiny

# License
This shiny code is licensed under the GPLv3. Please see the file LICENSE.md for information.
```
Automate_shiny
Copyright (C) 2022  Kan Etoh

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

You may contact the author of this code, Kan Etoh, at <kaneto@kumamoto-u.ac.jp>
```
# Author

Kan Etoh
<kaneto@kumamoto-u.ac.jp>
