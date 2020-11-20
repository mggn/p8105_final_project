BRFSS data exploration
================
Maria Guzman
11/18/2020

### About BRFSS

*“The Behavioral Risk Factor Surveillance System (BRFSS) is the nation’s
premier system of health-related telephone surveys that collect state
data about U.S. residents regarding their health-related risk behaviors,
chronic health conditions, and use of preventive services. Established
in 1984 with 15 states, BRFSS now collects data in all 50 states as well
as the District of Columbia and three U.S. territories. BRFSS completes
more than 400,000 adult interviews each year, making it the largest
continuously conducted health survey system in the world.”*

First, we need to import the data using read\_xpt

``` r
brfss_df =
  read_xpt("./final_project_large_data/LLCP2019.xpt") %>%
  janitor::clean_names()

head(brfss_df)
```

    ## # A tibble: 6 x 342
    ##   state fmonth idate imonth iday  iyear dispcode seqno    psu ctelenm1 pvtresd1
    ##   <dbl>  <dbl> <chr> <chr>  <chr> <chr>    <dbl> <chr>  <dbl>    <dbl>    <dbl>
    ## 1     1      1 0118~ 01     18    2019      1100 2019~ 2.02e9        1        1
    ## 2     1      1 0113~ 01     13    2019      1100 2019~ 2.02e9        1        1
    ## 3     1      1 0118~ 01     18    2019      1100 2019~ 2.02e9        1        1
    ## 4     1      1 0118~ 01     18    2019      1200 2019~ 2.02e9        1        1
    ## 5     1      1 0104~ 01     04    2019      1100 2019~ 2.02e9        1        1
    ## 6     1      1 0118~ 01     18    2019      1200 2019~ 2.02e9        1        1
    ## # ... with 331 more variables: colghous <dbl>, statere1 <dbl>, celphone <dbl>,
    ## #   ladult1 <dbl>, colgsex <dbl>, numadult <dbl>, landsex <dbl>, nummen <dbl>,
    ## #   numwomen <dbl>, respslct <dbl>, safetime <dbl>, ctelnum1 <dbl>,
    ## #   cellfon5 <dbl>, cadult1 <dbl>, cellsex <dbl>, pvtresd3 <dbl>,
    ## #   cclghous <dbl>, cstate1 <dbl>, landline <dbl>, hhadult <dbl>, sexvar <dbl>,
    ## #   genhlth <dbl>, physhlth <dbl>, menthlth <dbl>, poorhlth <dbl>,
    ## #   hlthpln1 <dbl>, persdoc2 <dbl>, medcost <dbl>, checkup1 <dbl>,
    ## #   bphigh4 <dbl>, bpmeds <dbl>, cholchk2 <dbl>, toldhi2 <dbl>, cholmed2 <dbl>,
    ## #   cvdinfr4 <dbl>, cvdcrhd4 <dbl>, cvdstrk3 <dbl>, asthma3 <dbl>,
    ## #   asthnow <dbl>, chcscncr <dbl>, chcocncr <dbl>, chccopd2 <dbl>,
    ## #   addepev3 <dbl>, chckdny2 <dbl>, diabete4 <dbl>, diabage3 <dbl>,
    ## #   havarth4 <dbl>, arthexer <dbl>, arthedu <dbl>, lmtjoin3 <dbl>,
    ## #   arthdis2 <dbl>, joinpai2 <dbl>, marital <dbl>, educa <dbl>, renthom1 <dbl>,
    ## #   numhhol3 <dbl>, numphon3 <dbl>, cpdemo1b <dbl>, veteran3 <dbl>,
    ## #   employ1 <dbl>, children <dbl>, income2 <dbl>, weight2 <dbl>, height3 <dbl>,
    ## #   pregnant <dbl>, deaf <dbl>, blind <dbl>, decide <dbl>, diffwalk <dbl>,
    ## #   diffdres <dbl>, diffalon <dbl>, smoke100 <dbl>, smokday2 <dbl>,
    ## #   stopsmk2 <dbl>, lastsmk2 <dbl>, usenow3 <dbl>, alcday5 <dbl>,
    ## #   avedrnk3 <dbl>, drnk3ge5 <dbl>, maxdrnks <dbl>, exerany2 <dbl>,
    ## #   exract11 <dbl>, exeroft1 <dbl>, exerhmm1 <dbl>, exract21 <dbl>,
    ## #   exeroft2 <dbl>, exerhmm2 <dbl>, strength <dbl>, fruit2 <dbl>,
    ## #   fruitju2 <dbl>, fvgreen1 <dbl>, frenchf1 <dbl>, potatoe1 <dbl>,
    ## #   vegetab2 <dbl>, flushot7 <dbl>, flshtmy3 <dbl>, tetanus1 <dbl>,
    ## #   pneuvac4 <dbl>, hivtst7 <dbl>, hivtstd3 <dbl>, ...

``` r
names(brfss_df)
```

    ##   [1] "state"    "fmonth"   "idate"    "imonth"   "iday"     "iyear"   
    ##   [7] "dispcode" "seqno"    "psu"      "ctelenm1" "pvtresd1" "colghous"
    ##  [13] "statere1" "celphone" "ladult1"  "colgsex"  "numadult" "landsex" 
    ##  [19] "nummen"   "numwomen" "respslct" "safetime" "ctelnum1" "cellfon5"
    ##  [25] "cadult1"  "cellsex"  "pvtresd3" "cclghous" "cstate1"  "landline"
    ##  [31] "hhadult"  "sexvar"   "genhlth"  "physhlth" "menthlth" "poorhlth"
    ##  [37] "hlthpln1" "persdoc2" "medcost"  "checkup1" "bphigh4"  "bpmeds"  
    ##  [43] "cholchk2" "toldhi2"  "cholmed2" "cvdinfr4" "cvdcrhd4" "cvdstrk3"
    ##  [49] "asthma3"  "asthnow"  "chcscncr" "chcocncr" "chccopd2" "addepev3"
    ##  [55] "chckdny2" "diabete4" "diabage3" "havarth4" "arthexer" "arthedu" 
    ##  [61] "lmtjoin3" "arthdis2" "joinpai2" "marital"  "educa"    "renthom1"
    ##  [67] "numhhol3" "numphon3" "cpdemo1b" "veteran3" "employ1"  "children"
    ##  [73] "income2"  "weight2"  "height3"  "pregnant" "deaf"     "blind"   
    ##  [79] "decide"   "diffwalk" "diffdres" "diffalon" "smoke100" "smokday2"
    ##  [85] "stopsmk2" "lastsmk2" "usenow3"  "alcday5"  "avedrnk3" "drnk3ge5"
    ##  [91] "maxdrnks" "exerany2" "exract11" "exeroft1" "exerhmm1" "exract21"
    ##  [97] "exeroft2" "exerhmm2" "strength" "fruit2"   "fruitju2" "fvgreen1"
    ## [103] "frenchf1" "potatoe1" "vegetab2" "flushot7" "flshtmy3" "tetanus1"
    ## [109] "pneuvac4" "hivtst7"  "hivtstd3" "hivrisk5" "pdiabtst" "prediab1"
    ## [115] "insulin1" "bldsugar" "feetchk3" "doctdiab" "chkhemo3" "feetchk" 
    ## [121] "eyeexam1" "diabeye"  "diabedu"  "toldcfs"  "havecfs"  "workcfs" 
    ## [127] "toldhepc" "trethepc" "prirhepc" "havehepc" "havehepb" "medshepb"
    ## [133] "hpvadvc3" "hpvadsht" "imfvpla1" "shingle2" "lcsfirst" "lcslast" 
    ## [139] "lcsnumcg" "lcsctscn" "hadmam"   "howlong"  "hadpap2"  "lastpap2"
    ## [145] "hpvtest"  "hplsttst" "hadhyst2" "pcpsaad3" "pcpsadi1" "pcpsare1"
    ## [151] "psatest1" "psatime"  "pcpsars1" "pcpsade1" "pcdmdec1" "bldstool"
    ## [157] "lstblds3" "hadsigm3" "hadsgco1" "lastsig3" "cncrdiff" "cncrage" 
    ## [163] "cncrtyp1" "csrvtrt3" "csrvdoc1" "csrvsum"  "csrvrtrn" "csrvinst"
    ## [169] "csrvinsr" "csrvdein" "csrvclin" "csrvpain" "csrvctl2" "hlthcvr1"
    ## [175] "aspirin"  "hombpchk" "homrgchk" "wherebp"  "sharebp"  "wtchsalt"
    ## [181] "dradvise" "indortan" "numburn3" "sunprtct" "wkdayout" "wkendout"
    ## [187] "cimemlos" "cdhouse"  "cdassist" "cdhelp"   "cdsocial" "cddiscus"
    ## [193] "caregiv1" "crgvrel3" "crgvlng1" "crgvhrs1" "crgvprb3" "crgvalzd"
    ## [199] "crgvper1" "crgvhou1" "crgvexpt" "acedeprs" "acedrink" "acedrugs"
    ## [205] "aceprisn" "acedivrc" "acepunch" "acehurt1" "aceswear" "acetouch"
    ## [211] "acetthem" "acehvsex" "pfpprvn3" "typcntr8" "nobcuse7" "asbialch"
    ## [217] "asbidrnk" "asbibing" "asbiadvc" "asbirduc" "marijan1" "usemrjn2"
    ## [223] "rsnmrjn1" "foodstmp" "birthsex" "somale"   "sofemale" "trnsgndr"
    ## [229] "rcsgendr" "rcsrltn2" "casthdx2" "casthno2" "qstver"   "qstlang" 
    ## [235] "metstat"  "urbstat"  "mscode"   "ststr"    "strwt"    "rawrake" 
    ## [241] "wt2rake"  "imprace"  "chispnc"  "crace1"   "cprace"   "cllcpwt" 
    ## [247] "dualuse"  "dualcor"  "llcpwt2"  "llcpwt"   "rfhlth"   "phys14d" 
    ## [253] "ment14d"  "hcvu651"  "rfhype5"  "cholch2"  "rfchol2"  "michd"   
    ## [259] "ltasth1"  "casthm1"  "asthms1"  "drdxar2"  "lmtact2"  "lmtwrk2" 
    ## [265] "prace1"   "mrace1"   "hispanc"  "race"     "raceg21"  "racegr3" 
    ## [271] "race_g1"  "sex"      "ageg5yr"  "age65yr"  "age80"    "age_g"   
    ## [277] "htin4"    "htm4"     "wtkg3"    "bmi5"     "bmi5cat"  "rfbmi5"  
    ## [283] "chldcnt"  "educag"   "incomg"   "smoker3"  "rfsmok3"  "drnkany5"
    ## [289] "drocdy3"  "rfbing5"  "drnkwk1"  "rfdrhv7"  "totinda"  "metvl11" 
    ## [295] "metvl21"  "maxvo21"  "fc601"    "actin12"  "actin22"  "padur1"  
    ## [301] "padur2"   "pafreq1"  "pafreq2"  "minac11"  "minac21"  "strfreq" 
    ## [307] "pamiss2"  "pamin12"  "pamin22"  "pa2min"   "pavig12"  "pavig22" 
    ## [313] "pa2vigm"  "pacat2"   "paindx2"  "pa150r3"  "pa300r3"  "pa30022" 
    ## [319] "pastrng"  "parec2"   "pastae2"  "ftjuda2"  "frutda2"  "grenda1" 
    ## [325] "frnchda"  "potada1"  "vegeda2"  "misfrt1"  "misveg1"  "frtres1" 
    ## [331] "vegres1"  "frutsu1"  "vegesu1"  "frtlt1a"  "veglt1a"  "frt16a"  
    ## [337] "veg23a"   "fruite1"  "vegete1"  "flshot7"  "pneumo3"  "aidtst4"

per the environment and the code above, we can see that there are
418,268 observations and 342 variables. Upon first glance, the names do
not seem that intuitive, so we will have to refer to the codebook at the
following link:
<https://www.cdc.gov/brfss/annual_data/2019/pdf/codebook19_llcp-v2-508.HTML>

Based, off of this codebook and our interest in vaccines, I will peruse
the data and select: -a participant id  
\-location: it would be great to focus on new york  
\-demographic variables -variables pertaining to flu vaccine usage

#### poking through the codebook and the clean names

These are the variables of interest; I will refine and clean tomorrow
11/19/2020

“state”  
“fmonth”  
“idate”  
“imonth” “iday” “iyear”  
“seqno” “psu” “statere1” “ladult1” “colgsex” “numadult” “landsex”
“nummen” “numwomen” “respslct” “cadult1” “cellsex” “pvtresd3”
“cstate1” “hhadult” “sexvar” “genhlth” “physhlth” “menthlth”
“poorhlth” “hlthpln1 = health insurance coverage” “persdoc2”
“medcost”  
“checkup1” “chcscncr” “chcocncr” “chccopd2” “addepev3” “chckdny2”
“marital” “educa” “renthom1” “numhhol3” “numphon3” “veteran3”
“employ1” “children” “income2”  
“weight2” “pregnant”

“flushot7” “flshtmy3” “tetanus1” “pneuvac4”

“foodstmp” “birthsex” “somale” “sofemale” “trnsgndr” “rcsgendr”
“rcsrltn2” “casthdx2” “imprace” “chispnc” “crace1” “cprace” “cllcpwt”

“prace1” “mrace1” “hispanc” “race” “raceg21” “racegr3” “race\_g1” “sex”
“ageg5yr” “age65yr” “age80”  
“age\_g” “htin4” “htm4” “wtkg3” “bmi5” “bmi5cat” “rfbmi5” “chldcnt”
“educag” “incomg” “smoker3”
