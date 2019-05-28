/*--------------------------------------------------------------------------------------*

   *******************************************************
   *** Copyright 2015, Rho, Inc.  All rights reserved. ***
   *******************************************************

   MACRO:      CatChk.sas

   PURPOSE:    Create displays to facilitate discovery of patterns/inconsistencies in categorical data

   ARGUMENTS:  


   RETURNS:    Figures:
                  ???      => ???


   CALLS:      %RhoTablesSetup (project macro)


   PROGRAM HISTORY:

   DATE        PROGRAMMER        DESCRIPTION
   ---------   ---------------   ----------------------------------------------------
   25MAR2015   Brett Jepson      Create macro
*--------------------------------------------------------------------------------------*/

%macro catchk(outpath,dset,subset,var,grpvar1,grpvar2,grpvar3,datevar,interval,study,minnum,test,alphabar=0.1,alphaheat=0.25);

ods listing close;

%if &datevar. ^= %then %do;
   %if &grpvar3. ^= %then %do;
      %let grpsort = &grpvar3. &grpvar2. &grpvar1.;
      %let grpsort_first = &grpvar3. &grpvar2.;
      %let grpsort_2last = &grpvar2.;
      %let grppage_high = &grpvar3.;
      %let grppage_low  = &grpvar2.;
      %let grppan_var = date;
      %let grpsort_last = &grpvar1.;
      %let grpsort_notmiss = and ^missing(&grpvar1.) and ^missing(&grpvar2.) and ^missing(&grpvar3.);
   %end;
   %else %if &grpvar2. ^= %then %do;
      %let grpsort = &grpvar2. &grpvar1.;
      %let grpsort_first = &grpvar2.;
      %let grpsort_2last = &grpvar2.;
      %let grppage_high = ;
      %let grppage_low  = &grpvar2.;
      %let grppan_var = date;
      %let grpsort_last = &grpvar1.;
      %let grpsort_notmiss = and ^missing(&grpvar1.) and ^missing(&grpvar2.);
   %end;
   %else %if &grpvar1. ^= %then %do;
      %let grpsort = &grpvar1.;
      %let grpsort_first = ;
      %let grpsort_2last = ;
      %let grppage_high = ;
      %let grppage_low  = ;
      %let grppan_var = date;
      %let grpsort_last = &grpvar1.;
      %let grpsort_notmiss = and ^missing(&grpvar1.);
   %end;
%end;
%else %if &datevar. = %then %do;
   %if &grpvar3. ^= %then %do;
      %let grpsort = &grpvar3. &grpvar2. &grpvar1.;
      %let grpsort_first = &grpvar3.;
      %let grpsort_2last = &grpvar3.;
      %let grppage_high = ;
      %let grppage_low  = &grpvar3.;
      %let grppan_var = &grpvar2.;
      %let grpsort_last = &grpvar1.;
      %let grpsort_notmiss = and ^missing(&grpvar1.) and ^missing(&grpvar3.);
   %end;
   %else %if &grpvar2. ^= %then %do;
      %let grpsort = &grpvar2. &grpvar1.;
      %let grpsort_first = ;
      %let grpsort_2last = ;
      %let grppage_high = ;
      %let grppage_low  = ;
      %let grppan_var = &grpvar2.;
      %let grpsort_last = &grpvar1.;
      %let grpsort_notmiss = and ^missing(&grpvar1.);
   %end;
   %else %if &grpvar1. ^= %then %do;
      %let grpsort = &grpvar1.;
      %let grpsort_first = ;
      %let grpsort_2last = ;
      %let grppage_high = ;
      %let grppage_low  = ;
      %let grppan_var = ;
      %let grpsort_last = &grpvar1.;
      %let grpsort_notmiss = and ^missing(&grpvar1.);
   %end;
%end;

%if &datevar. = %then %let interval = ;

proc sort data = &dset. out = dset1 (keep = &grpvar1. &grpvar2. &grpvar3. &datevar. &var.);
   by &grpvar3. &grpvar2. &grpvar1.;
   &subset.;
run;

data dset2;
   set dset1;
   by &grpvar3. &grpvar2. &grpvar1.;

   *Setting calendar time frames for collections;
   %if &datevar. ^= %then %do;
    if ^missing(&datevar.) then do;
      vyear = year(&datevar.);
      %if &interval. ^= %then group = ceil(month(&datevar.)/(12/&interval.));;

      %if &interval. = 4 %then %do;
         pangroup = compress(put(vyear,4.)) ||', Quarter ' || compress(put(group,2.));
      %end;
      %else %if &interval. = 2 %then %do;
         pangroup = compress(put(vyear,4.)) ||', Half ' || compress(put(group,2.));
      %end;
      %else %if &interval. = 1 %then %do;
         pangroup = compress(put(vyear,4.));
       %end;
      %else %if &interval. = 12 %then %do;
         pangroup = compress(put(vyear,4.)) ||', Month ' || compress(put(month(&datevar.),z2.)) || ' '  || compress(put(&datevar.,monname.));
      %end;
      %else %if &interval. = 0 or &interval. = %then %do;
         pangroup = 'Overall';
      %end;
      %else %if &interval. ^= %then %do;
         pangroup = compress(put(vyear,4.)) ||', Period ' || compress(put(group,2.));
      %end;
    end;
   %end;
   %else %if &grpvar2. ^=  %then %do;
         pangroup = &grpvar2.;
   %end;
   %else %if &grpvar2. =  %then %do;
         pangroup = 'All';
   %end;

run;

proc sql noprint;
   select count(distinct pangroup) into :numgrp from dset2;
   select count(distinct &var.) into :numsections from dset2;
quit;


*Categoricl variable percentages;
proc sort data = dset2;
    by &grpsort_first. pangroup &grpsort_last.;
run;

ods output crosstabfreqs = freqs1_ (keep = rowpercent pangroup &grpsort_first. &grpsort_last. frequency &var. where=(^missing(&grpsort_last.) and ^missing(&var.)));
proc freq data = dset2;
     by &grpsort_first. pangroup;
      table &grpsort_last.*&var.;
run;

ods output crosstabfreqs = freqs1_all_ (keep = rowpercent pangroup &var. &grpsort_first. frequency where=(^missing(pangroup) and ^missing(&var.)));
proc freq data = dset2;
     by &grpsort_first. ;
      table pangroup*&var.;
run;

proc sort data = freqs1_all_;
     by &grpsort_first. pangroup &var.;
run;

proc sort data = freqs1_;
     by &grpsort_first. pangroup &var.;
run;


data freqs1_all;
   merge freqs1_ freqs1_all_ (rename=(frequency=total) drop=rowpercent);
     by &grpsort_first. pangroup &var.;
   panel_frq = frequency;
   inBar = 1;
   output;
   panel_frq = total - frequency;
   inBar = 0;
   output;
run;


proc sort data = freqs1_;
   by pangroup &grpsort_first. &grpsort_last. &var.;
run;

proc sort data = freqs1_ out = freqs1_pangroup nodupkey;
   by pangroup &grpsort_first. &grpsort_last.;
run;

data shell;
   set freqs1_pangroup (keep=pangroup &grpsort_first. &grpsort_last. &var.);
   by pangroup &grpsort_first. &grpsort_last.;
   rowpercent1 = 0;
   if first.&grpsort_last. then output;
run;

data freqs1;
   merge freqs1_ (in=b) shell (in=a);
   by pangroup &grpsort_first. &grpsort_last. &var.;
   if a or b;
   if missing(rowpercent) then rowpercent = 0;
run;

proc sort data = freqs1;
      by &grpsort_first. pangroup &grpsort_last.;
run;

ods output crosstabfreqs = freqs2 (keep = pangroup &grpsort_first. &grpsort_last. frequency where=(^missing(pangroup) and ^missing(&grpsort_last.)));
proc freq data = dset2;
   %if &grpsort_first. ^= %then by &grpsort_first.;;
   table &grpsort_last.*pangroup;
run;

*Categorical variable percentages;

proc sort data = dset2;
   by &grpsort_first. pangroup &grpsort_last. &var.;
run;

ods output crosstabfreqs = freqs3 (keep = rowpercent pangroup &var. &grpsort_first. &grpsort_last. frequency);
   proc freq data = dset2;
   by &grpsort_first. pangroup;
   table &grpsort_last.*&var.;
run;

*Annotations;
data freqs2;
   set freqs2;
   grpvar_total = Frequency;
   if pangroup ne ' ' &grpsort_notmiss.;
   keep pangroup &grpsort_first. &grpsort_last. grpvar_total;
run;

data freqs;
      merge freqs1 (where=(^missing(&var.) &grpsort_notmiss.)) 
    freqs1 (where=(missing(&var._temp) &grpsort_notmiss.) rename=(frequency=total &var.=&var._temp) keep = pangroup frequency &var. &grpsort_first. &grpsort_last.)
    freqs3 (where=(^missing(&var.) &grpsort_notmiss.) rename=(rowpercent=rowpercent_&var.))
         ;
      by &grpsort_first. pangroup &grpsort_last.;
run;

proc sort data = freqs2;
    by &grpsort_first. pangroup &grpsort_last.;
run;

data plot;
   merge freqs freqs2;
    by &grpsort_first. pangroup &grpsort_last.;
      %if &minnum. ^= %then if grpvar_total > &minnum.;;
run;

proc sort data = plot (keep=pangroup &grpsort_first. &grpsort_last.) out = plot_pangroup nodupkey;
   by pangroup &grpsort_first. &grpsort_last.;
run;

proc sort data = plot out = plot_pages;
   by &grpsort_first. pangroup &grpsort_last.;
run;

%if &grppage_low. ^= %then %do;
data _null_;
   set plot_pages (keep=&grpsort_first.) end=eof;
   by &grppage_high. &grppage_low.;
   retain groupcount .;
   if last.&grppage_low. then do;
      groupcount + 1;
      call symputx ("group_first_"||compress(groupcount),&grppage_low.);
      %if &grppage_high. ^= %then %do;
      call symputx ("group_second_"||compress(groupcount),&grppage_high.);
      %end;
      if eof then do;
         call symputx ("num1groups",compress(groupcount));
      end;
      output;
   end;
run;
%end;


proc sort data = plot out = plot_final1;
   by pangroup &grpsort_first. &grpsort_last. &var.;
run;

*Chi-square Tests;
*Compare to all within each panel;
proc sort data = freqs1_all;
   by pangroup &grpsort_first. &grpsort_last. &var.;
run;

ods listing;
ods output 
%if %upcase(&test.) = CHISQ or &test. =  %then chisq = test_all1 (keep=pangroup &grpsort_first. &grpsort_last. statistic prob where=(statistic='Chi-Square'));
%if %upcase(&test.) = FISHER %then fishersexact = test_all1 (keep=pangroup &grpsort_first. &grpsort_last. nvalue1 name1 where=(name1='XP2_FISH'));
;
proc freq data = freqs1_all;
   by pangroup &grpsort_first. &grpsort_last.;
   weight panel_frq/zeros;
   table &var.*inBar / 
      %if %upcase(&test.) = CHISQ or &test. =  %then chisq;
      %if %upcase(&test.) = FISHER %then fisher;
;
run;
ods listing close;

data _null_;
   set plot_final1 end=eof;
   if eof then do;
      call symputx ('type',vtype(&var.));
   end;
run;

*Overall bar;
**Total in panel overall;
proc means data = freqs1_all_ sum;
   by pangroup;
   var frequency;
   output out = bar_total sum = grpvar_total;
run;

data plot_overall_bar;
   merge freqs1_all_
      %if &type. = N %then (rename=(&var.=&var.1));
      bar_total (drop=_type_ _freq_);
   ;
   by pangroup;
   %if &type. = N %then &var. = compress(put(&var.1,best12.));;
   rowpercent2 = rowpercent;
   rowpercent4 = rowpercent;
   grpvar_total2 = grpvar_total;
   grpvar_total4 = grpvar_total;
   grpvar_total = .;
   percent2 = put(rowpercent,3.0);
   percent4 = put(rowpercent,3.0);
   &grpsort_last. = '_All_                         ';
run;

***********************Heat Map Code*******************************;
*Running this before any p-value conclusions so we can control for multiple comparisons;


/*%if %upcase(&heatmap.) = YES %then %do;*/
*heat map of pairwise comparisons;
proc sort data = freqs1 out = freqs1_heat;
   by &grpsort_last.;
run;

data freqs_heat bar_format (keep = &grpsort_last. numord);
   set freqs1_heat end=eof;
   retain numord 0;
   by &grpsort_last.;
   if first.&grpsort_last. then numord = numord + 1;
   if eof then do;
      call symputx ('numbar',numord);
   end;
   output freqs_heat;
   if first.&grpsort_last. then output bar_format;
run;

proc sort data = freqs_heat;
   by pangroup &grpsort_first. ;
run;

%do i = 1 %to &numbar.;
   %do j = &i. %to &numbar.;  
      %if &i. ^= &j. %then %do;

      ods output
%if %upcase(&test.) = CHISQ or &test. =  %then chisq = test_heat_&i._&j. (keep=pangroup &grpsort_first. statistic prob where=(statistic='Chi-Square'));
%if %upcase(&test.) = FISHER %then fishersexact = test_heat_&i._&j. (keep=pangroup &grpsort_first. nvalue1 name1 where=(name1='XP2_FISH'));
;
proc freq data = freqs_heat;
   where numord in (&i. &j.);
   by pangroup &grpsort_first. ;
   weight frequency/zeros;
   table &var.*&grpsort_last. / 
      %if %upcase(&test.) = CHISQ or &test. =  %then chisq;
      %if %upcase(&test.) = FISHER %then fisher;
   ;
run;



  %if %sysfunc(EXIST(test_heat_&i._&j.)) %then %do;
data test_heat_&i._&j.;
   set test_heat_&i._&j.
      %if %upcase(&test.) = CHISQ or &test. =  %then (drop=statistic);
      %if %upcase(&test.) = FISHER %then (rename=(nvalue1=prob) drop = name1);
   ;
   grp1 = &i.;
   grp2 = &j.;
   where ^missing(pangroup);
   keep pangroup &grpsort_first. prob: grp1 grp2;
run;

  %end;
  %end;
      %else %do;

data test_heat_&i._&j.;
   set freqs_heat;
   where numord = &i.;
   prob = 1;
   grp1 = &i.;
   grp2 = &j.;
   keep pangroup &grpsort_first. prob: grp1 grp2;
run;

proc sort data = test_heat_&i._&j. nodupkey;
   by pangroup &grpsort_first. ;
   where ^missing(pangroup);
run;
      %end; 
   %end;
%end;

data test_heat1;
   set
%do i = 1 %to &numbar.;
   %do j = &i. %to &numbar.; 
      %if %sysfunc(EXIST(test_heat_&i._&j.)) %then test_heat_&i._&j.;
   %end;
%end;
   ;
run;

proc sort data = test_heat1;
   by grp1;
run;

*Put all-p-values together for multiple comparisons adjustment;
data all_pvals;
   set test_all1 (in=a)
       test_heat1 (in=b where=(grp1^=grp2))
       ;
   if a then plot = 'Stacked';
   else if b then plot = 'Heat';
   %if %upcase(&test.) = FISHER %then %do;
      if a then prob = nvalue1;
   %end;
   raw_p = prob;
   test = _n_;
run;

proc sort data = all_pvals;
   by pangroup;
run; 

ods output pvalues = all_pvals_1;
proc multtest inpvalues=all_pvals holm;
   by pangroup;
run; 

data test_all
     test_heat2
   ;
   merge all_pvals_1 all_pvals;
   by pangroup test;
   prob = stepdownbonferroni;
   if plot = 'Stacked' then output test_all;
   else if plot = 'Heat' then output test_heat2;
run; 

data test_heat;
   set test_heat2
       test_heat1 (where=(grp1=grp2))
   ;
   if prob > &alphaheat. then prob_new = &alphaheat.;
   else prob_new = prob;
run;

proc sort data = test_heat;  
   by grp1 grp2;
run;

*Back to stacked bar;
data plot_final2;
   merge plot_final1 
      %if &type. = N %then (rename=(&var.=&var.1));
      test_all 
      %if %upcase(&test.) = CHISQ or &test. =  %then (drop=statistic);
      %if %upcase(&test.) = FISHER %then (drop = name1 nvalue1);
      ;
   by pangroup &grpsort_first. &grpsort_last.;

   %if &type. = N %then &var. = compress(put(&var.1,best12.));;
   rowpercent4 = rowpercent;
   grpvar_total4 =   grpvar_total;
   percent4 = put(rowpercent4,3.0);

   if .z < prob < &alphabar. then do;
      rowpercent2 = rowpercent;
         grpvar_total2 =  grpvar_total;
       grpvar_total = .;
       percent2 = put(rowpercent2,3.0);
   end;

run;

data plot_final;
   set plot_overall_bar
         plot_final2 
   ;
run;

proc template; 
   define style STYLES.MYSTYLE3;
   
   %do i = 1 %to &numsections. ;
      %if &i. = 1 %then style GraphData1 from GraphData1/color=black;; 
      %if &i. = 2 %then style GraphData2 from GraphData2/color=gold;; 
      %if &i. = 3 %then style GraphData3 from GraphData3/color=bilg;;
      %if &i. = 4 %then style GraphData4 from GraphData4/color=cream;;
      %if &i. = 5 %then style GraphData5 from GraphData5/color=bipb;; 
      %if &i. = 6 %then style GraphData6 from GraphData6/color=depk;;
      %if &i. = 7 %then style GraphData7 from GraphData7/color=vioy;;
      %if &i. = 8 %then style GraphData8 from GraphData8/color=delg;;
      %if &i. = 9 %then style GraphData9 from GraphData9/color=rose;; 
      %if &i. = 10 %then style GraphData10 from GraphData10/color=cyan;;
      %if &i. = 11 %then style GraphData11 from GraphData11/color=vigb;;

   %end;
end; 
run; 

*Setting number of columns based on number of groups;
data _null_;
%if &interval. = 1 or &interval. = 2 or &grpvar3. ^=  %then %do;
   %if 1 <= %sysfunc(floor(&numgrp.)) and %sysfunc(floor(&numgrp.)) <= 3         %then cols = &numgrp.;;
   %if 4 = %sysfunc(floor(&numgrp.))                                             %then cols = 2;;
   %if 5 <= %sysfunc(floor(&numgrp.)) and %sysfunc(floor(&numgrp.)) <= 9         %then cols = 3;;
   %if 10 <= %sysfunc(floor(&numgrp.)) and %sysfunc(floor(&numgrp.)) <= 16       %then cols = 4;;
%end;

%else %if &interval. = 4 %then %do;
   %if 1 <= %sysfunc(floor(&numgrp.)) and %sysfunc(floor(&numgrp.)) <= 3         %then cols = &numgrp.;;
   %if 4 = %sysfunc(floor(&numgrp.))                                             %then cols = 2;;
   %if 5 <= %sysfunc(floor(&numgrp.))                                            %then cols = 4;;
%end;

%else %if &interval. = 12 %then %do;
   %if 1 <= %sysfunc(floor(&numgrp.)) and %sysfunc(floor(&numgrp.)) <= 3         %then cols = &numgrp.;;
   %if 4 = %sysfunc(floor(&numgrp.))                                             %then cols = 2;;
   %if 5 <= %sysfunc(floor(&numgrp.)) and %sysfunc(floor(&numgrp.)) <= 12        %then cols = 4;;
   %if 13 <= %sysfunc(floor(&numgrp.))                                           %then cols = 4;;
%end; 
%else %do;
   %if 1 <= %sysfunc(floor(&numgrp.)) and %sysfunc(floor(&numgrp.)) <= 3         %then cols = &numgrp.;;
   %if 4 = %sysfunc(floor(&numgrp.))                                             %then cols = 2;;
   %if 5 <= %sysfunc(floor(&numgrp.)) and %sysfunc(floor(&numgrp.)) <= 12        %then cols = 4;;
   %if 13 <= %sysfunc(floor(&numgrp.))                                           %then cols = 4;;
%end;

%if &interval. = 1 %then %do;
   int = "By_Year";
%end;
%else %if &interval. = 2 %then %do;
   int = "By_Half_Year";
%end;
%else %if &interval. = 4 %then %do;
   int = "By_Quarter";
%end;
%else %if &interval. = 12 %then %do;
   int = "By_Month";
%end;

%else %if &grppan_var ^= date and &grppan_var ^= %then %do;
   int = "By_&grppan_var.";
%end;


%else %if &grppan_var = or &interval. = 0 %then %do;
   int = "Overall";
%end;
%else %if &grppan_var = date and &interval. ^=  %then %do;
   int = "By_Period";
%end;
   int_title = tranwrd(int,'_',' ');
    call symputx('int_title',int_title);
    call symputx('int',int);
    call symputx('cols',cols);
run;

options orientation = landscape gstyle;

*Start multiple plots;
%if &grpsort_first. ^=  %then %do i = 1 %to &num1groups.;
proc sort data = plot_final out = plot_&i.;
   %if &grpsort_first. = &grpvar3. &grpvar2. and &grpvar3. ^=  %then %do;
      where &grppage_low. in ("&&group_first_&i." "") and &grppage_high. in ("&&group_second_&i." "");
   %end;

   %else %do;
      where &grppage_low. in ("&&group_first_&i." "");
   %end;
   by &var.;
run;

ods listing style=mystyle3 gpath= "&outpath." image_dpi = 200;
ods graphics / height= 7.5in width= 9.5in reset=index imagefmt=png
%if &grppage_high. ^=  %then %do;
   imagename="CatChk_&study._&var._&grpvar3._&grpvar2._&grpvar1._&int._&&group_second_&i.._&&group_first_&i." 
%end;
%else %if &grppage_low. ^=  %then %do;
   imagename="CatChk_&study._&var._&grpvar3._&grpvar2._&grpvar1._&int._&&group_first_&i.." 
%end;
%else %if &grpsort_first. = &grpvar2. %then %do;
   imagename="CatChk_&study._&var._&grpvar2._&grpvar1._&int._&&group_second_&i.._&&group_first_&i.." 
%end;
;

proc sgpanel data = plot_&i.;

   panelby pangroup / columns = &cols. novarname colheaderpos=top onepanel spacing = 5 ;

   vbar &grpsort_last. / group=&var. response=rowpercent4 datalabel=grpvar_total4 /*datalabelfitpolicy=rotate*/ transparency = 0.7;
   vbar &grpsort_last. / group=&var. response=rowpercent2 datalabel=grpvar_total2 /*datalabelfitpolicy=rotate*/;

   colaxis label = "&grpsort_last.";

   rowaxis label = 'Percent' grid offsetmax=0.2 min = 0 max = 100;

   footnote ' ';
   %if &minnum. > 0 %then %do;
   footnote j=l "Note: Bars with <= &minnum. observations are not shown.";
   %end;
   %if %upcase(&test.) = CHISQ or &test. =  %then footnote2 j=l "Darkened bars (except the '_All_' bar) are significant by the Chi-square homogeneity test (p<&alphabar.) comparing an individual bar to the rest of the panel";;
   %if %upcase(&test.) = FISHER %then footnote2 j=l "Darkened bars (except the '_All_' bar) are significant by the Fisher's exact test (p<&alphabar.) comparing an individual bar to the rest of the panel";;

   title "&study.";
   title2 "Categorical Check: &var.";
   %if &grppage_high. ^= %then %do;
      title3 "By &grppage_high. and &grppage_low.";
   %end;
   %else %if &grppage_low. ^= %then %do;
      title3 "By &grppage_low.";
   %end;

   title4 "Paneled &int_title.";
   title5 "&subset. ";
   %if &grppage_high. ^= %then %do;
      title6 "&grppage_high.: &&group_second_&i.., &grppage_low.: &&group_first_&i..";
   %end;
   %else %if &grppage_low. ^= %then %do;
      title6 "&grppage_low.: &&group_first_&i..";
   %end;

run;
ods graphics off;

%end;

*Start single plot;
%if &grpsort_first. =  %then %do;
ods listing style=mystyle3 gpath= "&outpath." image_dpi = 200;
ods graphics / height= 7.5in width= 9.5in reset=index imagefmt=png
%if &grpvar2. ^= %then %do;
   imagename="CatChk_&study._&var._&grpvar2._&grpvar1._&int." 
%end;
%else %if &grpvar1. ^= %then %do;
   imagename="CatChk_&study._&var._&grpvar1._&int." 
%end;
;

proc sgpanel data = plot_final;

   panelby pangroup / columns = &cols. novarname colheaderpos=top onepanel spacing = 5 ;

   vbar &grpsort_last. / group=&var. response=rowpercent2 datalabel=grpvar_total2 /*datalabelfitpolicy=rotate*/;
   vbar &grpsort_last. / group=&var. response=rowpercent4 datalabel=grpvar_total4 /*datalabelfitpolicy=rotate*/ transparency = 0.7;

   colaxis label = "&grpsort_last.";

   rowaxis label = 'Percent' grid offsetmax=0.2 min = 0 max = 100;

   footnote ' ';
   %if &minnum. > 0 %then %do;
   footnote j=l "Note: Bars with <= &minnum. observations are not shown.";
   %end;
   %if %upcase(&test.) = CHISQ or &test. =  %then footnote2 j=l "Darkened bars (except the '_All_' bar) are significant by the Chi-square homogeneity test (p<&alphabar.)"||
         " comparing an individual bar to the rest of the panel";;
   %if %upcase(&test.) = FISHER %then footnote2 j=l "Darkened bars (except the '_All_' bar) are significant by the Fisher's exact test (p<&alphabar.)"||
         " comparing an individual bar to the rest of the panel";;
   title "&study.";
   title2 "Categorical Check: &var.";
   %if &grppage_high. ^= %then %do;
      title3 "By &grppage_high. and &grppage_low.";
   %end;
   %else %if &grppage_low. ^= %then %do;
      title3 "By &grppage_low.";
   %end;

   title4 "Paneled &int_title.";
   title5 "&subset. ";

   %if &grppage_high. ^= %then %do;
      title6 "&grppage_high.: &&group_second_&i.., &grppage_low.: &&group_first_&i..";
   %end;
   %else %if &grppage_low. ^= %then %do;
      title6 "&grppage_low.: &&group_first_&i..";
   %end;

run;
ods graphics off;
%end;

%if &grpsort_first. ^=  %then %do i = 1 %to &num1groups.;
proc sort data = plot_final out = plot_&i.;
   %if &grpsort_first. = &grpvar3. &grpvar2. and &grpvar3. ^=  %then %do;
      where &grppage_low. in ("&&group_first_&i." "") and &grppage_high. in ("&&group_second_&i." "");
   %end;

   %else %do;
      where &grppage_low. in ("&&group_first_&i." "");
   %end;
   by &var.;
run;

ods listing style=mystyle3 gpath= "&outpath." image_dpi = 200;
ods graphics / height= 7.5in width= 9.5in reset=index imagefmt=png
%if &grppage_high. ^=  %then %do;
   imagename="CatChk_&study._&var._&grpvar3._&grpvar2._&grpvar1._&int._&&group_second_&i.._&&group_first_&i." 
%end;
%else %if &grppage_low. ^=  %then %do;
   imagename="CatChk_&study._&var._&grpvar3._&grpvar2._&grpvar1._&int._&&group_first_&i.." 
%end;
%else %if &grpsort_first. = &grpvar2. %then %do;
   imagename="CatChk_&study._&var._&grpvar2._&grpvar1._&int._&&group_second_&i.._&&group_first_&i.." 
%end;
;
proc sgpanel data = plot_&i.;

   panelby pangroup / columns = &cols. novarname colheaderpos=top onepanel spacing = 5 ;

   vbar &grpsort_last. / group=&var. response=rowpercent4 datalabel=grpvar_total4 /*datalabelfitpolicy=rotate*/ transparency = 0.7;
   vbar &grpsort_last. / group=&var. response=rowpercent2 datalabel=grpvar_total2 /*datalabelfitpolicy=rotate*/;

   colaxis label = "&grpsort_last.";

   rowaxis label = 'Percent' grid offsetmax=0.2 min = 0 max = 100;

   footnote ' ';
   %if &minnum. > 0 %then %do;
   footnote j=l "Note: Bars with <= &minnum. observations are not shown.";
   %end;
   %if %upcase(&test.) = CHISQ or &test. =  %then footnote2 j=l "Darkened bars (except the '_All_' bar) are significant by the Chi-square homogeneity test (p<&alphabar.)"||
         " comparing an individual bar to the rest of the panel";;
   %if %upcase(&test.) = FISHER %then footnote2 j=l "Darkened bars (except the '_All_' bar) are significant by the Fisher's exact test (p<&alphabar.)"||
         " comparing an individual bar to the rest of the panel";;

   title "&study.";
   title2 "Categorical Check: &var.";
   %if &grppage_high. ^= %then %do;
      title3 "By &grppage_high. and &grppage_low.";
   %end;
   %else %if &grppage_low. ^= %then %do;
      title3 "By &grppage_low.";
   %end;

   title4 "Paneled &int_title.";
   title5 "&subset. ";
   %if &grppage_high. ^= %then %do;
      title6 "&grppage_high.: &&group_second_&i.., &grppage_low.: &&group_first_&i..";
   %end;
   %else %if &grppage_low. ^= %then %do;
      title6 "&grppage_low.: &&group_first_&i..";
   %end;

run;
ods graphics off;

%end;

***********************Histogram by &grpvar1. Code*******************************;

***Single Plot****;
%if &grpsort_first. =  %then %do;
ods listing style=mystyle3 gpath= "&outpath." image_dpi = 200;
ods graphics / height= 7.5in width= 9.5in reset=index imagefmt=png
%if &grpvar2. ^= %then %do;
   imagename="CatChk2_&study._&var._&grpvar2._&grpvar1._&int." 
%end;
%else %if &grpvar1. ^= %then %do;
   imagename="CatChk2_&study._&var._&grpvar1._&int." 
%end;
;

proc sgpanel data = plot_final noautolegend;

   panelby &grpsort_last. / novarname colheaderpos=top onepanel spacing = 5 ;

   vbar &var. / group=&var. response=rowpercent2 datalabel=percent2;
   vbar &var. / group=&var. response=rowpercent4 datalabel=percent4 transparency = 0.7;

   colaxis label = "&var.";

   rowaxis label = 'Percent' grid offsetmax=0.2 min = 0;

   footnote ' ';
   %if &minnum. > 0 %then %do;
   footnote j=l "Note: Bars with <= &minnum. observations are not shown.";
   %end;
   %if %upcase(&test.) = CHISQ or &test. =  %then footnote2 j=l "Darkened bars (except the '_All_' bar) are significant by the Chi-square homogeneity test (p<&alphabar.)"||
            " comparing an individual panel to the remaining panels";;
   %if %upcase(&test.) = FISHER %then footnote2 j=l "Darkened bars (except the '_All_' bar) are significant by the Fisher's exact test (p<&alphabar.)"||
            " comparing an individual panel to the remaining panels";;
   title "&study.";
   title2 "Categorical Check: &var.";
   %if &grppage_high. ^= %then %do;
      title3 "By &grppage_high. and &grppage_low.";
   %end;
   %else %if &grppage_low. ^= %then %do;
      title3 "By &grppage_low.";
   %end;

   title4 "Paneled by &grpsort_last.";
   title5 "&subset. ";

   %if &grppage_high. ^= %then %do;
      title6 "&grppage_high.: &&group_second_&i.., &grppage_low.: &&group_first_&i..";
   %end;
   %else %if &grppage_low. ^= %then %do;
      title6 "&grppage_low.: &&group_first_&i..";
   %end;


run;
ods graphics off;
%end;

***Mutliple Plots****;

%if &grpsort_first. ^=  %then %do i = 1 %to &num1groups.;
ods listing style=mystyle3 gpath= "&outpath." image_dpi = 200;
ods graphics / height= 7.5in width= 9.5in reset=index imagefmt=png
%if &grppage_high. ^=  %then %do;
   imagename="CatChk2_&study._&var._&grpvar3._&grpvar2._&grpvar1._&int._&&group_second_&i.._&&group_first_&i." 
%end;
%else %if &grppage_low. ^=  %then %do;
   imagename="CatChk2_&study._&var._&grpvar3._&grpvar2._&grpvar1._&int._&&group_first_&i.." 
%end;
%else %if &grpsort_first. = &grpvar2. %then %do;
   imagename="CatChk2_&study._&var._&grpvar2._&grpvar1._&int._&&group_second_&i.._&&group_first_&i.." 
%end;
;

proc sgpanel data = plot_&i. noautolegend;

   panelby &grpsort_last. / novarname colheaderpos=top onepanel spacing = 5 ;

   vbar &var. / group=&var. response=rowpercent2 datalabel=percent2;
   vbar &var. / group=&var. response=rowpercent4 datalabel=percent4 transparency = 0.7;

   colaxis label = "&var.";

   rowaxis label = 'Percent' grid offsetmax=0.2 min = 0 max = 100;

   footnote ' ';
   %if &minnum. > 0 %then %do;
   footnote j=l "Note: Bars with <= &minnum. observations are not shown.";
   %end;
   %if %upcase(&test.) = CHISQ or &test. =  %then footnote2 j=l "Darkened bars (except the '_All_' bar) are significant by the Chi-square homogeneity test (p<&alphabar.)"||
         " comparing an individual panel to the remaining panels";;
   %if %upcase(&test.) = FISHER %then footnote2 j=l "Darkened bars (except the '_All_' bar) are significant by the Fisher's exact test (p<&alphabar.)"||
         " comparing an individual panel to the remaining panels";;

   title "&study.";
   title2 "Categorical Check: &var.";
   %if &grppage_high. ^= %then %do;
      title3 "By &grppage_high. and &grppage_low.";
   %end;
   %else %if &grppage_low. ^= %then %do;
      title3 "By &grppage_low.";
   %end;

   title4 "Paneled by &grpsort_last.";
   title5 "&subset. ";
   %if &grppage_high. ^= %then %do;
      title6 "&grppage_high.: &&group_second_&i.., &grppage_low.: &&group_first_&i..";
   %end;
   %else %if &grppage_low. ^= %then %do;
      title6 "&grppage_low.: &&group_first_&i..";
   %end;

run;
ods graphics off;

%end;

***********************Histogram by &var. Code*******************************;

***Single Plot****;
%if &grpsort_first. =  %then %do;
ods listing style=mystyle3 gpath= "&outpath." image_dpi = 200;
ods graphics / height= 7.5in width= 9.5in reset=index imagefmt=png
%if &grpvar2. ^= %then %do;
   imagename="CatChk3_&study._&var._&grpvar2._&grpvar1._&int." 
%end;
%else %if &grpvar1. ^= %then %do;
   imagename="CatChk3_&study._&var._&grpvar1._&int." 
%end;
;

proc sgpanel data = plot_final noautolegend;

   panelby &var. / novarname colheaderpos=top onepanel spacing = 5 ;

   vbar &grpsort_last. / group=&grpsort_last. response=rowpercent2 datalabel=percent2;
   vbar &grpsort_last. / group=&grpsort_last. response=rowpercent4 datalabel=percent4 transparency = 0.7;

   colaxis label = "&grpsort_last.";

   rowaxis label = 'Percent' grid offsetmax=0.2 min = 0;

   footnote ' ';
   %if &minnum. > 0 %then %do;
   footnote j=l "Note: Bars with <= &minnum. observations are not shown.";
   %end;
   %if %upcase(&test.) = CHISQ or &test. =  %then footnote2 j=l "Darkened bars (except the '_All_' bar) are significant by the Chi-square homogeneity test (p<&alphabar.)"||
            " comparing an individual &grpsort_last. to the remaining values of &grpsort_last.";;
   %if %upcase(&test.) = FISHER %then footnote2 j=l "Darkened bars (except the '_All_' bar) are significant by the Fisher's exact test (p<&alphabar.)"||
            " comparing an individual &grpsort_last. to the remaining values of &grpsort_last.";;
   title "&study.";
   title2 "Categorical Check: &var.";
   %if &grppage_high. ^= %then %do;
      title3 "By &grppage_high. and &grppage_low.";
   %end;
   %else %if &grppage_low. ^= %then %do;
      title3 "By &grppage_low.";
   %end;

   title4 "Paneled by &var.";
   title5 "&subset. ";

   %if &grppage_high. ^= %then %do;
      title6 "&grppage_high.: &&group_second_&i.., &grppage_low.: &&group_first_&i..";
   %end;
   %else %if &grppage_low. ^= %then %do;
      title6 "&grppage_low.: &&group_first_&i..";
   %end;


run;
ods graphics off;
%end;

***Mutliple Plots****;

%if &grpsort_first. ^=  %then %do i = 1 %to &num1groups.;
ods listing style=mystyle3 gpath= "&outpath." image_dpi = 200;
ods graphics / height= 7.5in width= 9.5in reset=index imagefmt=png
%if &grppage_high. ^=  %then %do;
   imagename="CatChk3_&study._&var._&grpvar3._&grpvar2._&grpvar1._&int._&&group_second_&i.._&&group_first_&i." 
%end;
%else %if &grppage_low. ^=  %then %do;
   imagename="CatChk3_&study._&var._&grpvar3._&grpvar2._&grpvar1._&int._&&group_first_&i.." 
%end;
%else %if &grpsort_first. = &grpvar2. %then %do;
   imagename="CatChk3_&study._&var._&grpvar2._&grpvar1._&int._&&group_second_&i.._&&group_first_&i.." 
%end;
;

proc sgpanel data = plot_&i. noautolegend;

   panelby &var. / novarname colheaderpos=top onepanel spacing = 5 ;

   vbar &grpsort_last. / group=&grpsort_last. response=rowpercent2 datalabel=percent2;
   vbar &grpsort_last. / group=&grpsort_last. response=rowpercent4 datalabel=percent4 transparency = 0.7;

   colaxis label = "&grpsort_last.";

   rowaxis label = 'Percent' grid offsetmax=0.2 min = 0 max = 100;

   footnote ' ';
   %if &minnum. > 0 %then %do;
   footnote j=l "Note: Bars with <= &minnum. observations are not shown.";
   %end;
   %if %upcase(&test.) = CHISQ or &test. =  %then footnote2 j=l "Darkened bars (except the '_All_' bar) are significant by the Chi-square homogeneity test (p<&alphabar.)"||
         " comparing an individual &grpsort_last. to the remaining values of &grpsort_last.";;
   %if %upcase(&test.) = FISHER %then footnote2 j=l "Darkened bars (except the '_All_' bar) are significant by the Fisher's exact test (p<&alphabar.)"||
         " comparing an individual &grpsort_last. to the remaining values of &grpsort_last.";;

   title "&study.";
   title2 "Categorical Check: &var.";
   %if &grppage_high. ^= %then %do;
      title3 "By &grppage_high. and &grppage_low.";
   %end;
   %else %if &grppage_low. ^= %then %do;
      title3 "By &grppage_low.";
   %end;

   title4 "Paneled by &var.";
   title5 "&subset. ";
   %if &grppage_high. ^= %then %do;
      title6 "&grppage_high.: &&group_second_&i.., &grppage_low.: &&group_first_&i..";
   %end;
   %else %if &grppage_low. ^= %then %do;
      title6 "&grppage_low.: &&group_first_&i..";
   %end;

run;
ods graphics off;

%end;




****************************************************************************************************************;
*last p-value;
data heatmap1 (drop=grp1_ &grpsort_last.);
   merge bar_format (rename=(numord=grp1_)) test_heat (in=a rename=(grp1=grp1_) drop=&grpsort_last.) ;
   by grp1_;
   if a;
   grp1 = &grpsort_last.;
run;

proc sort data = heatmap1;
   by grp2;
run;

data heatmap2 (drop=grp2_ &grpsort_last.);
   merge bar_format (rename=(numord=grp2_)) heatmap1 (in=a rename=(grp2=grp2_)) ;
   by grp2_;
   if a;
   grp2 = &grpsort_last.;
run;

proc sort data = heatmap2;
   by &grpsort_first.  pangroup;
run;

data heatmap3;
   set heatmap2 end = eof;
   by &grpsort_first. pangroup;
   retain count 0 ;
   if first.pangroup then count = count + 1;
   if eof then do;
      call symputx ('figvar',count);
   end;
run;

%let rows = %sysfunc(ceil(&figvar./&cols.));

data heatmap;
   set heatmap3;
   by &grpsort_first. pangroup;
   %do i = 1 %to &figvar.;
      if count = &i. then grp1_&i. = grp1;
      if first.pangroup and count = &i. then do;
         call symputx ("title_&i.",pangroup);
      end;
      if grp1 = grp2 then do;
         grp3_&i. = grp1;
      end;
   %end;
run;

proc sort data = heatmap;
   by grp1 grp2 &grpsort_first. pangroup ;
run;

%if &grpsort_first. ^=  %then %do k = 1 %to &num1groups.;
proc sort data = heatmap out = heatmap_&k.;
   %if &grpsort_first. = &grpvar3. &grpvar2. and &grpvar3. ^=  %then %do;
      where &grppage_low. in ("&&group_first_&k." "") and &grppage_high. in ("&&group_second_&k." "");
   %end;

   %else %do;
      where &grppage_low. in ("&&group_first_&k." "");
   %end;
   by pangroup;
run;

*Beginning of Heat Map;
proc template;
  define statgraph heatmapparm;
    begingraph/ designwidth=15in designheight=8in;

    rangeattrmap name="densityrange" ;
      range 0 - 0.05     / rangecolormodel=(darkred red) ;
      range 0.05 < - &alphaheat. / rangecolormodel=(red white) ;
      range &alphaheat. < - MAX  / rangecolormodel=(white) ;
    endrangeattrmap ;
   rangeattrvar attrvar=rangevar var=prob_new attrmap="densityrange" ;

    rangeattrmap name="diagonal" ;
      range &alphaheat.     / rangecolormodel=(gray) ;
    endrangeattrmap ;
   rangeattrvar attrvar=rangevar2 var=prob_new attrmap="diagonal" ;


    layout lattice/rows = &rows. columns = &cols.;
    %do i = 1 %to &figvar.;
      layout overlay / xaxisopts=(label=" ") yaxisopts=(label=" ");
        entry "&&title_&i." / location = outside valign = top;
        heatmapparm x = grp1_&i. y = grp2 colorresponse = rangevar /  name = "heatmap_&i." outlineattrs = GraphData2(color=black pattern=solid thickness=5);
        continuouslegend "heatmap_&i." / orient = vertical location = outside /*valign = bottom*/;
        heatmapparm x = grp3_&i. y = grp2 colorresponse = rangevar2;
      endlayout;
    %end;
    endlayout;
    endgraph;
  end;
run;

options nodate nonumber;

%if &grpvar2. ^= %then                 ods pdf file = "&outpath.\CatChk_Heatmap_&study._&var._&grpvar2._&grpvar1._&int._&&group_first_&k...pdf";; 
%if &grpvar2. = and &grpvar1. ^= %then ods pdf file = "&outpath.\CatChk_Heatmap_&study._&var._&grpvar1._&int._&&group_first_&k...pdf";; 

   title "&study.";
   title2 "Categorical Check: Heat Map of Pairwise Comparisons of &var. distribution by &grpsort_last.";
   %if &grppage_high. ^= %then %do;
      title3 "By &grppage_high. and &grppage_low.";
   %end;
   %else %if &grppage_low. ^= %then %do;
      title3 "By &grppage_low.";
   %end;

   title4 "Paneled &int_title.";
   title5 "&subset. ";

   %if &grppage_high. ^= %then %do;
      title6 "&grppage_high.: &&group_second_&k.., &grppage_low.: &&group_first_&k..";
   %end;
   %else %if &grppage_low. ^= %then %do;
      title6 "&grppage_low.: &&group_first_&k..";
   %end;
   %if %upcase(&test.) = CHISQ or &test. =  %then footnote j=l "Note: Color gradient is based on the p-value from the Chi-square homogeneity test of the applicable"||
         " pairwise comparison.";;
   %if %upcase(&test.) = FISHER %then footnote j=l "Note: Color gradient is based on the p-value from the Fisher's exact test of the applicable pairwise comparison.";;
   
proc sgrender data=heatmap_&k. template=heatmapparm;
run;
/*ods graphics off;*/

ods pdf close;
%end;

%if &grpsort_first. =  %then %do ;
*Beginning of Heat Map;
proc template;
  define statgraph heatmapparm;
    begingraph/ designwidth=15in designheight=8in;

    rangeattrmap name="densityrange" ;
      range 0 - 0.05     / rangecolormodel=(darkred red) ;
      range 0.05 < - &alphaheat. / rangecolormodel=(red white) ;
      range &alphaheat. < - 1  / rangecolormodel=(white) ;
    endrangeattrmap ;
   rangeattrvar attrvar=rangevar var=prob_new attrmap="densityrange" ;

    rangeattrmap name="diagonal" ;
      range 0.14 - 0.15     / rangecolormodel=(gray) ;
    endrangeattrmap ;
   rangeattrvar attrvar=rangevar2 var=prob_new attrmap="diagonal" ;


    layout lattice/rows = &rows. columns = &cols.;
    %do i = 1 %to &figvar.;
      layout overlay / xaxisopts=(label=" ") yaxisopts=(label=" ");
        entry "&&title_&i." / location = outside valign = top;
        heatmapparm x = grp1_&i. y = grp2 colorresponse = rangevar /  name = "heatmap_&i." outlineattrs = GraphData2(color=black pattern=solid thickness=5);
        continuouslegend "heatmap_&i." / /*orient = vertical*/ location = outside valign = bottom;
        heatmapparm x = grp3_&i. y = grp2 colorresponse = rangevar2;
      endlayout;
    %end;
    endlayout;
    endgraph;
  end;
run;

%if &grpvar2. ^= %then                 ods pdf file = "&outpath.\CatChk_Heatmap_&study._&var._&grpvar2._&grpvar1._&int..pdf";; 
%if &grpvar2. = and &grpvar1. ^= %then ods pdf file = "&outpath.\CatChk_Heatmap_&study._&var._&grpvar1._&int..pdf";; 

   title "&study.";
   title2 "Categorical Check: Heat Map of Pairwise Comparisons of &var. distribution by &grpsort_last.";
   %if &grppage_high. ^= %then %do;
      title3 "By &grppage_high. and &grppage_low.";
   %end;
   %else %if &grppage_low. ^= %then %do;
      title3 "By &grppage_low.";
   %end;

   title4 "Paneled &int_title.";
   title5 "&subset. ";

   %if &grppage_high. ^= %then %do;
      title6 "&grppage_high.: &&group_second_&i.., &grppage_low.: &&group_first_&i..";
   %end;
   %else %if &grppage_low. ^= %then %do;
      title6 "&grppage_low.: &&group_first_&i..";
   %end;
   %if %upcase(&test.) = CHISQ or &test. =  %then footnote j=l "Note: Color gradient is based on the p-value from the Chi-square homogeneity test of the applicable"||
         " pairwise comparison.";;
   %if %upcase(&test.) = FISHER %then footnote j=l "Note: Color gradient is based on the p-value from the Fisher's exact test of the applicable pairwise comparison.";;

proc sgrender data=heatmap template=heatmapparm;
run;
/*ods graphics off;*/

ods pdf close;
%end;

/*%end;*/

/*proc datasets library=work;*/
/*   delete heatmap: chitest: bar_format dset: freqs: plot_: shell;*/
/*run;*/

%mend catchk;

