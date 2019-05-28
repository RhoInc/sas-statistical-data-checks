/*--------------------------------------------------------------------------------------*

   *******************************************************
   *** Copyright 2013, Rho, Inc.  All rights reserved. ***
   *******************************************************

   MACRO:      DigitPref.sas

   PURPOSE:    Create displays to facilitate discovery of digit preferences in recording continuous measurements

   ARGUMENTS:  


   RETURNS:    Figures:
                  ???      => ???


   CALLS:      %RhoTablesSetup (project macro)


   PROGRAM HISTORY:

   DATE        PROGRAMMER        DESCRIPTION
   ---------   ---------------   ----------------------------------------------------
   15NOV2013   Brett Jepson      Create macro
   19NOV2013   Kaitie Fernandez   Updated
*--------------------------------------------------------------------------------------*/

%macro digitpref(outpath,dset,subset,var,lastdig,grpvar1,grpvar2,grpvar3,datevar,interval,study,minnum);

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

proc format;
   value digval -1 = 'None'
                 0 = '0'
                 1 = '1'
                 2 = '2'
                 3 = '3'
                 4 = '4'
                 5 = '5'
                 6 = '6'
                 7 = '7'
                 8 = '8'
                 9 = '9'
   ;
run;

proc sort data = &dset. out = dset1 (keep = &grpvar1. &grpvar2. &grpvar3. &datevar. &var.);
   by &grpvar3. &grpvar2. &grpvar1.;
   &subset.;
run;

data _null_;
   set dset1 end=eof;
   if eof then do;
      call symputx ('type',vtype(&var.));
   end;
run;

data dset2 (drop=decplace);
   length &var._temp $8;
   set dset1;
   by &grpvar3. &grpvar2. &grpvar1.;

   *Checking number of recorded decimal places;
   %if &type. = C %then %do;
      &var._temp = &var.;
      if ^missing(&var._temp) then do;
         decplace = index(&var._temp,'.');
         if decplace = 0 then numdec = 0;
         else if decplace > 0 then numdec = length(substr(&var._temp,decplace+1));
      end;
      if ^missing(&var.) then do;
         digit = floor(&var./&lastdig. - 10*floor(&var./(10*&lastdig.)));
         if numdec <= -log10(&lastdig.) - 1 and digit = 0 then digit = -1;
      end;
   %end;
   %else %if &type. = N %then %do;
   	 %if %index(&lastdig.,.)>0 %then %let diglen = %sysfunc(length(&lastdig.));
   	 %else %let diglen = 1;

      if ^missing(&var.) then do;
         &var._temp = compress(put(&var.,18.5));
         decplace = index(&var._temp,'.');
         if decplace = 0 then numdec = 0;
         else if decplace > 0 then numdec = length(substr(&var._temp,decplace+1));
      end;
      if ^missing(&var.) then do;
         digit = floor(&var./&lastdig. - 10*floor(&var./(10*&lastdig.)));
         if numdec <= -log10(&lastdig.) - 1 and digit = 0 then digit = -1;
      end;
   %end;


   *Setting calendar time frames for collections;
   %if &datevar. ^= %then %do;
      if ^missing(&datevar.) then do;
      vyear = year(&datevar.);
      group = ceil(month(&datevar.)/(12/&interval.));
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


   label numdec = 'Number of Decimals'
         digit = "Last Digit (&lastdig. Place)"
         ;
run;


proc sql noprint;
   select count(distinct pangroup) into :numgrp from dset2;
quit;


*Digit value percentages;
proc sort data = dset2;
 	by &grpsort_first. pangroup &grpsort_last.  digit;
run;

ods output crosstabfreqs = freqs1_ (keep = rowpercent pangroup digit &grpsort_first. &grpsort_last. frequency);
proc freq data = dset2;
  	by &grpsort_first. pangroup;
   	table &grpsort_last.*digit;
run;

ods output crosstabfreqs = freqs1_all_ (keep = pangroup digit &grpsort_first. frequency);
proc freq data = dset2;
  	by &grpsort_first.;
   	table pangroup*digit;
run;

proc sort data = freqs1_all_;
  	by &grpsort_first. pangroup;
run;

data freqs1_all;
	merge freqs1_ freqs1_all_ (rename=(frequency=total));
  	by &grpsort_first. pangroup;
	if ^missing(&grpsort_last.) and ^missing(digit);
	panel_frq = frequency;
	inBar = 1;
	output;
	panel_frq = total - frequency;
	inBar = 0;
	output;
run;


proc sort data = freqs1_;
   by pangroup &grpsort_first. &grpsort_last. digit;
run;

proc sort data = freqs1_ out = freqs1_pangroup nodupkey;
   by pangroup &grpsort_first. &grpsort_last.;
run;

data shell;
   set freqs1_pangroup (keep=pangroup &grpsort_first. &grpsort_last. );
   by pangroup &grpsort_first. &grpsort_last.;
   rowpercent1 = 0;
   	if first.&grpsort_last. then do;
   %do i = -1 %to 9;
   		digit = &i;
		output;
   %end;
   end;
run;

data freqs1;
   merge freqs1_ (in=b) shell (in=a);
   by pangroup &grpsort_first. &grpsort_last. digit;
   if a or b;
   if missing(rowpercent) then rowpercent = 0;
run;

proc sort data = freqs1;
   	by &grpsort_first. pangroup &grpsort_last.;
run;

ods output crosstabfreqs = freqs2 (keep = rowpercent pangroup &grpsort_first. &grpsort_last. frequency);
proc freq data = dset2;
	%if &grpsort_first. ^= %then by &grpsort_first.;;
	table &grpsort_last.*pangroup;
run;

*Number of digits percentages;

proc sort data = dset2;
	by &grpsort_first. pangroup &grpsort_last. numdec;
run;

ods output crosstabfreqs = freqs3 (keep = rowpercent pangroup numdec &grpsort_first. &grpsort_last. frequency);
	proc freq data = dset2;
	by &grpsort_first. pangroup;
	table &grpsort_last.*numdec;
run;

*Annotations;
data freqs2;
	set freqs2;
	grpvar_total = Frequency;
	if pangroup ne ' ' &grpsort_notmiss.;
	keep pangroup &grpsort_first. &grpsort_last. grpvar_total;
run;

data freqs;
   	merge freqs1 (where=(^missing(digit) &grpsort_notmiss.)) 
    freqs1 (where=(missing(dig) &grpsort_notmiss.) rename=(frequency=total digit=dig) keep = pangroup frequency digit &grpsort_first. &grpsort_last.)
    freqs3 (where=(^missing(numdec) &grpsort_notmiss.) rename=(rowpercent=rowpercent_numdig))
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
   	format digit digval.;
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
   by pangroup &grpsort_first. &grpsort_last. digit;
run;

*Chi-square and multinomial p-values;
ods output onewaychisq = chitest (keep=pangroup &grpsort_first. &grpsort_last. name1 nvalue1);
proc freq data = plot_final1;
	where digit ^= -1;
   by pangroup &grpsort_first. &grpsort_last.;
   weight rowpercent/zeros;
   table digit / chisq testp = (10 10 10 10 10 10 10 10 10 10);
run;


ods output onewayfreqs = nodigit /*(keep=pangroup &grpsort_first. &grpsort_last. digit percent)*/;
proc freq data = plot_final1;
   by pangroup &grpsort_first. &grpsort_last.;
   weight rowpercent;
   table digit;
run;

*Compare to all within each panel;
proc sort data = freqs1_all;
   by pangroup &grpsort_first. &grpsort_last. digit;
run;

ods output chisq = chitest_all (keep=pangroup &grpsort_first. &grpsort_last. statistic prob where=(statistic='Chi-Square'));
proc freq data = freqs1_all;
	where digit ^= -1;
   by pangroup &grpsort_first. &grpsort_last.;
   weight panel_frq/zeros;
   table digit*inBar / chisq;
run;

data plot_final (drop=name1_drop digit_drop);
   merge plot_final1 chitest (where=(name1_drop="P_PCHI") rename=(name1=name1_drop))  nodigit (rename=(digit=digit_drop) where=(digit_drop=-1)) chitest_all (drop=statistic);
   by pangroup &grpsort_first. &grpsort_last.;
   if nvalue1 < 0.01 or prob < 0.01 then do;
      rowpercent2 = rowpercent;
         grpvar_total2 =  grpvar_total;
		 grpvar_total = .;
   end;
	else if percent > 5 then do;
		if digit = -1 then rowpercent3 = rowpercent;
         grpvar_total3 =  grpvar_total;
	end;
/*	if nvalue1 >= 0.01 and prob >= 0.01 then do;*/
		rowpercent4 = rowpercent;
         grpvar_total4 =   grpvar_total;
/*	end;*/

	if digit = -1 and missing(rowpercent3) then rowpercent3 = 0;
run;

proc template; 
   define style STYLES.MYSTYLE3; 
   style GraphData1 from GraphData1/color=black; 
   style GraphData2 from GraphData2/color=gold; 
   style GraphData3 from GraphData3/color=bilg; 
   style GraphData4 from GraphData4/color=cream;
   style GraphData5 from GraphData5/color=bipb; 
   style GraphData6 from GraphData6/color=depk;
   style GraphData7 from GraphData7/color=vioy; 
   style GraphData8 from GraphData8/color=delg;
   style GraphData9 from GraphData9/color=rose; 
   style GraphData10 from GraphData10/color=cyan;
   style GraphData11 from GraphData11/color=vigb;
end; 
run; 

*Setting number of columns based on number of groups;
data _null_;
%if &interval. = 1 or &interval. = 2 or &grpvar3. ^=  %then %do;
   %if 1 <= %sysfunc(floor(&numgrp.)) and %sysfunc(floor(&numgrp.)) <= 3 %then %do;
      cols = &numgrp.;
   %end;
   %else %if 4 = %sysfunc(floor(&numgrp.)) %then %do; 
      cols = 2;
   %end;
   %else %if 5 <= %sysfunc(floor(&numgrp.)) and %sysfunc(floor(&numgrp.)) <= 9 %then %do; 
      cols = 3;
   %end;
   %else %if 10 <= %sysfunc(floor(&numgrp.)) and %sysfunc(floor(&numgrp.)) <= 16 %then %do; 
      cols = 4;
   %end;
%end;

%else %if &interval. = 4 %then %do;
   %if 1 <= %sysfunc(floor(&numgrp.)) and %sysfunc(floor(&numgrp.)) <= 3 %then %do; 
      cols = &numgrp.;
   %end;
   %else %if 4 = %sysfunc(floor(&numgrp.)) %then %do; 
      cols = 2;
   %end;
   %else %if 5 <= %sysfunc(floor(&numgrp.)) %then %do; 
      cols = 4;
   %end;
%end;

%else %if &interval. = 12 %then %do;
   %if 1 <= %sysfunc(floor(&numgrp.)) and %sysfunc(floor(&numgrp.)) <= 3 %then %do; 
      cols = &numgrp.;
   %end;
   %else %if 4 = %sysfunc(floor(&numgrp.)) %then %do; 
      cols = 2;
   %end;
   %else %if 5 <= %sysfunc(floor(&numgrp.)) and %sysfunc(floor(&numgrp.)) <= 12 %then %do; 
      cols = 4;
   %end;
   %else %if 13 <= %sysfunc(floor(&numgrp.)) %then %do; 
      cols = 4;
   %end;
%end;
%else %do;
   %if 1 <= %sysfunc(floor(&numgrp.)) and %sysfunc(floor(&numgrp.)) <= 3 %then %do; 
      cols = &numgrp.;
   %end;
   %else %if 4 = %sysfunc(floor(&numgrp.)) %then %do; 
      cols = 2;
   %end;
   %else %if 5 <= %sysfunc(floor(&numgrp.)) and %sysfunc(floor(&numgrp.)) <= 12 %then %do; 
      cols = 4;
   %end;
   %else %if 13 <= %sysfunc(floor(&numgrp.)) %then %do; 
      cols = 4;
   %end;
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


%else %if &grppan_var = or &interval. = %then %do;
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
   by digit;
run;

ods listing style=mystyle3 gpath= "&outpath." image_dpi = 200;
ods graphics / height= 7.5in width= 9.5in reset=index 
%if &grppage_high. ^=  %then %do;
	imagename="Digitpref_&study._&var._&grpvar3._&grpvar2._&grpvar1._&int._&&group_second_&i.._&&group_first_&i." 
%end;
%else %if &grppage_low. ^=  %then %do;
	imagename="Digitpref_&study._&var._&grpvar3._&grpvar2._&grpvar1._&int._&&group_first_&i.." 
%end;
%else %if &grpsort_first. = &grpvar2. %then %do;
	imagename="Digitpref_&study._&var._&grpvar2._&grpvar1._&int._&&group_second_&i.._&&group_first_&i.." 
%end;
;

proc sgpanel data = plot_&i.;

   panelby pangroup / columns = &cols. novarname colheaderpos=top onepanel spacing = 5 uniscale=row;

   vbar &grpsort_last. / group=digit response=rowpercent2 datalabel=grpvar_total2;
   vbar &grpsort_last. / group=digit response=rowpercent4 datalabel=grpvar_total4 transparency = 0.7;
   vbar &grpsort_last. / group=digit response=rowpercent3 fillattrs=(color=black);

   colaxis label = "&grpsort_last.";

   rowaxis label = 'Percent' grid offsetmax=0.2 min = 0 max = 100;

   format digit digval.;

   footnote ' ';
   %if &minnum. > 0 %then %do;
   footnote j=l "Note: Bars with <= &minnum. observations are not shown.";
   %end;
   footnote2 j=l "Darkened bars fit at least one of the following criteria:";
   footnote3 j=l "1) significant by the Chi-square goodness-of-fit test (p<0.01) within each bar, including digits with proportions of 0";
   footnote4 j=l "2) significant by the Chi-square goodness-of-fit test (p<0.01) comparing an individual bar to the rest of the panel";
   footnote5 j=l "3) have > 5% with no digit in the last digit (&lastdig.) place";
   title "&study.";
   title2 "Digit Preference: &var.";
   %if &grppage_high. ^= %then %do;
		title3 "By &grppage_high. and &grppage_low.";
	%end;
   %else %if &grppage_low. ^= %then %do;
		title3 "By &grppage_low.";
	%end;

   title4 "Paneled &int_title.";
   title5 "Last Digit: &lastdig.";
   title6 "&subset. ";
   %if &grppage_high. ^= %then %do;
		title7 "&grppage_high.: &&group_second_&i.., &grppage_low.: &&group_first_&i..";
	%end;
   %else %if &grppage_low. ^= %then %do;
		title7 "&grppage_low.: &&group_first_&i..";
	%end;

run;
ods graphics off;

%end;

*Start single plot;
%if &grpsort_first. =  %then %do;
ods listing style=mystyle3 gpath= "&outpath." image_dpi = 200;
ods graphics / height= 7.5in width= 9.5in reset=index 
%if &grpvar2. ^= %then %do;
	imagename="Digitpref_&study._&var._&grpvar2._&grpvar1._&int." 
%end;
%else %if &grpvar1. ^= %then %do;
	imagename="Digitpref_&study._&var._&grpvar1._&int." 
%end;
;

proc sgpanel data = plot_final;

   panelby pangroup / columns = &cols. novarname colheaderpos=top onepanel spacing = 5 uniscale=row;

   vbar &grpsort_last. / group=digit response=rowpercent2 datalabel=grpvar_total2;
   vbar &grpsort_last. / group=digit response=rowpercent4 datalabel=grpvar_total4 transparency = 0.7;
   vbar &grpsort_last. / group=digit response=rowpercent3 fillattrs=(color=black);

   colaxis label = "&grpsort_last.";

   rowaxis label = 'Percent' grid offsetmax=0.2 min = 0 max = 100;

   format digit digval.;

/*   keylegend "Digit" ;*/

   footnote ' ';
   %if &minnum. > 0 %then %do;
   footnote j=l "Note: Bars with <= &minnum. observations are not shown.";
   %end;
   footnote2 j=l "Darkened bars fit at least one of the following criteria:";
   footnote3 j=l "1) significant by the Chi-square goodness-of-fit test (p<0.01) within each bar, including digits with proportions of 0";
   footnote4 j=l "2) significant by the Chi-square goodness-of-fit test (p<0.01) comparing an individual bar to the rest of the panel";
   footnote5 j=l "3) have > 5% with no digit in the last digit (&lastdig.) place";

      title "&study. ";
   title2 "Digit Preference: &var.";
   %if &grppage_high. ^= %then %do;
		title3 "By &grppage_high. and &grppage_low.";
	%end;
   %else %if &grppage_low. ^= %then %do;
		title3 "By &grppage_low.";
	%end;

   title4 "Paneled &int_title.";
   title5 "Last Digit: &lastdig.";
   title6 "&subset. ";

   %if &grppage_high. ^= %then %do;
		title7 "&grppage_high.: &&group_second_&i.., &grppage_low.: &&group_first_&i..";
	%end;
   %else %if &grppage_low. ^= %then %do;
		title7 "&grppage_low.: &&group_first_&i..";
	%end;


run;
ods graphics off;
%end;
%mend digitpref;
