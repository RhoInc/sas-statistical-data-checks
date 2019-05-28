/*----------------------- Copyright 2013, Rho, Inc.  All rights reserved. ------------------------\

  Project:      Statistical data checks

    Program:    longitudinalPlot.sas

      Purpose:  Generate ID-level series plots with outliers identified by a longitudinal mixed model

      Macros:   %dataExist, %variableExist, %directoryExist, %argumentCheck, %stackRTFs, %RTF2PDF7

    /---------------------------------------------------------------------------------------------\
      Parameters:
    \---------------------------------------------------------------------------------------------/

      [REQUIRED]

        [ data ]        - one- or two-level input dataset
        [ outcomeVar ]  - dependent variable, plotted on the y-axis
        [ IDvar ]       - subject variable
                            + defaults to USUBJID
        [ discVar ]     - discrete covariates
                            + required if [ discCont ] is set to DISCRETE or DISC
        [ contVar ]     - continuous covariates and continuous and/or discrete interactions
                            + required if [ discCont ] is set to CONTINUOUS or CONT
        [ discCont ]    - x-axis (longitudinal) variable type indicator, DISCRETE/DISC or
                          CONTINUOUS/CONT, which specifies which list contains the x-axis variable:
                          [ discVar ] or [ contVar ]
                            + defaults ot DISCRETE
                              > The first variable of the [ discVar ] argument will be
                                plotted on the x-axis if [ discCont ] is set to DISCRETE or DISC
                              > The first variable of the [ contVar ] argument will be
                                plotted on the x-axis if [ discCont ] is set to CONTINOUS or CONT

      [optional]

        [ byVar ]       - BY variable
        [ outPath ]     - output directory (where plots and outlier data will be sent)
                            + defaults to directory of program which calls longitudinalPlot
        [ outName ]     - output name
                            + defaults to 'Longitudinal Plot - &outcomeVar by [ longitudinal variable ]
        [ cutoff ]      - residual cutoff to identify outliers
                            + defaults to 90
                                > Identifies studentized residuals outside the upper
                                  &cutoff percent of the distribution (one-sided)
        [ method ]      - estimation method - PROC MIXED statement option
                            + defaults to REML
        [ DDFM ]        - denominator degrees of freedom - PROC MIXED MODEL statement DDFM option
                            + defaults to residual
        [ random ]      - random effects - PROC MIXED random statement input
                            + defaults to INTERCEPT
        [ type ]        - covariance structure - PROC MIXED random statement type option
                            + defaults to UN
        [ residual ]    - residual of type
                            + defaults to Student
        [ print ]       - print everything, outliers only, or only those IDs with at least one
                          outlier <ALL/OUTLIERS/IDS>
                            + defaults to OUTLIERS
        [ yRefLine ]    - list of variables with y-axis reference line values
        [ orientation ] - figure orientation, LANDSCAPE or PORTRAIT
                            + defaults to LANDSCAPE
        [ panelRows ]   - number of rows in figure
                            + defaults to 3 in LANDSCAPE and 5 in PORTRAIT
        [ panelCols ]   - number of columns in figure
                            + defaults to 1
        [ width ]       - figure width in inches
                            + defaults to 9.5 in LANDSCAPE, 7 in PORTRAIT
        [ height ]      - figure height in inches
                            + defaults to 7 in LANDSCAPE, 9.5 in PORTRAIT

    /---------------------------------------------------------------------------------------------\
      Examples:
    \---------------------------------------------------------------------------------------------/

        %longitudinalPlot
            (data       = DERIVE.ADLB
            ,outcomeVar = AVAL
            ,discVar    = AVISITN
            ,contVar    = BASE
            ,by         = PARAM);

/-------------------------------------------------------------------------------------------------\
  Program history:
\-------------------------------------------------------------------------------------------------/

    Date        Programmer          Description
    ----------  ------------------  --------------------------------------------------------------
    2013-08-15  Spencer Childress   Create
    2013-11-06  Spencer Childress   Add [ method ], [ byVar ], and [ DDFM ]
    2014-03-25  Spencer Childress   Add listing output
                                    Update outlier subset (to subset on &IDvar/&byVar combinations)
                                    Update output outlier dataset name
                                    Add list of missing covariates to output dataset and
                                        listing
    2014-03-27  Spencer Childress   Add [ discCont ]
    2015-03-28  Spencer Childress   Add [ yRefLine ]
    2014-06-17  Spencer Childress   Add [ width ] and [ height ]
    2014-07-01  Spencer Childress   Update [ cutoff ] to accept percentage instead of Z-score
    2014-07-02  Spencer Childress   Add [ fileType ]
                                    Update to output by-values to individual files
    2014-12-23  Spencer Childress   Update to identify one-sided outliers
    2015-01-15  Spencer Childress   Update to name listing with [ outName ]
                                    Update to identify outliers with studentized residuals
                                        calculated with the current observation deleted and compare
                                        those residuals with quantiles from the t-distribution
    2015-01-22  Spencer Childress   Update [ DDFM ] to default to RESIDUAL
    2015-12-04  Spencer Childress   Update second argument to TINV() function to 1 if degrees
                                        of freedom are equal to 1
                                    Update sort of input dataset to PROC REPORT
                                    Add [ residual ]
    2016-04-09  Spencer Childress   Remove [ fileType ] and generate both both RTF and PDF outputs
    2016-04-12  Spencer Childress   Rename from 'mixedPlot' to 'longitudinalPlot'

\------------------------------------------------------------------------------------------------*/

%macro longitudinalPlot
    (data        = 
    ,outcomeVar  = 
    ,IDvar       = USUBJID
    ,discVar     = 
    ,contVar     = 
    ,discCont    = DISCRETE
    ,byVar       = 
    ,outPath     = 
    ,outName     = 
    ,cutoff      = 90
    ,method      = REML
    ,DDFM        = RESIDUAL
    ,random      = INTERCEPT
    ,type        = UN
    ,residual    = STUDENT
    ,print       = OUTLIERS
    ,yRefLine    = 
    ,orientation = LANDSCAPE
    ,panelRows   = 
    ,panelCols   = 
    ,width       = 
    ,height      = 
    ) / minoperator;

  %*Point SASAUTOS to helper macros.;
    options sasautos = ('S:\BASESTAT\RhoUtil\sas-statisticalDataChecks\Macros\helperMacros' sasautos);

  %*Determine option settings to return their value at the end of the macro.;
    %local xWaitOption notesOption dateOption numberOption lsOption i;

    %let  xWaitOption = %sysfunc(getoption(xwait));
    %let  notesOption = %sysfunc(getoption(notes));
    %let   dateOption = %sysfunc(getoption(date));
    %let numberOption = %sysfunc(getoption(number));
    %let     lsOption = %sysfunc(getoption(ls));

    options noxwait nonotes nodate nonumber
        ls = 150;

  %*Clear titles and footnotes.;
    title;
    footnote;

    %put;
    %put %nrstr(/-------------------------------------------------------------------------------------------------\);
    %put %nrstr(  %longitudinalPlot beginning execution);
    %put %nrstr(\-------------------------------------------------------------------------------------------------/);
    %put;

        /***--------------------------------------------------------------------------------------\
          Parameter error checks
        \--------------------------------------------------------------------------------------***/

          %*[ data ];
            %dataExist(data);
                %if not &dataExist %then %goto exit;

          %*[ IDvar/outcomeVar/coVar/byVar ];
            %let coVar = %sysfunc(prxchange(%str(s/\*+/ /), -1, %nrbquote(&discVar &contVar)));
            %variableExist(&data, IDvar, 1);
                %if not &variableExist %then %goto exit;
            %variableExist(&data, outcomeVar, 1);
                %if not &variableExist %then %goto exit;
            %variableExist(&data, coVar, 1, Model requires at least one covariate.  Execution terminating.);
                %if not &variableExist %then %goto exit;
            %variableExist(&data, byVar);
                %if not &variableExist %then %goto exit;

          %*[ discCont ];
            %let discCont = %upcase(%substr(%nrbquote(&discCont), 1, 4));

            %if %nrbquote(&discCont) = %then %do;
                %put %str(    --> [ discCont ] unspecified.  Defaulting to DISCRETE.);

                %let discCont = DISC;
            %end;
            %else %do;
                %if not (&discCont in DISC CONT) %then %do;
                    %put %str(    --> [ discCont ] contains an unacceptable value.  Defaulting to DISCRETE.);

                    %let discCont = DISC;
                %end;
            %end;

          %*[ indepVar ];
            %if &discCont = DISC
                %then %let indepVar = %scan(%nrbquote(&discVar), 1, %str( *));
                %else %let indepVar = %scan(%nrbquote(&contVar), 1, %str( *));

          %*[ outPath ];
            %directoryExist(outPath,_plots_);
                %if not &directoryExist %then %goto exit;

          %*[ outName ];
            %if %nrbquote(&outName) = %then %do;
                %let outName = Longitudinal Plot - &outcomeVar by &indepVar;

                %put %str(    --> [ outName ] defaulting to &outName..);
            %end;

            %let listingName = %nrbquote(&outName) Listing;

          %*[ method ];
            %argumentCheck(method, REML ML MIVQUE0 TYPE1 TYPE2 TYPE3, REML);

          %*[ DDFM ];
            %argumentCheck(DDFM, CONTAIN CON BETWITHIN BW RESIDUAL KENROG KR SATTERTHWAITE SATTERTH SAT KENWARDROGER RES, RESIDUAL);

          %*[ random ];
            %let random = %upcase(%sysfunc(prxchange(s/\s{2%str(,)}/%str( )/, -1,
                                  %sysfunc(prxchange(s/\s{2%str(,)}/%str( )/, -1, %nrbquote(&random))))));
            %if %nrbquote(&random) = %then %let random = INTERCEPT;
            %else %if not (&random in INT INTERCEPT) %then %do;
                %variableExist(&data, &random, 0);
                    %if not &variableExist %then %let random = INTERCEPT;
            %end;

          %*[ type ];
            %let _type_ = %scan(&type, 1, %str(%(@));
            %argumentCheck(_type_, ANTE AR ARH ARMA CS CSH FA VC FA0 FA1 HF TOEP TOEPH UN UNR, UN);

          %*[ residual ];
            %argumentCheck(residual, RESID RESIDUAL STUDENTRESID STUDENT PEARSONRESID RSTUDENT PRESSRES, STUDENT);

          %*[ cutoff ];
            %if %nrbquote(&cutoff) ne %then %do;
                %do i = 1 %to %length(&cutoff);
                    %let _char_ = %substr(&cutoff, &i, 1);

                    %if ^(%sysfunc(anydigit(%bquote(&_char_))) or %bquote(&_char_) = .) %then %do;
                        %put %str(    --> [ cutoff ] contains a value of "&_char_".);
                        %put %str(    -->     It can only take non-negative numeric arguments.);

                        %goto badCutoff;
                    %end;
                %end;

                %if %sysevalf(&cutoff gt 100) or %sysevalf(&cutoff lt 1) %then %do;
                    %put %str(    --> [ cutoff ] is outside the accepted percentile range of 1-100.);

                    %goto badCutoff;
                %end;
            %end;
            %else %do;
                %put %str(    --> [ cutoff ] unspecified.);

                %goto badCutoff;
            %end;

            %goto goodCutoff;

            %badCutoff:
                %put %str(    -->     Defaulting to 90, which identifies studentized residuals outside the 90th percentile.);

                %let cutoff = 90;

            %goodCutoff:
                %let alpha = %sysfunc(strip(%sysevalf(1 - &cutoff/100)));

          %*[ print ];
            %argumentCheck(print, ALL OUTLIERS IDS, OUTLIERS);

          %*[ yRefLine ];
            %variableExist(&data, yRefLine);

          %*[ orientation ];
            %argumentCheck(orientation, LANDSCAPE PORTRAIT, LANDSCAPE);

          %*[ panelRows/panelCols ];
            %if &orientation = LANDSCAPE %then %do;
                %argumentCheck(panelRows, 1 2 3 4 5 6 7 8 9 10, 3);
                %argumentCheck(panelCols, 1 2 3 4 5 6 7 8 9 10, 1);
            %end;
            %else %do;
                %argumentCheck(panelRows, 1 2 3 4 5 6 7 8 9 10, 1);
                %argumentCheck(panelCols, 1 2 3 4 5 6 7 8 9 10, 3);
            %end;

          %*[ width ];
            %if %nrbquote(&width) ne %then %do;
                %if %sysfunc(prxmatch(/[^\d.]/, %nrbquote(&width))) %then %do;
                    %if &orientation = LANDSCAPE %then %do;
                        %put %str(    --> [ width ] contains non-numeric value.  Defaulting to 9.5.);
                        %let width = 9.5;
                    %end;
                    %else %do;
                        %put %str(    --> [ width ] contains non-numeric value.  Defaulting to 7.);
                        %let width = 7;
                    %end;
                %end;
            %end;
            %else %do;
                %if &orientation = LANDSCAPE
                    %then %let width = 9.5;
                    %else %let width = 7;
            %end;

          %*[ height ];
            %if %nrbquote(&height) ne %then %do;
                %if %sysfunc(prxmatch(/[^\d.]/, %nrbquote(&height))) %then %do;
                    %if &orientation = LANDSCAPE %then %do;
                        %put %str(    --> [ height ] contains non-numeric value.  Defaulting to 7.);

                        %let height = 7;
                    %end;
                    %else %do;
                        %put %str(    --> [ height ] contains non-numeric value.  Defaulting to 9.5.);

                        %let height = 9.5;
                    %end;
                %end;
            %end;
            %else %do;
                %if &orientation = LANDSCAPE
                    %then %let height = 7;
                    %else %let height = 9.5;
            %end;

    %put;
    %put %nrstr(/-------------------------------------------------------------------------------------------------\);
    %put %nrstr(  PROC MIXED running);
    %put %nrstr(\-------------------------------------------------------------------------------------------------/);
    %put;

        /***--------------------------------------------------------------------------------------\
          Print PROC MIXED statements to log
        \--------------------------------------------------------------------------------------***/

            %put %str(    --> proc mixed);
            %put %str(    -->     data = _ForMixed_);
            %put %str(    -->     method = &method;);
            %if %nrbquote(&byVar) ne %then %do;
                %put %str(    -->     by);
                %put %str(    -->         &byVar;);
            %end;
            %put %str(    -->     class %sysfunc(strip(&IDvar &discVar)););
            %put %str(    -->     model);
            %put %str(    -->         &outcomeVar = %sysfunc(strip(&discVar &contVar)) / residual solution influence (iter = 2));
            %put %str(    -->             outpred = DF);
            %put %str(    -->             ddfm = &DDFM;);
            %if %nrbquote(&random) ne %then %do;
                %put %str(    -->     random &random /);
                %if &type ne %then
                    %put %str(    -->         type  = &type);
                %put %str(    -->         subject = &IDvar;);
            %end;
            %put %str(    -->     ods output);
            %put %str(    -->         Influence = RStudent;);
            %put %str(    --> run;);

        /***--------------------------------------------------------------------------------------\
          Input data manipulation
        \--------------------------------------------------------------------------------------***/

          %*Calculate number of variables in discVar and contVar.;
            %let ncovars = %eval(
                %sysfunc(count(&discVar &contVar, %str( ))) +
                %sysfunc(count(&discVar &contVar, %str(*))) + 1);

          %*Sort input dataset by &byVar variables.;
            %if %nrbquote(&byVar) ne %then %do;
                proc sort
                    data = &Data
                    out  = _ForMixed_;
                    by &byVar;
                run;

                data _ForMixed_;
                    set _ForMixed_;

                    IDVar = cats(&IDvar, %sysfunc(prxchange(s/ +/%str(, )/, -1, &byVar)));

                    length covars $150;

                  %*Create list of missing covariates.;
                    %do i = 1 %to &ncovars;
                        if              missing(%scan(&discVar &contVar, &i, %str( *))) and
                           ^index(covars, vname(%scan(&discVar &contVar, &i, %str( *))))
                            then covars = catx(', ', covars, vname(%scan(&discVar &contVar, &i, %str( *))));
                    %end;

                    label covars = 'Missing Covariates';
                run;
            %end;
            %else %do;
                data _ForMixed_;
                    set &Data;

                    IDVar = &IDvar;

                    length covars $150;

                  %*Create list of missing covariates.;
                    %do i = 1 %to &ncovars;
                        if              missing(%scan(&discVar &contVar, &i, %str( *))) and
                           ^index(covars, vname(%scan(&discVar &contVar, &i, %str( *))))
                            then covars = catx(', ', covars, vname(%scan(&discVar &contVar, &i, %str( *))));
                    %end;

                    label covars = 'Missing Covariates';
                run;
            %end;

        /***--------------------------------------------------------------------------------------\
          PROC MIXED
        \--------------------------------------------------------------------------------------***/

            proc mixed
                data = _ForMixed_
                method = &method;
            %if %nrbquote(&byVar) ne %then
                by
                    &byVar;;
                class &IDvar &discVar;
                model
                    &outcomeVar = &discVar &contVar / residual solution influence (iter = 2)
                        outpred = DF
                        ddfm = &DDFM;
            %if %nrbquote(&random) ne %then %do;
                random &random /
                %if &type ne %then
                    type  = &type;
                    subject = &IDvar;
            %end;
                ods output
                    Influence = RStudent;
            run;

        /***--------------------------------------------------------------------------------------\
          Output data manipulation
        \--------------------------------------------------------------------------------------***/

            data MIXEDDataset;
                merge
                    DF
                    RStudent;

                if DF ge 1.12
                    then RStud_cutoff = tinv(&cutoff/100, DF - 1);
                else if DF gt  .z
                    then RStud_cutoff = tinv(&cutoff/100, DF    );*work-around since TINV() cannot take a DF value of < .12;
            run;

          %*Assign &outcomeVar label to macro variable YLabel and &IDvar label to macro variable IDLabel.;
            data _null_;
                if _n_ = 1 then do;
                    set &Data;
                    call symputx('YLabel',  vlabel(&outcomeVar));
                    call symputx('IDLabel', vlabel(&IDvar));
                end;
                stop;
            run;

          %*Assign outlier variable OUTLIER, populated only with outliers.
            Assign outlier variable _OUTLIER_, equal to &outcomeVar, with a * appended on outliers.;
            %macro outlierDataset / minoperator;

                proc sql;
                    create table outlierDataset&i as
                        select *,
                            case when abs(&residual) ge RStud_cutoff
                                 then &outcomeVar
                                 else .
                            end  as outlier label = "&YLabel (star - suspected outlier)",
                            case when abs(&residual) ge RStud_cutoff
                                 then cats(put(&outcomeVar, best.), '*')
                                 else strip(put(&outcomeVar, best.))
                            end  as _outlier_ label = "&YLabel"
                            from MIXEDDataset
                              where &bySubset
                                %if &print in OUTLIERS IDS %then
                                    and IDVar in (

                                        select distinct IDVar
                                            from MIXEDDataset
                                              where abs(&residual) ge RStud_cutoff

                                                  );
                    order by %if &byVar ne %then %sysfunc(prxchange(s/ +/%str(, )/, -1, &byVar)),; &IDvar, &indepVar;
                quit;

                %if %nrbquote(&byVar) ne %then %do;
                    data outlierDataset;
                        set outlierDataset
                            outlierDataset&i;
                    run;
                %end;

            %mend outlierDataset;

    %put;
    %put %nrstr(/-------------------------------------------------------------------------------------------------\);
    %put %nrstr(  PROC SGPANEL running);
    %put %nrstr(\-------------------------------------------------------------------------------------------------/);
    %put;

        /***--------------------------------------------------------------------------------------\
          Print PROC SGPANEL statements to log
        \--------------------------------------------------------------------------------------***/

          %*Print PROC SGPANEL statements to log.;
            %put %str(    --> proc sgpanel);
            %put %str(    -->     data = outlierDataset;);

            %put %str(    -->     panelby &IDvar / novarname);
            %put %str(    -->         columns = &panelCols);
            %put %str(    -->         rows = &panelRows;);
            %put %str(    -->     series);
            %put %str(    -->         x = &indepVar);
            %put %str(    -->         y = &outcomeVar / markers);
            %put %str(    -->             name = 'Actual');
            %put %str(    -->             markerattrs = %();
            %put %str(    -->                 color = blue);
            %put %str(    -->                 symbol = circle%););
            %put %str(    -->     series);
            %put %str(    -->         x = &indepVar);
            %put %str(    -->         y = pred / markers);
            %put %str(    -->             name = 'Predicted');
            %put %str(    -->             markerattrs = %();
            %put %str(    -->                 color = green);
            %put %str(    -->                 symbol = circlefilled%););
            %put %str(    -->     scatter);
            %put %str(    -->         x = &indepVar);
            %put %str(    -->         y = outlier /);
            %put %str(    -->             name = 'Outliers');
            %put %str(    -->             markerattrs = %();
            %put %str(    -->                 symbol = starfilled);
            %put %str(    -->                 color = red);
            %put %str(    -->                 size = 2mm%););
            %put %str(    -->     rowaxis);
            %put %str(    -->         type = linear;);
            %put %str(    -->     colaxis);
            %if &discCont = DISC
                %then %put %str(    -->         type = discrete;);
                %else %put %str(    -->         type = linear;);
            %if &yRefLine ne %then %do yref = 1 %to %sysfunc(countw(&yRefLine));
                %let yref&yref = %scan(&yRefLine, &yref);
                %if &yref = 1 %then
                      %let color = lightgreen;
                %else %if &yref = 2 %then
                      %let color = darkgreen;
                %else %let color = gray;

                    %put %str(    -->     refline &&yref&yref / noclip);
                    %put %str(    -->         name = "&&yref&yref");
                    %put %str(    -->         legendlabel = "&&yref&yref");
                    %put %str(    -->         axis = y);
                    %put %str(    -->         lineattrs = %();
                    %put %str(    -->             color = &color%););
            %end;
            %if &yRefLine =
                %then %put %str(    -->     keylegend 'Actual' 'Predicted';);
                %else %put %str(    -->     keylegend 'Actual' 'Predicted' "%sysfunc(prxchange(s/ +/%nrbquote(" ")/, -1, &yRefLine))";);
            %put %str(    --> run;);

        /***--------------------------------------------------------------------------------------\
          PROC SGPANEL
        \--------------------------------------------------------------------------------------***/

            %let longitudinalPlotFiles = ;

            %macro SGPANEL;
                title1 j = c "&outcomeVar by &indepVar";
            %if       %nrbquote(&byVar)      ne %then
                title2 j = c "%nrbquote(&byTitle)";;

                footnote1 j = c "Outliers identified by residuals produced from a mixed model with";
                footnote2 j = c "dependent variable %upcase(&outcomeVar) and effects %upcase(%sysfunc(tranwrd(&discVar &contVar, %str( ), %str(, )))).";

                options
                    orientation = &orientation;
                ods listing
                    gpath = "%sysfunc(pathname(work))";
                ods results off;
                    ods graphics on /
                        reset = all
                        border = no
                        width = &width.in
                        height = &height.in
                        imagename = "Longitudinal Plot - &outcomeVar by &indepVar";

                    %if %nrbquote(&byVar) ne %then %do;
                        ods rtf
                            file = "&outPath\&outName&i..rtf";
                        %let longitudinalPlotFiles = &longitudinalPlotFiles|&outName&i;
                    %end;
                    %else
                        ods rtf
                            file = "&outPath\&outName..rtf";;

                            proc sgpanel
                                data = outlierDataset&i;

                                panelby &IDvar / novarname
                                    columns = &panelCols
                                    rows = &panelRows;
                                series
                                    x = &indepVar
                                    y = &outcomeVar / markers
                                        name = 'Actual'
                                        markerattrs = (
                                            color = blue
                                            symbol = circle);
                                series
                                    x = &indepVar
                                    y = pred / markers
                                        name = 'Predicted'
                                        markerattrs = (
                                            color = green
                                            symbol = circlefilled);
                                scatter
                                    x = &indepVar
                                    y = outlier /
                                        name = 'Outliers'
                                        markerattrs = (
                                            symbol = starfilled
                                            color = red
                                            size = 2mm);
                                rowaxis
                                    type = linear;
                                colaxis
                                    type = %if &discCont = DISC %then discrete; %else linear;;
                            %if &yRefLine ne %then %do yref = 1 %to %sysfunc(countw(&yRefLine));
                                %let yref&yref = %scan(&yRefLine, &yref);
                                %if &yref = 1 %then
                                      %let color = lightgreen;
                                %else %if &yref = 2 %then
                                      %let color = darkgreen;
                                %else %let color = gray;

                                refline &&yref&yref / noclip
                                    name = "&&yref&yref"
                                    legendlabel = "&&yref&yref"
                                    axis = y
                                    lineattrs = (
                                        color = &color);
                            %end;
                                keylegend 'Actual' 'Predicted' %if &yRefLine ne %then "%sysfunc(prxchange(s/ +/%nrbquote(" ")/, -1, &yRefLine))";;
                            run;
                        ods rtf close;

                    ods graphics off;
                ods results;

            %mend  SGPANEL;

        /***--------------------------------------------------------------------------------------\
          By-group processing
        \--------------------------------------------------------------------------------------***/

          %*Generate longitudinal plot for each combination of by-values.;
            %if %nrbquote(&byVar) ne %then %do;
                proc sql noprint;
                    select       distinct catx('~', %sysfunc(prxchange(%str(s/( +)/, /), -1, &byVar))),
                           count(distinct catx('~', %sysfunc(prxchange(%str(s/( +)/, /), -1, &byVar))))
                      into      :byVals  separated by '|',
                                :nbyVals
                        from MIXEDDataset;
                quit;

                %put %str(    --> Number of BY values: %sysfunc(left(&nbyVals)));

              %*Instantiate dataset to which each by-value dataset will be appended.;
                data outlierDataset;
                    delete;
                run;

                %do i = 1 %to &nbyVals;
                    %let byVal   = %qscan(%nrbquote(&byVals), &i, |);

                    data _null_;
                        if _n_ = 1 then do;
                            set MIXEDDataset;

                            length bySubset $1000;
                            call missing(bySubset);

                            %do j = 1 %to %sysfunc(countw(&byVar));
                                if vtype(%scan(&byVar, &j)) = 'N'
                                    then bySubset = catx(' and ',
                                        bySubset,
                                        catx(' = ',
                                            scan("&byVar",    &j),
                                            scan("&byVal", &j, '~')));
                                    else bySubset = catx(' and ',
                                        bySubset,
                                        catx(' = ',
                                            scan("&byVar",    &j),
                                            quote(scan("&byVal", &j, '~'))));

                                byTitle =
                                    prxchange('s/ and /, /', -1,
                                    prxchange('s/"//'      , -1,
                                        bySubset));
                            %end;

                            call symputx('bySubset', bySubset);
                            call symputx('byTitle' , byTitle );
                        end;
                        stop;
                    run;

                    %let byMessage = ;

                    %put %str(    --> Processing data subset on &bySubset..);

                    %outlierDataset;

                    %SGPANEL;

                    %if &i = &nbyVals %then %put ;
                %end;
            %end;
          %*Otherwise, generate a single plot.;
            %else %do;
                %let i        = ;
                %let bySubset = 1;
                %outlierDataset;

                %SGPANEL;
            %end;

        /***--------------------------------------------------------------------------------------\
          Stack output RTFs, convert to PDF, and output outlier dataset
        \--------------------------------------------------------------------------------------***/

            %if &byVar ne %then %do;
                %stackRTFs
                    (directory = &activeDirectory
                    ,files = &longitudinalPlotFiles
                    ,out = &outName);
            %end;

            %rtf2pdf7
                (in  = &outPath\&outName..rtf
                ,out = &outPath\&outName..pdf);

          %*Output dataset with all relevant variables.;
            data _plots_.&outcomeVar._&indepVar (keep = &IDvar &outcomeVar &byVar &discVar %sysfunc(tranwrd(&contVar, *, %str( ))) _outlier_ covars pred outlier &residual);
                retain &IDvar &indepVar _outlier_ covars;
                set outlierDataset;
            run;

    %put;
    %put %nrstr(/-------------------------------------------------------------------------------------------------\);
    %put %nrstr(  PROC REPORT running);
    %put %nrstr(\-------------------------------------------------------------------------------------------------/);
    %put;

        data forReport;
            set outlierDataset (where = (not ("&print" = 'OUTLIERS' and abs(&residual) lt RStud_cutoff)));
        run;

        /***--------------------------------------------------------------------------------------\
          Print PROC REPORT statements to log
        \--------------------------------------------------------------------------------------***/

            %put %str(    --> proc report nowd spanrows);
            %put %str(    -->     data = forReport);
            %put %str(    -->     style(column) = [);
            %put %str(    -->         cellwidth = 2in];);
            %put ;
            %put %str(    -->     column &byVar &IDvar &indepVar _outlier_ covars;);
            %put ;
            %if &byVar ne %then %do;
                %put %str(    -->     define &byVar / display center order);
                %put %str(    -->         width = 20;);
                %put %str(    -->         style(column) = [);
                %put %str(    -->             font_weight = bold);
                %put %str(    -->             font_size = 5];);
            %end;
            %put %str(    -->     define &IDvar / display center order);
            %put %str(    -->         width = 20;);
            %put %str(    -->     define &indepVar / display center);
            %put %str(    -->         width = 20;);
            %put %str(    -->     define _outlier_ / display center);
            %put %str(    -->         width = 20;);
            %put %str(    -->     define covars / display center);
            %put %str(    -->         width = 20;);
            %if &byVar ne %then %do;
                %put ;
                %put %str(    -->     break after &byVar /);
                %put %str(    -->         page;);
            %end;
            %put %str(    --> run;);

        /***--------------------------------------------------------------------------------------\
          PROC REPORT
        \--------------------------------------------------------------------------------------***/

            %macro REPORT;

                proc template;
                    define style outlier;
                        parent = styles.printer;
                        replace rowheader /
                            background = cx336699;
                        replace Header /
                            background = cx336699
                            foreground = cxFFFFFF;
                    end;
                run;

                proc report nowd spanrows
                    data = forReport
                    style(column) = [
                        cellwidth = 2in];

                    column %if &byVar ne %then bookmark; &IDvar &indepVar _outlier_ covars;

                    %if &byVar ne %then
                        define bookmark / display center order
                            width = 20
                            style(column) = [
                                font_weight = bold
                                font_size = 5];;
                    define &IDvar / display center order
                        width = 20;
                    define &indepVar / display center
                        width = 20;
                    define _outlier_ / display center
                        width = 20;
                    define covars / display center
                        width = 20;

                    %if &byVar ne %then
                        break after bookmark /
                            page;;
                run;

            %mend  REPORT;

        /***--------------------------------------------------------------------------------------\
          By-group processing
        \--------------------------------------------------------------------------------------***/

            %if &byVar ne %then %do;

                /***------------------------------------------------------------------------------\
                  Table of contents
                \------------------------------------------------------------------------------***/

                    proc sql;
                        create table ToC as
                            select distinct
                                    '{\field {\*\fldinst HYPERLINK \\l "' ||
                                    prxchange('s/[- ]/_/', -1, strip(&byVar)) ||
                                    '"}{\fldrslt ' ||
                                    strip(&byVar) || '}}' as Hyperlink label = "&byVar"
                                from forReport;
                    run;

                /***------------------------------------------------------------------------------\
                  Bookmarks
                \------------------------------------------------------------------------------***/

                    proc sort
                        data = forReport;
                        by &byVar &IDvar &indepVar;
                    run;

                    data forReport;
                        set forReport;

                        bookmark = '{\*\bkmkstart ' ||
                            prxchange('s/[- ]/_/', -1, strip(&byVar)) || '}'            ||
                                                       strip(&byVar)  || '{\*\bkmkend ' ||
                            prxchange('s/[- ]/_/', -1, strip(&byVar)) || '}';;

                        label
                            bookmark = "&byVar"
                        ;
                    run;

                %end;

        /***--------------------------------------------------------------------------------------\
          Listing output
        \--------------------------------------------------------------------------------------***/

            title;
            footnote;

            options
                orientation = &orientation;

            ods results off;
                ods rtf
                    file = "&outPath\&listingName..rtf";

                    title "&outcomeVar Listing";
                    %if &print ne OUTLIERS %then footnote '* - Suspected Outlier';;

                    %if &byVar ne %then %do;
                        proc report nowd missing spanrows
                            data = ToC;

                            column Hyperlink;

                            define Hyperlink / display center
                                width = 100
                                style(column) = [
                                    font_weight = bold
                                    font_size = 5
                                    cellwidth = 10in];
                        run;
                    %end;
                    %REPORT;
                ods rtf close;
            ods results;

            %rtf2pdf7
                (in  = &outPath\&listingName..rtf
                ,out = &outPath\&listingName..pdf);

    %exit:

    %put;
    %put %nrstr(/-------------------------------------------------------------------------------------------------\);
    %put %nrstr(  %longitudinalPlot ending execution);
    %put %nrstr(\-------------------------------------------------------------------------------------------------/);
    %put;

  %*Reset environment.;
    %if ^%sysfunc(libref(_plots_)) %then libname _plots_ clear;;
    options &xWaitOption &notesOption &dateOption &numberOption 
        ls = &lsOption;
    title;
    footnote;

%mend  longitudinalPlot;
