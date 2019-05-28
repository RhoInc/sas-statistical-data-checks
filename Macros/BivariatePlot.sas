/*----------------------- Copyright 2013, Rho, Inc.  All rights reserved. ------------------------\

  Project:      Statistical data checks

    Program:    bivariatePlot.sas

      Purpose:  Generate scatter plots with outliers identified by a general linear model

      Macros:   %dataExist, %variableExist, %directoryExist, %argumentCheck, %stackRTFs, %RTF2PDF7

    /---------------------------------------------------------------------------------------------\
      Parameters:
    \---------------------------------------------------------------------------------------------/

      [REQUIRED]

        [ data ]        - one- or two-level input dataset
        [ IDvar ]       - subject identifier variable
        [ xVar ]        - variable to be plotted on the x-axis
        [ yVar ]        - variable to be plotted on the y-axis

      [optional]

        [ discVar ]     - discrete covariates
        [ contVar ]     - continuous covariates
        [ byVar ]       - by variable
        [ siteVar ]     - site variable
                            + if specified, bivariatePlot outputs a plot for each site which only
                              identifies that site's subjects, but analysis is run on the pooled data
        [ sepbyVar ]    -  If set to Y, Listing paginates by BYVAR and excel output has separate sheets by BYVAR.
                           (Can take the values of Yes No yes no YES NO) 
        [ sepbySite ]   -  If set to Y, listing and excel output has separate sheets by BYVAR.
                           (Can take the values of Yes No yes no YES NO) 
        [ outlier ]     - outlier detection method < COOKD/RES >
                            + defaults to COOKD
                               > Cook's Distance or studentized residual
        [ cutoff ]      - outlier detection threshold
                            + defaults to 7*

                                > [ cutoff ] associated with COOKD identifies Cook's
                                  distances greater than [ cutoff ]/(number of
                                  observations in input dataset or in individual By
                                  groups)

                                > [ cutoff ] associated with RES identifies studentized
                                  residuals outside the upper [ cutoff ] percentile of the
                                  distribution 
                                      + defaults to 95 if outlier is set to RES

            * Opinions differ regarding what cut-off value to choose to identify highly
              influential points.  A simple operational guideline of Di > 1, where Di
              represents the ith observation's Cook's distance, has been suggested.  Others
              have suggested a cutoff of Di > 4/n, where n is the number of observations,
              might be used.
        [ prediction ]  - graphical prediction type < ELLIPSE/CLI >
                            + defaults to ELLIPSE
        [ outPath ]     - output directory
        [ outName ]     - output file name
 
        [ orientation ] - figure orientation < LANDSCAPE/PORTRAIT >
                            + defaults to LANDSCAPE
        [ width ]       - figure width in inches
                            + defaults to 10 in LANDSCAPE, 7 in PORTRAIT
        [ height ]      - figure height in inches
                            + defaults to 7 in LANDSCAPE, 10 in PORTRAIT

    /---------------------------------------------------------------------------------------------\
      Examples:
    \---------------------------------------------------------------------------------------------/

        %bivariatePlot
            (data = DERIVE.ADSL
            ,xVar = HEIGHT
            ,yVar = WEIGHT);

/-------------------------------------------------------------------------------------------------\
  Program history:
\-------------------------------------------------------------------------------------------------/

    Date        Programmer          Description
    ----------  ------------------  --------------------------------------------------------------
    2013-12-05  Spencer Childress   Create
    2014-02-18  Spencer Childress   Add PROC GLM outlier detection
                                    Add [ discVar ], [ contVar ], [ outlier ], [ cutoff ], and
                                        [ prediction ]
                                    Update [ depVar ] to [ xVar ]
                                    Update [ indepVar ] to [ yVar ]
    2014-03-11  Spencer Childress   Add XAXIS and YAXIS statements to prevent plot space from
                                    truncating
    2014-04-22  Spencer Childress   Add [ siteVar ] and [ site ]
                                    Update [ cutoff ] input given [ outlier ] = RES
    2014-05-07  Spencer Childress   Add code to print PROC GLM and PROC SGPLOT to log
                                    Add title and update footnotes
    2014-05-21  Spencer Childress   Drop [ site ] and update to output plot for each site
                                        if [ siteVar ] is specified
                                    Make [ outPath ] optional
    2014-06-17  Spencer Childress   Add [ width ] and [ height ]
    2014-06-30  Spencer Childress   Add [ fileType ]
                                    Update to output by-values to individual files
    2014-07-31  Spencer Childress   Add t-distribution cutoff
    2014-12-16  Spencer Childress   Add [ outName ]
    2014-12-23  Spencer Childress   Update to identify one-sided outliers
    2015-01-15  Spencer Childress   Update to identify outliers with studentized residuals with
                                        the current observation deleted
    2016-04-09  Spencer Childress   Remove [ fileType ] and output both .rf and .pdf files
    2017-11-20  Julie Dorais        Edit scatterplot visuals (label, legend, etc.)
                                    Add [ sepbyvar] [xlabel] and [ylabel]
                                    Add rtf and excel listing
    2017-12-07  Julie Dorais        Add [ sepbysite] allowing listing output to separate by site. Update header.  
                                    Add macro-generated warning if both sitevar and byvar are specified.
                                    Updates (in progress)


\------------------------------------------------------------------------------------------------*/

%macro bivariatePlot
    (data        = 
    ,IDvar       = USUBJID
    ,xVar        = 
    ,yVar        = 
    ,discVar     = 
    ,contVar     = 
    ,byVar       = 
    ,siteVar     = 
    ,sepbyVar    =
    ,sepbySite   = 
    ,outlier     = COOKD
    ,cutoff      = /*7*/
    ,prediction  = ELLIPSE
    ,outPath     = 
    ,outName     = 
    ,orientation = LANDSCAPE
    ,width       = 
    ,height      = 
/*    ,xlabel      =*/
/*    ,ylabel      = */
    ) / minoperator;



/*   %*Get today's date for filenames.;*/
   %global fdate;
     %let fdate = %sysfunc(today(),yymmddd10.); 
     %put &fdate;

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
    %put %nrstr(  %bivariatePlot beginning execution);
    %put %nrstr(\-------------------------------------------------------------------------------------------------/);
    %put;

        /***--------------------------------------------------------------------------------------\
          Parameter error checks
        \--------------------------------------------------------------------------------------***/

          %*<data>;
            %dataExist(data);
                %if not &dataExist %then %goto exit;

          %*<IDvar/xVar/yVar/discVar/contVar/byvar/siteVar>;
            %let coVar = %sysfunc(prxchange(%str(s/\*+/ /), -1, %nrbquote(&discVar &contVar)));
            %variableExist(&data, IDvar, 1);
                %if not &variableExist %then %goto exit;
            %variableExist(&data, xVar, 1);
                %if not &variableExist %then %goto exit;
            %variableExist(&data, yVar, 1);
                %if not &variableExist %then %goto exit;
            %variableExist(&data, byVar);
                %if not &variableExist %then %goto exit;
            %variableExist(&data, coVar);
                %if not &variableExist %then %goto exit;
            %variableExist(&data, siteVar);
                %if not &variableExist %then %goto exit;


          %*<sitevar and byvar>; 
             %if %nrbquote(&SiteVar) ne and %nrbquote(&Byvar) ne %then %do;
                %put %str(    --> Site variable and by variable cannot both be specified. );
                %goto exit;
             %end;


          %*<outPath>;
            %directoryExist(outPath,_plots_);
                %if not &directoryExist %then %goto exit;

          %*<outName>;
            %if %nrbquote(&outName) ne
                %then %let outName = %sysfunc(prxchange(s/[%str(<>:%"\/\\\|\?\*)]/_/, -1, %nrbquote(&outName)));/*"*/
            %else %do;
                %let outName = Bivariate Plot - &xVar by &yVar;

                %put %str(    --> outName defaulting to &outName..);
                %put ;
            %end;

          %*<siteVar>;
            %if %nrbquote(&SiteVar) ne %then %do;
                data _null_;
                    if _n_ = 1 then do;
                        set &data;

                        if vtype(&SiteVar) = 'N'
                            then call symputx('SiteType', "strip(put(&SiteVar, " || strip(vformat(&SiteVar)) || '))');
                            else call symputx('SiteType', "&SiteVar");
                    end;
                    stop;
                run;

               %*Replace "Site" from site variables that has the word;
               data &data;
                  set &data; 
                     if vtype(&SiteVar) = 'C' then do;
                     &SiteVar=strip(tranwrd(&SiteVar, "Site",""));
                     end;
                run;
            %end;

          %*<outlier>;
            %argumentCheck(outlier, COOKD RES, COOKD);

          %*<cutoff>;
            %if %nrbquote(&cutoff) ne %then %do;
                %do i = 1 %to %length(&cutoff);
                    %let _char_ = %substr(&cutoff, &i, 1);

                    %if ^(%sysfunc(anydigit(%bquote(&_char_))) or %bquote(&_char_) = .) %then %do;
                        %put %str(    --> [ cutoff ] contains a value of "&_char_".);
                        %put %str(    -->     It can only take non-negative numeric arguments.);

                        %goto badCutoff;
                    %end;
                %end;

                %if &outlier = RES and (%sysevalf(&cutoff gt 100) or %sysevalf(&cutoff lt 1)) %then %do;
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
                %if &outlier = COOKD %then %do;
                    %put %str(    -->     Defaulting to 7, which identifies Cook%'s distances greater than);/*'*/
                    %put %str(    -->     7/(number of observations in &data or in each BY group, if [ by ] is specified).);
                    %put ;

                    %let cutoff = 7;
                %end;
                %else %if &outlier = RES %then %do;
                    %put %str(    --> Defaulting to 95, which identifies studentized residuals outside the 95th percentile.);
                    %put ;

                    %let cutoff = 95;
                %end;

            %goodCutoff:
                %if &outlier = COOKD
                    %then %let alpha = 0.05;
                %else %if &outlier = RES
                    %then %let alpha = %sysfunc(strip(%sysevalf(1 - &cutoff/100)));

          %*<prediction>;
            %local ELLIPSE CLI CLM;

            %if %nrbquote(&prediction) ne %then %do i = 1 %to %sysfunc(countw(&prediction));
                %let _prediction_ = %scan(%upcase(&prediction), &i);

                data _null_;
                    if "&_prediction_" not in ('ELLIPSE' 'CLM' 'CLI') then do;
                         put "    --> " '"' "&_prediction_" '" is an unacceptable value for [ prediction ].';
                         put "    -->     [ prediction ] can take a value of ELLIPSE and/or CLI." /;
                    end;
                    else if "&_prediction_" = 'ELLIPSE' then call symputx('ELLIPSE', '1  ');
                    else if "&_prediction_" = 'CLI'     then call symputx('CLI',     'CLI');
                    else if "&_prediction_" = 'CLM'     then call symputx('CLM',     'CLM');
                run;
            %end;

          %*<orientation>;
            %argumentCheck(orientation, LANDSCAPE PORTRAIT, LANDSCAPE);

          %*<width>;
            %if %nrbquote(&width) ne %then %do;
                %if %sysfunc(prxmatch(/[^\d.]/, %nrbquote(&width))) %then %do;
                    %if &Orientation = LANDSCAPE %then %do;
                        %put %str(    --> [ width ] contains non-numeric value.  Defaulting to 10.);
                        %let width = 10;
                    %end;
                    %else %do;
                        %put %str(    --> [ width ] contains non-numeric value.  Defaulting to 7.5.);
                        %let width = 7.5;
                    %end;
                %end;
            %end;
            %else %do;
                %if &Orientation = LANDSCAPE
                    %then %let width = 10;
                    %else %let width = 7.5;
            %end;

          %*<height>;
            %if %nrbquote(&height) ne %then %do;
                %if %sysfunc(prxmatch(/[^\d.]/, %nrbquote(&height))) %then %do;
                    %if &Orientation = LANDSCAPE %then %do;
                        %put %str(    --> [ height ] contains non-numeric value.  Defaulting to 7.5.);
                        %let height = 7.5;
                    %end;
                    %else %do;
                        %put %str(    --> [ height ] contains non-numeric value.  Defaulting to 10.);
                        %let height = 10;
                    %end;
                %end;
            %end;
            %else %do;
                %if &Orientation = LANDSCAPE
                    %then %let height = 7.5;
                    %else %let height = 10;
            %end;



           %*< sepbyvar >;  
            %argumentCheck(sepbyvar, YES NO Yes No yes no Y N y n, N);

            %if %nrbquote(&byVar) ne and %nrbquote(&sepbyvar) ne 
            %then %let sepbyvar = %upcase(%substr(%nrbquote(&sepbyvar), 1, 1));  /*&sepbyvar takes values of "Y" or "N"*/

            /*If missing BYVAR but not missing SEPBYVAR*/
            %if %nrbquote(&byVar)= and %nrbquote(&sepbyvar) ne  %then %do;
                %put %str(    --> [ BYVAR ] is missing.  SEPBYVAR will be set to missing.);

                %let sepbyvar = ;
            %end;
  
          %*<sepbysite>;
            %argumentCheck(sepbysite, YES NO Yes No yes no Y N y n, N);

            %if %nrbquote(&siteVar) ne and %nrbquote(&sepbysite) ne 
            %then %let sepbysite = %upcase(%substr(%nrbquote(&sepbysite), 1, 1));  /*&sepbysite takes values of "Y" or "N"*/

            /*If missing SITEVAR but not missing SEPBYSITE*/
            %if %nrbquote(&siteVar)= and %nrbquote(&sepbysite) ne  %then %do;
                %put %str(    --> [ SITEVAR ] is missing.  SEPBYSITE will be set to missing.);

                %let sepbysite = ;
            %end;



    %put;
    %put %nrstr(/-------------------------------------------------------------------------------------------------\);
    %put %nrstr(  PROG GLM running);
    %put %nrstr(\-------------------------------------------------------------------------------------------------/);
    %put;

        /***--------------------------------------------------------------------------------------\
          Print PROC GLM statements to log
        \--------------------------------------------------------------------------------------***/

            %put %str(    --> proc glm);
            %put %str(    -->     data = inputDataset;);
                %if %nrbquote(&byVar) ne %then
            %put %str(    -->     by &byVar;);
                %if %nrbquote(&discVar) ne %then
            %put %str(    -->     class &discVar;);
            %put %str(    -->     model);
            %put %str(    -->         &yVar = &xVar &discVar &contVar;);
            %put %str(    -->     output);
            %put %str(    -->         out = residuals);
            %put %str(    -->         rstudent = rStudentResid);
            %put %str(    -->         cookd = CookD);
            %put %str(    -->         h = Lev;);
            %put %str(    -->     ods output);
            %put %str(    -->         overallANOVA = DF %();
            %put %str(    -->             keep = Source DF &byVar);
            %put %str(    -->             where = (Source = 'Error')%););
            %put %str(    --> quit;);

        /***--------------------------------------------------------------------------------------\
          Input data manipulation
        \--------------------------------------------------------------------------------------***/

          %*Reduce &data to relevant variables, subset on non-missing &xVar and &yVar, and sort, if necessary.;
            proc sql;
                create table Inputdataset as
                    select %sysfunc(prxchange(%str(s/( +)/, /), -1, &IDvar &byVar &xVar &yVar &discVar &contVar &SiteVar)), count(1) as _FREQ_
                        from &data (where = (n(&yVar, &xVar) = 2))
                    %if &byVar ne %then group by %sysfunc(prxchange(%str(s/( +)/, /), -1, &byVar));;
            quit;

            %put %str(    --> Number of observations with non-missing &yVar and non-missing &xVar: &sqlobs..);
            %put ;

        /***--------------------------------------------------------------------------------------\
          PROC GLM;
        \--------------------------------------------------------------------------------------***/

            ods listing close;
                proc glm
                    data = inputDataset;
                    %if %nrbquote(&byVar) ne %then
                    by &byVar;;
                    %if %nrbquote(&discVar) ne %then
                    class &discVar;;
                    model
                        &yVar = %sysfunc(strip(&xVar &discVar &contVar));
                    output
                        out = residuals
                        rstudent = rStudentResid
                        cookd = CookD
                        h = Lev;
                    ods output
                        overallANOVA = DF (
                            keep = Source DF &byVar
                            where = (Source = 'Error'));
                quit;
            ods listing;

        /***--------------------------------------------------------------------------------------\
          Output data manipulation
        \--------------------------------------------------------------------------------------***/

            %if %nrbquote(&byVar) = %then %do;
                proc sql;
                    create table GLMdataset as
                        select residuals.*, DF.DF
                            from residuals,DF;
                quit;
            %end;
            %else %do;
                data GLMdataset;
                    merge
                        residuals
                        DF;
                    by &byVar;
                run;
            %end;

            
            /*Get variable names and labels for the axis labels - JD */
            data _null_;
               set GLMdataset(obs=1);
               if vlabel(&xvar.)^= vname(&xvar.) then call symputx("xvarlabel", vlabel(&xvar.)); 
               if vlabel(&yvar.)^= vname(&yvar.) then call symputx("yvarlabel", vlabel(&yvar.)); 
            run;


          %*Define outlier cutoffs, variables, and labels.;
            %macro outlierDataset;

             %*Create dataset for report (JD: This was previously done for outlierDataset&i below. I separated this to have an overall dataset for the listing) ;
                data GLMdataset_out;
                    set GLMdataset ;
                    if "&outlier" = 'COOKD' then CookD_cutoff = &cutoff/_FREQ_;
                    if "&outlier" = 'RES'   then RStud_cutoff = tinv(&cutoff/100, DF - 1);

                    if .z lt CookD_cutoff lt CookD or .z lt RStud_cutoff le abs(rStudentResid) then do;
                        &xVar._outlier = &xVar;
                        &yVar._outlier = &yVar;
                    end;
               run;

            %*Create dataset(s) for scatterplots;
                data outlierDataset&i;
                    set GLMdataset_out (where = (&bySubset));
                    if .z lt CookD_cutoff lt CookD or .z lt RStud_cutoff le abs(rStudentResid) then do;
                        %if &SiteVar = %then %do;
                                     outlier = catx('/', %if &IDvar ne %then &IDvar,;
                                                         putn(&xVar, vformat(&xVar)),
                                                         putn(&yVar, vformat(&yVar)));
                        %end;
                        %else %do;
                            if &SiteType = "%nrbquote(&Site)"
                                then outlier = catx('/', %if &IDvar ne %then &IDvar,;
                                                         putn(&xVar, vformat(&xVar)),
                                                         putn(&yVar, vformat(&yVar)));
                            else     outlier = catx('/', putn(&xVar, vformat(&xVar)),
                                                         putn(&yVar, vformat(&yVar)));
                        %end;
                   end;
               if ^missing(outlier) then call missing (&xvar, &yvar); /*JD: Only create one scatter point for outliers (prevents overlap)*/

               run;

            %mend outlierDataset;




    %put;
    %put %nrstr(/-------------------------------------------------------------------------------------------------\);
    %put %nrstr(  PROG SGPLOT running);
    %put %nrstr(\-------------------------------------------------------------------------------------------------/);
    %put;

        /***--------------------------------------------------------------------------------------\
          Print PROC SGPLOT statements to log
        \--------------------------------------------------------------------------------------***/

            %put %str(    --> proc sgplot);
            %put %str(    -->     data = outlierDataset;);
            %put %str(    -->     reg);
            %put %str(    -->         x = &xVar);
            %put %str(    -->         y = &yVar / &CLI &CLM);
            %put %str(    -->             alpha = &alpha;);
            %put %str(    -->             markerattrs = %();
            %put %str(    -->                 color = blue%));
            %put %str(    -->             lineattrs = %();
            %put %str(    -->                 color = green%));
                %if &ELLIPSE = 1 %then %do;
            %put %str(    -->     ellipse);
            %put %str(    -->         x = &xVar);
            %put %str(    -->         y = &yVar /);
            %put %str(    -->             alpha = &alpha);
            %put %str(    -->             transparency=0.5 lineattrs=(pattern=2);;);
                %end;
            %put %str(    -->     scatter);
            %put %str(    -->         x = &xVar._outlier);
            %put %str(    -->         y = &yVar._outlier /);
            %put %str(    -->             datalabel = outlier);
            %put %str(    -->             markerattrs = %();
            %put %str(    -->                 symbol = starfilled);
            %put %str(    -->                 color = red);
            %put %str(    -->                 size = 2mm%););
            %put %str(    -->                 legendlabel="Suspected outlier" ;);
            %put %str(    -->     xaxis);
            %put %str(    -->         offsetmax = .05;);
            %put %str(    -->     yaxis);
            %put %str(    -->         offsetmax = .05;);
            %put %str(    --> run;);

        /***--------------------------------------------------------------------------------------\
          PROC SGPLOT
        \--------------------------------------------------------------------------------------***/

            %let bivariatePlotFiles = ;

            %macro SGPLOT;

            proc sql noprint;
               select min(&xVar._outlier) into :minvar from outlierDataset&i;
            quit;

                    title1 j = c "&xVar by &yVar";
                %if       %nrbquote(&byVar)      ne %then
                    title2 j = c "%nrbquote(&byTitle)";
                %else %if %nrbquote(&SiteVar) ne %then
                    title2 j = c "Site %nrbquote(&Site)";;

                          footnote1 j = c " ";
                %if &outlier = COOKD
                    %then footnote2 j = c "Outliers are labeled Subject ID/x-value/y-value and identified by their Cook%str(%')s distance, a measure of the influence of a data point.";
                    %else footnote2 j = c "Outliers are labeled Subject ID/x-value/y-value and identified by their studentized residual, a measure of a data point%str(%')s distance from the regression line divided by its standard deviation.";;
                %if %nrbquote(&SiteVar) ne
                    %then footnote3 j = c "Only subjects from site %nrbquote(&SiteOut) are identified by Subject ID.";;

                options
                    orientation = &Orientation;
                ods listing
                    gpath = "%sysfunc(pathname(work))";

                ods results off;
                    ods graphics on /
                        reset     = all
                        border    = no
                        width     = &width  in
                        height    = &height in
                        imagename = "&outName";

                    %if %nrbquote(&byVar) ne %then %do;
                        ods rtf
                            file = "&outPath\&outName&i..rtf";
                        %let bivariatePlotFiles = &bivariatePlotFiles|&outName&i;
                    %end;
                    %else %if %nrbquote(&SiteVar) ne %then %do;
                        ods rtf
                            file = "&outPath\&outName - Site %nrbquote(&SiteOut)  Plot_&fdate..rtf";
                        %let bivariatePlotFiles = &bivariatePlotFiles|&outName - Site %nrbquote(&siteOut);
                    %end;
                    %else
                        ods rtf
                            file = "&outPath\&outName.  Plot_&fdate..rtf";;
                            proc sgplot
                                data = outlierDataset&i;
                                reg
                                    x = &xVar
                                    y = &yVar / &CLI &CLM
                                        alpha = &alpha
                                        markerattrs = (
                                            color = blue) 
                                        lineattrs = (
                                            color = green);
                            %if &ELLIPSE = 1 %then
                                ellipse
                                    x = &xVar
                                    y = &yVar /
                                        alpha = &alpha
                                 transparency=0.5 lineattrs=(pattern=2);;  

                                *If all outliers not missing*;
                            %if &minvar^=. %then %do; /*JD*/
                                scatter
                                    x = &xVar._outlier
                                    y = &yVar._outlier /
                                        datalabel = outlier
                                        markerattrs = (
                                            symbol = starfilled
                                            color = red
                                            size = 2mm)
                                        legendlabel="Suspected outlier"  ;
                                 %end;
                                xaxis
                                    offsetmax = .05
                                    /*Add in labels to axis - JD*/

/*                                       %if %nrbquote(&xlabel) ne %then label ="&xlabel." ;*/
                                       /*%else*/ %if %symexist(xvarlabel) %then label = "%upcase(&xvar.): &xvarlabel"; 
                                       %else label  = "%upcase(&xvar.)";
                                       ;

                                 yaxis
                                    offsetmax = .05
/*                                       %if %nrbquote(&ylabel) ne %then label ="&ylabel.";*/
                                       /*%else*/ %if %symexist(yvarlabel) %then label = "%upcase(&yvar.): &yvarlabel."; 
                                       %else label  = "%upcase(&yvar.)"; 
                                       ;
                            run;
                        ods rtf close;

                    ods graphics  off;
                ods results;

            %mend SGPLOT;

        /***--------------------------------------------------------------------------------------\
          By-group processing
        \--------------------------------------------------------------------------------------***/

            %if %nrbquote(&byVar) ne %then %do;
                proc sql noprint;
                    select       distinct catx('~', %sysfunc(prxchange(%str(s/( +)/, /), -1, &byVar))),
                           count(distinct catx('~', %sysfunc(prxchange(%str(s/( +)/, /), -1, &byVar))))
                      into      :ByVals  separated by '|',
                                :nByVals
                        from Inputdataset;
                quit;

                %put %str(    --> Number of BY values: %sysfunc(left(&nByVals)));
                %put ;

                %do i = 1 %to &nByVals;
                    %let ByVal   = %qscan(%nrbquote(&ByVals), &i, |);

                    data _null_;
                        if _n_ = 1 then do;
                            set GLMdataset;

                            length bySubset $1000;
                            call missing(bySubset);

                            %do j = 1 %to %sysfunc(countw(&byVar));
                                if vtype(%scan(&byVar, &j)) = 'N'
                                    then bySubset = catx(' and ', bySubset, catx(' = ',       scan("&byVar",    &j),
                                                                                              scan("&ByVal", &j, '~')));
                                    else bySubset = catx(' and ', bySubset, catx(' = ',       scan("&byVar",    &j),
                                                                                        quote(scan("&ByVal", &j, '~'))));
                                byTitle =
                                    prxchange('s/ and/,/', -1,
                                    prxchange('s/"//'    , -1, bySubset));
                            %end;

                            call symputx('bySubset', bySubset);
                            call symputx('byTitle ', byTitle );
                        end;
                        stop;
                    run;

                    %let ByMessage = ;

                    %put %str(    --> Processing data subset on &bySubset..);

                    %outlierDataset;

                    %SGPLOT;

                    %if &i = &nByVals %then %put ;
                %end;
            %end;
          %*Generate scatter plot for each site.;
            /*%else*/ %if %nrbquote(&SiteVar) ne %then %do;
                proc sql noprint;
                    select distinct &SiteVar,        count(distinct &SiteVar)
                      into :Sites separated by '|', :nSites
                        from Inputdataset;
                quit;

                %do i = 1 %to &nSites;
                    %let Site = %qscan(%nrbquote(&Sites), &i, |);

                    data _null_;
                                                SiteOut = prxchange('s/[<>:"\/\\\|\?\*)]+/_/', -1, "&Site");
                        call symputx('SiteOut', SiteOut);
                    run;

                    %put %str(    --> Processing site %nrbquote(&Site).);

                    %let bySubset = 1;
                    %outlierDataset;

                    %SGPLOT;

                    %if &i = &nSites %then %put ;
                %end;
            %end;
          %*Otherwise, generate a single plot.;
            %else %do;
                %let i        = 1;
                %let bySubset = 1;
                %outlierDataset;

                %SGPLOT;
            %end;

          %*Output dataset with outliers.;
            data _plots_.&xVar._&yVar;
                set outlierDataset:;
            run;

            %if &byVar ne %then %do;
                %stackRTFs
                    (directory = &activeDirectory
                    ,files = &bivariatePlotFiles
                    ,out = &outName. Plot_&fdate.);
            %end;

            %rtf2pdf7
                (in  = &activeDirectory\&&outName.  Plot_&fdate..rtf
                ,out = &activeDirectory\&&outName.  Plot_&fdate..pdf);


%***Create listing;


      /*Round all numeric variables to 2 decimal points*/
         proc sql noprint;
            select name into :numlist separated by ' ' from dictionary.columns
            where libname="WORK" and memname="GLMDATASET_OUT" and type = 'num';
         quit;
         %put &numlist;

      /*Dataset for report: only outliers*/
        data forReport_;
            set GLMdataset_out (where = (^missing(&xVar._outlier)));
   
            %local i next_var;
            %do i=1 %to %sysfunc(countw(&numlist)); 
            %let next_var = %scan(&numlist, &i);
            &next_var = round(&next_var, 0.01);  
         %end;
         run;


      %if %nrbquote(&byVar) ne or %nrbquote(&siteVar) ne %then %do;
         proc sort data=forReport_ out=forreport;
            by %if %nrbquote(&siteVar) ne %then &siteVar; %if %nrbquote(&byVar) ne %then &byvar;;
         run;

      %end; 

      %if %nrbquote(&byVar) = and %nrbquote(&siteVar) = %then %do;
         data forreport;
            set forReport_;
         run;
      %end;


      
   /*Get count of distinct byvars and sites with outliers*/
   /* byvar*/
    %if %nrbquote(&byvar) ne %then %do;
                proc sql noprint;
                    select       distinct   %sysfunc(prxchange(%str(s/( +)/, /), -1, &byVar)),
                           count (distinct   %sysfunc(prxchange(%str(s/( +)/, /), -1, &byVar)))
                      into      :ByVals_outlier  separated by '|',
                                :nbyvals_outlier
                        from forreport;
                quit;
      %end;

   /*sitevar*/
   %if %nrbquote(&sitevar) ne %then %do;
                proc sql noprint;
                    select distinct &SiteVar,        count(distinct &SiteVar)
                      into :Sites_outlier separated by '|', 
                           :nSites_outlier
                        from forreport;
                quit;
    %end;


*Create listing;


%let varlist=&discvar &contvar;

    title;
     footnote;

    %if %nrbquote(&sepbysite)^=Y %then %do;

     ods results off;
     ods rtf file = "&outPath\&outname. Outlier Listing_&fdate..rtf" style=printer;  
     title1 j = c "Outlier Listing: &xVar. by &yVar.";
      options nolabel VALIDVARNAME=UPCASE;
      proc report nowd data = forReport spanrows
                  ;
                  column 
                        %if %nrbquote(&SiteVar) ne %then &SiteVar;
                        %if %nrbquote(&byvar) ne %then &byvar; 
                        id &xVar._outlier  &yVar._outlier &varlist; 

                   %if %nrbquote(&SiteVar) ne %then define &SiteVar / display center order group
                    ;;
                   %if %nrbquote(&byvar) ne %then define &byvar / display center order group
                    ;;

                     define id / display center order
                        ; 
                    define &xVar._outlier / "&xvar." display center
                        ;  
                    define &yVar._outlier / "&yvar." display center
                        ;
                   %if &varlist ne %then %do;
                   %local i next_var;
                   %do i=1 %to %sysfunc(countw(&varlist)); 
                        %let next_var = %scan(&varlist, &i);;
                    define &next_var / display center
                      ;
                      %end;
                     %end;

                  %if %nrbquote(&SiteVar) ne %then
                     break after &SiteVar  /
                            page;;
                  %if &sepbyvar=Y %then
                     break after &byvar  /
                            page;;
                run;
                ods rtf close;
            ods results;

      %end;



      %*Separate listings by site (only sites with outliers);
      %else %if %nrbquote(&sepbysite)=Y %then %do;


      %macro listingbysite;

        %do i=1 %to &nsites_outlier;

         %let Site_ = %qscan(%nrbquote(&Sites_outlier),&i,|);
               data _null_;
                  SiteOut_ = prxchange('s/[<>:"\/\\\|\?\*)]+/_/', -1, "&Site_");
                  call symputx('SiteOut_', SiteOut_);
               run;

        data forReport&i;
            set forReport;
            where %nrbquote(&sitevar)="&Site_";
        run;

        ods results off;
        ods rtf file = "&outPath\&outname. Outlier Listing_Site %nrbquote(&SiteOut_)_&fdate..rtf" style=printer;  
        title1 j = c "Outlier Listing: &xVar. by &yVar.";
        title2 j=c "Site %nrbquote(&SiteOut_)";
         options nolabel VALIDVARNAME=UPCASE;
         proc report nowd data = forReport&i spanrows;
                     column 
                           %if %nrbquote(&byvar) ne %then &byvar; 
                           id &xVar._outlier  &yVar._outlier &varlist; 

                       %if %nrbquote(&byvar) ne %then define &byvar / display center order group
                       ;;

                       define id / display center order
                           ; 
                       define &xVar._outlier / "&xvar." display center
                           ;  
                       define &yVar._outlier / "&yvar." display center
                           ;
                      %if &varlist ne %then %do;
                      %local j next_var;
                      %do j=1 %to %sysfunc(countw(&varlist)); 
                           %let next_var = %scan(&varlist, &j);;
                       define &next_var / display center
                         ;
                         %end;
                        %end;

                     %if &sepbyvar=Y %then
                        break after &byvar  /
                               page;;
                   run;
                   ods rtf close;
               ods results;

      %end;
   %mend listingbysite;
   %listingbysite;
   %end;



%***EXCEL OUTPUT;

   data forexcel;
             retain &SiteVar &byVar id &xVar._outlier  &yVar._outlier &varlist; 
             set forreport; 
             keep &SiteVar &byVar id &xVar._outlier  &yVar._outlier &varlist;
    run;


  /*Byvar (sheets separated by byvar)*/;


   %if &byVar ne and &sepbyvar=Y and &sitevar= %then %do;
      %macro exportdataBY;

        %do i=1 %to &nbyvals_outlier;

            proc export data = forexcel (where=(&byVar ="%qscan(%nrbquote(&ByVals_outlier),&i,|)"))
                  outfile = "&outPath\&outname. Outliers_&fdate..xlsx"
                  dbms = excel  replace;
                  sheet = "%qscan(%nrbquote(&ByVals_outlier),&i,|)";
                  %if &i=1 %then newfile=yes;;
            run;
         %end;
      %mend exportdataBy;
      %exportdataBy;
   %end;

/*Site (sheets separated by site)*/
   %else %if %nrbquote(&sitevar) ne %then %do;


       %macro exportdataSITE;

        %do i=1 %to &nsites_outlier;

            %let Site_ = %qscan(%nrbquote(&Sites_outlier), &i, |);
               data _null_;
                  SiteOut_ = prxchange('s/[<>:"\/\\\|\?\*)]+/_/', -1, "&Site_");
                  call symputx('SiteOut_', SiteOut_);
               run;

            %if %nrbquote(&sepbysite)^=Y %then %do;

             proc export data = forexcel (where=(%nrbquote(&sitevar)= "%qscan(%nrbquote(&Sites_outlier),&i,|)"))
                  outfile = "&outPath\&outname. Outliers_&fdate..xlsx"
                  dbms = excel  replace;
                  sheet = "%qscan(%nrbquote(&Sites_outlier),&i,|)";
                  %if &i=1 %then newfile=yes;;
               run;
            %end;

           %if %nrbquote(&sepbysite)=Y %then %do;

            proc export data = forexcel (where=(%nrbquote(&sitevar)= "&Site_"))
                  outfile = "&outPath\&outname. Outliers_Site %nrbquote(&SiteOut_)_&fdate..xlsx"
                  dbms = excel  replace;
                  sheet="Outliers";
                  newfile=yes;
            run;


            data _null_;
               fname = 'todelete';
               rc = filename(fname, "&outPath\&outname. Outliers_Site %nrbquote(&SiteOut_)_&fdate..xlsx.bak");;
               rc = fdelete(fname);
               rc = filename(fname);
           run;

         %end;

       %end;
      %mend exportdataSITE;
      %exportdataSITE;
      %end;


/*Single file and sheet (whether or not byvar is missing)*/
   %else %if &sepbyvar^=Y and %nrbquote(&sitevar)= %then %do;

      proc export data = forexcel 
            outfile = "&outPath\&outname. Outliers_&fdate..xlsx"
            dbms = excel  replace;
            sheet = "Outliers";
            newfile=Yes;
      run;

   %end;

       options label validvarname=any;





    %exit:

    %put;
    %put %nrstr(/-------------------------------------------------------------------------------------------------\);
    %put %nrstr(  %bivariatePlot ending execution);
    %put %nrstr(\-------------------------------------------------------------------------------------------------/);
    %put;



%*Delete bak files.;

    data _null_;
      fname = 'todelete';
      rc = filename(fname, "&outPath\&outname. Outliers_&fdate..xlsx.bak");;
      rc = fdelete(fname);
      rc = filename(fname);
  run;



  %*Reset environment.;
    %if ^%sysfunc(libref(_plots_)) %then libname _plots_ clear;;
    options &xWaitOption &notesOption &dateOption &numberOption 
        ls = &lsOption;
    title;
    footnote;

%mend  bivariatePlot;

