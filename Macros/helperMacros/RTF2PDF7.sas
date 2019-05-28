/* ---------------------- Copyright 2012, Rho, Inc.  All rights reserved. ---------------------- */

/*-----------------------------------------------------------------------------------------------*/
/*
/*    Program:      RTF2PDF7
/*
/*      Purpose:    Convert .rtf files to .pdf files
/*
/*      Params:     in  - fully-qualified file path\name with .rtf extension
/*                  out - fully-qualified file path\name with .pdf extension
/*
/*      Input:      &In
/*
/*      Output:     &Out
/*
/*  Program History:
/*
/*      Date        Programmer          Description
/*      ----------  ------------------  ---------------------------------------------------------
/*      2012-10-08  Brandon Welch       Create
/*      2015-07-10  Spencer Childress   Update to generate temporary copy of .dotm file
/*      2015-11-01  Spencer Childress   Update logic
/*
/*-----------------------------------------------------------------------------------------------*/

%macro RTF2PDF7
    (In  = 
    ,Out = 
    );

    options noxwait xsync;

    %if %nrbquote(&In) = or %nrbquote(&Out) = %then %do;
        %put %str(--> Both the In and Out arguments must be specified. Execution terminating.);
        %goto exit;
    %end;
    %else %if ^%sysfunc(prxmatch(%str(/\.rtf *$/i), %nrbquote(&In ))) or
              ^%sysfunc(prxmatch(%str(/\.pdf *$/i), %nrbquote(&Out))) %then %do;
        %put %str(--> The In parameter does not have a .rtf extension or the Out parameter does not have a .pdf extension. Execution terminating.);
        %goto exit;
    %end;

    %if %sysfunc(fileexist(&In)) %then %do;
        %let tempDir  = %sysfunc(pathname(temp));
        %let dateTime = %sysfunc(prxchange(%str(s/-//), -1, %sysfunc(date(), yymmdd10.)))T%sysfunc(prxchange(%str(s/://), -1, %sysfunc(time(), time8.)));
        %let tempFile = &tempDir\&dateTime..dotm;

        data _null_;
            file "&tempDir\Path.txt";
             put "&In";
             put "&Out";
        RUN;

        %put %str(--> Temporary .dotm file: &tempFile);

        x %unquote(%nrbquote(')copy "S:\BASESTAT\RhoUtil\sas-statisticalDataChecks\Macros\helperMacros\RTF2PDF7.dotm"
                                    "&tempDir"%nrbquote('));
        x %unquote(%nrbquote(')ren  "&tempDir\RTF2PDF7.dotm"
                                    "&dateTime..dotm"%nrbquote('));

        %let rc = %sysfunc(system(start /WAIT /MIN winword "&tempFile" /mRTF2PDF7 /q /w && exit));

        x %unquote(%nrbquote(')del  "&tempFile"%nrbquote('));
    %end;
    %else %do;
        %put %str(--> &In does not exist. Execution terminating);
        %goto exit;
    %end;

    %exit:

%mend  RTF2PDF7;
