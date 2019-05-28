/*----------------------- Copyright 2016, Rho, Inc.  All rights reserved. ------------------------\

    Program:      stackRTFs.sas
 
      Purpose:    Stack .rtf files
 
      Input:      All files in [ directory ] or [ files ]
 
      Output:     [ out ]
 
    /---------------------------------------------------------------------------------------------\
      Parameters:
    \---------------------------------------------------------------------------------------------/

        [ directory ]   - directory containing .rtf files
        [ files ]       - |-delimited list of partially- or fully-qualified .rtf files
                            > if partially-qualified, [ directory ] must be specified
        [ out ]         - partially- or fully-qualified output .rtf file
                            > if partially-qualified, [ directory ] must be specified
        [ deleteRTFs ]  - delete input .rtf files?
                            > Yes/1, case insensitive, will delete input .rtf files

/-------------------------------------------------------------------------------------------------\
  Program history:
\-------------------------------------------------------------------------------------------------/

    Date        Programmer          Description
    ----------  ------------------  --------------------------------------------------------------
    2016-04-09  Spencer Childress   Create

\------------------------------------------------------------------------------------------------*/

%macro stackRTFs
    (directory = 
    ,files = 
    ,out = 
    ,deleteRTFs = Yes) / minoperator;

    %local nRTFs i _RTFs_;

    %put;
    %put %nrstr(/-------------------------------------------------------------------------------------------------\);
    %put %nrstr(  %stackRTFs beginning execution);
    %put %nrstr(\-------------------------------------------------------------------------------------------------/);
    %put;

        /***--------------------------------------------------------------------------------------\
          Parameter error checking
        \--------------------------------------------------------------------------------------***/

            %*[ directory ];
                %if %sysfunc(fileexist(%nrbquote(&directory))) %then %do;
                    %sysexec %substr(&directory, 1, 2);
                    %sysexec cd &directory;
                %end;
                %else %if %nrbquote(&directory) ne %then %do;
                    %put %str(    --> [ &directory ] does not exist.);
                    %put %str(    -->     Execution terminating.);
                    %goto exit;
                %end;
            %*[ files ];
                %else %if %nrbquote(&files) = %then %do;
                    %put %str(    --> Neither [ directory ] nor [ files ] specified.  Execution terminating.);
                    %goto exit;
                %end;

                %if %nrbquote(&files) ne %then %let files = %sysfunc(prxchange(s/^\|+//, 1, %nrbquote(&files)));

            %*[ out ];
                %if %nrbquote(&out) = %then %do;
                    %put %str(    --> [ out ] not specified.  Execution terminating.);
                    %goto exit;
                %end;
                %else %if not %sysfunc(prxmatch(/^[a-z]:/i, %nrbquote(&out))) %then %let out = &directory\&out;

                %if not %sysfunc(prxmatch(/\.rtf *$/i, %nrbquote(&out))) %then %let out = &out..rtf;

        /***--------------------------------------------------------------------------------------\
          Capture .rtf files to stack
        \--------------------------------------------------------------------------------------***/

            %if %nrbquote(&directory) ne %then %do;
                fileName contents pipe %unquote(%nrbquote(')dir /a "&directory\*"%nrbquote('));
                    data contents (where = (upcase(ext) = 'RTF'));
                        infile contents pad lrecl = 100 end = eof;
                        input @1 fileInfo $100.;

                        date = scan(fileInfo, 1, ' ');
                        time = scan(fileInfo, 2, ' ');
                        ampm = scan(fileInfo, 3, ' ');
                        size = scan(fileInfo, 4, ' ');
                        file = substr(fileInfo, index(fileInfo, strip(size)) + length(size) + 1);
                        ext  = ifc(index(fileInfo, '.'), scan(fileInfo, -1, '.'), ' ');
                        name = strip(tranwrd(file, cats('.', ext), ' '));
                    run;
                fileName contents clear;

                proc sql noprint;
                    select catx('\', "&directory", file)
                        into :file1-:file9999
                            from contents %if %nrbquote(&files) ne %then
                              where upcase(file) in (%upcase("%sysfunc(prxchange(%nrbquote(s/\|/" "/), -1, %nrbquote(&files)))"))
                                 or upcase(name) in (%upcase("%sysfunc(prxchange(%nrbquote(s/\|/" "/), -1, %nrbquote(&files)))"));;
                quit;

                %let nRTFs = &SQLobs;
            %end;
            %else %do i = 1 %to %sysfunc(countw(%nrbquote(&files), |));
                    %let file&i = %scan(%nrbquote(files), &i, |);
                %if %nrbquote(&directory) ne and not %sysfunc(prxmatch(/^[a-z]:/i, %nrbquote(&&file&i))) %then
                    %let file&i = &directory\&file;
                %if not %sysfunc(prxmatch(/\.rtf *$/i, %nrbquote(&&file&i))) %then
                    %let file&i = &&file&i..rtf;

                %let nRTFs = &i;
            %end;

            %let _RTFs_ = ;
            %let nPARs = 0;

            %do i = 1 %to &nRTFs;
                %if %sysfunc(fileexist(%nrbquote(&&file&i))) %then %do;
                    %let _RTFs_ = &_RTFs_ _rtf_&i (in = rtf&i);

                    data _rtf_&i;
                        length fname&i $1000;

                        infile "&&file&i"
                            length = linelen
                            fileName = fname&i
                            lrecl = 32767;

                        input lineIn $varying2000. linelen;

                        fileName = fname&i;
                        fileNumber = &i;
                    run;

                    proc sql noprint;
                        select count(1)
                          into :nPARs&i separated by ' '
                            from _rtf_&i
                              where prxmatch('/^{\\par}/i', lineIn);
                    quit;

                    %let nPARs = %eval(&nPARs + &&nPARs&i);

                    %if %upcase(&deleteRTFs) in YES 1 %then %sysexec del "&&file&i";
                %end;
                %else %put %str(    --> [ &&file&i ] does not exist.);
            %end;

        /***--------------------------------------------------------------------------------------\
          Stack and output .rtf files
        \--------------------------------------------------------------------------------------***/

            data _RTFs_;
                set &_RTFs_
                    end = eof;
                by fileNumber;

                if rtf1 then do;
                    if last.fileNumber then lineIn = substr(lineIn, 1, length(lineIn) - 1);
                end;
                else do;
                    if first.fileNumber then lineIn = '\page' || lineIn;
                end;

                if eof then do;
                    lineIn = cats(lineIn, '}');
                end;

              %*Remove {\par} tags from RTF if it is neither the first nor last.;
                retain nPAR 0;
                if prxmatch('/^{\\par}/i', lineIn) then do;
                    nPAR + 1;
                    lineIn = prxchange('s/^{\\par}//i', 1, lineIn);
                end;
            run;

            data _null_;
                set _RTFs_;

                file "&out"
                    lrecl = 2000;

                linelen = length(lineIn);
                put lineIn $varying2000. linelen;
            run;

        %exit:

    %put;
    %put %nrstr(/-------------------------------------------------------------------------------------------------\);
    %put %nrstr(  %stackRTFs ending execution);
    %put %nrstr(\-------------------------------------------------------------------------------------------------/);
    %put;

%mend  stackRTFs;
