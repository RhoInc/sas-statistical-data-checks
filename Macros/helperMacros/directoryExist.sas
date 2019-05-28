%macro directoryExist
    (directory
    ,libName);

    %local SASFile;
    %global directoryExist activeDirectory;
    %let directoryExist = 1;

    options nonotes;

    /**-------------------------------------------------------------------------------------------\
      <directory> unspecified:
    \-------------------------------------------------------------------------------------------**/

        %if %nrbquote(&directory) = %then %do;
            %if %nrbquote(&directory) =
                %then %put %str(    --> <directory> parameter unspecified.  Attempting to determine calling program directory.);

            %if %symexist(rgv_currfolder)
                %then %let SASFile = &rgv_currfolder&rgv_currfile;
            %else %if %sysfunc(prxmatch(/^program %str(%")/i, &SysProcessName))
                %then %let SASFile = %scan(&SysProcessName, 2, %str(%"));
            %else %if %sysfunc(prxmatch(/^dms/i, &SysProcessName))
                %then %let SASFile = %sysget(SAS_ExecFilePath);

            %if %nrbquote(&SASFile) = %then %do;
                %put %str(    -->     Unable to determine calling program directory. Please specify <directory> parameter.);
                %put %str(    -->     Execution terminating.);

                %let directoryExist = 0;
                %goto exitDirectoryExist;
            %end;
            %else %do;
                %let activeDirectory = %substr(&SASFile, 1, %length(&SASFile) - %length(%scan(&SASFile, -1, \)) - 1);

                %put %str(    -->     Defaulting to calling program directory:);
                %put %str(    -->     &directory..);
            %end;
        %end;
        %else %do;
            %let activeDirectory = &&&directory;

            %if not %sysfunc(fileexist(%nrbquote(&activeDirectory))) %then %do;
                %put %str(    --> &activeDirectory does not exist.  Attempting to create.);
                %sysexec mkdir "&activeDirectory";
            %end;
        %end;

    /**-------------------------------------------------------------------------------------------\
      Setting active directory, capture directory contents, and assigning LIBNAME to directory:
    \-------------------------------------------------------------------------------------------**/

        %if %sysfunc(fileexist(&activeDirectory)) %then %do;
            %put %str(    --> Active directory is set to &activeDirectory..);

            %sysexec %substr(&activeDirectory, 1, 2);
            %sysexec cd &activeDirectory;
        %end;
        %else %do;
            %let directoryExist = 0;
            %goto exitDirectoryExist;
        %end;

        %put %str(    -->     WORK.CONTENTS contains contents of &activeDirectory..);

        fileName contents pipe %unquote(%nrbquote(')dir /a "&activeDirectory\*"%nrbquote('));
            data contents;
                infile contents pad
                    lrecl = 100
                    end = eof;
                input @1 fileInfo $100.;

                date = scan(fileInfo, 1, ' ');
                time = scan(fileInfo, 2, ' ');
                ampm = scan(fileInfo, 3, ' ');
                size = scan(fileInfo, 4, ' ');
                file = substr(fileInfo, index(fileInfo, strip(size)) + length(size) + 1);
                ext  = lowcase(ifc(index(fileInfo, '.'), scan(fileInfo, -1, '.'), ' '));
            run;
        fileName contents clear;

        %if %sysfunc(prxmatch(%str(/^[a-z_][a-z0-9_]{0,7}$/i), %nrbquote(&libName))) %then %do;
            %put %str(    -->     LIBNAME &libName points to &activeDirectory..);

            libname &libName "&activeDirectory";
        %end;
        %else %if %nrbquote(&libName) ne %then %put %str(    -->     &libName is an invalid LIBNAME name.);

    %exitDirectoryExist:

    options notes;

%mend  directoryExist;
