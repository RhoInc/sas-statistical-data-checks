/*----------------------------------------------------------------------------------*

   *******************************************************
   *** Copyright 2015, Rho, Inc.  All rights reserved. ***
   *******************************************************

   MACRO:      adlib_rtf2pdf

   PURPOSE:    Convert an RTF file to PDF using Adlib

   ARGUMENTS:  rtfFile => <rtf file>    fully qualified Input RTF file name (REQUIRED)
               pdfFile => <pdf file>    fully qualified Output PDF file name (REQUIRED)
               keepXML => <NO/YES>      keep the .XML instruction file? (defaults to NO)

   CALLS:      %getUNC (autocall)

   DETAILS:    The .xml instruction file is created in the output .pdf directory with
               the pdfFile= file name and an .xml extension.  It is deleted upon
               successful completion of the macro, unless keepXML=YES.



         MODIFIED Adlib Input Directory FOR TESTING ADLIB ENTERPRISE

         %INCLUDE 'T:\StatProg\Adlib_Test\Macros\adlib_rtf2pdf.sas' IN THE SAS PROGRAM




   PROGRAM HISTORY:

   Date        Programmer        Description
   ---------   ---------------   ----------------------------------------------------
   15Jul2015   John Ingersoll    original
   11Jul2017   John Ingersoll    [U001] add userid and macro name to the xml job ticket filename
   11Jul2017   John Ingersoll    [U002] update for Adlib Enterprise folder

*-----------------------------------------------------------------------------------*/
%macro adlib_rtf2pdf( rtfFile=, pdfFile=, keepXML=NO );

   %local rtfDir rtfFn rtfExt pdfDir pdfFn pdfExt xmlFile;

   %*----------------------------------------------------;
   %* Set SAS Options for this macro                     ;
   %*----------------------------------------------------;
   %local xwaitOpt;
   %let xwaitOpt = %sysfunc(getoption(XWAIT));
   options noxwait;


   %*----------------------------------------------------;
   %* Process the keepXML= Parameter                     ;
   %*----------------------------------------------------;
   %local copyMove;
   %let keepXML = %upcase(&keepXML);
   %if &keepXML = YES %then %let copyMove = copy;
   %else                    %let copyMove = move;


   %*----------------------------------------------------;
   %* Process the File Parameters                        ;
   %*----------------------------------------------------;
   %local i type fileSpec dlm unc;
   %do i=1 %to 2;

      %let type = %scan( rtf pdf, &I );
      %let fileSpec = &&&type.File;

      %*--- Remove Quotes from File Spec ---*;
      %let fileSpec = %qscan(&fileSpec,1,%str(%"%'));

      %*--- Empty File Spec ---*;
      %if %nrbquote(&fileSpec) = %then %do;
         %put;
         %put %nrstr(%%)adlib_rtf2pdf => ERROR: The &type.File= parameter is missing. ;
         %put;
         %goto ENDMAC;
      %end;

      %*--- Convert / to \ ---*;
      %let fileSpec = %sysfunc( translate(&fileSpec,\,/) );

      %*--- Parse the Directory, Filename, and Extension ---*;
      %let dlm = %sysfunc( findc(&fileSpec,\,B) );
      %if &dlm = 0 or &dlm = %length(&fileSpec) %then %do;
         %put;
         %put %nrstr(%%)adlib_rtf2pdf => ERROR: Incomplete &type.File= parameter. ;
         %put %nrstr(%%)adlib_rtf2pdf => ERROR: It must be a fully qualified file specification. ;
         %put;
         %goto ENDMAC;
      %end;
      %let &type.Dir = %substr( &fileSpec, 1, &dlm );
      %let &type.Fn  = %substr( &fileSpec, %eval(&dlm+1) );
      %if %sysfunc( indexc(&&&type.Fn,.) ) > 0 %then %do;
         %let &type.Ext = %sysfunc( scan(&&&type.Fn,-1,.) );
         %let &type.Fn  = %sysfunc( tranwrd(&&&type.Fn,.&&&type.Ext,) );
      %end;

      %*--- Construct .xml filename from PDF filespec (before converting drive letter) ---*;
      %if &type = pdf %then %let xmlFile = &pdfDir.&SYSUSERID..&pdfFn..adlib_rtf2pdf.xml;          %* [U001] ;

      %*--- Convert Drive Letters to Network Name ---*;
      %if %length(&&&type.Dir) > 2 %then
         %if %substr(&&&type.Dir,2,1) = : %then %do;
            %let unc = %getUNC(%substr(&&&type.Dir,1,1));
            %let &type.Dir = &unc%substr(&&&type.Dir,3);
         %end;
      %if &unc = %then %do;
         %put;
         %put %nrstr(%%)adlib_rtf2pdf => ERROR: Incomplete &type.File= parameter. ;
         %put %nrstr(%%)adlib_rtf2pdf => ERROR: It must be a fully qualified file specification. ;
         %put;
         %goto ENDMAC;
      %end;

   %end;

   %*--- Make sure Input RTF File exists ---*;
   %if %sysfunc( fileexist("&rtfFile") ) = 0 %then %do;
      %put;
      %put %nrstr(%%)adlib_rtf2pdf => ERROR: The rtfFile= input file does not exist. ;
      %put %nrstr(%%)adlib_rtf2pdf => ERROR: (&rtfFile) ;
      %put;
      %goto ENDMAC;
   %end;

   %*--- Make sure Output PDF Directory exists ---*;
   %if %sysfunc( fileexist("&pdfDir") ) = 0 %then %do;
      %put;
      %put %nrstr(%%)adlib_rtf2pdf => ERROR: The pdfFile= directory does not exist. ;
      %put %nrstr(%%)adlib_rtf2pdf => ERROR: (&pdfDir) ;
      %put;
      %goto ENDMAC;
   %end;



   *=================================;
   * Create the XML Instruction File ;
   *=================================;
   data _null_;

      file "&xmlFile";

      *--- Put XML Header (fixed) ---*;
      put '<?xml version="1.0" encoding="ISO-8859-1" ?>';
      put '<?AdlibExpress applanguage="USA" ?>';
      put '<!DOCTYPE JOBS SYSTEM "\\chh-ps1\adlib\System\DTD\AdlibExpress.dtd">';
      put '<JOBS xmlns:JOBS="http://www.adlibsoftware.com" xmlns:JOB="http://www.adlibsoftware.com">';
      put '<JOB>';
      put ;

      *--- Specify Input RTF ---*;
      put '<JOB:DOCINPUTS>';
      put "   <JOB:DOCINPUT FOLDER=""&rtfDir"" FILENAME=""&rtfFn..&rtfExt"" />";
      put '</JOB:DOCINPUTS>';
      put ;

      *--- Specify Output PDF ---*;
      put '<JOB:DOCOUTPUTS>';
      put "   <JOB:DOCOUTPUT DOCTYPE=""PDF"" FOLDER=""&pdfDir"" FILENAME=""&pdfFn..&pdfExt"" />";
      put '</JOB:DOCOUTPUTS>';
      put ;

      *--- Start Job Settings (fixed) ---*;
      put '<JOB:SETTINGS>';
      put ;

      *--- MS Word Conversion Options (fixed) ---*;
      put '<JOB:NATIVEAPPSETTINGS>';
      put '   <JOB:MSWORD ALLOWA4LETTERPAPERRESIZE="No" USEHTMLPARAGRAPHAUTOSPACING="No" USEPRINTERMETRICS="No" />';
      put '</JOB:NATIVEAPPSETTINGS>';
      put ;

      *--- PDF Settings (fixed) ---*;
      put '<JOB:PDFSETTINGS PDFVERSION="PDFVersion14" CONVERTUSINGNATIVEAPPLICATIONSUPPORT="Yes">';
      put '   <JOB:FONTEMBEDDING ENABLED="Yes" MULTILANGUAGEFONTS="Yes" PARTIALFONTS="Yes" STANDARDFONTS="No" />';
      put '   <JOB:OPENSETTINGS  ENABLED="Yes" PAGEMODE="Pdf.PageMode" />';
      put '</JOB:PDFSETTINGS>';
      put ;

      *--- End of Job (fixed) ---*;
      put '</JOB:SETTINGS>';
      put '</JOB>';
      put '</JOBS>';

   run;


   *==========================================;
   * Submit the XML Instruction File to Adlib ;
   *==========================================;
   x "&copyMove /Y ""&xmlFile"" ""I:\RHO_PUB\PDF_AdLib\ent.input\"" ";           %* [U002] ;


   %ENDMAC:

   %*----------------------------------------------------;
   %* Reset SAS Options to original settings             ;
   %*----------------------------------------------------;
   options &xwaitOpt;

%mend;
