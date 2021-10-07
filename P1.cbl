       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM1.
       AUTHOR. TUCKER ARNOLD.
                      
      ***********************************************************
      *
      * This program is meant to help provide a report for Bennett
      * Shoes, a small company that sells high end shoes and boots
      * The report this program provides should help Bennett Shoes
      * determine what employees need to be given raises and which
      * and which employees have already recieved one within a year
      *
      *********
      * INPUT:
      *       The input file, named PR1F21-Knox.txt, contains the 
      *       following information:
      *       1. Store ID
      *       2. Employee ID
      *       3. Employee Position
      *       4. Employee Last Name
      *       5. Employee First Name
      *       6. Employee Middle Initial
      *       7. Hire Date
      *       8. Employee Status
      *       9. Seperation Date
      *       10. Starting Yearly Salary
      *       11. Date of Last Pay Increase
      *       12. Current Yearly Salary
      *
      *********
      * OUTPUT:
      *        The EMPLOYEE REPORT contains the following information:
      *****
      *        DETAIL LINE:
      *          1. EMPLOYEE ID
      *          2. EMPLOYEE POSITION
      *          3. EMPLOYEE FIRST NAME
      *          4. EMPLOYEE LAST NAME 
      *          5. EMPLOYEE STATUS
      *          6. LAST INCREASE (RAISE)
      *          7. CURRENT SALARY
      *****
      *        FINAL TOTALS:
      *          1. SALARY TOTAL
      *********
      * CALCULATIONS:
      *
      *   SALARY TOTAL =
      *        THE SUM OF CURRENT SALARY AND ANY INCREASE THE EMPLOYEE RECIEVED  
      ***********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

         SELECT INPUT-FILE
           ASSIGN TO 'PR1FA21-Knox.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

         SELECT REPORT-FILE
           ASSIGN TO PRINTER 'REPORT'.

        DATA DIVISION.
        FILE SECTION.

         FD INPUT-FILE
          RECORD CONTAINS 75 CHARACTERS.
         01 INPUT-RECORD.
          05 RF-STORE-ID                        PIC A(4).
          05 RF-EMPLOYEE-ID                     PIC X(5).
          05 RF-EMPLOYEE-POS                    PIC A(2).
          05 RF-EMPLOYEE-LAST-NAME              PIC X(10).
          05 RF-EMPLOYEE-FIRST-NAME             PIC X(10).
          05 RF-EMPLOYEE-MIDDLE-INITIAL         PIC X(1).
          05 FILLER                             PIC X(2).
          05 RF-HIRE-DATE                       PIC 9(8).
          05 RF-EMPLOYEE-STATUS                 PIC A(1).
          05 FILLER                             PIC X(8).
          05 RF-STARTING-YEARLY-SALARY          PIC 9(8).
          05 RF-DATE-LAST-PAY-INCREASE          PIC 9(8).
          05 RF-CURRENT-YEARLY-SALARY           PIC 999999V99.

         FD REPORT-FILE
          RECORD CONTAINS 80 CHARACTERS.

         01 REPORT-LINE        PIC X(80).

         WORKING-STORAGE SECTION.

         01 FLAGS-N-SWITCHES.
           05 EOF-FLAG           PIC X  VALUE ' '.
              88 OUT-DATA        VALUE 'N'.

         01 TOTAL-SAL.
           05 TS-SALARY-TOTAL    PIC S9(7)V99    VALUE +0.

        01 SPACING.
           05 PROPER-SPACING     PIC S9          VALUE +2.

      ********** OUTPUT AREA **********

         01 HEADING-ONE.
           05 H1-DATE PIC 9999/99/99.             
           05         PIC X(25)   VALUE SPACES.
           05         PIC A(13)   VALUE 'BENNETT SHOES'.
           05         PIC X(20)   VALUE SPACES. 
           05         PIC XXX    VALUE 'TCA'.                
         01 HEADING-TWO.
            05        PIC X(34)   VALUE SPACES.    
            05        PIC X(15)   VALUE 'EMPLOYEE REPORT'.
         01 HEADING-THREE.
            05        PIC X(35)   VALUE SPACES.
            05        PIC X(13)   VALUE 'KNOXVILLE, TN'.
         01 HEADING-FOUR.
            05        PIC X(3)    VALUE SPACES.
            05        PIC X(3)    VALUE 'EMP'.
            05        PIC X(4)    VALUE SPACES.
            05        PIC X(3)    VALUE 'EMP'.
            05        PIC X(6)    VALUE SPACES.
            05        PIC X(3)    VALUE 'EMP'.
            05        PIC X(9)    VALUE SPACES.
            05        PIC X(3)    VALUE 'EMP'.
            05        PIC X(8)    VALUE SPACES.
            05        PIC X(3)    VALUE 'EMP'.
            05        PIC X(6)    VALUE SPACES.
            05        PIC X(4)    VALUE 'LAST'.
            05        PIC X(7)    VALUE SPACES.
            05        PIC X(8)    VALUE 'CURRENT'.
         01 HEADING-FIVE.
            05        PIC X(3)    VALUE SPACES.
            05        PIC X(2)    VALUE 'ID'.
            05        PIC X(5)    VALUE SPACES.
            05        PIC X(3)    VALUE 'POS'.        
            05        PIC X(2)    VALUE SPACES.
            05        PIC X(10)   VALUE 'FIRST NAME'.
            05        PIC X(3)    VALUE SPACES.
            05        PIC X(9)    VALUE 'LAST NAME'.
            05        PIC X(3)    VALUE SPACES.
            05        PIC X(6)    VALUE 'STATUS'.
            05        PIC X(3)    VALUE SPACES.
            05        PIC X(8)    VALUE 'INCREASE'.
            05        PIC X(6)    VALUE SPACES.
            05        PIC X(6)    VALUE 'SALARY'.
         01 DETAIL-LINE.
            05        PIC X(2)    VALUE SPACES.
            05        DL-EMP-ID          PIC X(5).
            05        PIC X(3)    VALUE SPACES.
            05        DL-EMP-POS         PIC X(2).
            05        PIC X(3)    VALUE SPACES.
            05        DL-FIRST-NAME      PIC X(10).
            05        PIC X(3)    VALUE SPACES.
            05        DL-LAST-NAME       PIC X(10).
            05        PIC X(3)    VALUE SPACES.
            05        DL-STATUS          PIC A(1).
            05        PIC X(5)    VALUE SPACES.
            05        DL-LAST-INCREASE   PIC 99/99/9999.
            05        PIC X(3)    VALUE SPACES.  
            05        DL-CURRENT-SALARY  PIC $999,999.99.
         01 TOTAL-LINE.
            05  FILLER      PIC X(44)   VALUE SPACES.
            05              PIC X(13)   VALUE 'SALARY TOTAL:'.
            05              PIC X(1)    VALUE SPACES.
            05        TL-SALARY-TOTAL    PIC $9,999,999.99.

       PROCEDURE DIVISION.

            100-MAIN-MODULE.
              
              PERFORM 125-HOUSEKEEPING
              PERFORM 150-READ-RECORDS
              PERFORM 250-END-ROUTINE
              .
              
            125-HOUSEKEEPING.
              OPEN   INPUT  INPUT-FILE
                  OUTPUT  REPORT-FILE 
              ACCEPT H1-DATE FROM DATE YYYYMMDD
              PERFORM 140-WRITE-HEADER
                  .

            140-WRITE-HEADER. 
                WRITE REPORT-LINE FROM HEADING-ONE
              AFTER ADVANCING 2 LINES

              MOVE 2 TO PROPER-SPACING

                WRITE REPORT-LINE FROM HEADING-TWO
              AFTER ADVANCING 2 LINES
                WRITE REPORT-LINE FROM HEADING-THREE
              AFTER ADVANCING 2 LINES
                WRITE REPORT-LINE FROM HEADING-FOUR
              AFTER ADVANCING 2 LINES
              
              MOVE 1 TO PROPER-SPACING
                WRITE REPORT-LINE FROM HEADING-FIVE
              AFTER ADVANCING PROPER-SPACING
              MOVE 2 TO PROPER-SPACING
              .        

            150-READ-RECORDS.
              PERFORM UNTIL OUT-DATA
                READ INPUT-FILE
                  AT END
                    MOVE 'N' TO EOF-FLAG
                  NOT AT END                    
                      PERFORM 200-MOVE-DATA
                  END-READ
                END-PERFORM
                  .
              
            200-MOVE-DATA.             
                MOVE RF-EMPLOYEE-ID TO DL-EMP-ID
                MOVE RF-EMPLOYEE-POS TO DL-EMP-POS
                MOVE RF-EMPLOYEE-FIRST-NAME TO DL-FIRST-NAME
                MOVE RF-EMPLOYEE-LAST-NAME TO DL-LAST-NAME
                MOVE RF-EMPLOYEE-STATUS TO DL-STATUS
                MOVE RF-DATE-LAST-PAY-INCREASE TO DL-LAST-INCREASE
                MOVE RF-CURRENT-YEARLY-SALARY TO DL-CURRENT-SALARY
                MOVE DETAIL-LINE TO REPORT-LINE
                PERFORM 225-WRITE-LINE
                MOVE 1 TO PROPER-SPACING.
                ADD RF-CURRENT-YEARLY-SALARY TO TS-SALARY-TOTAL
                .

            225-WRITE-LINE.    
                WRITE REPORT-LINE
                    AFTER ADVANCING PROPER-SPACING                
                .

            250-END-ROUTINE.
            PERFORM 300-TOTAL-SALARY
              CLOSE INPUT-FILE
                REPORT-FILE 
              STOP RUN
              .

            300-TOTAL-SALARY.
                MOVE TS-SALARY-TOTAL TO TL-SALARY-TOTAL

                MOVE 2 TO PROPER-SPACING
                
                WRITE REPORT-LINE FROM TOTAL-LINE
                  AFTER ADVANCING PROPER-SPACING
                .
