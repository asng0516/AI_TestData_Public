       IDENTIFICATION DIVISION.
       PROGRAM-ID.  VSAMDEMO.
       AUTHOR.  MICRO FOCUS LTD.

      ****************************************************************
      * Copyright Micro Focus Limited 1992-98. All Rights Reserved.  *
      * This demonstration program is provided for use by users of   *
      * Micro Focus products and may be used, modified and           *
      * distributed as part of your application provided that you    *
      * properly acknowledge the copyright of Micro Focus in this    *
      * material.                                                    *
      ****************************************************************

      ****************************************************************
      *
      *            VSAMDEMO.CBL
      *            MAINFRAME SAMPLE PROGRAM
      *
      ***
      ****************************************************************
      *
      *         THIS PROGRAM CONTAINS TWO DELIBERATE SOURCE ERRORS, TO
      *         DEMONSTRATE SYNTAX ERROR HANDLING. THE TUTORIAL IN THE
      *         CHAPTER "USING MAINFRAME EXPRESS" IN THE GETTING STARTED
      *         BOOK SHOWS HOW TO FIX THESE ERRORS.
      *
      ****************************************************************
      *
      *         THE PURPOSE OF THIS PROGRAM IS TO PRESENT A MOCK
      *         SALES APPLICATION. IT PERFORMS I/O ON FIVE DATA FILES.
      *         IT ALSO DOES AN INTERNAL SORT AND THEREFORE USES A
      *         SORT WORK FILE.
      *
      *         INPUT FILES:                 OUTPUT FILES:
      *         ---------------------------------------------
      *            SALEDATA.DAT                 FOREIGN.DAT
      *            SALESINQ.DAT                 EASTVSAM.DAT
      *                                         WESTVSAM.DAT
      *
      *         NOTE THAT THESE ARE EBCDIC FILES, AND THAT THE OUTPUT
      *         FILES ARE STRUCTURALLY THE SAME AS MAINFRAME VSAM FILES.
      *
      *           THE FIRST FILE, SALES-DATA, IS A SEQUENTIAL FILE
      *         CONTAINING CURRENT RELATED SALES INFORMATION (E.G.
      *         WHICH SALES OFFICE AND SALES REP IS RESPONSIBLE FOR
      *         EACH STATE). THIS FILE IS FIRST SORTED, USING A COBOL
      *         INTERNAL SORT SO THAT IT MAY BE READ INTO TWO TABLES.
      *         BOTH TABLES (SALES-TABLE AND FOREIGN-TABLE) WILL
      *         CONTAIN INFORMATION SHOWING WHICH STATES ARE VALID
      *         AND WHICH SALES REP FROM WHICH OFFICE IS TO BE ASSIGNED
      *         A CUSTOMER INQUIRY. SALES-TABLE WILL CONTAIN INFO
      *         RELATED TO U.S. ACCOUNTS, WHILE FOREIGN-TABLE WILL
      *         CONTAIN INFO RELATED TO INTERNATIONAL ACCOUNTS.
      *
      *            THE NEXT FILE, INQUIRY, IS A VARIABLE
      *         LENGTH SEQUENTIAL FILE, AND HAS BEEN SUPPLIED WITH
      *         THIS PROGRAM. IT CONTAINS INQUIRIES MADE BY CUSTOMERS
      *         ON MICRO FOCUS PRODUCTS. SINCE A CUSTOMER MAY BE
      *         INTERESTED IN MORE THAN ONE PRODUCT, IT HAS A VARIABLE
      *         SIZED TABLE AT THE END OF THE RECORD FOR PRODUCTS.
      *
      *             THE THIRD FILE, FOREIGN-REGION, IS A VARIABLE LENGTH
      *         VSAM FILE THAT WILL BE AUTOMATICALLY CREATED BY THIS
      *         PROGRAM. VALID INQUIRIES FROM FOREIGN CUSTOMERS WILL
      *         BE WRITTEN OUT TO IT.
      *
      *            THE FOURTH AND FIFTH FILES, EAST-REGION AND
      *         WEST-REGION', ARE BOTH TRUE VARIABLE LENGTH VSAM
      *         FILES JUST LIKE FOREIGN-REGION. ALL OF THESE ARE
      *         AUTOMATICALLY CREATED BY THIS PROGRAM. THE PROGRAM
      *         WILL ALSO CREATE OTHER PHYSICAL FILES
      *         WHICH ARE THE VSAM INDEXES FOR EACH OF THE TWO VSAM
      *         FILES. THIS ILLUSTRATES THAT A MICRO FOCUS INDEXED
      *         FILE IS SIMILAR TO A MAINFRAME VSAM FILE CLUSTER IN
      *         PHYSICAL STRUCTURE, AND IS ALSO LOGICALLY EQUIVALENT.
      *
      *            THE PROGRAM WILL SORT THE SEQUENTIAL FILE
      *         SALES-DATA BY STATE, AND THEN READ RECORDS INTO TWO
      *         TABLES IN WORKING-STORAGE THAT WILL BE SEARCHED LATER,
      *         TO DETERMINE WHICH CUSTOMER INQUIRY SHOULD BE ASSIGNED
      *         TO WHICH SALES OFFICE AND SALES REP.
      *
      *            THE PROGRAM WILL NEXT OPEN THE SEQUENTIAL FILE
      *         'INQUIRY' AND READ EACH RECORD. UPON READING EACH
      *         RECORD, IT WILL THEN DETERMINE WHICH TABLE TO
      *         SEARCH TO VALIDATE THE INQUIRY AND DETERMINE WHICH
      *         VSAM FILE TO PLACE THE CUSTOMER INQUIRY. THE
      *         VSAM FILE EAST-REGION WILL CONTAIN INQUIRIES
      *         ASSIGNED TO THE EASTERN REGIONAL SALES OFFICE, WHILE
      *         THE VSAM FILE WEST-REGION WILL CONTAIN INQUIRIES
      *         ASSIGNED TO THE WESTERN REGIONAL SALES OFFICE.
      *
      *            ALL INTERNATIONAL INQUIRIES WILL BE PLACED IN THE
      *         VSAM FILE FOREIGN-REGION.
      ****************************************************************


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      * WORK FILE FOR COBOL INTERNAL SORT
           SELECT SORT-FILE    ASSIGN TO UT-S-SORTWRK.

      * INPUT FILE THAT CONTAINS SALESREP/TERRITORY INFORMATION
           SELECT SALES-DATA   ASSIGN UT-S-SALEDATA
               ORGANIZATION SEQUENTIAL.

      * INPUT FILE CONTAINING NEW CUSTOMER PRODUCT INQUIRIES
           SELECT INQUIRY      ASSIGN UT-S-INQUIRY
               ORGANIZATION SEQUENTIAL.

      * OUTPUT FILE FOR ILLEGAL INQUIRIES
           SELECT BAD-DATA    ASSIGN BADDATA
               ORGANIZATION SEQUENTIAL.

      * OUTPUT FILE FOR FOREIGN SALES REGION INQUIRIES
           SELECT FOREIGN-REGION ASSIGN FRGNVSAM
               ORGANIZATION INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS FOREIGN-CUST-NUM
               ALTERNATE RECORD KEY IS FOREIGN-SALESREP WITH DUPLICATES
               ALTERNATE RECORD KEY IS FOREIGN-STATE    WITH DUPLICATES.

      * OUTPUT FILE FOR EASTERN SALES REGION INQUIRIES
           SELECT EAST-REGION ASSIGN EASTVSAM
               ORGANIZATION INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS EAST-CUST-NUM
               ALTERNATE RECORD KEY IS EAST-SALESREP WITH DUPLICATES
               ALTERNATE RECORD KEY IS EAST-STATE    WITH DUPLICATES.

      * OUTPUT FILE FOR WESTERN SALES REGION INQUIRIES
           SELECT WEST-REGION ASSIGN WESTVSAM
               ORGANIZATION INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS WEST-CUST-NUM
               ALTERNATE RECORD KEY IS WEST-SALESREP WITH DUPLICATES
               ALTERNATE RECORD KEY IS WEST-STATE    WITH DUPLICATES.


       DATA DIVISION.
       FILE SECTION.

       COPY 'VSAMFS'.

       WORKING-STORAGE SECTION.

       COPY 'VSAMWS'.

       PROCEDURE DIVISION.

       MAINLINE-PARAGRAPH.
           PERFORM SORT-STATES.
           PERFORM PROCESS-DATA.
           STOP RUN.

       SORT-STATES.
           SORT SORT-FILE ON ASCENDING KEY SORT-STATE USING
                SALES-DATA OUTPUT PROCEDURE IS PROCESS-STATES.
       SORT-STATES-EXIT.
           EXIT.

       PROCESS-STATES.
           MOVE 1 TO SUBSCRPT F-SUBSCRPT PERFORM-FLAG.
           PERFORM RETURN-RECORDS UNTIL PERFORM-FLAG EQUAL TO 0.
       PROCESS-STATES-EXIT.
           EXIT.

       RETURN-RECORDS.
           RETURN SORT-FILE INTO TEMP-SORT-RECORD
                                 AT END MOVE 0 TO PERFORM-FLAG.
           IF PERFORM-FLAG EQUAL TO 1
               IF TEMP-STATE GREATER THAN 'ZZ'
                  MOVE TEMP-STATE        TO F-TABLE-STATE(F-SUBSCRPT)
                  MOVE TEMP-SALESREP     TO F-TABLE-SALESREP(F-SUBSCRPT)
                  MOVE TEMP-SALES-OFFICE TO F-TABLE-OFFICE(F-SUBSCRPT)
                  ADD 1 TO F-SUBSCRPT
               ELSE
                  MOVE TEMP-STATE        TO TABLE-STATE(SUBSCRPT)
                  MOVE TEMP-SALESREP     TO TABLE-SALESREP(SUBSCRPT)
                  MOVE TEMP-SALES-OFFICE TO TABLE-OFFICE(SUBSCRPT)
                  ADD 1 TO SUBSCRPT.
               IF SUBSCRPT GREATER THAN 51
                  MOVE 0 TO PERFORM-FLAG
                  DISPLAY 'STATE RECORDS EXCEED 51-IGNORED OTHERS'.
               IF F-SUBSCRPT GREATER THAN 25
                  MOVE 0 TO PERFORM-FLAG
                  DISPLAY 'FOREIGN RECORDS EXCEED 25-IGNORED OTHERS'.
       RETURN-RECORDS-EXIT.
           EXIT.

       PROCESS-DATA.
           OPEN INPUT INQUIRY.
           OPEN OUTPUT FOREIGN-REGION BAD-DATA EAST-REGION WEST-REGION.
           MOVE 1 TO PERFORM-FLAG.
           PERFORM PROCESS-INQUIRY THRU PROCESS-INQUIRY-EXIT
                                   UNTIL PERFORM-FLAG EQUAL TO 0.
           CLOSE INQUIRY FOREIGN-REGION
                 EAST-REGION WEST-REGION BAD-DATA.
       PROCESS-DATA-EXIT.
           EXIT.

       PROCESS-INQUIRY.
           READ INQUIRY
               AT END
               MOVE 0 TO PERFORM-FLAG
               GO TO PROCESS-INQUIRY-EXIT.
           IF INQY-STATE GREATER THAN 'WY'
               SET FOREIGN-INDEX TO 1
               SEARCH FOREIGN-INFO-TABLE AT END
                   MOVE 'T' TO BAD-DATA-CODE
                   PERFORM WRITE-BAD-DATA
                   GO TO PROCESS-INQUIRY-EXIT
               WHEN F-TABLE-STATE(FOREIGN-INDEX) EQUAL INQY-STATE
                   PERFORM WRITE-FOREIGN-RECORD
                   GO TO PROCESS-INQUIRY-EXIT.
           SET SALES-INDEX TO 1.
           SEARCH SALES-INFO-TABLE AT END
               MOVE 'T' TO BAD-DATA-CODE
               PERFORM WRITE-BAD-DATA
           WHEN TABLE-STATE(SALES-INDEX) EQUAL INQY-STATE AND
               TABLE-OFFICE (SALES-INDEX) EQUAL 'WEST'
                   PERFORM WRITE-WEST-RECORD
           WHEN TABLE-STATE(SALES-INDEX) EQUAL INQY-STATE AND
               TABLE-OFFICE (SALES-INDEX) EQUAL 'EAST'
                   PERFORM WRITE-EAST-RECORD.
       PROCESS-INQUIRY-EXIT.
           EXIT.

       WRITE-FOREIGN-RECORD.
           MOVE 20 TO FOREIGN-INTERESTS.
           MOVE SPACES TO FOREIGN-REGION-RECORD.
           MOVE INQY-CUST-NUM               TO FOREIGN-CUST-NUM.
           MOVE F-TABLE-SALESREP(FOREIGN-INDEX) TO FOREIGN-SALESREP.
           MOVE F-TABLE-OFFICE(FOREIGN-INDEX)   TO FOREIGN-OFFICE.
           MOVE INQY-COMPANY                TO FOREIGN-COMPANY.
           MOVE INQY-CUST-NAME              TO FOREIGN-CUST-NAME.
           MOVE INQY-ADDRESS-1              TO FOREIGN-ADDRESS-1.
           MOVE INQY-ADDRESS-2              TO FOREIGN-ADDRESS-2.
           MOVE INQY-CITY                   TO FOREIGN-CITY.
           MOVE INQY-STATE                  TO FOREIGN-STATE.
           MOVE INQY-AREA-CODE              TO FOREIGN-AREA-CODE.
           MOVE INQY-PHONE-1                TO FOREIGN-PHONE-1.
           MOVE INQY-PHONE-2                TO FOREIGN-PHONE-2.
           MOVE INQY-INTERESTS              TO FOREIGN-INTERESTS.
           MOVE INQY-PRODUCTS               TO FOREIGN-PRODUCTS.
           WRITE FOREIGN-REGION-RECORD INVALID KEY
               MOVE 'D' TO BAD-DATA-CODE
               PERFORM WRITE-BAD-DATA.
       WRITE-FOREIGN-RECORD-EXIT.
           EXIT.

       WRITE-EAST-RECORD.
           MOVE 20 TO EAST-INTERESTS.
           MOVE SPACES TO EAST-REGION-RECORD.
           MOVE INQY-CUST-NUM               TO EAST-CUST-NUM.
           MOVE TABLE-SALESREP(SALES-INDEX) TO EAST-SALESREP.
           MOVE TABLE-OFFICE(SALES-INDEX)   TO EAST-OFFICE.
           MOVE INQY-COMPANY                TO EAST-COMPANY.
           MOVE INQY-CUST-NAME              TO EAST-CUST-NAME.
           MOVE INQY-ADDRESS-1              TO EAST-ADDRESS-1.
           MOVE INQY-ADDRESS-2              TO EAST-ADDRESS-2.
           MOVE INQY-CITY                   TO EAST-CITY.
           MOVE INQY-STATE                  TO EAST-STATE.
           MOVE INQY-AREA-CODE              TO EAST-AREA-CODE.
           MOVE INQY-PHONE-1                TO EAST-PHONE-1.
           MOVE INQY-PHONE-2                TO EAST-PHONE-2.
           MOVE INQY-INTERESTS              TO EAST-INTERESTS.
           MOVE INQY-PRODUCTS               TO EAST-PRODUCTS.
           WRITE EAST-REGION-RECORD INVALID KEY
               MOVE 'D' TO BAD-DATA-CODE
               PERFORM WRITE-BAD-DATA.
       WRITE-EAST-RECORD-EXIT.
           EXIT.

       WRITE-WEST-RECORD.
           MOVE 20 TO WEST-INTERESTS.
           MOVE SPACES TO WEST-REGION-RECORD.
           MOVE INQY-CUST-NUM               TO WEST-CUST-NUM.
           MOVE TABLE-SALESREP(SALES-INDEX) TO WEST-SALESREP.
           MOVE TABLE-OFFICE(SALES-INDEX)   TO WEST-OFFICE.
           MOVE INQY-COMPANY                TO WEST-COMPANY.
           MOVE INQY-CUST-NAME              TO WEST-CUST-NAME.
           MOVE INQY-ADDRESS-1              TO WEST-ADDRESS-1.
           MOVE INQY-ADDRESS-2              TO WEST-ADDRESS-2.
           MOVE INQY-CITY                   TO WEST-CITY.
           MOVE INQY-STATE                  TO WEST-STATE.
           MOVE INQY-AREA-CODE              TO WEST-AREA-CODE.
           MOVE INQY-PHONE-1                TO WEST-PHONE-1.
           MOVE INQY-PHONE-2                TO WEST-PHONE-2.
           MOVE INQY-INTERESTS              TO WEST-INTERESTS.
           MOVE INQY-PRODUCTS               TO WEST-PRODUCTS.
           WRITE WEST-REGION-RECORD INVALID KEY
               MOVE 'D' TO BAD-DATA-CODE
               PERFORM WRITE-BAD-DATA.
       WRITE-WEST-RECORD-EXIT.
           EXIT.

       WRITE-BAD-DATA.
           MOVE INQY-CUST-NUM               TO BAD-CUST-NUM.
           MOVE INQY-COMPANY                TO BAD-COMPANY.
           MOVE INQY-CUST-NAME              TO BAD-CUST-NAME.
           MOVE INQY-ADDRESS-1              TO BAD-ADDRESS-1.
           MOVE INQY-ADDRESS-2              TO BAD-ADDRESS-2.
           MOVE INQY-CITY                   TO BAD-CITY.
           MOVE INQY-STATE                  TO BAD-STATE.
           MOVE INQY-AREA-CODE              TO BAD-AREA-CODE.
           MOVE INQY-PHONE-1                TO BAD-PHONE-1.
           MOVE INQY-PHONE-2                TO BAD-PHONE-2.
           MOVE INQY-INTERESTS              TO BAD-INTERESTS.
           MOVE INQY-PRODUCTS               TO BAD-PRODUCTS.
           WRITE BAD-DATA-RECORD.
           MOVE SPACES TO BAD-DATA-RECORD.
       WRITE-BAD-DATA-EXIT.
           EXIT.

       PROGRAM-TERMINATOR.
