       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKING.
      *>******************************************************************
      *> This program processes a single banking transaction from an
      *> input file against an accounts file.
      *> It supports creating new accounts (NEW), deposits (DEP),
      *> withdrawals (WDR), and balance checks (BAL).
      *>
      *> MODIFICATION:
      *> - Added validation to reject transactions > 999,999.99.
      *> - Corrected logic to handle account balances up to 9,999,999,999.99.
      *> - Ensured file format for balances is consistently 13 characters.
      *>******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO "input.txt".
           SELECT ACC-FILE ASSIGN TO "accounts.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TMP-FILE ASSIGN TO "temp.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD IN-FILE.
       01 IN-RECORD             PIC X(18).

       FD ACC-FILE.
       01 ACC-RECORD-RAW        PIC X(25).

       FD TMP-FILE.
       01 TMP-RECORD            PIC X(25).

       FD OUT-FILE.
       01 OUT-RECORD            PIC X(80).

       WORKING-STORAGE SECTION.
       77 IN-ACCOUNT            PIC 9(6).
       77 IN-ACTION             PIC X(3).
       *> MOD: Added a larger field to read the raw input amount for validation.
       77 IN-RAW-AMOUNT         PIC 9(7)V99.
       77 IN-AMOUNT             PIC 9(6)V99.
       *> MOD: Added a flag to track if the transaction is valid.
       77 IS-VALID-TRANSACTION  PIC X VALUE 'Y'.

       77 ACC-ACCOUNT           PIC 9(6).
       77 ACC-BALANCE           PIC 9(10)V99.

       77 TMP-BALANCE           PIC 9(10)V99.
       77 MATCH-FOUND           PIC X VALUE "N".
       77 UPDATED               PIC X VALUE "N".

       77 BALANCE-TEXT          PIC X(20).
       77 BALANCE-ALPHA         PIC X(20).
       *> MOD: This field was unused. It is now used correctly to format
       *>      the full 10-digit balance for file I/O and display.
       77 LARGE-FORMATTED       PIC Z(9)9.99.

       PROCEDURE DIVISION.

       MAIN.
           PERFORM READ-INPUT
           PERFORM VALIDATE-TRANSACTION

           *> MOD: Only process the transaction if it passes validation.
           IF IS-VALID-TRANSACTION = 'Y'
               PERFORM PROCESS-RECORDS
               IF MATCH-FOUND = "N"
                   IF IN-ACTION = "NEW"
                       PERFORM APPEND-ACCOUNT
                       MOVE "ACCOUNT CREATED" TO OUT-RECORD
                   ELSE
                       MOVE "ACCOUNT NOT FOUND" TO OUT-RECORD
                   END-IF
               END-IF
           END-IF

           PERFORM FINALIZE
           STOP RUN.

       READ-INPUT.
           OPEN INPUT IN-FILE
           READ IN-FILE AT END
               DISPLAY "NO INPUT"
               STOP RUN
           END-READ
           CLOSE IN-FILE

           MOVE IN-RECORD(1:6) TO IN-ACCOUNT
           MOVE IN-RECORD(7:3) TO IN-ACTION
           *> MOD: Read amount into the larger raw field first.
           MOVE FUNCTION NUMVAL(IN-RECORD(10:9)) TO IN-RAW-AMOUNT.

      *> MOD: New paragraph to validate the transaction amount.
       VALIDATE-TRANSACTION.
           IF IN-RAW-AMOUNT > 999999.99
               MOVE "N" TO IS-VALID-TRANSACTION
               MOVE "TRANSACTION REJECTED: AMOUNT EXCEEDS LIMIT."
                 TO OUT-RECORD
           ELSE
               MOVE IN-RAW-AMOUNT TO IN-AMOUNT
           END-IF.

       PROCESS-RECORDS.
           OPEN INPUT ACC-FILE
           OPEN OUTPUT TMP-FILE
           PERFORM UNTIL 1 = 2 *> Loop until explicit exit
               READ ACC-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       MOVE ACC-RECORD-RAW(1:6) TO ACC-ACCOUNT
                       *> MOD: Read the full 13-character balance from the file.
                       MOVE FUNCTION NUMVAL(ACC-RECORD-RAW(10:13))
                           TO ACC-BALANCE
                       IF ACC-ACCOUNT = IN-ACCOUNT
                           MOVE "Y" TO MATCH-FOUND
                           PERFORM APPLY-ACTION
                       ELSE
                           WRITE TMP-RECORD FROM ACC-RECORD-RAW
                       END-IF
           END-PERFORM
           CLOSE ACC-FILE
           CLOSE TMP-FILE.

       APPLY-ACTION.
           MOVE ACC-BALANCE TO TMP-BALANCE
           EVALUATE IN-ACTION
               WHEN "DEP"
                   ADD IN-AMOUNT TO TMP-BALANCE
                   MOVE "DEPOSIT SUCCESSFUL." TO OUT-RECORD
               WHEN "WDR"
                   IF TMP-BALANCE >= IN-AMOUNT
                       SUBTRACT IN-AMOUNT FROM TMP-BALANCE
                       MOVE "WITHDRAWAL SUCCESSFUL." TO OUT-RECORD
                   ELSE
                       MOVE "INSUFFICIENT FUNDS." TO OUT-RECORD
                   END-IF
               WHEN "BAL"
                   MOVE SPACES TO OUT-RECORD
                   MOVE "CURRENT BALANCE: " TO BALANCE-TEXT
                   *> MOD: Use the larger format field for displaying balance.
                   MOVE TMP-BALANCE TO LARGE-FORMATTED
                   MOVE LARGE-FORMATTED TO BALANCE-ALPHA
                   STRING BALANCE-TEXT DELIMITED BY SIZE
                          FUNCTION TRIM(BALANCE-ALPHA) DELIMITED BY SIZE
                          INTO OUT-RECORD
               WHEN OTHER
                   MOVE "UNKNOWN ACTION." TO OUT-RECORD
           END-EVALUATE

           MOVE IN-ACCOUNT TO TMP-RECORD(1:6)
           MOVE "   "       TO TMP-RECORD(7:3) *> Clear action field
           *> MOD: Use the larger format field to write the full balance.
           MOVE TMP-BALANCE TO LARGE-FORMATTED
           MOVE LARGE-FORMATTED TO TMP-RECORD(10:13)

           WRITE TMP-RECORD
           MOVE "Y" TO UPDATED.

       APPEND-ACCOUNT.
           OPEN EXTEND ACC-FILE
           MOVE IN-ACCOUNT TO ACC-RECORD-RAW(1:6)
           MOVE "   "       TO ACC-RECORD-RAW(7:3)
           *> MOD: Use the larger format field to ensure consistent file format.
           MOVE IN-AMOUNT TO LARGE-FORMATTED
           MOVE LARGE-FORMATTED TO ACC-RECORD-RAW(10:13)

           WRITE ACC-RECORD-RAW
           CLOSE ACC-FILE.

       FINALIZE.
           IF UPDATED = "Y"
               CALL "SYSTEM" USING "mv temp.txt accounts.txt"
           END-IF
           OPEN OUTPUT OUT-FILE
           WRITE OUT-RECORD
           CLOSE OUT-FILE.
