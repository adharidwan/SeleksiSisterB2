IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKING.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACC-FILE ASSIGN TO "accounts.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TMP-FILE ASSIGN TO "temp.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD IN-FILE.
       01 IN-RECORD             PIC X(19).

       FD ACC-FILE.
       01 ACC-RECORD-RAW        PIC X(19).

       FD TMP-FILE.
       01 TMP-RECORD            PIC X(19).

       FD OUT-FILE.
       01 OUT-RECORD            PIC X(80).

       WORKING-STORAGE SECTION.
       77 IN-ACCOUNT            PIC 9(6).
       77 IN-ACTION             PIC X(3).
       77 IN-AMOUNT             PIC 9(6)V99.

       77 ACC-ACCOUNT           PIC 9(6).
       77 ACC-ACTION            PIC X(3).
       77 ACC-BALANCE           PIC 9(6)V99.

       77 NEW-BALANCE           PIC 9(6)V99.
       77 MATCH-FOUND           PIC X VALUE "N".
       77 UPDATED               PIC X VALUE "N".
       77 EOF-FLAG              PIC X VALUE "N".

       77 FORMATTED-BALANCE     PIC 9(6).99.
       77 BALANCE-TEXT          PIC X(20).

       PROCEDURE DIVISION.

       MAIN.
           PERFORM READ-INPUT
           PERFORM PROCESS-RECORDS
           IF MATCH-FOUND = "N"
               IF IN-ACTION = "NEW"
                   PERFORM APPEND-ACCOUNT
                   MOVE "ACCOUNT CREATED" TO OUT-RECORD
                   PERFORM WRITE-OUTPUT
               ELSE
                   MOVE "ACCOUNT NOT FOUND" TO OUT-RECORD
                   PERFORM WRITE-OUTPUT
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

           MOVE FUNCTION NUMVAL(IN-RECORD(1:6)) TO IN-ACCOUNT
           MOVE IN-RECORD(7:3) TO IN-ACTION
           MOVE FUNCTION NUMVAL(IN-RECORD(10:9)) TO IN-AMOUNT.

       PROCESS-RECORDS.
           OPEN INPUT ACC-FILE
           OPEN OUTPUT TMP-FILE
           MOVE "N" TO EOF-FLAG
           PERFORM UNTIL EOF-FLAG = "Y"
               READ ACC-FILE
                   AT END
                       MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       MOVE FUNCTION NUMVAL(ACC-RECORD-RAW(1:6)) 
                           TO ACC-ACCOUNT
                       MOVE ACC-RECORD-RAW(7:3) TO ACC-ACTION  
                       MOVE FUNCTION NUMVAL(ACC-RECORD-RAW(10:9))
                           TO ACC-BALANCE
                       IF ACC-ACCOUNT = IN-ACCOUNT
                           MOVE "Y" TO MATCH-FOUND
                           PERFORM APPLY-ACTION
                       ELSE
                           WRITE TMP-RECORD FROM ACC-RECORD-RAW
                       END-IF
               END-READ
           END-PERFORM
           CLOSE ACC-FILE
           CLOSE TMP-FILE.

       APPLY-ACTION.
           MOVE ACC-BALANCE TO NEW-BALANCE
           EVALUATE IN-ACTION
               WHEN "DEP"
                   ADD IN-AMOUNT TO NEW-BALANCE
                   MOVE "DEPOSITED MONEY" TO OUT-RECORD
                   PERFORM WRITE-OUTPUT
                   PERFORM WRITE-UPDATED-RECORD
               WHEN "WDR"
                   IF ACC-BALANCE >= IN-AMOUNT
                       SUBTRACT IN-AMOUNT FROM NEW-BALANCE
                       MOVE "WITHDREW MONEY" TO OUT-RECORD
                       PERFORM WRITE-OUTPUT
                       PERFORM WRITE-UPDATED-RECORD
                   ELSE
                       MOVE "INSUFFICIENT FUNDS" TO OUT-RECORD
                       PERFORM WRITE-OUTPUT
                       WRITE TMP-RECORD FROM ACC-RECORD-RAW
                   END-IF
               WHEN "BAL"
                   MOVE SPACES TO OUT-RECORD
                   MOVE "BALANCE: " TO BALANCE-TEXT
                   MOVE ACC-BALANCE TO FORMATTED-BALANCE
                   STRING BALANCE-TEXT DELIMITED SIZE
                          FORMATTED-BALANCE DELIMITED SIZE
                          INTO OUT-RECORD
                   PERFORM WRITE-OUTPUT
                   WRITE TMP-RECORD FROM ACC-RECORD-RAW
               WHEN "NEW"
                   MOVE "ACCOUNT ALREADY EXISTS" TO OUT-RECORD
                   PERFORM WRITE-OUTPUT
                   WRITE TMP-RECORD FROM ACC-RECORD-RAW
               WHEN OTHER
                   MOVE "UNKNOWN ACTION" TO OUT-RECORD
                   PERFORM WRITE-OUTPUT
                   WRITE TMP-RECORD FROM ACC-RECORD-RAW
           END-EVALUATE.

       WRITE-UPDATED-RECORD.
           MOVE IN-ACCOUNT TO FORMATTED-BALANCE
           MOVE FORMATTED-BALANCE(1:6) TO TMP-RECORD(1:6)
           MOVE "BAL" TO TMP-RECORD(7:3)
           MOVE NEW-BALANCE TO FORMATTED-BALANCE
           MOVE FORMATTED-BALANCE TO TMP-RECORD(10:9)
           WRITE TMP-RECORD
           MOVE "Y" TO UPDATED.

       APPEND-ACCOUNT.
           OPEN EXTEND ACC-FILE
           MOVE IN-ACCOUNT TO FORMATTED-BALANCE
           MOVE FORMATTED-BALANCE(1:6) TO ACC-RECORD-RAW(1:6)
           MOVE "BAL" TO ACC-RECORD-RAW(7:3)
           MOVE IN-AMOUNT TO FORMATTED-BALANCE
           MOVE FORMATTED-BALANCE TO ACC-RECORD-RAW(10:9)
           WRITE ACC-RECORD-RAW
           CLOSE ACC-FILE.

       WRITE-OUTPUT.
           OPEN OUTPUT OUT-FILE
           WRITE OUT-RECORD
           CLOSE OUT-FILE.

       FINALIZE.
           IF UPDATED = "Y"
               CALL "SYSTEM" USING "mv temp.txt accounts.txt"
           END-IF.