IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKING.
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
       01 OUT-RECORD            PIC X(120).

       WORKING-STORAGE SECTION.
       77 IN-ACCOUNT            PIC 9(6).
       77 IN-ACTION             PIC X(3).
       77 IN-RAW-AMOUNT         PIC 9(7)V99.
       77 IN-AMOUNT             PIC 9(6)V99.
       77 IS-VALID-TRANSACTION  PIC X VALUE 'Y'.

       77 ACC-ACCOUNT           PIC 9(6).
       77 ACC-BALANCE           PIC 9(10)V99.
       77 TMP-BALANCE           PIC 9(10)V99.
       77 MATCH-FOUND           PIC X VALUE "N".
       77 UPDATED               PIC X VALUE "N".
       
       77 RAI-TO-IDR-RATE       PIC 9(9) VALUE 119714660.
       77 IDR-BALANCE           PIC 9(18).
       77 RAI-FORMATTED         PIC Z(9)9.99.
       77 IDR-FORMATTED         PIC Z(12)Z,ZZZ,ZZZ,ZZ9.
       77 BALANCE-TEXT          PIC X(40).
       77 IDR-TEXT              PIC X(40).
       77 BALANCE-ALPHA         PIC X(25).
       77 IDR-ALPHA             PIC X(25).

       *> Bonus: Variabel Kalkulasi Bunga (Telah Diperbarui)
       77 CMD-ARG               PIC X(20).
       *> Suku bunga 20% (0.20) per interval untuk demonstrasi yang terlihat
       77 INTEREST-RATE         PIC 9V99 VALUE 0.20.
       77 INTEREST-AMOUNT       PIC S9(10)V99.
       77 SLEEP-DURATION-SEC    PIC 9(9) VALUE 23.
       77 FILE-HAS-RECORDS      PIC X.

       PROCEDURE DIVISION.
       
       ACCEPT CMD-ARG FROM COMMAND-LINE.

       IF FUNCTION TRIM(CMD-ARG) = "--apply-interest"
           PERFORM APPLY-INTEREST-IN-LOOP
       ELSE
           PERFORM RUN-SINGLE-TRANSACTION
       END-IF.
       STOP RUN.

       APPLY-INTEREST-IN-LOOP.
           PERFORM UNTIL 1 = 2
               CALL "C$SLEEP" USING SLEEP-DURATION-SEC
               PERFORM CALCULATE-ALL-INTEREST
               PERFORM SWAP-ACCOUNT-FILE
           END-PERFORM.

       CALCULATE-ALL-INTEREST.
           MOVE 'N' TO FILE-HAS-RECORDS.
           OPEN INPUT ACC-FILE.
           OPEN OUTPUT TMP-FILE.
           PERFORM UNTIL 1 = 2
               READ ACC-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       MOVE 'Y' TO FILE-HAS-RECORDS
                       MOVE ACC-RECORD-RAW(1:6) TO ACC-ACCOUNT
                       MOVE FUNCTION NUMVAL(ACC-RECORD-RAW(10:13))
                           TO ACC-BALANCE
                       
                       *> Logika kalkulasi yang disederhanakan agar terlihat
                       COMPUTE INTEREST-AMOUNT ROUNDED = 
                           ACC-BALANCE * INTEREST-RATE
                       
                       ADD INTEREST-AMOUNT TO ACC-BALANCE
                       
                       MOVE ACC-ACCOUNT TO TMP-RECORD(1:6)
                       MOVE "BAL"       TO TMP-RECORD(7:3)
                       MOVE ACC-BALANCE TO RAI-FORMATTED
                       MOVE RAI-FORMATTED TO TMP-RECORD(10:13)
                       WRITE TMP-RECORD
           END-PERFORM.
           CLOSE ACC-FILE.
           CLOSE TMP-FILE.

       SWAP-ACCOUNT-FILE.
           IF FILE-HAS-RECORDS = 'Y'
               OPEN INPUT TMP-FILE
               OPEN OUTPUT ACC-FILE
               PERFORM UNTIL 1 = 2
                   READ TMP-FILE
                       AT END
                           EXIT PERFORM
                       NOT AT END
                           WRITE ACC-RECORD-RAW FROM TMP-RECORD
               END-PERFORM
               CLOSE TMP-FILE
               CLOSE ACC-FILE
           END-IF.

       RUN-SINGLE-TRANSACTION.
           PERFORM READ-INPUT
           PERFORM VALIDATE-TRANSACTION

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

           PERFORM FINALIZE.

       READ-INPUT.
           OPEN INPUT IN-FILE
           READ IN-FILE AT END
               DISPLAY "NO INPUT"
               STOP RUN
           END-READ
           CLOSE IN-FILE

           MOVE IN-RECORD(1:6) TO IN-ACCOUNT
           MOVE IN-RECORD(7:3) TO IN-ACTION
           MOVE FUNCTION NUMVAL(IN-RECORD(10:9)) TO IN-RAW-AMOUNT.

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
           PERFORM UNTIL 1 = 2 
               READ ACC-FILE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       MOVE ACC-RECORD-RAW(1:6) TO ACC-ACCOUNT
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
                   PERFORM DISPLAY-BALANCE-WITH-IDR
               WHEN OTHER
                   MOVE "UNKNOWN ACTION." TO OUT-RECORD
           END-EVALUATE

           MOVE IN-ACCOUNT TO TMP-RECORD(1:6)
           MOVE "BAL"       TO TMP-RECORD(7:3)
           MOVE TMP-BALANCE TO RAI-FORMATTED
           MOVE RAI-FORMATTED TO TMP-RECORD(10:13)

           WRITE TMP-RECORD
           MOVE "Y" TO UPDATED.

       DISPLAY-BALANCE-WITH-IDR.
           MOVE SPACES TO OUT-RECORD
           MOVE TMP-BALANCE TO RAI-FORMATTED
           MOVE "RAI STONE BALANCE: " TO BALANCE-TEXT
           MOVE RAI-FORMATTED TO BALANCE-ALPHA
           COMPUTE IDR-BALANCE = TMP-BALANCE * RAI-TO-IDR-RATE
           MOVE IDR-BALANCE TO IDR-FORMATTED
           MOVE " | IDR EQUIVALENT: Rp" TO IDR-TEXT
           MOVE IDR-FORMATTED TO IDR-ALPHA
           STRING BALANCE-TEXT DELIMITED BY SIZE
                  FUNCTION TRIM(BALANCE-ALPHA) DELIMITED BY SIZE
                  IDR-TEXT DELIMITED BY SIZE
                  FUNCTION TRIM(IDR-ALPHA) DELIMITED BY SIZE
                  INTO OUT-RECORD.

       APPEND-ACCOUNT.
           OPEN EXTEND ACC-FILE
           MOVE IN-ACCOUNT TO ACC-RECORD-RAW(1:6)
           MOVE "BAL"       TO ACC-RECORD-RAW(7:3)
           MOVE IN-AMOUNT TO RAI-FORMATTED
           MOVE RAI-FORMATTED TO ACC-RECORD-RAW(10:13)
           WRITE ACC-RECORD-RAW
           CLOSE ACC-FILE.

       FINALIZE.
           IF UPDATED = "Y"
               OPEN OUTPUT ACC-FILE
               OPEN INPUT TMP-FILE
               PERFORM UNTIL 1 = 2
                   READ TMP-FILE
                       AT END
                           EXIT PERFORM
                       NOT AT END
                           WRITE ACC-RECORD-RAW FROM TMP-RECORD
               END-PERFORM
               CLOSE TMP-FILE
               CLOSE ACC-FILE
           END-IF
           OPEN OUTPUT OUT-FILE
           WRITE OUT-RECORD
           CLOSE OUT-FILE.