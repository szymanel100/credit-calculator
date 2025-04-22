       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOAN-OVERPAYMENT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  LOAN-AMOUNT         PIC 9(8)V99 VALUE 0.
       77  MONTHLY-INTEREST    PIC 9V9999 VALUE 0.
       77  MONTHS              PIC 9(3) VALUE 0.
       77  MONTHLY-PAYMENT     PIC 9(7)V99 VALUE 0.
       77  MONTH               PIC 9(3) VALUE 1.
       77  BALANCE             PIC 9(8)V99 VALUE 0.
       77  INTEREST            PIC 9(7)V99 VALUE 0.
       77  TOTAL-INTEREST      PIC 9(8)V99 VALUE 0.
       77  OVERPAYMENT1        PIC 9(7)V99 VALUE 0.
       77  OVERPAYMENT2        PIC 9(7)V99 VALUE 0.
       77  OVERPAYMENT3        PIC 9(7)V99 VALUE 0.
       77  OVERPAYMENT-MONTH1  PIC 9(3) VALUE 0.
       77  OVERPAYMENT-MONTH2  PIC 9(3) VALUE 0.
       77  OVERPAYMENT-MONTH3  PIC 9(3) VALUE 0.
       77  OVERPAYMENT         PIC 9(7)V99 VALUE 0.
       77 WS-MONTHS-PAID PIC 9(3).

       PROCEDURE DIVISION.
       DISPLAY "Kalkulator spłaty kredytu z nadpłatami".
       DISPLAY "Podaj kwotę kredytu: " WITH NO ADVANCING.
       ACCEPT LOAN-AMOUNT.
       DISPLAY "Podaj liczbę miesięcy: " WITH NO ADVANCING.
       ACCEPT MONTHS.
       DISPLAY "Podaj miesięczne oprocentowanie (np. 1.5): " WITH NO ADVANCING.
       ACCEPT MONTHLY-INTEREST.
       DISPLAY "Podaj wysokość miesięcznej raty: " WITH NO ADVANCING.
       ACCEPT MONTHLY-PAYMENT.

       DISPLAY "Podaj miesiąc 1 nadpłaty (0 jeśli brak): " WITH NO ADVANCING.
       ACCEPT OVERPAYMENT-MONTH1.
       IF OVERPAYMENT-MONTH1 > 0
           DISPLAY "Podaj kwotę 1 nadpłaty: " WITH NO ADVANCING
           ACCEPT OVERPAYMENT1
       END-IF.

       DISPLAY "Podaj miesiąc 2 nadpłaty (0 jeśli brak): " WITH NO ADVANCING.
       ACCEPT OVERPAYMENT-MONTH2.
       IF OVERPAYMENT-MONTH2 > 0
           DISPLAY "Podaj kwotę 2 nadpłaty: " WITH NO ADVANCING
           ACCEPT OVERPAYMENT2
       END-IF.

       DISPLAY "Podaj miesiąc 3 nadpłaty (0 jeśli brak): " WITH NO ADVANCING.
       ACCEPT OVERPAYMENT-MONTH3.
       IF OVERPAYMENT-MONTH3 > 0
           DISPLAY "Podaj kwotę 3 nadpłaty: " WITH NO ADVANCING
           ACCEPT OVERPAYMENT3
       END-IF.

       MOVE LOAN-AMOUNT TO BALANCE.
       PERFORM UNTIL BALANCE <= 0 OR MONTH > MONTHS
           COMPUTE INTEREST = BALANCE * MONTHLY-INTEREST / 100
           ADD INTEREST TO TOTAL-INTEREST
           SUBTRACT MONTHLY-PAYMENT FROM BALANCE
           SUBTRACT OVERPAYMENT FROM BALANCE

           IF MONTH = OVERPAYMENT-MONTH1
               MOVE OVERPAYMENT1 TO OVERPAYMENT
           ELSE IF MONTH = OVERPAYMENT-MONTH2
               MOVE OVERPAYMENT2 TO OVERPAYMENT
           ELSE IF MONTH = OVERPAYMENT-MONTH3
               MOVE OVERPAYMENT3 TO OVERPAYMENT
           ELSE
               MOVE 0 TO OVERPAYMENT
           END-IF

           DISPLAY "Miesiąc: " MONTH
           DISPLAY "Saldo: " BALANCE
           DISPLAY "Odsetki w tym miesiącu: " INTEREST
           DISPLAY "Nadpłata: " OVERPAYMENT

           ADD 1 TO MONTH
       END-PERFORM.
        
       COMPUTE WS-MONTHS-PAID = MONTH - 1.
       DISPLAY "Suma odsetek: " TOTAL-INTEREST.
       DISPLAY "Liczba miesięcy do spłaty: " WS-MONTHS-PAID.
       STOP RUN.
