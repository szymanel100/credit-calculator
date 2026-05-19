IDENTIFICATION DIVISION.
PROGRAM-ID. LOAN-CALCULATOR.

DATA DIVISION.
WORKING-STORAGE SECTION.
77  LOAN-AMOUNT            PIC 9(8)V99 VALUE 0.
77  ANNUAL-INTEREST        PIC 9(2)V9999 VALUE 0. 
77  MONTHLY-INTEREST       PIC 9V999999 VALUE 0.
77  MONTHS                 PIC 9(3) VALUE 0.
77  MONTHS-LEFT            PIC 9(3) VALUE 0.
77  MONTHLY-PAYMENT        PIC 9(7)V99 VALUE 0.
77  PRINCIPAL-PAYMENT      PIC 9(7)V99 VALUE 0.
77  MONTH                  PIC 9(3) VALUE 1.
77  BALANCE                PIC S9(8)V99 VALUE 0.
77  INTEREST               PIC S9(7)V99 VALUE 0.
77  TOTAL-INTEREST         PIC 9(8)V99 VALUE 0.
77  OVERPAYMENT1           PIC 9(7)V99 VALUE 0.
77  OVERPAYMENT2           PIC 9(7)V99 VALUE 0.
77  OVERPAYMENT3           PIC 9(7)V99 VALUE 0.
77  OVERPAYMENT-MONTH1     PIC 9(3) VALUE 0.
77  OVERPAYMENT-MONTH2     PIC 9(3) VALUE 0.
77  OVERPAYMENT-MONTH3     PIC 9(3) VALUE 0.
77  OVERPAYMENT            PIC 9(7)V99 VALUE 0.
77  WS-MONTHS-PAID         PIC 9(3) VALUE 0.
77  WS-CURRENT-PAYMENT     PIC 9(8)V99 VALUE 0.
77  DUMMY-WAIT             PIC X VALUE SPACE.

PROCEDURE DIVISION.
    DISPLAY "--- KALKULATOR RAT MALEJACYCH ---".
    
    DISPLAY " ". 
    DISPLAY "Podaj kwote kredytu: " WITH NO ADVANCING.
    ACCEPT LOAN-AMOUNT.
    
    DISPLAY "Podaj liczbe miesiecy (np. 360 dla 30 lat): " WITH NO ADVANCING.
    ACCEPT MONTHS.
    
    DISPLAY "Podaj oprocentowanie ROCZNE (np. 5.75): " WITH NO ADVANCING.
    ACCEPT ANNUAL-INTEREST.

    *> Obliczenie stopy miesięcznej
    COMPUTE MONTHLY-INTEREST = ANNUAL-INTEREST / 12.

    DISPLAY "Miesiac 1 nadplaty (0=brak): " WITH NO ADVANCING.
    ACCEPT OVERPAYMENT-MONTH1.
    IF OVERPAYMENT-MONTH1 > 0
        DISPLAY "Kwota 1 nadplaty: " WITH NO ADVANCING
        ACCEPT OVERPAYMENT1
    END-IF.

    DISPLAY "Miesiac 2 nadplaty (0=brak): " WITH NO ADVANCING.
    ACCEPT OVERPAYMENT-MONTH2.
    IF OVERPAYMENT-MONTH2 > 0
        DISPLAY "Kwota 2 nadplaty: " WITH NO ADVANCING
        ACCEPT OVERPAYMENT2
    END-IF.

    DISPLAY "Miesiac 3 nadplaty (0=brak): " WITH NO ADVANCING.
    ACCEPT OVERPAYMENT-MONTH3.
    IF OVERPAYMENT-MONTH3 > 0
        DISPLAY "Kwota 3 nadplaty: " WITH NO ADVANCING
        ACCEPT OVERPAYMENT3
    END-IF.

    MOVE LOAN-AMOUNT TO BALANCE.

    PERFORM UNTIL BALANCE <= 0 OR MONTH > MONTHS
        IF MONTH = OVERPAYMENT-MONTH1
            MOVE OVERPAYMENT1 TO OVERPAYMENT
        ELSE IF MONTH = OVERPAYMENT-MONTH2
            MOVE OVERPAYMENT2 TO OVERPAYMENT
        ELSE IF MONTH = OVERPAYMENT-MONTH3
            MOVE OVERPAYMENT3 TO OVERPAYMENT
        ELSE
            MOVE 0 TO OVERPAYMENT
        END-IF END-IF END-IF

        *> 1. Oblicz ile miesięcy zostało do końca planowanej spłaty
        COMPUTE MONTHS-LEFT = MONTHS - MONTH + 1

        *> 2. Oblicz stałą część kapitałową na dany miesiąc
        COMPUTE PRINCIPAL-PAYMENT = BALANCE / MONTHS-LEFT

        *> 3. Oblicz bieżące odsetki od aktualnego salda zadłużenia
        COMPUTE INTEREST = BALANCE * (MONTHLY-INTEREST / 100)
        ADD INTEREST TO TOTAL-INTEREST
        
        *> 4. Rata podstawowa w tym miesiącu (kapitał + odsetki)
        COMPUTE MONTHLY-PAYMENT = PRINCIPAL-PAYMENT + INTEREST
        
        *> 5. Całkowity wydatek użytkownika w tym miesiącu (wraz z nadpłatą)
        COMPUTE WS-CURRENT-PAYMENT = MONTHLY-PAYMENT + OVERPAYMENT
        
        *> 6. Aktualizacja salda: odejmujemy ratę kapitałową oraz nadpłatę
        COMPUTE BALANCE = BALANCE - PRINCIPAL-PAYMENT - OVERPAYMENT

        *> 7. Korekta dla ostatniej raty (lub wcześniejszego zamknięcia długu przez nadpłaty)
        IF BALANCE < 0
            ADD BALANCE TO WS-CURRENT-PAYMENT
            MOVE 0 TO BALANCE
        END-IF

        DISPLAY "Miesiac: " MONTH 
                " | Wplata calk: " WS-CURRENT-PAYMENT
                " | Rata bez nadp.: " MONTHLY-PAYMENT
                " | Odsetki: " INTEREST
                " | Saldo: " BALANCE
        
        ADD 1 TO MONTH
    END-PERFORM.

    COMPUTE WS-MONTHS-PAID = MONTH - 1.
    DISPLAY "================================".
    DISPLAY "Suma odsetek: " TOTAL-INTEREST.
    DISPLAY "Splata trwala: " WS-MONTHS-PAID " miesiecy.".
    
    DISPLAY "Nacisnij ENTER, aby zakonczyc...".
    ACCEPT DUMMY-WAIT.
    STOP RUN.
