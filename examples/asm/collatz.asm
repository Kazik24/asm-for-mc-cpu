;*******************************
; Program calculating Collatz sequence
;*******************************

    nop               ; zawsze na początku programu BedrockCore™

    mov  r2,#0x40     ; załaduj adres początku tablicy z liczbami w RAMie do r2 
    mov  r1,[r2]      ; załaduj początkowa wartość z komórki RAM do r1
    mov  r4,#1        ; załaduj wartość stop (jeśli zostanie osiągnięta, zatrzymuje program)
Calc:
    jmpz @NumEven,r1  ; jeśli parzysta skocz do NumEven (skacze jeśli najmłodszy bit jest 0)
    shl  r3,r1        ; r3 = 2*r1
    add  r1,r1,r3     ; r1 = r1 + r3
    add  r1,r1,#1     ; r1 = r1 + 1 czyli r1 = 3*(wartość początkowa r1) + 1
    add  r15,r15,#1   ; skok względny o 1 instrukcje w przód (omija następne shr)
NumEven:
    shr  r1,r1        ; podziel na 2
    add  r2,r2,#2     ; przesuń wskaźnik r2 do następnej komórki
    mov  [r2],r1      ; wpisz wartość do komórki ram o adresie r2
    eq   r3,r1,r4     ; sprawdź czy obliczona wartość jest równa wartości stop
    jmpz @Calc,r3     ; skocz na początek pętli jeśli nie była równa
    halt              ; zatrzymaj program