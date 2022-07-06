.ORIG #OOPS   ; Bad .ORIG operand
AND R1, ,     ; Bad instruction (or operands)
LABEL ADD R0  ; Duplicate label
LABEL JMP RET ; Bad operand
.END

.ORIG x3000    ; Likely overlapping first block
ADD R0, R0, R0
ADD R0, R0, R0
.END

.ORIG x3001       ; Overlaps second block
ADD R0, R0, LABEL ; Operand type mismatch
BR LABEL          ; Invalid reference to duplicate label
TOO_FAR .BLKW 0
.END

.ORIG x3500
BR TOO_FAR ; Label too distant for offset to fit
.END

.ORIG x4000 ; Bad block (missing .END)
