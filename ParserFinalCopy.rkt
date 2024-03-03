#lang racket
(require data/either)

#|
Hallee Pham
COMP-SCI 441 Programming Languages (Spring 2024)

NOTES:
-This program only successfully scans each token of an input file
-In each function for the nonterminals, the logic of passing a failure to the preceding functions is NOT correct (I could not figure it out)
-Only Sample Input file "File04.txt" gives the correct input
|#

;Function that reads in input file and stores each line of the file as an element of a list
(define (get-lines filename)  
  (file->lines filename #:mode 'text))

;Function that turns the input into tokens the parser can understand
(define (split-lines str-list)
  (define (tokenize-one-item st)
    (cond
      [(equal? (string-ref st 0) #\() (list 'leftparen (tokenize-one-item (substring st 1)))]       ;seperates the left parenthese from a token
      [(equal? (string-ref st (string-length (substring st 1))) #\)) (list (tokenize-one-item(substring st 0 (string-length (substring st 1)))) 'rightparen)] ;seperates the right parenthese of a token
      [(equal? st "$$") 'eof]
      [(equal? st ":") 'colon]
      [(equal? st "if") 'if]
      [(equal? st "while") 'while]
      [(equal? st "endwhile") 'endwhile]
      [(equal? st "read") 'read]
      [(equal? st "write") 'write]
      [(equal? st "goto") 'goto]
      [(equal? st "gosub") 'gosub]
      [(equal? st "return") 'return]
      [(equal? st "break") 'break]
      [(equal? st "end") 'end]
      [(equal? st "true") 'true]
      [(equal? st "false") 'false]
      [(equal? st "<") 'lessthan]
      [(equal? st ">") 'greaterthan]
      [(equal? st ">=") 'greaterthanequalto]
      [(equal? st "<=") 'lessthanequalto]
      [(equal? st "<>") 'doesnotequal]
      [(equal? st "=") 'equal]
      [(equal? st "+") 'plus]
      [(equal? st "-") 'minus]
      [(equal? st "*") 'muliplication]
      [(equal? st "/") 'division]
      [(equal? st ";") 'semicolon]
      [(equal? st "(") 'leftparen]
      [(equal? st ")") 'rightparen]
      [(label? st) 'label]
      [(id? st) 'id]
      [(num? st) 'num]
      [(numsign? st) 'numsign]
      [(digit? st) 'digit]
      [else
       (displayln "SCAN ERROR: Illegal character ") (exit #t)]))      ;if an unknown symbol is scanned, the program exits
  (map tokenize-one-item str-list))

;Main parse function that takes in the input file name
(define (parse filename)
  (define lines (map string-split (get-lines filename)))              ;creates a list of lines
  (define tokenized-line-list (map split-lines lines))                ;tokenizes each line
  (define alltokens (flatten tokenized-line-list))                    ;turns the whole list of lists into one list of all tokens
  (displayln "Full token list:")                                      ;prints out the fully tokenized list (USED FOR TESTING)
  (displayln alltokens)
  (displayln (parse-program alltokens))
  )


;THE FOLLOWING ARE FUNCTIONS FOR NONTERMINALS
;Notes: the logic for the failure branches are incorrect

;program -> linelist $$
(define (parse-program token-list)
  [let
      ([linelist-result (parse-linelist token-list)])                  ;tries to parse a linelist
    (if (success? linelist-result)
        (if (equal? (from-success 0 linelist-result) 'eof)             ;if parsing a linelist is a succes, check if it is followed by and eof marker
            (success "Accept")                                         ;if it is followed by a $$, accept the whole program
            (failure "Syntax Error: Expected an eof marker"))          ;if it is not followed by $$, then throw syntax error
        (failure "Syntax Error: Expected a linelist"))])


;linelist -> line linelist | epsilon
(define (parse-linelist token-list)
  [let
      [(line-result (parse-line token-list))]                                  ;tries to parse a line
    (if (success? line-result)
        [let                                                                   ;if parsing a line is a success, it tries to parse a linelist next
            [(linelist-result (parse-linelist (from-success 0 line-result)))]  
          (if (success? linelist-result)                                       ;if parsing a linelist is a success, it returns 'linelist and the rest of the unparsed tokens
              (success (list ('linelist (from-success 0 linelist-result))))    
              (failure "Syntax Error: Expected a linelist"))]                  ;If it cannot parse a linelist, throws an error 
        (failure "Syntax Error: Expected a line"))])                           
        

; line -> label stmt linetail
(define (parse-line token-list)
  (if (equal? (first token-list) 'label)                                           ;checks if the first token is a label
      [let
          [(stmt-result (parse-stmt (rest token-list)))]                           ;if the first token is a label, try to parse a stmt
        (if (success? stmt-result)               
            [let                                                                   ;if parsing a stmt is a success, try to parse a linetail
                [(linetail-result (parse-linetail (from-success 0 stmt-result)))]           
              (if (success? linetail-result)
                  (success (list ('line (from-success 0 linetail-result))))        ;if parsing is success, return 'line and the rest of the unparsed tokens
                  (failure "Syntax Error: Expected a linetail"))]
            (failure "Syntax Error: Expected a stmt"))]
      (failure "Syntax Error: Expected a label")))
              


; stmt -> id = expr | if (boolean) stmt | while (boolean) linelist endwhile | read id | write expr | goto id | gosub id | return | break | end
(define (parse-stmt token-list)
  [cond
    ;stmt -> id = expr
    [(if (and (equal? (first token-list) 'if)                                  ;checks if the first and second tokens are 'id and 'equal, respectivley
              (equal? (second token-list) 'equal))
         [let
             [(expr-result (rest (rest token-list)))]                          ;if it is a success, try to parse an expr
           (if (success? expr-result)
               (success (list ('stmt (from-success 0 expr-result))))           ;if parsing an expr is successful, return 'stmt and rest of unparsed tokens
               (failure "Syntax Error: Expected an expr"))]
         (failure "Syntax Error: Expected an id ="))]

    ;stmt -> if (boolean) stmt
    [[and (equal? (first token-list) 'if)                                               ;checks if the first token is 'if and the second is 'leftparen
          (equal? (second token-list) 'leftparen)]
     [let
         [(boolean-result (parse-boolean (rest (rest token-list))))]                    ;if first two tokens match, try to parse a boolean
       (if (success? boolean-result)
           (if (equal? (first (from-success 0 boolean-result)) 'rightparen)             ;if there is a boolean, check if the token that precedes it is a 'rightparen
               [let
                   [(stmt-result (parse-stmt (rest (from-success 0 boolean-result))))]  ;if all is successful, try to parse a stmt
                 (if (success? stmt-result)
                     (success (list ('stmt (from-success 0 stmt-result))))              ;if successfully parsed a stmt, return 'stmt and the rest of unparsed tokens
                     (failure "Syntax Error: Expected a stmt"))]
               (failure "Syntax Error: Expected a closing parenthesis"))
           (failure "Syntax Error: Expected a boolean"))]
     (failure "Syntax Error: Expected opening parenthesis")]
    
    ;stmt -> while (boolean) linelist endwhile
    [[and (equal? (first token-list) 'while)                                                     ;checks if the first and second tokens are 'while and 'leftparen
          (equal? (second token-list) 'leftparen)]
     [let
         [(boolean-result (parse-boolean (rest (rest token-list))))]                             ;if first two tokens match, try to parse a boolean
       (if (success? boolean-result)
           (if (equal? (first (from-success 0 boolean-result)) 'rightparen)                      ;if a boolean is parsed, check if a 'rightparen follows it
               [let
                   [(linelist-result (parse-linelist (rest (from-success 0 boolean-result))))]   ;if all former results are success, try to parse a linelist
                 (if (success? linelist-result)
                     (if (equal? (first (from-success 0 linelist-result)) 'endwhile)             ;check if 'endwhile follows the linelist
                         (success (list ('stmt (rest (from-success 0 linelist-result)))))        ;return 'stmt and unparsed tokens
                         (failure "Syntax Error: Expected a 'endwhile'"))
                     (failure "Syntax Error: Expected a linelist"))]
               (failure "Syntax Error: Expected a closing parenthesis"))
           (failure "Syntax Error: Expected a boolean"))]
     (failure "Syntax Error: Expected a 'while'")]
    
    ;stmt -> read id
    [(if (and [(equal? (first token-list) 'read)]                                      ;checks if the first token is 'read and the second token is 'id
              [(equal? (second token-list) 'id)])
         (success (list (rest (rest token-list))))                                     ;if the first two tokens match, return 'stmt and unparsed tokens
         (failure "Syntax Error: Expected a 'read id'"))]

    ;stmt -> write expr
    [(if (equal? (first token-list) 'write)                                      ;checks if the first token is 'write
         [let
             [(expr2-result (parse-expr (rest token-list)))]                     ;if first token matches, try to parse an expr
           (if (success? expr2-result)
               (success (list ('stmt (from-success expr2-result))))              ;if an expr is parsed, return 'stmt and unparsed tokens
               (failure "Syntax Error: Expected an expr"))]
         (failure "Syntax Error: Expected a 'while'"))]
    
    ;stmt -> goto id | gosub id
    [(if (and (or (equal? (first token-list 'goto)) (equal? (first token-list) 'gosub))          ;checks if the first token is either 'goto or 'gosub and that the second token is 'id
              [(equal? (second token-list) 'id)])
         (success (list ('stmt (rest(rest token-list)))))                                        ;if first two tokens match, then return 'stmt and rest of unparsed tokens
         (failure "Syntax Error: Expected a 'gosub id' or 'goto id"))]
    
    ;stmt -> return | break | end                                                                ;checks if the first token is 'return, 'break, or 'end
    [(if (or (equal? (first token-list) 'return)
             (equal? (first token-list 'break)) 
             (equal? (first token-list 'end)))
         (success (list ('stmt (rest token-list))))                                              ;if first token matches, return 'stmt and rest of unparsed tokens
         (failure "Syntax Error: Expected a 'return' , 'break', or 'end'"))]
    [else
     (failure "Syntax Error: Expected a stmt")]])
    
         
; linetail -> ;stmt+ | epsilon
(define (parse-linetail token-list)
  [let
      ((stmt-result (parse-stmt token-list)))                                            ;try to parse a stmt
    (if (success? stmt-result)                 
        (if (equal? (from-success 0 stmt-result) '())                                    ;if the stmt parsed successfully and the rest of the unparsed token list is empty, return 'linetail
            (success (list ('stmt (from-success 0 stmt-result))))                        ;this satisfies the condition of "one or more" stmts
            (parse-stmt (from-success 0 stmt-result)))                                   ;if the stmt parsed successfully BUT there is still unparsed tokens, try to parse a stmt again
        (failure "Syntax Error: Expected a stmt"))])
           

; expr -> id etail | num etail | (expr)
(define (parse-expr token-list)                                                   
  [cond
    ;expr -> id etail
    [(if (or [(equal? (first token-list) 'id)]                                            ;checks if the first token is either 'id or 'num
             [(equal? (first token-list) 'num)])
         [let
             [(etail-result (parse-etail (rest token-list)))]                             ;if first token matches, then try to parse an etail
           (if (success? etail-result)
               (success (list 'expr (from-success 0 etail-result)))                       ;if an etail is parsed, return 'expr and rest of unparsed tokens
               (failure "Syntax Error: expected an etail"))]
         (failure "Syntax Error: expected an id or num"))]

    ;expr -> num etail
    [(equal? (first token-list) 'leftparen)                                               ;checks if the first token is an opening parenthesis
     [let
         [(expr-result (parse-expr (rest token-list)))]                                   ;if the first token is 'leftparen, then try to parse an expr
       (if (success? expr-result)
           (if (equal? (from-success expr-result) 'rightparen)                            ;if an expr is parsed, check if the following token is a closing parenthesis
               (success (list ('expr (from-success 0 expr-result))))                      ;if all results are success, return 'expr and rest of unparsed tokens
               (failure "Syntax Error: Expected an closing parenthesis"))
           (failure "Syntax Error: Expected an expr"))]]])


; etail -> + expr | - expr | * expr | / expr | epsilon
(define (parse-etail token-list)
  (if (or [(equal? (first token-list) 'plus)]                                         ;checks if the first token is an operator ('plus, 'minus, 'multiplication, 'division)
          [(equal? (first token-list) 'minus)]
          [(equal? (first token-list) 'multiplication)]
          [(equal? (first token-list) 'division)])
      [let
          [(expr-result (parse-expr (rest token-list)))]                              ;if first token matches, try to parse an expr
        (if (success? expr-result)
            (success (list ('etail (from-success 0 expr-result))))                    ;if an expr is parsed, return 'etail and the rest of unparsed tokens
            (failure "Syntax Error: Expected an expression"))]
      (failure "Syntax Error: expected an arithmetic operator")))


; boolean -> true | false | expr bool-op expr
(define (parse-boolean token-list)
  [cond
    [(equal? (first token-list) 'true) (success 'boolean)]                            ;if first token is 'true, return 'boolean and rest of unparsed tokens
    [(equal? (first token-list) 'false) (success 'boolean)]                           ;if first token is 'false, return 'boolean and rest of unparsed tokens
    [else                                                                             ;boolean -> expr bool-op expr
     [let
         [(expr1-result (parse-expr token-list))]                                     ;checks if the first token is an expr
       (if (success? expr1-result)                                                    ;if it was an expression, the success should return 'expr and the rest of the unparsed tokens
           [let
               [(bool-op-result (second (from-success 0 expr1-result)))]              ;checks if a bool-op follows the first expr
             (if (success? bool-op-result)                                            
                 [let
                     ([expr2-result (rest (from-success 0 bool-op-result))])          ;if it is a bool-op, try to parse another expr
                   (if (success? expr2-result)
                       (success list ('boolean (from-success 0 expr2-result)))        ;if a second expr is parsed, then return 'boolean and rest of unparsed tokens
                       (failure "Syntax Error: Expected an expression"))]
                 (failure "Syntax Error: Expected a bool-op"))]
           (failure "Syntax Error: Expected an expression"))]]])

; bool-op -> < | > | >= | <= | <> | =
(define (parse-bool-op token)
  [cond
    [(equal? token 'lessthan) (success (list ('bool-op)))]                            ;checks if the first token is any of the bool-op, if yes, return 'bool-op and rest of unparsed tokens
    [(equal? token 'greaterthan) (success (list ('bool-op)))]
    [(equal? token 'greaterthanequalto) (success (list ('bool-op)))]
    [(equal? token 'lessthanequalto) (success (list ('bool-op)))]
    [(equal? token 'doesnotequal) (success (list ('bool-op)))]
    [(equal? token 'equal) (success (list ('bool-op)))]
    [else
     (failure "Syntax Error: Expected a boolean operator")]])

;THE FOLLOWING FUNCTIONS ARE USED TO TOKENIZE

;label -> id: | epsilon
(define (label? token)
  (regexp-match? #rx"^[a-zA-Z][a-zA-Z0-9]*:$" token)               ;alphabetic character followed by 0 or more alpganumeric characters followed by a colon
  )

;id -> [a-zA-Z][a-zA-Z0-9]*
(define (id? token)
  (regexp-match? #rx"^[a-zA-Z][a-zA-Z0-9]*$" token)                ;alphabetic character followed by 0 or more alpganumeric characters
  )

;num -> numsign digit digit*
(define (num? token)                                               ;a numsign followed by a digit followed by 0 or more digits
  (or (regexp-match? #rx"^[+-]?[1-9][0-9]*$" token) (regexp-match? #rx"^[+-]?0$" token)))

;numsign -> + | - | epsilon
(define (numsign? token)
  (regexp-match? #rx"[+-]" token))

;digit -> [0-9]
(define (digit? token)
  (regexp-match? #rx"^[0-9]$" token)
  )