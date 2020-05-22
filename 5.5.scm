; target 描述的是一个寄存器，被编译出的代码段应该将表达式的值保存到这里
; linkage 相关表达式的目标代码在完成自己的执行之后应该如何继续下去
(define (compile exp target linkage)
    (cond 
        [(self-evaluating? exp) (compile-self-evaluating exp target linkage)]
        [(quoted? exp) (compile-variable exp target linkage)]
        [(variale? exp) (compile-variable exp target linkage)]
        [(assignment? exp) (compile-assignment exp target linkage)]
        [(definition? exp) (compile-definition exp target linkage)]
        [(if? exp) (compile-if exp target linkage)]
        [(lambda? exp) (compile-lambda exp target linkage)]
        [(begin? exp) 
         (compile-sequence (begin-actions exp) target linkage)]
        [(cond? exp) (compile (cond-if exp) exp target linkage)]
        [(application? exp) (compile-application exp tart linkage)]
        [else (error "Unkonow expression type -- COMPILE" exp)]))

; 指令序列由是哪个部分组成 
; 为什么需要 needs 和 modifies ?  needs 和 modifies 并没有直接的关联
; needs 需要的寄存器的集合 (整个指令序列要求寄存器要求寄存器 env 和 continue 已经被初始化, 即需要从 env 或者 continue 获取值？)
; modifies 修改的那些寄存器的集合
; statements 指序列中的实际指令
; 在代码生成时，为每个指令序列关联有关寄存器的使用信息，
; 所在组合指令的过程中，也会从各个成分序列的相关信息中推到出组合后产生的寄存器使用信息。
(define (make-instruction-sequence needs modifies statements)
    (list needs modifies statements))

(define (empty-instruction-sequence) (make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
    (cond [(eq? linkage 'return) (make-instruction-sequence '(continue) '() '([goto (reg continue)]))]
          [(eq? linkage 'next) (empty-instruction-sequence)]
          [(else (make-instruction-sequence '() '() '([goto (label ,linkage)])))]))

(define (end-with-linkage linkage instruction-sequence) ; 组合指令序列与链接序列
    (preserving ; 组合指令序列
        '(continue) ; 需要用到的寄存器
        instruction-sequence
        (compile-linkage linkage))


;; 简单表达式编译, target 即表达式求值的结果

; 1 
; return (list needs modifies statements))
(define (compile-self-evaluating exp target linkage)
    (end-with-linkage
        linkage
        (make-instruction-sequence 
            '() 
            (list target)
            ; 将 exp 的值赋值给 target (target 在解释器中是一个寄存器的值)
            '([assign ,target (const ,exp)]))))

; '(1 2)
(define (compile-quoted exp target linkage)
    (end-with-linkage 
        linkage
        (make-instruction-sequence 
            '()
            (list target)
            '([assign ,target (const ,(text-of-quotation exp))]))))

; a => (assign val (op lookup-veriable-value) (const a) (reg env))
(define (compile-variable exp target linkage)
    (end-with-linkage
        linkage
        (make-instruction-sequence
            '(env)
            (list target)
            '([assign ,target (op lookup-variable-value) (const ,exp) (reg env)]))))


(define (compile-assignment exp target linkage)
    (let ([var (assignment-variable exp)]
          ; target 多半情况都是解释器中的 register.val
          ; compile 返回一个 指令序列
          [get-value-code (compile (assignment-value exp) 'val 'next)])
        (end-with-linkage
            ;该过程可以保证如果其第一个参数中的某个寄存器的值在第二个指令序列中需要使用的话
            ;该寄存器就不会受到第一个指令序列执行的影响
            (preserving 
                '(env) 
                get-value-code
                (make-instruction-sequence 
                    '(env val)
                    (list target)
                    '([perform (op set-variable-value) (const ,var) (reg val) (reg env)]
                      [assign ,target (const ok)])))))) ; 对于语句来说其没有返回值，所以赋值一个 ok 常量到 target

(define (compile-definition exp target linkage)
    (let ([var (definition-variable exp)]
          [get-value-code (compile (definition-value exp) 'val 'next)])
        (end-with-linkage linkage
            (preserving 
                '(env)
                ; 这部分指令执行完毕后会将 register.val 赋值，并且继续执行后续('next)的指令
                ; 即下一个参数生成的指令
                get-value-code 
                (make-instruction-sequence
                    '(env val)
                    (list target)
                    '([perform (op define-variable!) (const ,var) (reg val) (reg env)]
                      [assign ,target (const ok)]))))))

(define (compile-if exp target linkage)
    (let ([t-branch (make-label 'true-branch)] ; label 特殊处理
          [f-branch (make-label 'false-branch)]
          [after-if (make-label 'after-if)])
        (let ([consequent-linkage (if (eq? linkage 'next) after-if linkage)]) 
            (let ([p-code (compile (if-predicate exp) 'val 'next)]
                  ; true branch code 在指令完成后的 linkage 为 next 时需要特殊处理跳过 false branch code
                  [c-code (compile (if-consequent exp) target consequent-linkage)] 
                  [a-code (compile (if-alternative exp) target linkage)]) 
                (preserving 
                    '(env continue)
                    p-code
                    (append-instruction-sequences ; 组合任意多的指令序列
                        (make-instruction-sequence
                            '(val)
                            '()
                            '([test (op false?) (reg val)] ; 编译测试指令
                              [branch (label ,f-branch)]))
                        (parallel-instruction-sequences
                            (append-instruction-sequences t-branch c-code) ; 编译 true 部分 拼接 label + 指令序列
                            (append-instruction-sequences f-branch a-code)) ; 编译 false 部分
                        after-if)))))) ; label 拼接


(define (compile-sequence seq target linkage)
    (if (last-exp? seq)
        (compile (first-exp seq) target linkage)
        (preserving
            '(env continue)
            (compile (first-exp seq) target 'next)
            (compile-sequence (rest-exps seq) target linkage))))

;(define a (lambda (arg1 arg2) (+ arg1 arg2)))
(define (compile-lambda exp target linkage)
    (let ([proc-entry (make-label 'entry)]
          [after-lambda (make-label 'after-lambda)]) 
        ; 在编译 lambda 表达式时我们还需要生成过程体的目标代码，将过程体仅挨着过程对象是很方便的
        ; 在 linkage 为 return 和 标号时这样做也确实很合适，但是当 linkage 为 next 时
        ; 我们在构建过程对象时就需要跳过过程体的目标代码
        (let ([lambda-linkage (if (eq? linkage 'next) after-lambda linkage)]) 
            (append-instruction-sequences
                (tack-on-instruction-sequences
                    (end-with-linkage 
                        lambda-linkage
                        (make-instruction-sequence
                            '(env)
                            (list target)
                            ; 构造过程对象并将其赋值给目标对象
                            ; assign val (op make-compile-procedure) (label test) (reg env)
                            ; 过程对象在运行时构造，构造的方式就是组合定义时的环境和其对应的过程体的入口点。
                            '([assign ,target 
                                        (op make-compile-procedure)
                                        (label ,proc-entry) ; 过程对象和过程体通过 proc-entry 进行关联
                                        (reg env)]))) ; 定义 lambda 表达式时的环境才是最珍贵的
                    (compile-label-body exp proc-entry)) ; 可有可无,只是找个方便的位置安放
                after-lambda))))

(define (compile-lambda-body exp proc-entry)
    (let ([formals (lambda-parameters exp)]) 
        (append-instruction-sequences
            (make-instruction-sequences
                '(env proc argl)
                '(env)
                '(,proc-entry
                    [assign env (op compile-procedure-env) (reg proc)]
                    [assign env (op extend-environment) (const ,formals) (reg argl) (reg env)]))
            (compile-sequences (lambda-body exp) 'val 'return))))

;; 编译过程中最本质的东西就是过程应用的编译
; proc 运算符求值的结果
; arg1 运算对象的求值结果列表表


(define (compile-application exp target linkage)
    (let ((proc-code (compile (operator exp) 'proc 'next)) ; 最常见的就是 compile-self-evaluating
        (operand-codes 
            (map (lambda
                    (operand) (compile operand 'val 'next))
                (operands exp))))
        (preserving 
            '(env continue) 
            proc-code
            (preserving '(proc continue)
                (construct-arglist operand-codes) 
                (compile-procedure-call target linkage))))) ; 将 argl 应用于 proc


(define (construct-arglist operandcodes)
    (let ([operand-codes (reverse operand-codes)]) 
        (if (null? operand-codes)
            (make-instruction-sequence '() '(argl) '([assign argl (const ())]))
            (let ([code-to-get-last-arg 
                (append-instruction-sequences
                    (car operand-codes)
                    (make-instruction-sequence '(val) '(argl)
                        ; 初始化 argl 为空的 list，并将 val 赋值给 argl
                        '([assign argl (op list) (reg val)])))]) 
                (if (null? (cdr operand-codes)
                    code-to-get-last-arg
                    (preserving 
                        '(env)
                        code-to-get-last-arg
                        (code-to-get-last-arg
                            (cdr operand-codes)))))))))

(define (code-to-get-rest-args operand-codes)
    (let ([code-for-next-arg
            (preserving
                '(argl)
                (car operand-codes)
                (make-instruction-sequence
                    '(val argl)
                    '(argl)
                    '([assign argl (op cons) (reg val) (reg argl)])))]) 
        (if (null? (cdr operand-codes)
            code-for-next-arg
            (preserving 
                '(env)
                code-for-next-arg
                (code-to-get-rest-args (cdr operand-codes)))))))


(define (compile-procedure-call target linkage)
    (let ([primitive-branch (make-label 'primitive-branch)]
          [compiled-branch (make-label 'compiled-branch)]
          [after-call (make-label 'after-call)]) 
        (let ([compiled-linkage (if (eq? linkage 'next) after-call linkage)]) 
            (append-instruction-sequences
                (make-instruction-sequences 
                    '(proc) 
                    '()
                    '([test (op primitive-procedure?) (reg proc)]
                      [brnach (label ,primitive-branch)]))
                (parallel-instruction-sequences ; 这两个分支不是顺序执行的，因此不用 append 而使用 parallel
                    (append-instruction-sequences
                        compiled-branch ; label name
                        (compile-proc-appl target compile-linkage)) ; 编码应用过程
                    (append-instruction-sequence
                        primitive-branch
                        (end-with-linkage 
                            linkage
                            (make-instruction-sequence
                                '(proc argl)
                                (list target)
                                '([assign ,target
                                    (op apply-primitive-procedure)
                                    (reg proc)
                                    (reg argl)])))))
                after-call))))


(define (compile-proc-appl target linkage)
    (cond 
        [(and (eq? target 'val) (not (eq? linkage 'return)))
            (make-instruction-sequence  ; 修改的寄存器凭什么多于需要的寄存器呢？
                '(proc)
                all-regs
                '([assign continue (label ,linkage)]
                  [assign val (op compiled-procedure-entry) (reg proc)]
                  [goto (reg val)]))]
        [(and (not (eq? target 'val) (not eq? linkage 'return)))
            (let ([proc-return (make-label 'proc-return)]) 
            (make-instruction-sequences
                '(proc)
                all-regs
                '([assign continue (label ,proc-return)]
                  [assign val (op compiled-procedure-entry) (reg proc)]
                  [goto (reg val)]
                  'proc-return
                  [assign ,target (reg val)]
                  [goto (label ,linkage)])))]
        [(and (eq? target 'val) (eq? linkage 'return)) 
            (make-instruction-sequence
                '(proc continue)
                all-regs
                '([assign val (op compiled-procedure-entry) (reg proc)]
                  [goto (reg val)]))]
        ; 只有在编译过程时才需要 return 链接，过程总是在 val 返回他们的值
        [(and (not (eq? target 'val)) (eq? linkage 'return)))
            (error "return linkage, target not val -- COMPILE" target)]))



; 需要确实结果序列所需要和修改的寄存器的集合
(define (append-instruction-sequences . seqs) 
    (define (append-2-sequences seq1 seq2)
        (make-instruction-sequence
            (list-union (registers-needs seq1) ; 需要的寄存器集合,
                       ; 被序列 1 修改过的寄存器就已经被隐式初始化了，所以要从 seq2.needed 中过滤掉
                       (list-difference (registers-needed seq2)
                                        (registers-modified seq1)))
            (list-unon (registers-modified seq1)
                       (registers-modified seq2))
            (append (statements seq1) (statements seq2))))
    (define (append-seq-list seqs)
        (if (null? seqs)
            (empty-instruction-sequence)
            (append-2-sequences (car seqs)
                                (append-seq-list (cdr seqs)))))
    (append-seq-list seqs))


(define (list-union s1 s2)
    (cond [(null? s1) s2]
          [(memq (car s1) (list-union (cdr s1) s2))]
          [else (cons (car s1) (list-union (cdr s1) s2))]))

(define (list-difference s1 s2)
    (cond [(null? s1) '()]
          [(memq (car s1) s2) (list-difference (cdr s1) s2)]
          [else (cons (cdr s1) (list-difference (cdr s1) s2))]))




(define (preserving regs seq1 seq2)
    (if (null? regs)
        (append-instruction-sequences seq1 seq2)
        (let ([first-reg (car regs)]) 
            (if (and (needs-register? seq2 first-reg)
                     (modifies-register? seq1 first-reg))
                (preserving 
                    (cdr regs)
                    (make-instruction-sequence
                        ; seq1 需要添加 save first-reg, 所以添加 first-reg 到 registers-needed 中
                        (list-union (list first-reg)
                                    (registers-needed seq1))
                        (list-difference (registers-modified seq1)
                                         (list first-reg))
                        (append '([save ,first-reg])
                                (statements seq1)
                                '([restore ,first-reg])))
                    seq2)
                (preserving (cdr regs) seq1 seq2))))


; 这里对寄存器各种修改，添加的分析都是为了 preserving 中的方便 save 和 restore ?
(define (tack-on-instruction-sequence seq body-seq)
    (make-instruction-sequence
        (registers-needed seq)
        (registers-modified seq)
        (append (statements seq) (statements body-seq))))


; 合成后的指令序列  seq1 或者 seq2 都可能被执行到
; 所以组合后的指令序列的寄存器应该是 seq1 和 seq2 的寄存器的并集
(define (parallel-instruction-sequences seq1 seq2)
    (make-instruction-sequence
        (list-union (registers-needed seq1) (registers-needed seq2))
        (list-union (registers-modified seq1) (registers-modified seq2))))


