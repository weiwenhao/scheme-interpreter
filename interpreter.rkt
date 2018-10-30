(define (my-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) ; 定义lambda 也就是求值 (lambda (x) (+ 1 x)) 类似这样的语句
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (my-eval (cond->if exp) env))
        ((application? exp) ; 求值复合表达式 例如(+ (+ 1 2) 2)
         (my-apply (my-eval (operator exp) env) ; 运算符部分求值
                (list-of-values (operands exp) env))) ; 运算对象部分求值. 求值序的必然结果
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure) ; +
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure) ; 自定义的procedure 也就是通过求值lambda的得到的procedure
         (eval-sequence
          (procedure-body procedure) ; body
          (extend-environment ; env
           (procedure-parameters procedure) ; key list ;形参
           arguments ; value list. 实参
           (procedure-environment procedure)))) ; 指向的框架环境
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env)))) ; rest 为剩余部分, 既 cdr的意思


(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))


(define (eval-sequence exps env)
  (cond ((last-exp? exps) (my-eval (first-exp exps) env))
        (else (my-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

; 确定exp的开头是否为某个tag
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; 条件if (if (pred?) (true) (false)))
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; begin
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

; 用于cond->if,把一个序列变换为一个表达式. 既把剩余的表示式添加begin进行继续操作
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

; set!形式 (set! var 123)
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp) ; get key
                       (my-eval (assignment-value exp) env) ; get value and eval
                       env) ; env
  'ok)

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


; definition 定义的value部分可以是内部数据类型或者是lambda
; (define test 123)
; (define test (lambda (arg) <body>)

(define (definition? exp)
  (tagged-list? exp 'define))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (my-eval (definition-value exp) env)
    env)
  'ok)


(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)  ; 适用于 (define test 123)
      (caadr exp))) ; 适用于 (define (test arg1) <body>)

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters  (define (test arg1) <body>) => (define test (lambda (arg1) <body))
                   (cddr exp)))) ; body 

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body))) ; 指明数据类型为l



; appliaction 是任意复合表达式,这种表达式的car是运算符,其cdr是运算对象的表 (add 1 2)
(define (application? exp) (pair? exp)) ; application的过滤放在最后,(any xxx) 剩余的形式都可以理解为application形式

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

; 派生表达式实现cond ((cond? exp) (eval (cond->if exp) env))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

; 谓词检测
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

; 基本过程 
; (apply-primitive-procedure <proc> <args>) 能够将给定的过程应用于表<args>里的参数值, 并返回这一应用的结果
; (primitive-procedure? <proc>) 检查<proc>是否为一个基本过程


; lambda 表达式是由符号lambda开始的表  (lambda (arg1 arg2) <body>)

;((lambda? exp) ; 定义lambda 也就是求值 (lambda (x) (+ 1 x)) 类似这样的语句
;         (make-procedure (lambda-parameters exp)
;                         (lambda-body exp)
;                         env))
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

; 复合过程 复合过程由形式参数,过程体和环境,通过构造函数make-procedure制作 (lambda (arg1 arg2) (display 1))
;=> (procedure (arg1 arg2) (display 1) env)
(define (make-procedure parameters body env)
  (list 'procedure parameters body env)) ; 添加了标识符procedure

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

; 将环境表示为一个框架的表. 一个环境的外围环境就是这个表的cdr. 空环境则直接用空表表示
; 框架属于环境. 一个环境由框架 + 外围环境组成. 外围环境可以为空
(define (enclosing-environment env) (cdr env)) ; 外围环境

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

; 扩展环境
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments" vars vals)
          (error "Too few arguments" vars vals))))

; 查找变量 在下面的表示中,求职器为了找到一个给定变量的约束,可能需要搜索许多个框架,这种方式称为深约束.
; 避免这一低效性的方法是采用一种称为语法作用域的策略
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) ; 当前环境的框架为null既 '()
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable constraint" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; 修改变量
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val)) ; 修改
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; 设置变量. 始终操作给定环境的第一个框架
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

; 每个基础过程名必须有一个约束,以便当eval求值一个应用基本过程的运算符时,可以找到相应的对象,并将这个对象传给apply
; 为此我们必须创建一个初始环境,在其中建立起基本过程的名字与一个唯一对象的关联
;(cond ((primitive-procedure? procedure)
;         (apply-primitive-procedure procedure arguments))

; 定义基本过程
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'pair? pair?)
        (list 'display display)
        (list '> >)
        (list 'eq? eq?)
        (list '* *)
        (list '- -)
        (list '+ +)))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define true #t)
(define false #f)

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names) ; primitive-rocedure-names 是一个无参过程, '(+ - * true? eq? ...)
                             (primitive-procedure-objects) ; 与之对应的过程 (list + - true? eq? ...)
                             the-empty-environment))) ; '() 空的环境
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

; 获取proc部分 (primitive + ??)
(define (primitive-implementation proc) (cadr proc))



(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))


; i/o 交互
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (my-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))
(driver-loop)