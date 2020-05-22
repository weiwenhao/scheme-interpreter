eval-dispatch
    (test (op self-evaluating?) (reg exp))
    (branch (label ev-self-eval))
    (test (op variable?) (reg exp))
    (branch (label ev-variale))
    (test (op quoted?) (reg exp))
    (branch (label ev-quoted))
    (test (op assignment?) (reg exp))
    (branch (label ev-assignment))
    (test (op definition?) (reg exp))
    (branch (op definition?) (reg exp))
    (test (op if?) (reg exp))
    (branch (label ev-if))
    (test (op lambda?) (reg exp))
    (branch (label ev-lambda))
    (test (op begin?) (reg exp))
    (branch (label ev-begin))
    (test (op application?) (reg exp)) ; 调用过程  (+ 1 2) (abs -1)
    (branch (label ev-application))
    (goto (label unknown-expression-type))

; 简单表达式求值
ev-self-eval
    (assign val (reg exp))
    (goto (reg continue))
ev-variable
    (assign val (op lookup-variable-value) (reg exp) (reg env))
    (goto (reg continue))
ev-quoted
    (assign val (op text-of-quotation) (reg exp))
    (goto (reg continue))
ev-lambda
    (assign unev (op lambda-parameters) (reg exp))
    (assign exp (op lambda-body) (reg exp))
    (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
    (goto (reg continue))

; 过程应用的求值 (+ 1 2)
ev-application
    (save continue)
    (save env)
    (assign unev (op operands) (reg exp)) ; 运算对象
    (save unev)
    ; 运算符 (对运算符求值) , exp 用于保存被求值的表达式, eval-dispatch 会去分析它
    (assign exp (op operator) (reg exp)) 
    (assign continue (label ev-appl-did-operator))
    (goto (label eval-dispatch))

ev-appl-did-operator
    (resotre unev) ; 尚未求值的参数
    (restore env) ; 从队列中取出一个值并赋给 env
    (assign arg1 (op empty-arglist)) ; 初始化运算对象表
    (assign proc (reg val)) ; 运算符求值的结果
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    (save proc)  ; 这里则继续执行 (save arg1)
ev-appl-operand-loop
    (save arg1)
    (assign exp (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    ; 最后一次求值所需 exp 已经放入到 register exp 中
    (branch (label ev-appl-last-arg))
    (save env)
    (save unev)
    (assign continue (label ev-appl-accumulate-arg))
    (goto (label eval-dipatch))

ev-appl-accumulate-arg
    (restore unev)
    (resotre env)
    (restore arg1)
    (assign arg1 (op adjoin-arg) (reg val) (reg arg1)) ; 一个寄存器真的只能保存一个值吗？？？
    (assign unev (op rest-operands) (reg unev))
    (goto (label ev-appl-operand-loop))

; 相比 ev-appl-operand-loop 少了 save arg1 和 save env save unev 这些入栈操作
ev-appl-last-arg
    (assign continue (label ev-appl-accum-last-arg))
    (goto (label eval-dispatch))

ev-appl-accum-last-arg
    (restore arg1)
    (assign arg1 (op adjoin-arg) (reg val) (reg arg1))
    (resotre proc)
    (goto (label apply-dipatch))


; apply-dispatch 有两种情况， 即基本过程 + / - 等， 和组合过程(即自定义过程)
apply-dispatch
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-apply))
    (test (op compound-procedure?) (reg proc))
    (branch (label compound-apply))
    (goto (label unknownprocedure-type))

primitive-apply
    assign val (op apply-primitive-procedure) (reg proc) (reg arg1)
    (resotre continue)
    (goto (reg continue))

compound-apply
    (assign unev (op procedure-parameters) (reg proc)) ; unev 应该是形参了
    ; 这里应该是整个外界环境，但是不知道从何而来呀,小小寄存器如何保存整个外界环境呢
    (assign env (op procedure-evironment) (reg proc)) 
    (assign env (op extend-environment) （reg unev) (reg argl) (reg env)) ; 通过参数扩充环境
    (assign unev (op procedure-body) (reg proc))
    (goto (label ev-sequence))

ev-begin
    (assign unev (op begin-actions) (reg exp))
    (save continue)
    (goto (label ev-sequence))

; 最后一条表达式不需要 save unev 和 save env
; 这对过程调用 （尤其是尾递归） 来说能节省一次栈空间的使用
; 因为对于最后一条表达式，其只需要根据 env 进行求值，求值完成后(结果保存在 val)
; 其求值的结果无论是是么对于本次过程调用内部的 ev-sequence 已经没有任何意义了
; 直接去往 continue 继续进行其他的求值即可
ev-sequence
    (assign exp (op first-exp) (reg unev))
    (test （op last-exp?) (reg unev))
    (branch (label ev-sequence-last-exp))
    (save unev)
    (save env)
    (assign continue (label ev-sequence-continue))
    (goto (label eval-dispatch))
ev-sequence-continue
    (resotre env)
    (resotre unev)
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-sequence))
ev-sequence-last-exp
    ; restore continue 使得最后一条表达式完成求值后，就可以直接继续后续的操作
    ; 而不需要再回到 ev-sequence-continue 来徒增多余的判断
    (restore continue)
    (goto (label eval-dispatch))


ev-if
   （save exp)
    (save env) ; env 感觉就是个密， 其值到底是如何变化的, 其中的值又是如何扩展的？
    (save continue)
    (assign continue (label ev-if-decide))
    (assign exp (op if-perdicate) (reg exp))
    (goto (label eval-dispatch))

ev-if-decide
    (restore continue)
    (restore env)
    (resotre exp)
    (test (op true?) (reg val))
    (branch (label ev-if-consequent)) ; 原来有继续执行机制
ev-if-alternative
    (assign exp (op if-alternative) (reg exp))
    (goto (label eval-dispatch))
ev-if-consequent
    (assign exp (op if-consequent) (reg exp))
    (goto (label eval-dispatch))

ev-assignment
    (assign unev (op assign-variable) （reg exp))
    (save unev)
    (assign exp (op assignment-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-assignment-1))
    (goto (label eval-dispatch)) ; evaluate the assignment value
ev-assignment-1
    (resotre continue)
    (restore env)
    (restore unev)
    (perform (op set-variable-value!) (reg unev) (reg val) (reg env)) ; 这里是在干嘛？
    (assign val (const ok))
    (goto (reg continue))


ev-definition
    (assign unev (op definition-variable) (reg exp))
    (save unev) ; save variable for later
    (assign exp (op definition-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-definition-1))
    (goto (label eval-dipatch))
ev-definition-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform (op define-variable!) (reg unev) (reg val) (reg env)) ; 应该是扩展 env, env 只是字符串都有点不合适了吧
    (assign val (const ok))
    (goto (reg continue))


read-eval-print-loop
    (perform (op initialize-stack))
    (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
    (assign exp (op read))
    (assign env (op get-global-environment))
    (assign continue (label print-result))
    (goto (label eval-dispatch))

print-result
    (perform (op announce-output) (const ";;; EC-Eval value:"))
    (perform (op user-print) (reg val))
    (goto (label read-eval-print-loop))

unknown-expression-type
    (assign val (scont unknown-expression-type-error))
    (goto (label signal-error))

unkonwn-procedure-type
    (restore continue)
    (assign val (const unkonwn-procedure-type-error))
    (goto (label signal-error))

signal-error
    (perform (op user-print) (reg val))
    (goto (label read-eval-print-loop))