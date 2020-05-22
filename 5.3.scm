(
begin-garbage-collection
    (assign free (const 0))
    (assign scan (sconst 0))
    (assign old (reg root)) ; 首先重新分配寄存器 root 中的值到寄存器 old
    (assign relocate-continue (label reassign-root))
    ; relocate-old-result-in-new 从 register.old 获取需要移动的对象的指针
    ; 将重新分配后的对象的地址放入到 resiger.new 中
    (goto (label relocate-old-result-in-new))
reassign-root
    (assign root (req new))
    (goto (label gc-loop))

; 迁移主要分为的两个操作
; 1. 把整个 pair 对象原封不动的迁移过来即 
   ; the-cars.old => new-cars.new / the-cdrs.old => new.cdrs.new
; 2. update new-cars.new 和 new-cdrs.new 中的值
gc-loop
    (test (op = ) (reg scan) (reg free))
    (branch (label gc-flip))
    ; (op vector-ref) (reg new-cars) (reg scan) 找到 new-cars.scan 的值
    (assign old (op vector-ref) (reg new-cars) (reg scan)) 
    (assign relocate-continue (label update-car))
    (goto (label relocate-old-result-in-new))

; 修改在 the-cadr 中的指针下标到 new-cadr(只能在引用的值已经迁移确定后才行, 所以这算是深度优先遍历了) 
update-car
    ; 将 new-cars[reg.scan] 设置为 register.new
    (perform (op vector-set!) (reg new-cars) (reg scan) (reg new))
    (assign old (op vector-ref) (reg new-cdrs) (reg scan))
    (assign relocate-continue (label upate-cdr))
    (goto (label relocate-old-result-in-new))
update-cdr
    (perform (op vector-set!) (reg new-cdrs) (reg scan) (reg new))
    ; relocate-old-result-in-new 迁移过来一组或者2组 pair, scan 就是继续把迁移过来的 pair 中的 car 和 cdr 进行继续修改
    (assign scan (op +) (reg scan) (const 1)) 
    (goto (label gc-loop))

; cdr 如何保存指针？ '(2 3) [2|point] point = '(3) [3|'()]
; 扫描指针 scan(其实就是下标) 指向的是一个本身已经移入新存储区的对象，但它的 car 和 cdr指针仍然指着老存储区里的对象
; ?? 这事一种什么结构？？
; 需要从 the-cars, the-cdrs, 和 new-cars 和 new-cdrs 来理解
; the-car 中可以保存两种类型的值(下标(p5)或者字符(n4), 下标也可以称为指针)
relocate-old-result-in-new
    (test (op pointer-to-pair?) (reg old))
    (branch (label pair))
    (assign new (reg old))
    (goto (reg relocate-continue))

pair
    ; the-cdr 在塞满的一刻起，其之前的空间其实都已经被占用完全
    ; 所以才需要通过 root 来确定可及还是不可及
    (assign oldcr (op vector-ref) (reg the-cars) (reg old))
    (test (op broken-heart?) (reg oldcr))
    (branch (bale already-moved))
    ;; old 检测到还有未移动的, old
    (assign new (reg free)) ; new localtion for pair new 此时是一个指向空 car/cdr 的 下标
    ;; update free pointer , free 在前面跑？ scan 在后面追？
    (assign free (op +) (reg free) (const 1))
    ;; copy the car and cdr to new memory
    (perform (op vector-set!) (reg new-cars) (reg new) (reg oldcr))
    (assign oldcr (op vectory-ref) (reg the-cdrs) (reg old))
    (perform (op vector-set!) (reg new-cdrs) (reg new) (reg oldcr))

    ;; construct ht broken heard to old
    (perform (op vector-set!) (reg the-cars) (reg old) (const broken-heart))
    (perform (op vector-set!) (reg the-cdrs) (reg old) (reg new)) ;; 这里的 new 是新的存储下标 
    (goto (reg relocate-continue))
already-moved
    (assign new (op vector-ref) (reg the-cdrs) (reg old))
    (goto (reg relocate-continue))

gc-flip
    (assign temp (reg the-cdrs))
    (assign the-cdrs (reg new-cdrs))
    (assign new-cdrs (reg temp))
    (assign temp (reg the-cars))
    (assign the-cars (reg new-cars))
    (assign new-cars (reg temp)) 
)
